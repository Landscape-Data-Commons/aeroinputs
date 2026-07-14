#' Compare AERO output files between two pipeline runs
#'
#' Checks for differences in the per-plot `.ini` configuration files and
#' canopy-gap `.txt` files produced by [generate_aero_inputs()]. Useful for
#' validating that a code change does not alter model inputs beyond floating-
#' point rounding.
#'
#' The function compares:
#' \itemize{
#'   \item **File inventory** — which plots are present in each run (common,
#'     only-old, only-new).
#'   \item **INI numeric fields** — `soil_sand_fraction`, `soil_clay_fraction`,
#'     `veg_cover_fraction`, and `veg_mean_height` are compared numerically;
#'     text fields (`wind_location`, `gap_obsv`) are compared as strings.
#'   \item **Gap TXT files** — gap-distance values are compared element-wise.
#' }
#'
#' @param old_dir Character string. Path to the `aero_inputdata/` directory
#'   produced by the **previous** pipeline run.
#' @param new_dir Character string. Path to the `aero_inputdata/` directory
#'   produced by the **new** pipeline run.
#' @param ini_tol Numeric. Absolute tolerance for INI numeric fields. Diffs
#'   smaller than this are not flagged as changes. Defaults to `1e-4`.
#' @param verbose Logical. If `TRUE` (default), prints a summary to the
#'   console.
#'
#' @return A named list (invisibly) with elements:
#'   \describe{
#'     \item{`ini_summary`}{Data frame with per-field statistics across all
#'       common plots: number of plots, number changed, max absolute diff,
#'       and mean diff.}
#'     \item{`ini_changes`}{Data frame of individual plot-field pairs where
#'       `|diff| > ini_tol`. Empty if all fields are within tolerance.}
#'     \item{`txt_detail`}{Data frame with one row per common gap TXT file:
#'       number of values, whether files are identical, and max absolute diff.}
#'     \item{`txt_summary`}{Single-row data frame summarising gap file
#'       agreement across all plots.}
#'     \item{`only_old_ini`}{Character vector of INI files present only in
#'       `old_dir`.}
#'     \item{`only_new_ini`}{Character vector of INI files present only in
#'       `new_dir`.}
#'     \item{`only_old_txt`}{Character vector of TXT files present only in
#'       `old_dir`.}
#'     \item{`only_new_txt`}{Character vector of TXT files present only in
#'       `new_dir`.}
#'   }
#'
#' @seealso [generate_aero_inputs()]
#' @export
compare_aero_outputs <- function(old_dir,
                                 new_dir,
                                 ini_tol = 1e-4,
                                 verbose = TRUE) {

  old_dir <- gsub("\\\\", "/", old_dir)
  new_dir <- gsub("\\\\", "/", new_dir)

  if (!dir.exists(old_dir)) stop("old_dir does not exist: ", old_dir)
  if (!dir.exists(new_dir)) stop("new_dir does not exist: ", new_dir)

  msg <- function(...) if (verbose) cat(...)

  # ---- helpers ---------------------------------------------------------------

  parse_ini <- function(path) {
    lines <- readLines(path, warn = FALSE)
    # Drop section headers and blank lines
    lines <- lines[!grepl("^\\[|^\\s*$", lines)]
    # Split on first ": " only, preserving multi-token values (e.g. wind_location)
    keys <- trimws(sub(":.*", "", lines))
    vals <- trimws(sub("^[^:]+:\\s*", "", lines))
    setNames(as.list(vals), keys)
  }

  parse_txt <- function(path) {
    vals <- suppressWarnings(as.numeric(readLines(path, warn = FALSE)))
    vals[!is.na(vals)]  # drop blank/non-numeric lines
  }

  # Attempt numeric coercion; return NA (silently) for text values like paths
  # or multi-token fields (e.g. "32.6 -106.7" for wind_location).
  try_numeric <- function(x) {
    x <- trimws(x)
    if (grepl("\\s", x)) return(NA_real_)  # multi-token → not a scalar numeric
    suppressWarnings(as.numeric(x))
  }

  # ---- file inventory --------------------------------------------------------

  old_ini <- list.files(old_dir, pattern = "\\.ini$")
  new_ini <- list.files(new_dir, pattern = "\\.ini$")
  old_txt <- list.files(file.path(old_dir, "gap"), pattern = "\\.txt$")
  new_txt <- list.files(file.path(new_dir, "gap"), pattern = "\\.txt$")

  only_old_ini <- setdiff(old_ini, new_ini)
  only_new_ini <- setdiff(new_ini, old_ini)
  common_ini   <- intersect(old_ini, new_ini)
  only_old_txt <- setdiff(old_txt, new_txt)
  only_new_txt <- setdiff(new_txt, old_txt)
  common_txt   <- intersect(old_txt, new_txt)

  msg(sprintf(
    "INI  \u2014 common: %d | only-old: %d | only-new: %d\n",
    length(common_ini), length(only_old_ini), length(only_new_ini)
  ))
  msg(sprintf(
    "TXT  \u2014 common: %d | only-old: %d | only-new: %d\n\n",
    length(common_txt), length(only_old_txt), length(only_new_txt)
  ))

  if (length(only_old_ini) > 0)
    msg("  Plots only in old: ", paste(only_old_ini, collapse = ", "), "\n")
  if (length(only_new_ini) > 0)
    msg("  Plots only in new: ", paste(only_new_ini, collapse = ", "), "\n")

  # ---- compare INI fields ----------------------------------------------------

  ini_rows <- do.call(rbind, lapply(common_ini, function(f) {
    old_kv <- parse_ini(file.path(old_dir, f))
    new_kv <- parse_ini(file.path(new_dir, f))
    all_keys <- union(names(old_kv), names(new_kv))

    do.call(rbind, lapply(all_keys, function(k) {
      ov_raw <- old_kv[[k]]
      nv_raw <- new_kv[[k]]
      ov_num <- if (is.null(ov_raw)) NA_real_ else try_numeric(ov_raw)
      nv_num <- if (is.null(nv_raw)) NA_real_ else try_numeric(nv_raw)
      is_numeric_field <- !is.na(ov_num) && !is.na(nv_num)

      data.frame(
        file           = f,
        field          = k,
        old_value      = if (is.null(ov_raw)) NA_character_ else ov_raw,
        new_value      = if (is.null(nv_raw)) NA_character_ else nv_raw,
        numeric_field  = is_numeric_field,
        diff           = if (is_numeric_field) nv_num - ov_num else NA_real_,
        stringsAsFactors = FALSE
      )
    }))
  }))


  # Per-field summary — numeric fields only (text fields like gap_obsv excluded)
  numeric_ini <- ini_rows[ini_rows$numeric_field & !is.na(ini_rows$diff), ]
  ini_summary <- do.call(rbind, lapply(
    split(numeric_ini, numeric_ini$field),
    function(d) {
      data.frame(
        field        = d$field[[1]],
        n_plots      = nrow(d),
        n_changed    = sum(abs(d$diff) > ini_tol, na.rm = TRUE),
        max_abs_diff = max(abs(d$diff), na.rm = TRUE),
        mean_diff    = mean(d$diff, na.rm = TRUE),
        stringsAsFactors = FALSE
      )
    }
  ))
  rownames(ini_summary) <- NULL

  # Text field mismatches
  text_ini <- ini_rows[!ini_rows$numeric_field, ]
  text_mismatches <- text_ini[
    !is.na(text_ini$old_value) &
    !is.na(text_ini$new_value) &
    text_ini$old_value != text_ini$new_value,
  ]

  changed_ini <- ini_rows[
    !is.na(ini_rows$diff) & abs(ini_rows$diff) > ini_tol,
  ]

  # ---- compare TXT (gap) files -----------------------------------------------

  txt_rows <- do.call(rbind, lapply(common_txt, function(f) {
    ov <- parse_txt(file.path(old_dir, "gap", f))
    nv <- parse_txt(file.path(new_dir, "gap", f))
    len_match <- length(ov) == length(nv)
    data.frame(
      file         = f,
      n_old        = length(ov),
      n_new        = length(nv),
      identical    = len_match && isTRUE(all.equal(ov, nv)),
      max_abs_diff = if (len_match) max(abs(nv - ov)) else NA_real_,
      stringsAsFactors = FALSE
    )
  }))

  txt_summary <- data.frame(
    n_files       = nrow(txt_rows),
    n_identical   = sum(txt_rows$identical),
    n_length_diff = sum(txt_rows$n_old != txt_rows$n_new),
    n_value_diff  = sum(!txt_rows$identical & txt_rows$n_old == txt_rows$n_new),
    max_abs_diff  = if (all(is.na(txt_rows$max_abs_diff))) NA_real_
                    else max(txt_rows$max_abs_diff, na.rm = TRUE)
  )

  # ---- print results ---------------------------------------------------------

  msg("=== INI numeric field summary ===\n")
  msg(paste(capture.output(print(ini_summary, row.names = FALSE)), collapse = "\n"), "\n\n")

  if (nrow(changed_ini) > 0) {
    msg(sprintf(
      "=== INI plots with |diff| > %g (showing up to 10) ===\n", ini_tol
    ))
    top10 <- head(changed_ini[order(-abs(changed_ini$diff)), ], 10)
    msg(paste(capture.output(print(top10, row.names = FALSE)), collapse = "\n"), "\n\n")
  } else {
    msg(sprintf(
      "All INI numeric fields identical within tolerance (%g).\n\n", ini_tol
    ))
  }

  if (nrow(text_mismatches) > 0) {
    msg("=== INI text field mismatches ===\n")
    msg(paste(capture.output(print(text_mismatches, row.names = FALSE)), collapse = "\n"), "\n\n")
  }

  msg("=== TXT (gap) file summary ===\n")
  msg(paste(capture.output(print(txt_summary, row.names = FALSE)), collapse = "\n"), "\n\n")

  if (any(!txt_rows$identical)) {
    changed_txt <- txt_rows[!txt_rows$identical, ]
    msg("=== TXT files that differ (showing up to 10) ===\n")
    msg(paste(
      capture.output(print(head(changed_txt, 10), row.names = FALSE)),
      collapse = "\n"
    ), "\n")
  }

  invisible(list(
    ini_summary  = ini_summary,
    ini_changes  = changed_ini,
    txt_detail   = txt_rows,
    txt_summary  = txt_summary,
    only_old_ini = only_old_ini,
    only_new_ini = only_new_ini,
    only_old_txt = only_old_txt,
    only_new_txt = only_new_txt
  ))
}

