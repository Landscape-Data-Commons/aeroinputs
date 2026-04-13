#' Fetch LDC datasets for AERO input preparation
#'
#' Retrieves LDC datasets in a dependency chain (e.g. `gap -> height -> lpi ->
#' indicators`), filters to plots with a `BareSoil` value, aligns all tables to
#' the common set of `PrimaryKey`s, and optionally writes "Tall" CSV files to
#' disk. The outputs feed directly into [generate_aero_inputs()].
#'
#' @param token Optional LDC token (character) passed to `trex::fetch_ldc()`.
#'   When `NULL`, trex will attempt default authentication.
#' @param primary_keys Optional character vector of `PrimaryKey`s to use
#'   directly. When supplied, `excluded_project_keys` and `project_keys` are
#'   ignored. The header is still loaded/fetched (using the cache if available)
#'   and filtered to these keys for downstream spatial joins.
#' @param project_keys Optional character vector of `ProjectKey` values. When
#'   supplied, all `PrimaryKey`s belonging to those projects are used and
#'   `excluded_project_keys` is ignored. Ignored when `primary_keys` is
#'   supplied.
#' @param excluded_project_keys Character vector of `ProjectKey` values to
#'   exclude. Ignored when `primary_keys` or `project_keys` is supplied.
#'   Defaults to excluding all projects whose key starts with `"NWERN_"` and
#'   `"CFO_USGS"`. Pass `character(0)` to disable all exclusions.
#' @param chain_types Character vector of dataset names to fetch in order.
#' @param n_rec Optional integer; limit the number of `PrimaryKey`s processed
#'   (useful for testing). `NULL` uses all available keys.
#' @param n_offset Integer offset into the `PrimaryKey` list (1-based).
#'   Default `0` (no offset).
#' @param base_dir Base directory where the header cache and outputs are stored.
#' @param out_dir Output "Tall" directory. Defaults to
#'   `file.path(base_dir, "Tall")`.
#' @param header_cache_file Path to the header cache `.RData` file. If the file
#'   does not exist, the header is fetched from the API and saved here.
#' @param nonpublic_file Path to a CSV log file for non-public keys encountered
#'   during fetching.
#' @param base_url Base API URL passed to `trex::fetch_ldc()`.
#' @param write_out Logical; write CSV outputs to disk when `TRUE`.
#' @param verbose Logical; emit progress messages when `TRUE`.
#'
#' @return A named list with elements:
#'   \describe{
#'     \item{`data`}{Named list of data frames: `gap`, `height`, `lpi`,
#'       `indicators`, `header`.}
#'     \item{`final_keys`}{Character vector of `PrimaryKey`s present in all
#'       fetched datasets.}
#'     \item{`out_dir`}{Path to the output directory.}
#'   }
#'
#' @seealso [generate_aero_inputs()], [fetch_solus()]
#' @export
fetch_ldc_data <- function(
  token = NULL,
  primary_keys = NULL,
  project_keys = NULL,
  excluded_project_keys = NULL,
  chain_types = c("gap", "height", "lpi", "indicators"),
  n_rec = NULL,
  n_offset = 0,
  base_dir = file.path(tempdir(), "AERO_test"),
  out_dir = file.path(base_dir, "Tall"),
  header_cache_file = file.path(base_dir, "header.RData"),
  nonpublic_file = file.path(out_dir, "nonpublic_primarykeys.csv"),
  base_url = "https://devapi.landscapedatacommons.org/api/v1/",
  write_out = TRUE,
  verbose = TRUE
) {
  start_time <- Sys.time()
  on.exit(
    progress_message(
      "fetch_ldc_data completed in ", format_elapsed_time(start_time),
      verbose = verbose
    ),
    add = TRUE
  )

  progress_message("Starting LDC fetch workflow...", verbose = verbose)

  dir.create(base_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(out_dir,  recursive = TRUE, showWarnings = FALSE)

  # ---- Header: load from cache or fetch from API ----------------------------

  if (!is.null(primary_keys)) {
    # User supplied PrimaryKeys: highest priority, ignores project_keys and excluded_project_keys
    progress_message(
      "primary_keys supplied (", length(primary_keys),
      "); skipping project_keys and excluded_project_keys filter.",
      verbose = verbose
    )
    primary_keys <- unique(primary_keys[!is.na(primary_keys)])

    if (length(primary_keys) == 0) {
      stop("No valid primary_keys provided after removing NAs and duplicates.")
    }

    n_rec <- if (!is.null(n_rec) && !is.na(n_rec)) {
      min(n_rec, length(primary_keys))
    } else {
      length(primary_keys)
    }
    header <- .load_or_fetch_header(header_cache_file, token, base_url, verbose)

    stopifnot(all(c("ProjectKey", "PrimaryKey") %in% names(header)))
    header <- header |>
      dplyr::filter(PrimaryKey %in% primary_keys)

    progress_message(
      "Header filtered to ", nrow(header), " rows matching supplied primary_keys.",
      verbose = verbose
    )

  } else if (!is.null(project_keys)) {
    # User supplied ProjectKeys: fetch all PrimaryKeys belonging to those projects
    progress_message(
      "project_keys supplied (", length(project_keys),
      "); skipping excluded_project_keys filter.",
      verbose = verbose
    )
    project_keys <- unique(project_keys[!is.na(project_keys)])

    if (length(project_keys) == 0) {
      stop("No valid project_keys provided after removing NAs and duplicates.")
    }

    header <- .load_or_fetch_header(header_cache_file, token, base_url, verbose)
    stopifnot(all(c("ProjectKey", "PrimaryKey") %in% names(header)))

    header <- header |>
      dplyr::filter(ProjectKey %in% project_keys)

    if (nrow(header) == 0) {
      stop("No PrimaryKeys found for the supplied project_keys: ",
           paste(project_keys, collapse = ", "))
    }

    primary_keys <- header$PrimaryKey
    n_rec <- if (!is.null(n_rec) && !is.na(n_rec)) {
      min(n_rec, length(primary_keys))
    } else {
      length(primary_keys)
    }

    progress_message(
      "Header filtered to ", nrow(header), " rows matching supplied project_keys.",
      verbose = verbose
    )

  } else {
    # Normal path: load/fetch header then apply project key exclusion.
    # Default exclusion: all NWERN_* projects and CFO_USGS.
    header <- .load_or_fetch_header(header_cache_file, token, base_url, verbose)
    stopifnot(all(c("ProjectKey", "PrimaryKey") %in% names(header)))

    mandatory_exclusions <- c(
      "CFO_USGS",
      grep("^NWERN_", unique(header$ProjectKey), value = TRUE)
    )

    if (is.null(excluded_project_keys)) {
      excluded_project_keys <- mandatory_exclusions
      progress_message(
        "Using default excluded_project_keys (NWERN_* + CFO_USGS): ",
        paste(excluded_project_keys, collapse = ", "),
        verbose = verbose
      )
    } else {
      excluded_project_keys <- unique(c(excluded_project_keys, mandatory_exclusions))
      progress_message(
        "Merging user-supplied excluded_project_keys with mandatory exclusions (NWERN_* + CFO_USGS): ",
        paste(excluded_project_keys, collapse = ", "),
        verbose = verbose
      )
    }

    header_filtered <- header |>
      dplyr::filter(ProjectKey %notin% excluded_project_keys)

    primary_keys <- header_filtered$PrimaryKey
    n_rec <- if (!is.null(n_rec) && !is.na(n_rec)) {
      min(n_rec, length(primary_keys))
    } else {
      length(primary_keys)
    }
  }

  # ---- Apply offset and record limit ----------------------------------------

  if (!is.null(n_offset) && !is.na(n_offset) && n_offset > 0 && n_offset <= n_rec) {
    primary_keys <- primary_keys[n_offset:n_rec]
    progress_message("PrimaryKeys after applying n_offset and n_rec: ", 
      length(primary_keys), verbose = verbose)
  } else {
    progress_message("n_offset not applied. Using first ", n_rec, " PrimaryKeys.", 
      verbose = verbose)
    primary_keys <- primary_keys[seq_len(n_rec)]
  }

  # ---- Validate keys and coordinates ----------------------------------------

  num_na <- sum(is.na(primary_keys))
  if (num_na > 0) {
    progress_message("Removing ", num_na, " PrimaryKeys with NA values.", verbose = verbose)
    primary_keys <- primary_keys[!is.na(primary_keys)]
  } else {
    progress_message("No NA PrimaryKeys found.", verbose = verbose)
  }

  if (!is.null(header)) {
    num_na_coords <- sum(is.na(header$Longitude_NAD83) | is.na(header$Latitude_NAD83))
    if (num_na_coords > 0) {
      progress_message("Removing ", num_na_coords, " rows with NA coordinate values.", 
        verbose = verbose)
      header <- header[!is.na(header$Longitude_NAD83) & !is.na(header$Latitude_NAD83), ]
    } else {
      progress_message("No NA coordinate values found in header.", verbose = verbose)
    }
  }

  progress_message("PrimaryKeys to be used in fetch chain: ", length(primary_keys), 
    verbose = verbose)

  # ---- Fetch chain ----------------------------------------------------------

  chain_res <- fetch_chain(
    chain         = chain_types,
    start_keys    = primary_keys,
    token         = token,
    nonpublic_log = nonpublic_file,
    base_url      = base_url,
    verbose       = verbose
  )

  message("Fetch chain completed. Final PrimaryKeys: ", length(chain_res$final_keys))

  gap        <- chain_res$data[["gap"]]
  height     <- chain_res$data[["height"]]
  lpi        <- chain_res$data[["lpi"]]
  indicators <- chain_res$data[["indicators"]]

  # ---- Filter to plots with BareSoil ----------------------------------------

  geo_indicators <- indicators
  if (nrow(geo_indicators) > 0 && "BareSoil" %in% names(geo_indicators)) {
    geo_indicators <- geo_indicators |> 
      dplyr::filter(!is.na(BareSoil))
  } else if (nrow(geo_indicators) == 0) {
    progress_message("Indicators table empty; no BareSoil filtering applied.", 
      verbose = verbose)
  } else {
    progress_message("Indicators table has no 'BareSoil' column; skipping BareSoil filter.", 
      verbose = verbose)
  }

  final_keys <- if (nrow(geo_indicators) > 0) geo_indicators$PrimaryKey else character()
  progress_message("Number of final_keys with BareSoil assignment: ", length(final_keys), 
    verbose = verbose)

  # ---- Align all tables to final_keys ---------------------------------------

  header_aero <- safe_filter_by_final_keys(header, final_keys, "header", verbose = verbose)
  gap         <- safe_filter_by_final_keys(gap,    final_keys, "gap",    verbose = verbose)
  height      <- safe_filter_by_final_keys(height, final_keys, "height", verbose = verbose)
  lpi         <- safe_filter_by_final_keys(lpi,    final_keys, "lpi",    verbose = verbose)

  # ---- Write outputs --------------------------------------------------------

  if (write_out) {
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
    readr::write_csv(gap,            file.path(out_dir, "gap_tall.csv"))
    readr::write_csv(height,         file.path(out_dir, "height_tall.csv"))
    readr::write_csv(lpi,            file.path(out_dir, "lpi_tall.csv"))
    readr::write_csv(geo_indicators, file.path(out_dir, "geoIndicators.csv"))
    readr::write_csv(header_aero,    file.path(out_dir, "header.csv"))
    progress_message("Wrote outputs to: ", out_dir, verbose = verbose)
  }

  invisible(list(
    data = list(
      gap        = gap,
      height     = height,
      lpi        = lpi,
      indicators = geo_indicators,
      header     = header_aero
    ),
    final_keys = final_keys,
    out_dir    = out_dir
  ))
}

# ------------------------------------------------------------------------------
# Internal orchestration helpers (not exported)
# ------------------------------------------------------------------------------

#' Load header from cache or fetch it from the LDC API
#'
#' @param cache_file Path to the `.RData` cache.
#' @param token      API token (may be `NULL`).
#' @param base_url   LDC API base URL.
#' @param verbose    Passed to [progress_message()].
#' @return A data frame of header records.
#' @keywords internal
.load_or_fetch_header <- function(cache_file, token, base_url, verbose) {
  if (!file.exists(cache_file)) {
    progress_message("Header cache not found; fetching header from LDC...", verbose = verbose)
    t0     <- Sys.time()
    header <- trex::fetch_ldc(data_type = "header", token = token, 
      verbose = verbose, base_url = base_url)
    progress_message("Header fetched in ", format_elapsed_time(t0), verbose = verbose)
    save(header, file = cache_file)
  } else {
    progress_message("Loading header from cache: ", cache_file, verbose = verbose)
    header <- load_cached_object(cache_file, "header")
  }
  header
}

#' Fetch a single dataset for a set of PrimaryKeys
#'
#' Calls `trex::fetch_ldc()`, captures non-public key warnings, de-duplicates
#' the result, and optionally logs the non-public keys to a CSV file.
#'
#' @param data_type     Name of the dataset (e.g. `"gap"`).
#' @param keys          Character vector of `PrimaryKey`s to request.
#' @param token         API token (may be `NULL`).
#' @param nonpublic_log Path to a CSV log file (may be `NULL`).
#' @param base_url      LDC API base URL.
#' @param verbose       Passed to [progress_message()].
#' @return A named list with elements `data` (data frame) and `keys`
#'   (character vector of unique `PrimaryKey`s present in the response).
#' @keywords internal
fetch_dataset <- function(data_type, keys, token = NULL, nonpublic_log = NULL, 
  base_url = NULL, verbose = TRUE) 
{
  if (length(keys) == 0) {
    warning("No keys provided for data_type='", data_type, "'. Returning empty.")
    return(list(data = tibble::tibble(), keys = character()))
  }

  warned_keys <- character()

  dat <- withCallingHandlers(
    trex::fetch_ldc(
      data_type = data_type,
      keys      = keys,
      key_type  = "PrimaryKey",
      token     = token,
      base_url  = base_url
    ),
    warning = function(w) {
      newly_found <- extract_nonpublic_keys(conditionMessage(w))
      if (length(newly_found) > 0) {
        warned_keys <<- unique(c(warned_keys, newly_found))
      }
      invokeRestart("muffleWarning")
    },
    error = function(e) {
      stop("Error fetching data_type='", data_type, "': ", conditionMessage(e))
    }
  )

  if (!"PrimaryKey" %in% names(dat)) {
    stop("Returned data for '", data_type, "' does not include a PrimaryKey column.")
  }

  target_dat <- dedup_by_pk(dat)

  if (!is.null(nonpublic_log) && length(warned_keys) > 0) {
    progress_message(
      "Warning: ", length(warned_keys), " non-public keys encountered for data_type='",
      data_type, "'. Logging to: ", nonpublic_log,
      verbose = verbose
    )
    append_nonpublic_csv(keys = unique(warned_keys), data_type = data_type, file = nonpublic_log)
  } else if (length(warned_keys) > 0) {
    progress_message(
      "Warning: ", length(warned_keys), " non-public keys encountered for data_type='",
      data_type, "'. No log file specified.",
      verbose = verbose
    )
  } else {
    progress_message("No non-public keys encountered for data_type='", data_type, "'.", 
      verbose = verbose)
  }

  list(data = dat, keys = target_dat$PrimaryKey)
}

#' Run a chained sequence of dataset fetches
#'
#' Each step uses the `PrimaryKey`s returned by the prior step, progressively
#' narrowing to the intersection of available keys across all datasets.
#'
#' @param chain         Character vector of dataset names in fetch order.
#' @param start_keys    Character vector of starting `PrimaryKey`s.
#' @param token         API token (may be `NULL`).
#' @param nonpublic_log Path to a non-public key log CSV (may be `NULL`).
#' @param base_url      LDC API base URL.
#' @param verbose       Passed to [progress_message()].
#' @return A named list with elements `data` (named list of data frames) and
#'   `final_keys` (character vector).
#' @keywords internal
fetch_chain <- function(chain, start_keys, token = NULL, nonpublic_log = NULL, 
  base_url = NULL, verbose = TRUE) 
{
  out  <- list()
  keys <- start_keys

  progress_message("---- Starting fetch chain ----",                       verbose = verbose)
  progress_message("Initial PrimaryKeys: ", length(keys),                  verbose = verbose)
  progress_message("Datasets to fetch: ", paste(chain, collapse = " -> "), verbose = verbose)
  progress_message("--------------------------------",                     verbose = verbose)

  for (dt in chain) {
    if (length(keys) == 0) {
      progress_message("No more keys available. Stopping chain early.", verbose = verbose)
      break
    }

    progress_message("Fetching data_type = '", dt, "'",              verbose = verbose)
    progress_message("Requesting ", length(keys), " PrimaryKeys...", verbose = verbose)

    t0  <- Sys.time()
    res <- fetch_dataset(
      data_type     = dt,
      keys          = keys,
      token         = token,
      nonpublic_log = nonpublic_log,
      base_url      = base_url,
      verbose       = verbose
    )

    out[[dt]] <- res$data
    keys      <- res$keys

    elapsed <- round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 2)
    progress_message("Returned unique PrimaryKeys: ", length(keys),    verbose = verbose)
    progress_message("Rows fetched: ",                nrow(res$data),  verbose = verbose)
    progress_message("Elapsed time: ",                elapsed, " sec", verbose = verbose)
    progress_message("--------------------------------",               verbose = verbose)
  }

  progress_message("\n---- Fetch chain completed ----", verbose = verbose)
  progress_message("Final PrimaryKeys: ", length(keys), verbose = verbose)

  list(data = out, final_keys = keys)
}
