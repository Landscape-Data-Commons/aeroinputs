# utils.R — Internal helpers shared across aeroinputs functions.
# None of these are exported; they are used via direct calls within the package.

#' @importFrom utils write.csv write.table
NULL

# Suppress R CMD check notes for column names and tidy-eval pronouns used
# throughout the package.
utils::globalVariables(c(
  ".SD", ".data",
  "BareSoil", "Gap", "Latitude_NAD83", "Longitude_NAD83",
  "PK_texture", "PrimaryKey", "ProjectKey", "RecType",
  "SoilTexture", "clay", "ini_location", "sand"
))

# ------------------------------------------------------------------------------
# Messaging helpers
# ------------------------------------------------------------------------------

#' Format elapsed time as a human-readable string
#'
#' @param start_time A POSIXct start time (from `Sys.time()`).
#' @param end_time   A POSIXct end time. Defaults to `Sys.time()`.
#' @return A character string such as `"4.3 seconds"` or `"2 minutes 7.1 seconds"`.
#' @keywords internal
format_elapsed_time <- function(start_time, end_time = Sys.time()) {
  elapsed <- as.numeric(difftime(end_time, start_time, units = "secs"))

  if (elapsed < 60) {
    return(sprintf("%.1f seconds", elapsed))
  }

  minutes <- floor(elapsed / 60)
  seconds <- elapsed %% 60
  sprintf("%d minutes %.1f seconds", minutes, seconds)
}

#' Emit a progress message only when `verbose = TRUE`
#'
#' @param ... Arguments passed to [message()].
#' @param verbose Logical; when `FALSE` the message is suppressed.
#' @return Invisibly `NULL`.
#' @keywords internal
progress_message <- function(..., verbose = TRUE) {
  if (isTRUE(verbose)) message(...)
  invisible(NULL)
}

# ------------------------------------------------------------------------------
# Infix operator
# ------------------------------------------------------------------------------

#' Negated `%in%` operator
#'
#' @param x     A vector of values to test.
#' @param table A vector to match against.
#' @return A logical vector.
#' @keywords internal
`%notin%` <- function(x, table) !x %in% table

# ------------------------------------------------------------------------------
# Caching helpers
# ------------------------------------------------------------------------------

#' Load a named object from an `.RData` file without polluting the global env
#'
#' @param path        Path to the `.RData` file.
#' @param object_name Character string - the name of the object to extract.
#' @return The object stored under `object_name`.
#' @keywords internal
load_cached_object <- function(path, object_name) {
  e <- new.env(parent = emptyenv())
  load(path, envir = e)
  if (!exists(object_name, envir = e)) {
    stop("Cache file '", path, "' does not contain object '", object_name, "'.")
  }
  get(object_name, envir = e)
}

# ------------------------------------------------------------------------------
# Data-frame helpers
# ------------------------------------------------------------------------------

#' Remove rows with duplicate PrimaryKey values, keeping the first occurrence
#'
#' @param df A data frame with a `PrimaryKey` column.
#' @return The de-duplicated data frame.
#' @keywords internal
dedup_by_pk <- function(df) {
  df[!duplicated(df$PrimaryKey), ]
}

#' Safely filter a data frame to a set of final keys
#'
#' Returns an empty [tibble::tibble()] when `df` is `NULL`, empty, lacks a
#' `PrimaryKey` column, or when `final_keys` is empty.
#'
#' @param df         A data frame to filter.
#' @param final_keys Character vector of PrimaryKeys to keep.
#' @param table_name Label used in the progress message.
#' @param verbose    Passed to [progress_message()].
#' @return A filtered data frame (or an empty tibble).
#' @keywords internal
safe_filter_by_final_keys <- function(df, final_keys, table_name, verbose = TRUE) {
  progress_message(
    "Filtering dataset ", table_name, " with ", nrow(df),
    " rows to final_keys (", length(final_keys), " keys)...",
    verbose = verbose
  )

  if (is.null(df) || nrow(df) == 0) return(tibble::tibble())
  if (!"PrimaryKey" %in% names(df))  return(tibble::tibble())
  if (is.null(final_keys) || length(final_keys) == 0) return(tibble::tibble())

  df |> dplyr::filter(PrimaryKey %in% final_keys)
}

# ------------------------------------------------------------------------------
# Non-public key logging helpers (used by fetch_ldc_data)
# ------------------------------------------------------------------------------

#' Append non-public PrimaryKeys to a CSV log file
#'
#' Creates the file with a header on first write; appends on subsequent calls.
#'
#' @param keys      Character vector of PrimaryKeys.
#' @param data_type Name of the dataset being fetched.
#' @param file      Path to the CSV log file.
#' @return Invisibly `NULL`.
#' @keywords internal
append_nonpublic_csv <- function(keys, data_type, file) {
  if (length(keys) == 0) return(invisible(NULL))

  ts <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")

  df <- data.frame(
    timestamp   = rep(ts,        length(keys)),
    data_type   = rep(data_type, length(keys)),
    primary_key = keys,
    stringsAsFactors = FALSE
  )

  if (file.exists(file)) {
    write.table(df, file, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
  } else {
    write.csv(df, file, row.names = FALSE)
  }

  invisible(NULL)
}

#' Extract PrimaryKeys from a trex non-public-data warning message
#'
#' trex emits warnings of the form:
#' `"Some keys were not associated with publicly-available data. ... following keys: k1,k2"`.
#' This function parses those keys out.
#'
#' @param warn_msg A character string (the warning message).
#' @return A character vector of PrimaryKeys (may be empty).
#' @keywords internal
extract_nonpublic_keys <- function(warn_msg) {
  pattern <- "Some keys were not associated with publicly-available data\\."
  if (!grepl(pattern, warn_msg, fixed = FALSE)) return(character())

  m     <- regexec("following keys:\\s*(.*)$", warn_msg)
  parts <- regmatches(warn_msg, m)[[1]]
  if (length(parts) < 2) return(character())

  keys <- trimws(unlist(strsplit(parts[2], ",")))
  keys[nzchar(keys)]
}
