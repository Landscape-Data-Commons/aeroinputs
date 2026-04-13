#' Fetch SOLUS 100 soil property rasters
#'
#' Downloads SOLUS 100 soil-property layers from cloud storage, optionally
#' crops/reprojects them to a user-supplied spatial object, and returns either
#' a `SpatRaster`, a `SoilProfileCollection`, or both.
#'
#' SOLUS rasters are large and hosted remotely. On the first call the full
#' raster will be streamed; subsequent calls with the same `filename` will use
#' the on-disk file if `overwrite = FALSE`. For workflows that need many
#' subsets, download once with `return = "grid"` and then crop locally.
#'
#' @param x            Optional spatial object used to crop/reproject the
#'   output. Accepted types: `SpatRaster`, `SpatVector`, `sf`, `SpatialPoints*`,
#'   `RasterLayer`, `RasterStack`, or `SoilProfileCollection`.
#' @param depth_slices Numeric vector of depth slices (cm) to include.
#'   Available values: `0, 5, 15, 30, 60, 100, 150`.
#' @param variables    Character vector of SOLUS property names to include.
#'   Defaults to all 20 available properties.
#' @param output_type  Character vector of output types to include. One or more
#'   of `"prediction"`, `"relative prediction interval"`,
#'   `"95% low prediction interval"`, `"95% high prediction interval"`.
#' @param return       `"grid"` (default), `"spc"`, or `"both"`.
#' @param samples      Integer; number of cells to sample when `return` includes
#'   `"spc"` and `x` is not a point object. `NULL` returns all cells.
#' @param method       Interpolation method for SPC construction. One of
#'   `"linear"`, `"constant"`, `"fmm"`, `"periodic"`, `"natural"`,
#'   `"monoH.FC"`, `"hyman"`, `"step"`, `"slice"`.
#' @param max_depth    Maximum depth (cm) for interpolation. Default `151`.
#' @param filename     Optional path for writing the raster to disk.
#' @param overwrite    Logical; overwrite `filename` if it already exists.
#' @param verbose      Logical; emit progress messages.
#'
#' @return
#'   - `return = "grid"`: a `terra::SpatRaster`.
#'   - `return = "spc"`: an `aqp::SoilProfileCollection`.
#'   - `return = "both"`: a named list `list(grid = ..., spc = ...)`.
#'
#' @seealso [fetch_ldc_data()], [generate_aero_inputs()]
#' @export
fetch_solus <- function(
  x = NULL,
  depth_slices = c(0, 5, 15, 30, 60, 100, 150),
  variables = c(
    "anylithicdpt", "caco3",     "cec7",       "claytotal",  "dbovendry",
    "ec",           "ecec",      "fragvol",     "gypsum",     "ph1to1h2o",
    "resdept",      "sandco",    "sandfine",    "sandmed",    "sandtotal",
    "sandvc",       "sandvf",    "sar",         "silttotal",  "soc"
  ),
  output_type = c(
    "prediction",
    "relative prediction interval",
    "95% low prediction interval",
    "95% high prediction interval"
  ),
  return    = c("grid", "spc", "both"),
  samples   = NULL,
  method    = c("linear", "constant", "fmm", "periodic", "natural", "monoH.FC", "hyman", "step", "slice"),
  max_depth = 151,
  filename  = NULL,
  overwrite = FALSE,
  verbose   = TRUE
) {
  start_time <- Sys.time()
  on.exit(
    progress_message(
      "fetch_solus completed in ", format_elapsed_time(start_time),
      verbose = verbose
    ),
    add = TRUE
  )

  progress_message("Starting SOLUS retrieval...", verbose = verbose)

  method <- rlang::arg_match(method)
  return <- rlang::arg_match(return)

  progress_message("Reading SOLUS index...", verbose = verbose)
  solus_index <- .get_solus_index()

  progress_message("Filtering requested layers...", verbose = verbose)
  index_subset <- .subset_solus_index(
    solus_index  = solus_index,
    variables    = variables,
    depth_slices = depth_slices,
    output_type  = output_type
  )
  progress_message("Selected ", nrow(index_subset), " layer(s).", verbose = verbose)

  progress_message("Building SOLUS raster...", verbose = verbose)
  raster <- .build_solus_raster(index_subset)

  if (!missing(x) && !is.null(x)) {
    progress_message("Preparing raster for supplied spatial input...", verbose = verbose)
  }

  raster <- .prepare_solus_output_raster(
    raster    = raster,
    x         = x,
    filename  = filename,
    overwrite = overwrite
  )

  # Write to disk when filename is supplied and x was not provided
  # (when x is provided, terra::project/crop already writes via filename above)
  if (!is.null(filename) && (missing(x) || is.null(x))) {
    out_dir <- dirname(filename)
    if (!dir.exists(out_dir)) {
      dir.create(out_dir, recursive = TRUE)
      progress_message("Created directory: ", out_dir, verbose = verbose)
    }
    progress_message("Writing raster to disk...", verbose = verbose)
    raster <- terra::writeRaster(raster, filename = filename, overwrite = overwrite)
    progress_message("Raster written to: ", filename, verbose = verbose)
  }

  if (return == "grid") {
    progress_message("Returning grid output.", verbose = verbose)
    return(raster)
  }

  if (length(depth_slices) == 1 && method != "step") {
    stop(
      "Cannot interpolate SoilProfileCollection output with only one depth slice. ",
      "Use method = \"step\" or provide multiple depth slices.",
      call. = FALSE
    )
  }

  progress_message("Extracting raster values...", verbose = verbose)
  extracted_data <- .extract_solus_values(raster = raster, x = x, samples = samples)
  progress_message("Extracted ", nrow(extracted_data), " record(s).", verbose = verbose)

  progress_message("Converting extracted values to SoilProfileCollection...", verbose = verbose)
  spc <- .convert_solus_dataframe_to_spc(
    x         = extracted_data,
    idname    = "ID",
    method    = method,
    max_depth = max_depth
  )

  aqp::initSpatial(spc, terra::crs(raster)) <- ~ x + y

  if (return == "spc") {
    progress_message("Returning SoilProfileCollection output.", verbose = verbose)
    return(spc)
  }

  progress_message("Returning both grid and SoilProfileCollection outputs.", verbose = verbose)
  list(grid = raster, spc = spc)
}

# ------------------------------------------------------------------------------
# Internal helpers (not exported)
# ------------------------------------------------------------------------------

#' Download and parse the SOLUS HTML index from Google Cloud Storage
#'
#' @return A data frame describing all available SOLUS layers.
#' @keywords internal
.get_solus_index <- function() {
  solus_url <- "https://storage.googleapis.com/solus100pub/index.html"
  raw_index <- rvest::read_html(solus_url) |>
    rvest::html_table(header = FALSE) |>
    purrr::pluck(1)

  colnames(raw_index) <- raw_index[5, ]

  depth_lookup <- c(
    all_cm   = "all",
    `0_cm`   = "0",
    `5_cm`   = "5",
    `15_cm`  = "15",
    `30_cm`  = "30",
    `60_cm`  = "60",
    `100_cm` = "100",
    `150_cm` = "150"
  )

  raw_index |>
    dplyr::slice(-c(1:5, dplyr::n())) |>
    dplyr::mutate(
      depth       = dplyr::if_else(is.na(.data$depth) | .data$depth == "", "all_cm", .data$depth),
      depth_slice = factor(unname(depth_lookup[.data$depth]), levels = unique(depth_lookup))
    )
}

#' Filter the SOLUS index to the requested variables, depths, and output types
#'
#' @param solus_index  Data frame returned by [.get_solus_index()].
#' @param variables    Character vector of property names.
#' @param depth_slices Numeric vector of depth slices.
#' @param output_type  Character vector of output types.
#' @return A filtered data frame.
#' @keywords internal
.subset_solus_index <- function(solus_index, variables, depth_slices, output_type) {
  depth_filter <- as.character(c("all", depth_slices))

  solus_index |>
    dplyr::filter(
      .data$property %in% variables,
      as.character(.data$depth_slice) %in% depth_filter,
      .data$filetype %in% output_type
    ) |>
    dplyr::mutate(
      subproperty = stringr::str_remove(.data$filename, "\\.tif$"),
      scalar      = as.numeric(.data$scalar)
    )
}

#' Build a SpatRaster from the filtered SOLUS index using VSICURL streaming
#'
#' Scale/offset metadata is applied so extracted values are in original units.
#'
#' @param index_subset Data frame returned by [.subset_solus_index()].
#' @return A `terra::SpatRaster`.
#' @keywords internal
.build_solus_raster <- function(index_subset) {
  if (nrow(index_subset) == 0) {
    stop("No SOLUS layers matched the requested filters.", call. = FALSE)
  }

  raster <- terra::rast(paste0("/vsicurl/", index_subset$url))
  terra::scoff(raster) <- cbind(1 / index_subset$scalar, 0)
  raster
}

#' Prepare SOLUS raster output for a user-supplied spatial target
#'
#' When `x` is a `SpatRaster` the source is projected and resampled to match;
#' for vector inputs (or `sf`/`SpatVector`) it is cropped to the extent.
#'
#' @param raster    A `SpatRaster` from [.build_solus_raster()].
#' @param x         User-supplied spatial object (or `NULL`).
#' @param filename  Optional output file path.
#' @param overwrite Logical.
#' @return A `SpatRaster`.
#' @keywords internal
.prepare_solus_output_raster <- function(raster, x, filename = NULL, overwrite = FALSE) {
  if (missing(x) || is.null(x)) return(raster)

  x_prepared <- .normalize_solus_input(x = x, target_crs = terra::crs(raster))
  .validate_solus_extent(raster = raster, x = x_prepared)

  if (inherits(x_prepared, "SpatRaster")) {
    return(terra::project(
      raster,
      x_prepared,
      filename   = filename,
      overwrite  = overwrite,
      align_only = FALSE,
      mask       = TRUE,
      threads    = TRUE
    ))
  }

  terra::crop(raster, x_prepared, filename = filename, overwrite = overwrite)
}

#' Convert supported spatial input types to a terra class
#'
#' Handles `SoilProfileCollection`, legacy `Raster*`, `sf`, and plain
#' `SpatVector`. Always reprojects to `target_crs`.
#'
#' @param x          A spatial object.
#' @param target_crs CRS string (WKT or PROJ) to project vectors into.
#' @return A `SpatRaster` or `SpatVector`.
#' @keywords internal
.normalize_solus_input <- function(x, target_crs) {
  if (inherits(x, "SoilProfileCollection")) x <- methods::as(x, "sf")
  if (inherits(x, c("RasterLayer", "RasterStack"))) x <- terra::rast(x)
  if (!inherits(x, c("SpatRaster", "SpatVector")))  x <- terra::vect(x)
  if (inherits(x, "SpatVector")) x <- terra::project(x, target_crs)
  x
}

#' Validate that `x` overlaps the SOLUS raster extent
#'
#' @param raster A `SpatRaster`.
#' @param x      A `SpatRaster` or `SpatVector` already in the raster's CRS.
#' @return Invisibly `TRUE` (or stops with an error).
#' @keywords internal
.validate_solus_extent <- function(raster, x) {
  target_extent <- terra::ext(terra::project(terra::as.polygons(x, ext = TRUE), raster))
  source_extent <- terra::ext(raster)

  is_valid <- terra::relate(source_extent, target_extent, relation = "contains")[1] ||
    terra::relate(source_extent, target_extent, relation = "overlaps")[1]

  if (!is_valid) {
    stop("Extent of `x` is outside the boundaries of the SOLUS source data.", call. = FALSE)
  }

  invisible(TRUE)
}

#' Extract values from a SOLUS SpatRaster
#'
#' Routes to point extraction, regular sampling, or full data-frame conversion
#' depending on the arguments supplied.
#'
#' @param raster  A `SpatRaster`.
#' @param x       Optional `SpatVector` of points.
#' @param samples Optional integer; number of cells to sample.
#' @return A data frame with an `ID` column added.
#' @keywords internal
.extract_solus_values <- function(raster, x = NULL, samples = NULL) {
  if (!missing(x) && !is.null(x) && inherits(x, "SpatVector") && terra::is.points(x)) {
    dat <- terra::extract(raster, x)
  } else if (!missing(samples) && !is.null(samples)) {
    dat <- terra::spatSample(raster, size = samples, method = "regular", xy = TRUE)
  } else {
    dat <- terra::as.data.frame(raster, xy = TRUE, na.rm = FALSE)
  }

  dat$ID <- seq_len(nrow(dat))
  dat
}

#' Convert a data frame of extracted SOLUS values to a SoilProfileCollection
#'
#' Layer names are parsed to recover depth tokens. Output is then routed to
#' either step/slice or interpolation-based SPC construction.
#'
#' @param x         Data frame from [.extract_solus_values()].
#' @param idname    Column name to use as profile ID.
#' @param method    Interpolation method (validated by [rlang::arg_match()]).
#' @param max_depth Maximum depth in cm.
#' @return An `aqp::SoilProfileCollection`.
#' @keywords internal
.convert_solus_dataframe_to_spc <- function(x, idname = "ID", method, max_depth = 151) {
  method <- rlang::arg_match(
    method,
    values = c("slice", "step", "linear", "constant", "fmm", "periodic", "natural", "monoH.FC", "hyman")
  )

  extract_top_depth     <- function(nms) stringr::str_replace(nms, "^.*_(\\d+|all)_cm_.*$", "\\1")
  strip_depth_from_name <- function(nms) stringr::str_replace(nms, "^(.*)_(\\d+|all)_cm(.*)$", "\\1\\3")

  top_depth   <- extract_top_depth(colnames(x))
  colnames(x) <- strip_depth_from_name(colnames(x))

  horizon_data <- data.table::rbindlist(
    lapply(unique(top_depth[!top_depth %in% c("x", "y", "all", idname)]), function(depth_value) {
      data.frame(
        ID    = x[[idname]],
        depth = depth_value,
        x[which(top_depth == depth_value)],
        check.names = FALSE
      )
    })
  )

  site_data <- data.frame(
    ID = x[[idname]],
    x[top_depth %in% c("x", "y", "all")],
    check.names = FALSE
  )

  horizon_data$depth  <- as.numeric(horizon_data$depth)
  horizon_data        <- horizon_data[order(horizon_data$ID, horizon_data$depth), ]

  stepwise_bottom <- c(
    `0`   = 0, 
    `5`   = 15, 
    `15`  = 30, 
    `30`  = 60, 
    `60`  = 100, 
    `100` = 150, 
    `150` = max_depth
  )

  horizon_data$top    <- horizon_data$depth
  horizon_data$bottom <- unname(stepwise_bottom[as.character(horizon_data$depth)])

  id_vars     <- names(horizon_data) %in% c(idname, "depth", "x", "y", "top", "bottom")
  value_names <- names(horizon_data)[!id_vars]

  if (method %in% c("slice", "step")) {
    return(.build_step_or_slice_spc(horizon_data, site_data, idname, method, max_depth))
  }

  .build_interpolated_spc(horizon_data, site_data, idname, method, max_depth, value_names)
}

#' Build a step or slice SoilProfileCollection
#'
#' @param horizon_data Data frame of horizon-level records.
#' @param site_data    Data frame of site-level records.
#' @param idname       Profile ID column name.
#' @param method       `"slice"` or `"step"`.
#' @param max_depth    Maximum depth (cm).
#' @return An `aqp::SoilProfileCollection`.
#' @keywords internal
.build_step_or_slice_spc <- function(horizon_data, site_data, idname, method, max_depth) {
  if (method == "slice") {
    horizon_data$bottom <- horizon_data$top + 1
  } else {
    message(
      "SOLUS predictions represent depth slices. ",
      "Consider using method = \"slice\", \"constant\", or \"linear\"."
    )
    horizon_data$bottom[horizon_data$bottom == 0]    <- 5
    horizon_data$bottom[horizon_data$top   == 150]   <- max_depth
  }

  horizon_data <- as.data.frame(horizon_data)
  aqp::depths(horizon_data) <- stats::setNames(c(idname, "top", "bottom"), NULL)
  aqp::site(horizon_data)   <- site_data
  horizon_data
}

#' Build an interpolated SoilProfileCollection
#'
#' @param horizon_data Data frame of horizon-level records.
#' @param site_data    Data frame of site-level records.
#' @param idname       Profile ID column name.
#' @param method       Interpolation method.
#' @param max_depth    Maximum depth (cm).
#' @param value_names  Names of columns to interpolate.
#' @return An `aqp::SoilProfileCollection`.
#' @keywords internal
.build_interpolated_spc <- function(horizon_data, site_data, idname, method, max_depth, value_names) {
  min_depth            <- min(horizon_data$top, na.rm = TRUE)
  max_depth_observed   <- max(horizon_data$bottom, na.rm = TRUE)
  if (max_depth_observed == 150) max_depth_observed <- max_depth

  interpolation_fun <- if (method %in% c("linear", "constant")) stats::approxfun else stats::splinefun

  interpolation_depths <- min_depth:(max_depth_observed - 1)
  source_depths        <- unique(horizon_data$top)

  interpolated <- horizon_data[
    ,
    data.frame(
      top    = min_depth:(max_depth_observed - 1),
      bottom = (min_depth + 1):max_depth_observed,
      lapply(.SD, function(values) {
        if (sum(!is.na(values)) <= 1) return(rep(NA_real_, length(interpolation_depths)))
        interpolation_fun(source_depths, values, method = method)(interpolation_depths)
      })
    ),
    .SDcols = value_names,
    by = list(ID = horizon_data[[idname]])
  ]

  interpolated <- as.data.frame(interpolated)
  aqp::depths(interpolated) <- stats::setNames(c(idname, "top", "bottom"), NULL)
  aqp::site(interpolated)   <- site_data
  interpolated
}
