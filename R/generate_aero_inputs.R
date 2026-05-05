#' Build AERO input files from LDC "Tall" exports and a soil texture raster
#'
#' Converts LDC tall tables (`gap`, `height`, `lpi`, `header`,
#' `geoIndicators`) and a soil-texture raster into the AERO-ready inputs:
#' per-plot gap `.txt` files, `.ini` configuration files, and a combined
#' `input_data.csv` summary. Spatial operations use `terra` and `sf`
#' throughout (no legacy `raster`/`sp` dependency).
#'
#' @param data_dir    Directory that contains a `Tall/` sub-folder with the
#'   five CSV files produced by [fetch_ldc_data()]:
#'   `header.csv`, `gap_tall.csv`, `height_tall.csv`, `lpi_tall.csv`, and
#'   `geoIndicators.csv`.
#' @param output_dir  Directory where output files are written. Defaults to
#'   `file.path(data_dir, "aero_inputdata")`.
#' @param texture_file Path to a single-file or multi-layer GeoTIFF whose
#'   layer names include `"sand"` and `"clay"` (percent, not fraction).
#'   Typically the file produced by [fetch_solus()].
#' @param write_out   Logical; when `TRUE` writes gap `.txt`, `.ini`, and
#'   `input_data.csv` to `output_dir`. Default `TRUE`.
#' @param verbose     Logical; control progress messages. Default `TRUE`.
#'
#' @return A named list with elements:
#'   \describe{
#'     \item{`input_data`}{A data frame combining plot texture, bare-soil
#'       fraction, maximum height, and canopy gap per plot (or `NULL` when
#'       `write_out = FALSE`).}
#'     \item{`plots_texture`}{A data frame of plot coordinates with extracted
#'       sand/clay fractions.}
#'     \item{`files_written`}{Character vector of paths written to disk (empty
#'       when `write_out = FALSE`).}
#'   }
#'
#' @seealso [fetch_ldc_data()], [fetch_solus()]
#' @export
generate_aero_inputs <- function(
  data_dir,
  output_dir   = file.path(data_dir, "aero_inputdata"),
  texture_file = file.path(data_dir, "soil_texture_w_sand_frac.tif"),
  write_out    = TRUE,
  verbose      = TRUE
) {
  start_time <- Sys.time()
  on.exit(
    progress_message(
      "generate_aero_inputs completed in ", format_elapsed_time(start_time),
      verbose = verbose
    ),
    add = TRUE
  )

  # Normalise path separators (Windows safety)
  data_dir     <- gsub("\\\\", "/", data_dir)
  output_dir   <- gsub("\\\\", "/", output_dir)
  texture_file <- gsub("\\\\", "/", texture_file)

  # ---- Locate input CSVs ----------------------------------------------------

  files <- list(
    header = file.path(data_dir, "Tall", "header.csv"),
    gap    = file.path(data_dir, "Tall", "gap_tall.csv"),
    height = file.path(data_dir, "Tall", "height_tall.csv"),
    lpi    = file.path(data_dir, "Tall", "lpi_tall.csv"),
    geoind = file.path(data_dir, "Tall", "geoIndicators.csv")
  )

  missing_files <- names(files)[!file.exists(unlist(files))]
  if (length(missing_files) > 0) {
    missing_paths <- unlist(files)[!file.exists(unlist(files))]
    missing_info  <- paste(missing_files, "(", missing_paths, ")", sep = "")
    stop("Missing required files: ", paste(missing_info, collapse = ", "))
  }

  progress_message("All required files found. Proceeding with data processing...", verbose = verbose)

  # ---- Read CSVs ------------------------------------------------------------

  header      <- readr::read_csv(files$header, show_col_types = FALSE)
  gap_tall    <- readr::read_csv(files$gap,    show_col_types = FALSE)
  height_tall <- readr::read_csv(files$height, show_col_types = FALSE)
  lpi_tall    <- readr::read_csv(files$lpi,    show_col_types = FALSE)  # nolint (lpi_tall reserved for future use)
  geoind      <- readr::read_csv(files$geoind, show_col_types = FALSE)

  # Drop plots without coordinates
  header <- header |>
    dplyr::filter(!is.na(Longitude_NAD83) & !is.na(Latitude_NAD83))

  # Restrict tall tables to keys present in the filtered header
  gap_tall    <- gap_tall    |> dplyr::filter(PrimaryKey %in% header$PrimaryKey)
  height_tall <- height_tall |> dplyr::filter(PrimaryKey %in% header$PrimaryKey)

  # ---- Load and validate texture raster (terra, not raster) ----------------

  progress_message("Loading texture raster: ", texture_file, verbose = verbose)
  tex_stack <- terra::rast(texture_file)

  # Identify sand/clay layers by name
  if (terra::nlyr(tex_stack) > 2) {
    names(tex_stack) <- tolower(names(tex_stack))
    if (all(c("sand", "clay") %in% names(tex_stack))) {
      tex_stack <- tex_stack[[c("sand", "clay")]]
    } else {
      stop(
        "Texture raster has more than 2 layers and cannot identify sand/clay layers by name. ",
        "Ensure layer names contain 'sand' and 'clay', or supply a 2-layer raster."
      )
    }
  }

  if (terra::nlyr(tex_stack) == 2 && !all(c("sand", "clay") %in% names(tex_stack))) {
    names(tex_stack) <- ifelse(
      grepl("sand", names(tex_stack), ignore.case = TRUE), "sand",
      ifelse(grepl("clay", names(tex_stack), ignore.case = TRUE), "clay", names(tex_stack))
    )
  }

  if (!all(c("sand", "clay") %in% names(tex_stack))) {
    stop(
      "Texture raster does not have the required 'sand' and 'clay' layers. ",
      "Ensure layer names include 'sand' and 'clay'."
    )
  }

  # ---- Spatial extraction (sf + terra only) ---------------------------------

  # Build sf points from header (NAD83)
  sf_pts <- sf::st_as_sf(
    header,
    coords = c("Longitude_NAD83", "Latitude_NAD83"),
    crs    = 4269,
    remove = FALSE
  )

  # Reproject to the raster's CRS for accurate extraction
  sf_pts_proj <- sf::st_transform(sf_pts, terra::crs(tex_stack))

  # Extract sand/clay at each point using terra::extract
  extracted <- terra::extract(tex_stack, terra::vect(sf_pts_proj))
  plots_texture <- cbind(sf::st_drop_geometry(sf_pts), extracted)
  plots_texture <- plots_texture |> dplyr::filter(!is.na(sand))

  # Convert percent to fraction
  plots_texture <- plots_texture |>
    dplyr::mutate(
      sand = sand / 100,
      clay = clay / 100
    )

  # Reproject point geometry (built from NAD83 lon/lat) to WGS84
  sf_tex <- sf_pts |>
    dplyr::semi_join(plots_texture |> dplyr::select(PrimaryKey), by = "PrimaryKey") |>
    sf::st_transform(4326)

  # Extract WGS84 coordinates from geometry and join back to attributes
  wgs84_coords <- sf::st_coordinates(sf_tex)
  wgs84_lookup <- tibble::tibble(
    PrimaryKey      = sf_tex$PrimaryKey,
    Longitude_WGS84 = wgs84_coords[, 1],
    Latitude_WGS84  = wgs84_coords[, 2]
  )

  # Safeguard: fail if a PrimaryKey maps to multiple coordinate pairs
  coord_conflicts <- wgs84_lookup |>
    dplyr::summarise(
      n_coords = dplyr::n_distinct(
        paste0(round(Longitude_WGS84, 10), ",", round(Latitude_WGS84, 10))
      ),
      .by = PrimaryKey
    ) |>
    dplyr::filter(n_coords > 1)

  if (nrow(coord_conflicts) > 0) {
    bad_keys <- coord_conflicts$PrimaryKey
    bad_keys_msg <- paste(utils::head(bad_keys, 10), collapse = ", ")
    if (length(bad_keys) > 10) {
      bad_keys_msg <- paste0(bad_keys_msg, ", ...")
    }

    stop(
      "Some PrimaryKey values map to multiple WGS84 coordinate pairs: ",
      bad_keys_msg,
      ". Resolve duplicated coordinates before proceeding."
    )
  }

  plots_texture <- plots_texture |>
    dplyr::left_join(
      wgs84_lookup |>
        dplyr::distinct(PrimaryKey, .keep_all = TRUE),
      by = "PrimaryKey"
    )

  # Placeholder column required by downstream AERO logic
  plots_texture$SoilTexture <- NA_character_

  # ---- Compute per-plot metrics --------------------------------------------

  # Maximum canopy height (m) from height_tall
  max_height <- terradactyl::mean_height(
    height_tall = height_tall,
    method      = "max",
    omit_zero   = TRUE,
    by_line     = FALSE,
    tall        = TRUE
  ) |> dplyr::mutate(max_height = max_height / 100)

  # Bare-soil fraction from geoIndicators
  bare_soil <- geoind |> dplyr::select(PrimaryKey, BareSoil)

  # Canopy gap distances (fraction) from gap_tall
  canopy_gap <- gap_tall |>
    dplyr::filter(RecType == "C") |>
    dplyr::mutate(Gap = Gap / 100)

  # ---- Intersect to common PrimaryKeys -------------------------------------

  common_PK <- Reduce(
    intersect,
    list(
      unique(canopy_gap$PrimaryKey),
      unique(plots_texture$PrimaryKey),
      unique(max_height$PrimaryKey),
      unique(bare_soil$PrimaryKey)
    )
  )

  plots_texture <- plots_texture |> dplyr::filter(PrimaryKey %in% common_PK)

  # Construct a filesystem-safe key that optionally encodes soil texture class
  plots_texture <- plots_texture |>
    dplyr::mutate(
      SoilTexture = stringr::str_replace_na(SoilTexture, ""),
      SoilTexture = stringr::str_replace(SoilTexture, " ", "_"),
      PK_texture  = dplyr::if_else(
        SoilTexture == "",
        as.character(PrimaryKey),
        paste0(PrimaryKey, "_", SoilTexture)
      ),
      PK_texture = gsub("/", "-", PK_texture)
    )

  # ---- Write outputs -------------------------------------------------------

  if (write_out) {

    # Gap files
    gap_location <- file.path(output_dir, "gap")
    dir.create(gap_location, recursive = TRUE, showWarnings = FALSE)
    lapply(plots_texture$PK_texture, function(X) {
      pk        <- plots_texture$PrimaryKey[plots_texture$PK_texture == X]
      gaps      <- canopy_gap |> dplyr::filter(PrimaryKey == pk) |> dplyr::pull(Gap)
      file_name <- file.path(gap_location, paste0(X, ".txt"))
      readr::write_lines(gaps, file_name)
    })

    # INI configuration files
    lapply(plots_texture$PK_texture, function(X) {
      sel <- plots_texture[plots_texture$PK_texture == X, ]
      pk  <- sel$PrimaryKey[1]

      ini <- c(
        "[INPUT_VALUES]",
        paste("wind_location: ", sel$Latitude_WGS84[1], sel$Longitude_WGS84[1]),
        paste0("soil_sand_fraction: ", sel$sand[1]),
        paste0("soil_clay_fraction: ", sel$clay[1]),
        paste0("veg_cover_fraction: ",
          (100 - bare_soil$BareSoil[bare_soil$PrimaryKey == pk]) / 100
        ),
        paste0("veg_mean_height: ", max_height$max_height[max_height$PrimaryKey == pk][1]),
        paste0("gap_obsv: ./gap/", X, ".txt")
      )

      file_name <- file.path(output_dir, paste0(X, ".ini"))
      readr::write_lines(ini, file_name)
    })

    # Combined summary CSV
    input_data <- plots_texture |>
      dplyr::left_join(bare_soil,   by = "PrimaryKey") |>
      dplyr::left_join(max_height,  by = "PrimaryKey") |>
      dplyr::left_join(canopy_gap,  by = "PrimaryKey")

    input_data_file <- file.path(output_dir, "input_data.csv")
    readr::write_csv(input_data, input_data_file)

    files_written <- c(
      list.files(gap_location, full.names = TRUE),
      list.files(output_dir, pattern = "\\.ini$", full.names = TRUE),
      input_data_file
    )

  } else {
    input_data    <- NULL
    files_written <- character()
  }

  list(
    input_data    = input_data,
    plots_texture = plots_texture,
    files_written = files_written
  )
}
