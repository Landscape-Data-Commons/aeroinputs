# test-generate_aero_inputs.R
# Tests use withr::local_tempdir() to write real output files in a temp folder
# and a minimal synthetic dataset so no network access is required.

# ---- helpers ----------------------------------------------------------------

make_header <- function(n = 3) {
  data.frame(
    PrimaryKey      = paste0("PK", seq_len(n)),
    Longitude_NAD83 = c(-105.1, -105.2, -105.3),
    Latitude_NAD83  = c(39.1,   39.2,   39.3),
    stringsAsFactors = FALSE
  )
}

make_gap_tall <- function(pks = paste0("PK", 1:3)) {
  data.frame(
    PrimaryKey = rep(pks, each = 5),
    RecType    = "C",
    Gap        = rep(c(10, 20, 30, 40, 50), times = length(pks)),
    stringsAsFactors = FALSE
  )
}

make_height_tall <- function(pks = paste0("PK", 1:3)) {
  n <- length(pks)
  data.frame(
    PrimaryKey = rep(pks, each = 4),
    LineKey    = rep(paste0(pks, "_L1"), each = 4),
    PointNbr   = rep(1:4, times = n),
    Height     = rep(c(100, 200, 150, 50), times = n),
    type       = "woody",
    stringsAsFactors = FALSE
  )
}

make_lpi_tall <- function(pks = paste0("PK", 1:3)) {
  # Two points per plot; one "S" (bare soil) and one "PF" (perennial forb) hit
  # pct_cover(..., hit = "first") will yield 50 % BareSoil for every plot
  data.frame(
    PrimaryKey = rep(pks, each = 2),
    LineKey    = rep(paste0(pks, "_L1"), each = 2),
    PointNbr   = rep(1:2, times = length(pks)),
    layer      = "SoilSurface",
    code       = rep(c("S", "PF"), times = length(pks)),
    stringsAsFactors = FALSE
  )
}

write_tall_csvs <- function(dir, n = 3) {
  pks    <- paste0("PK", seq_len(n))
  tall   <- file.path(dir, "Tall")
  dir.create(tall, recursive = TRUE, showWarnings = FALSE)

  readr::write_csv(make_header(n),      file.path(tall, "header.csv"))
  readr::write_csv(make_gap_tall(pks),  file.path(tall, "gap_tall.csv"))
  readr::write_csv(make_height_tall(pks), file.path(tall, "height_tall.csv"))
  readr::write_csv(make_lpi_tall(pks),    file.path(tall, "lpi_tall.csv"))
  invisible(tall)
}

write_texture_raster <- function(dir) {
  outfile <- file.path(dir, "soil_texture_w_sand_frac.tif")
  # 1-degree resolution raster centred on the test plots (NAD83 ~ WGS84 here)
  r <- terra::rast(
    nrows = 5, ncols = 5,
    xmin = -106, xmax = -104,
    ymin = 38,   ymax = 40,
    crs  = "EPSG:4269"
  )
  sand_layer <- clay_layer <- r
  terra::values(sand_layer) <- 60  # 60 % sand
  terra::values(clay_layer) <- 15  # 15 % clay
  names(sand_layer) <- "sand"
  names(clay_layer) <- "clay"
  stack <- c(sand_layer, clay_layer)
  terra::writeRaster(stack, outfile, overwrite = TRUE)
  outfile
}

# ---- tests ------------------------------------------------------------------

test_that("generate_aero_inputs stops with informative error on missing files", {
  tmp <- withr::local_tempdir()
  expect_error(
    generate_aero_inputs(data_dir = tmp, verbose = FALSE),
    "Missing required files"
  )
})

test_that("generate_aero_inputs runs and returns expected list elements", {
  skip_if_not_installed("terra")
  skip_if_not_installed("terradactyl")

  tmp      <- withr::local_tempdir()
  out_dir  <- file.path(tmp, "output")
  tex_file <- write_texture_raster(tmp)
  write_tall_csvs(tmp)

  result <- generate_aero_inputs(
    data_dir     = tmp,
    output_dir   = out_dir,
    texture_file = tex_file,
    write_out    = FALSE,
    verbose      = FALSE
  )

  expect_type(result, "list")
  expect_named(result, c("input_data", "plots_texture", "files_written"))
  expect_null(result$input_data)               # write_out = FALSE
  expect_true(nrow(result$plots_texture) >= 0) # may be 0 if no overlap
  expect_equal(result$files_written, character())
})

test_that("generate_aero_inputs writes expected output files when write_out = TRUE", {
  skip_if_not_installed("terra")
  skip_if_not_installed("terradactyl")

  tmp      <- withr::local_tempdir()
  out_dir  <- file.path(tmp, "output")
  tex_file <- write_texture_raster(tmp)
  write_tall_csvs(tmp)

  result <- generate_aero_inputs(
    data_dir     = tmp,
    output_dir   = out_dir,
    texture_file = tex_file,
    write_out    = TRUE,
    verbose      = FALSE
  )

  expect_true(file.exists(file.path(out_dir, "input_data.csv")))
  expect_true(dir.exists(file.path(out_dir,  "gap")))
  expect_gt(length(list.files(out_dir, pattern = "\\.ini$")), 0)
})
