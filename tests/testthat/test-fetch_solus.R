# test-fetch_solus.R
# Tests that do NOT hit the network. fetch_solus() internals are tested
# individually; the top-level function is tested with a mocked index.

test_that(".subset_solus_index filters by variable, depth, and output_type", {
  # Build a minimal mock index
  idx <- tibble::tibble(
    property    = c("sandtotal", "sandtotal", "claytotal"),
    depth_slice = factor(c("0", "5", "0"), levels = c("all", "0", "5", "15")),
    filetype    = c("prediction", "prediction", "prediction"),
    filename    = c("sandtotal_0_cm_p.tif", "sandtotal_5_cm_p.tif", "claytotal_0_cm_p.tif"),
    url         = paste0("https://example.com/", c("f1.tif", "f2.tif", "f3.tif")),
    scalar      = "10"
  )

  result <- aeroinputs:::.subset_solus_index(
    solus_index  = idx,
    variables    = "sandtotal",
    depth_slices = 0,
    output_type  = "prediction"
  )

  expect_equal(nrow(result), 1L)
  expect_equal(result$property, "sandtotal")
})

test_that(".subset_solus_index returns zero rows when no match", {
  idx <- tibble::tibble(
    property    = "sandtotal",
    depth_slice = factor("0", levels = c("all", "0", "5")),
    filetype    = "prediction",
    filename    = "sandtotal_0_cm_p.tif",
    url         = "https://example.com/f.tif",
    scalar      = "10"
  )

  result <- aeroinputs:::.subset_solus_index(
    solus_index  = idx,
    variables    = "claytotal",   # not in index
    depth_slices = 0,
    output_type  = "prediction"
  )

  expect_equal(nrow(result), 0L)
})

test_that(".normalize_solus_input converts sf to SpatVector", {
  skip_if_not_installed("sf")
  skip_if_not_installed("terra")

  pt <- sf::st_as_sf(
    data.frame(x = -105, y = 39),
    coords = c("x", "y"),
    crs    = 4326
  )

  result <- aeroinputs:::.normalize_solus_input(pt, target_crs = "EPSG:4326")
  expect_s4_class(result, "SpatVector")
})

test_that(".extract_solus_values adds an ID column", {
  skip_if_not_installed("terra")

  r <- terra::rast(nrows = 3, ncols = 3, xmin = 0, xmax = 1, ymin = 0, ymax = 1)
  terra::values(r) <- runif(9)
  names(r) <- "sandtotal_0_cm_p"

  result <- aeroinputs:::.extract_solus_values(r, x = NULL, samples = NULL)
  expect_true("ID" %in% names(result))
  expect_equal(nrow(result), 9L)
})
