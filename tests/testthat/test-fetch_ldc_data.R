# test-fetch_ldc_data.R
# These tests do NOT call the LDC API. fetch_dataset() and fetch_chain()
# are tested with mocked trex::fetch_ldc() via testthat::local_mocked_bindings().

# ---- helpers ----------------------------------------------------------------

fake_header <- function() {
  tibble::tibble(
    ProjectKey      = c("PROJ_A", "PROJ_B", "BLM_AIM"),
    PrimaryKey      = c("PK1",    "PK2",    "PK3"),
    Longitude_NAD83 = c(-105.1,  -105.2,  -105.3),
    Latitude_NAD83  = c(39.1,     39.2,    39.3)
  )
}

fake_gap <- function() {
  tibble::tibble(
    PrimaryKey = c("PK1", "PK1", "PK2", "PK2"),
    RecType    = "C",
    Gap        = c(10, 20, 30, 40)
  )
}

fake_indicators <- function() {
  tibble::tibble(
    PrimaryKey = c("PK1", "PK2"),
    BareSoil   = c(30, 40)
  )
}

# ---- unit tests ------------------------------------------------------------

test_that("fetch_chain returns a list with data and final_keys", {
  # Stub trex::fetch_ldc so no HTTP calls are made
  local_mocked_bindings(
    fetch_ldc = function(data_type, keys, key_type, token, base_url) {
      if (data_type == "gap")        return(fake_gap())
      if (data_type == "indicators") return(fake_indicators())
      tibble::tibble(PrimaryKey = keys)
    },
    .package = "trex"
  )

  result <- aeroinputs:::fetch_chain(
    chain      = c("gap", "indicators"),
    start_keys = c("PK1", "PK2"),
    verbose    = FALSE
  )

  expect_type(result, "list")
  expect_named(result, c("data", "final_keys"))
  expect_true("gap" %in% names(result$data))
  expect_true("indicators" %in% names(result$data))
})

test_that("fetch_chain stops early when keys become empty", {
  local_mocked_bindings(
    fetch_ldc = function(data_type, keys, key_type, token, base_url) {
      tibble::tibble(PrimaryKey = character(0))
    },
    .package = "trex"
  )

  result <- aeroinputs:::fetch_chain(
    chain      = c("gap", "height"),
    start_keys = c("PK1"),
    verbose    = FALSE
  )

  # Only gap was fetched; height was skipped due to empty keys
  expect_true("gap" %in% names(result$data))
  expect_false("height" %in% names(result$data))
  expect_equal(result$final_keys, character(0))
})

test_that("fetch_ldc_data respects excluded_project_keys", {
  tmp <- withr::local_tempdir()

  # Provide a cached header so no API call is made for the header
  header <- fake_header()
  header_cache <- file.path(tmp, "header.RData")
  save(header, file = header_cache)

  local_mocked_bindings(
    fetch_ldc = function(data_type, keys, key_type, token, base_url) {
      if (data_type == "gap")        return(fake_gap())
      if (data_type == "indicators") return(fake_indicators())
      tibble::tibble(PrimaryKey = keys)
    },
    .package = "trex"
  )

  result <- fetch_ldc_data(
    base_dir          = tmp,
    header_cache_file = header_cache,
    chain_types       = c("gap", "indicators"),
    excluded_project_keys = "BLM_AIM",
    write_out         = FALSE,
    verbose           = FALSE
  )

  # PK3 is in BLM_AIM and should be excluded from the start
  expect_false("PK3" %in% result$final_keys)
})
