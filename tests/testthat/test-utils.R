test_that("format_elapsed_time returns seconds for short durations", {
  t0 <- Sys.time() - 5
  result <- aeroinputs:::format_elapsed_time(t0)
  expect_match(result, "seconds$")
  expect_false(grepl("minutes", result))
})

test_that("format_elapsed_time returns minutes for long durations", {
  t0 <- Sys.time() - 130
  result <- aeroinputs:::format_elapsed_time(t0)
  expect_match(result, "minutes")
  expect_match(result, "seconds")
})

test_that("progress_message emits a message when verbose = TRUE", {
  expect_message(
    aeroinputs:::progress_message("hello", verbose = TRUE),
    "hello"
  )
})

test_that("progress_message is silent when verbose = FALSE", {
  expect_silent(
    aeroinputs:::progress_message("hello", verbose = FALSE)
  )
})

test_that("%notin% is the negation of %in%", {
  x <- c("a", "b", "c")
  expect_equal(aeroinputs:::`%notin%`("a", x), FALSE)
  expect_equal(aeroinputs:::`%notin%`("z", x), TRUE)
  expect_equal(aeroinputs:::`%notin%`(c("a", "z"), x), c(FALSE, TRUE))
})

test_that("load_cached_object reads a saved object correctly", {
  tmp <- withr::local_tempfile(fileext = ".RData")
  my_obj <- data.frame(x = 1:3, y = letters[1:3])
  save(my_obj, file = tmp)

  loaded <- aeroinputs:::load_cached_object(tmp, "my_obj")
  expect_equal(loaded, my_obj)
})

test_that("load_cached_object stops when object name is absent", {
  tmp <- withr::local_tempfile(fileext = ".RData")
  my_obj <- 42
  save(my_obj, file = tmp)

  expect_error(
    aeroinputs:::load_cached_object(tmp, "nonexistent"),
    "does not contain object"
  )
})

test_that("dedup_by_pk removes duplicate PrimaryKey rows", {
  df <- data.frame(
    PrimaryKey = c("A", "A", "B"),
    value      = c(1, 2, 3),
    stringsAsFactors = FALSE
  )
  result <- aeroinputs:::dedup_by_pk(df)
  expect_equal(nrow(result), 2L)
  expect_equal(result$value, c(1, 3))
})

test_that("safe_filter_by_final_keys returns empty tibble for NULL df", {
  result <- aeroinputs:::safe_filter_by_final_keys(NULL, "A", "test", verbose = FALSE)
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0L)
})

test_that("safe_filter_by_final_keys filters correctly", {
  df <- data.frame(
    PrimaryKey = c("A", "B", "C"),
    val        = 1:3,
    stringsAsFactors = FALSE
  )
  result <- aeroinputs:::safe_filter_by_final_keys(df, c("A", "C"), "test", verbose = FALSE)
  expect_equal(nrow(result), 2L)
  expect_equal(result$PrimaryKey, c("A", "C"))
})

test_that("extract_nonpublic_keys returns empty for unrelated warning", {
  result <- aeroinputs:::extract_nonpublic_keys("Some unrelated warning message.")
  expect_equal(result, character())
})

test_that("extract_nonpublic_keys parses keys from trex warning format", {
  msg <- paste0(
    "Some keys were not associated with publicly-available data. ",
    "The following data may be limited. Data may still be returned for the ",
    "following keys: key1, key2, key3"
  )
  result <- aeroinputs:::extract_nonpublic_keys(msg)
  expect_equal(result, c("key1", "key2", "key3"))
})

test_that("append_nonpublic_csv creates a new file on first call", {
  tmp <- withr::local_tempfile(fileext = ".csv")
  aeroinputs:::append_nonpublic_csv(
    keys      = c("K1", "K2"),
    data_type = "gap",
    file      = tmp
  )
  df <- utils::read.csv(tmp)
  expect_equal(nrow(df), 2L)
  expect_true("primary_key" %in% names(df))
})

test_that("append_nonpublic_csv appends on subsequent calls", {
  tmp <- withr::local_tempfile(fileext = ".csv")
  aeroinputs:::append_nonpublic_csv(c("K1"), "gap",    tmp)
  aeroinputs:::append_nonpublic_csv(c("K2"), "height", tmp)

  df <- utils::read.csv(tmp)
  expect_equal(nrow(df), 2L)
})
