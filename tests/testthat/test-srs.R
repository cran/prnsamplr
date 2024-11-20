test_that("srs samples the right number", {
  srs_frame <- srs(ExampleData, ~stratum, ~nsample, ~rands)
  nsamp_list <- aggregate(srs_frame$nsample,
    by = list(stratum = srs_frame$stratum),
    FUN = unique
  )
  sampled_list <- aggregate(srs_frame$sampled,
    by = list(stratum = srs_frame$stratum),
    FUN = sum
  )
  expect_equal(sampled_list$x, nsamp_list$x)
})

test_that("error when stratid not found", {
  expect_error(srs(ExampleData, nsamp = ~nsample, prn = ~rands))
})

test_that("error when nsamp not found", {
  expect_error(srs(ExampleData, stratid = ~stratum, prn = ~rands))
})

test_that("error when prn not found", {
  expect_error(srs(ExampleData, stratid = ~stratum, nsamp = ~nsample))
})

test_that("error when nsamp not numeric", {
  data_copy <- ExampleData
  data_copy[1, "nsample"] <- "foo"
  expect_error(srs(data_copy, ~stratum, ~nsample, ~rands))
})

test_that("error when prn not numeric", {
  data_copy <- ExampleData
  data_copy[1, "rands"] <- "foo"
  expect_error(srs(data_copy, ~stratum, ~nsample, ~rands))
})

test_that("warning when too many nsamp in one stratid", {
  data_copy <- ExampleData
  data_copy[1, "nsample"] <- data_copy[1, "nsample"] + 1
  expect_warning(srs(data_copy, ~stratum, ~nsample, ~rands))
})

test_that("warning when prn < 0", {
  data_copy <- ExampleData
  data_copy[1, "rands"] <- -0.1
  expect_warning(srs(data_copy, ~stratum, ~nsample, ~rands))
})

test_that("warning when prn > 1", {
  data_copy <- ExampleData
  data_copy[1, "rands"] <- 1.1
  expect_warning(srs(data_copy, ~stratum, ~nsample, ~rands))
})

test_that("data.table input gives data.table output", {
  example_table <- data.table::as.data.table(ExampleData)
  expect_s3_class(srs(example_table, ~stratum, ~nsample, ~rands), "data.table")
})

test_that("tibble input gives tibble output", {
  example_tibble <- tibble::as_tibble(ExampleData)
  expect_s3_class(srs(example_tibble, ~stratum, ~nsample, ~rands), "tbl_df")
})
