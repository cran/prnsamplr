test_that("pps samples the right number", {
  pps_frame <- pps(ExampleData, ~stratum, ~nsample, ~rands, ~sizeM)
  nsamp_list <- aggregate(pps_frame$nsample,
    by = list(stratum = pps_frame$stratum),
    FUN = unique
  )
  sampled_list <- aggregate(pps_frame$sampled,
    by = list(stratum = pps_frame$stratum),
    FUN = sum
  )
  expect_equal(sampled_list$x, nsamp_list$x)
})

test_that("pps calculates lambda correctly", {
  pps_frame <- pps(ExampleData, ~stratum, ~nsample, ~rands, ~sizeM)
  nsamp_list <- aggregate(pps_frame$nsample,
    by = list(stratum = pps_frame$stratum),
    FUN = unique
  )
  lambda_list <- aggregate(pps_frame$lambda,
    by = list(stratum = pps_frame$stratum),
    FUN = sum
  )
  expect_equal(lambda_list$x, nsamp_list$x)
})

# actually it needs to be close to it, not necessarily equal (hence tolerance)
test_that("pps calculates the inverse of lambda correctly as well", {
  pps_frame <- pps(ExampleData, ~stratum, ~nsample, ~rands, ~sizeM)
  npop_list <- aggregate(
    pps_frame$npopul,
    by = list(stratum = pps_frame$stratum),
    FUN = unique
  )
  pps_sample <- pps_frame[pps_frame$sampled, ]
  pps_sample$weight <- 1 / pps_sample$lambda
  weight_list <- aggregate(
    pps_sample$weight,
    by = list(stratum = pps_sample$stratum),
    FUN = sum
  )
  expect_equal(weight_list$x, npop_list$x, tolerance = 0.1)
})

test_that("error when stratid not found", {
  expect_error(pps(ExampleData, nsamp = ~nsample, prn = ~rands, size = ~sizeM))
})

test_that("error when nsamp not found", {
  expect_error(pps(ExampleData,
    stratid = ~stratum,
    prn = ~rands, size = ~sizeM
  ))
})

test_that("error when prn not found", {
  expect_error(pps(ExampleData,
    stratid = ~stratum,
    nsamp = ~nsample, size = ~sizeM
  ))
})

test_that("error when size not found", {
  expect_error(pps(ExampleData,
    stratid = ~stratum,
    nsamp = ~nsample, prn = ~rands
  ))
})

test_that("error when nsamp not numeric", {
  data_copy <- ExampleData
  data_copy[1, "nsample"] <- "foo"
  expect_error(pps(data_copy, ~stratum, ~nsample, ~rands, ~sizeM))
})

test_that("error when prn not numeric", {
  data_copy <- ExampleData
  data_copy[1, "rands"] <- "foo"
  expect_error(pps(data_copy, ~stratum, ~nsample, ~rands, ~sizeM))
})

test_that("error when size not numeric", {
  data_copy <- ExampleData
  data_copy[1, "sizeM"] <- "foo"
  expect_error(pps(data_copy, ~stratum, ~nsample, ~rands, ~sizeM))
})

test_that("warning when too many nsamp in one stratid", {
  data_copy <- ExampleData
  data_copy[1, "nsample"] <- data_copy[1, "nsample"] + 1
  w <- capture_warnings(pps(data_copy, ~stratum, ~nsample, ~rands, ~sizeM))
  expect_match(
    w[1],
    "stratum with names st00001 have more than one corresponding value of nsample"
  )
})

test_that("warning when prn < 0", {
  data_copy <- ExampleData
  data_copy[1, "rands"] <- -0.1
  w <- capture_warnings(pps(data_copy, ~stratum, ~nsample, ~rands, ~sizeM))
  expect_match(
    w[1],
    "rands less than 0 found at rows 1"
  )
})

test_that("warning when prn > 1", {
  data_copy <- ExampleData
  data_copy[1, "rands"] <- 1.1
  w <- capture_warnings(pps(data_copy, ~stratum, ~nsample, ~rands, ~sizeM))
  expect_match(
    w[1],
    "rands greater than 1 found at rows 1"
  )
})


test_that("data.table input gives data.table output", {
  example_table <- data.table::as.data.table(ExampleData)
  expect_s3_class(
    pps(example_table, ~stratum, ~nsample, ~rands, ~sizeM),
    "data.table"
  )
})

test_that("tibble input gives tibble output", {
  example_tibble <- tibble::as_tibble(ExampleData)
  expect_s3_class(
    pps(example_tibble, ~stratum, ~nsample, ~rands, ~sizeM),
    "tbl_df"
  )
})
