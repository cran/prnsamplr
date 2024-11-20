test_that("error on invalid method", {
  expect_error(samp(foo, ExampleData, ~stratum, ~nsample, ~rands))
})

test_that("samp(srs, ...) returns the same as srs(...)", {
  expect_equal(
    samp(srs, ExampleData, ~stratum, ~nsample, ~rands),
    srs(ExampleData, ~stratum, ~nsample, ~rands)
  )
})

test_that("samp(pps, ...) returns the same as pps(...)", {
  expect_equal(
    samp(pps, ExampleData, ~stratum, ~nsample, ~rands, ~sizeM),
    pps(ExampleData, ~stratum, ~nsample, ~rands, ~sizeM)
  )
})
