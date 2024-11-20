test_that("transforming up from 0 leaves the order unchanged", {
  data_copy <- ExampleData
  data_transformed <- transformprn(data_copy, ~rands, "U", 0.0)
  data_copy_ordered <- data_copy[order(data_copy$rands), ]
  data_transformed_ordered <- data_transformed[order(data_transformed$rands), ]
  expect_equal(data_copy_ordered$id, data_transformed_ordered$id)
})

test_that("transforming down from 1 inverts the order", {
  data_copy <- ExampleData
  data_transformed <- transformprn(data_copy, ~rands, "D", 1.0)
  data_copy_ordered <- data_copy[order(data_copy$rands), ]
  data_transformed_ordered <- data_transformed[order(data_transformed$rands,
    decreasing = TRUE
  ), ]
  expect_equal(data_copy_ordered$id, data_transformed_ordered$id)
})

test_that("error when prn not found", {
  expect_error(transformprn(ExampleData, direction = "D", start = 0.1))
})

test_that("error when start is too high", {
  expect_error(transformprn(ExampleData, ~rands, "D", 2.42))
})

test_that("error when start is too low", {
  expect_error(transformprn(ExampleData, ~rands, "D", -0.69))
})

test_that("error when using an incorrect direction", {
  expect_error(transformprn(ExampleData, ~rands, "Q", 0.42))
})


test_that("data.table input gives data.table output", {
  example_table <- data.table::as.data.table(ExampleData)
  expect_s3_class(transformprn(example_table, ~rands, "U", 0), "data.table")
})

test_that("tibble input gives tibble output", {
  example_tibble <- tibble::as_tibble(ExampleData)
  expect_s3_class(transformprn(example_tibble, ~rands, "U", 0), "tbl_df")
})
