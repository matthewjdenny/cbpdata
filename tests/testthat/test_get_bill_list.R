test_that("We can get bill data for current year", {
  skip_on_cran()

  # test for year where data should not change
  results <- get_bill_list(2002,2002)
  # number of reaults that should be returned:
  expect_equal(length(results$bill_metadata), 6540)
})
