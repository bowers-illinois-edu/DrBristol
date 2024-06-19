# Test the p creation function
testthat::context("The simple unbiased p function test")

## The next lines are for use when creating the tests. Change interactive<-FALSE for production
interactive <- FALSE
if (interactive) {
  load_all() ## use  this during debugging
}

test_that("Warnings work", {
  expect_error(find_p(num_support=10,total_info=5))
  expect_error(find_p(num_support=2,total_info=10))
})

test_that("It gives the correct p-value",{
  res <- find_p(num_support=7,total_info=10)
  expect_true(all.equal(res,0.018648018648018651472))
})