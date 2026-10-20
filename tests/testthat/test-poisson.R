test_that("Poisson PMF and CDF work correctly", {
  expect_equal(poisson_pmf(0, 2), dpois(0, 2))
  expect_equal(poisson_cdf(2, 2), ppois(2, 2))
  expect_error(poisson_pmf(1, -1))
})

test_that("Poisson moments work correctly", {
  m <- poisson_moments(3)
  expect_equal(m$mean, 3)
  expect_equal(m$variance, 3)
})
