#' Poisson Distribution PMF
#'
#' @param x Vector of non-negative integer quantiles.
#' @param lambda Rate parameter (lambda > 0).
#' @return Probability mass function value.
#' @export
poisson_pmf <- function(x, lambda) {
  if (lambda <= 0) stop("Lambda must be positive")
  dpois(x, lambda)
}

#' Poisson Distribution CDF
#'
#' @param q Vector of quantiles.
#' @param lambda Rate parameter (lambda > 0).
#' @return Cumulative distribution function value.
#' @export
poisson_cdf <- function(q, lambda) {
  if (lambda <= 0) stop("Lambda must be positive")
  ppois(q, lambda)
}

#' Poisson Distribution Random Number Generator
#'
#' @param n Number of random values to generate.
#' @param lambda Rate parameter (lambda > 0).
#' @return Vector of random deviates.
#' @export
poisson_rnd <- function(n, lambda) {
  if (lambda <= 0) stop("Lambda must be positive")
  rpois(n, lambda)
}
