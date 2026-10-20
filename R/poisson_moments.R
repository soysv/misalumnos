#' Poisson Distribution Moments
#'
#' @param lambda Rate parameter (lambda > 0).
#' @return A list containing mean and variance.
#' @export
poisson_moments <- function(lambda) {
  if (lambda <= 0) stop("Lambda must be positive")
  list(
    mean = lambda,
    variance = lambda
  )
}
