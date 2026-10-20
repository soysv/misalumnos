# ============================================================
# Poisson Distribution
# 포아송 분포
# ============================================================


# ------------------------------------------------------------
# 1. PMF
# P(X = k)
# ------------------------------------------------------------

poisson_pmf <- function(k, lambda) {

  # 포아송 분포의 확률
  # P(X=k) = e^(-lambda) * lambda^k / k!

  result <- exp(-lambda) *
    lambda^k /
    factorial(k)

  return(result)
}


# ------------------------------------------------------------
# 2. CDF
# P(X <= k)
# ------------------------------------------------------------

poisson_cdf <- function(k, lambda) {

  # 0부터 k까지의 PMF를 모두 더함
  result <- sum(
    poisson_pmf(0:k, lambda)
  )

  return(result)
}


# ------------------------------------------------------------
# 3. RNG
# 랜덤한 포아송 데이터 생성
# ------------------------------------------------------------

poisson_rnd <- function(n, lambda) {

  # R의 기본 함수 rpois 사용
  result <- rpois(
    n = n,
    lambda = lambda
  )

  return(result)
}


# ------------------------------------------------------------
# 4. Moments
# ------------------------------------------------------------

poisson_moments <- function(lambda) {

  # 포아송 분포에서는
  # 평균 = lambda
  # 분산 = lambda

  return(
    list(
      mean = lambda,
      variance = lambda
    )
  )
}
