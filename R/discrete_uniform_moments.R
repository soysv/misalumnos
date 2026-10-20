# ============================================================
# Discrete Uniform Distribution의 평균과 분산
# ============================================================


discrete_uniform_moments <- function(a, b) {

  # 평균
  # E(X) = (a + b) / 2
  mean_value <- (a + b) / 2

  # 분산
  # Var(X) = ((b-a+1)^2 - 1) / 12
  variance_value <-
    ((b - a + 1)^2 - 1) / 12

  # 평균과 분산을 list 형태로 반환
  return(
    list(
      mean = mean_value,
      variance = variance_value
    )
  )
}


# 사용 예시:
#
# result <- discrete_uniform_moments(1, 6)
#
# result$mean
# result$variance
