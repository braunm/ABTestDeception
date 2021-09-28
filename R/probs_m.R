
#' @name probabilities
#' @title Targeting and Conversion Probabilities
#' @param alpha,pi,rho Input ratios
#' @param M Selected average prob
#' @param g Pr(X=P)
#' @param name name of column for value (e.g., 'PrTarg' or 'EY')
#' @details use max_QB to determine max, based
#' @return list of PA, PB,  QA, QB
#' @export
get_XZ3 <- function(alpha, pi, rho, M,  g, name='prob') {

  S <- SqRtTerm3(alpha, pi, rho, g)

  PA <- new_PA3(alpha, pi, rho, M, g, S)
  PB <- new_PB3(alpha, pi, rho, M, g, S)
  QA <- new_QA3(alpha, pi, rho, M, g, S)
  QB <- new_QB3(alpha, pi, rho, M, g, S)

  res <- tribble(~XZ, ~value,
                 'PA', PA,
                 'PB', PB,
                 'QA', QA,
                 'QB', QB) %>%
    separate(XZ, into=c('X', 'Z'), sep=1, remove=FALSE) %>%
    rename({{name}}:=value)


  return(res)
}

#' @describeIn probabilities Compute PA
#' @export
new_PA3 <- function(a, b, r, M, g, S) {


  n1 <-  -b * r * g + a * (b + r - 2 * b * r) * g + g - a * r + S - 1


  d1 <-  g * (a + 1) * (g * (b - 1) + 1) * (r - 1)


  res <- -M * n1 / d1
  return(res)
}

#' @describeIn probabilities Compute PB
#' @export
new_PB3 <- function(a, b, r, M, g, S) {


  n1 <- 4 * M * b
  d1 <- 1 + a * r + g * ((2 + a) * b - 1 - r * (a + b)) + S

  res <- n1 / d1

  return(res)
}

#' @describeIn probabilities Compute QA
#' @export
new_QA3 <- function(a, b, r, M, g, S) {

  n1 <- -a * (r - 2) + g * (b * r + a * (b + r - 2) - 1) - S + 1
  d1 <- (g - 1) * (a + 1) * (g * (b - 1) + 1) * (r - 1)

  res <- M * n1 / d1

  return(res)
}

#' @describeIn probabilities Compute QA
#' @export
new_QB3 <- function(a, b, r, M, g, S) {

  n1 <- a * b * g + (b - a - 2) * r * g + g + (a + 2) * r - S - 1
  d1 <-  (g - 1) * (a + 1) * (g * (b - 1) + 1) * (r - 1)
  res <- -M * n1 / d1

}


#' @describeIn probabilities Intermediate term
#' @export
SqRtTerm3 <- function(a, b, r, g) {


  q1 <- (g * a * b + g - 1)^2
  q2 <- ((g - 1) * a + g * b)^2 * r^2
  q3 <- 2 * (a * (g - 1)^2 - g * (a * (a + 4) + 1) * b * (g - 1) + g^2 * a * b^2) * r

  res <- sqrt(q1 + q2 + q3)

  return(res)
}


