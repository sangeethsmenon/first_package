#' Euclidean Algorithm for Greatest Common Divisor (GCD)
#'
#' This function calculates the greatest common divisor (GCD) of two positive integers
#' using the Euclidean algorithm.
#'
#' @param n A positive numeric value.
#' @param m A positive numeric value.
#'
#' @return The greatest common divisor (GCD) of n and m, which is a positive integer.
#'
#' @references
#' Wikipedia: Euclidean Algorithm - https://en.wikipedia.org/wiki/Euclidean_algorithm
#'
#' @export

euclidean <- function(n, m) {
  stopifnot(is.numeric(n), is.numeric(m))
  while (m != 0) {
    output<-n%%m
    n<-m
    m<-output
  }
  return(n)
}