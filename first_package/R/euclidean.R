euclidean <- function(n, m) {
  stopifnot(is.numeric(n),is.numeric(m))
  least_num <- min(n, m)
  output <- 1
  for (i in 1:least_num) {
    if (n %% i == 0 && m %% i == 0) {
      output <- i
    }
  }
  return(output)
}


