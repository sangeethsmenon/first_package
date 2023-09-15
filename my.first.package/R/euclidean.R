euclidean <- function(num1, num2) {
  least_num <- min(num1, num2)
  output <- 1
  for (i in 1:least_num) {
    if (num1 %% i == 0 && num2 %% i == 0) {
      output <- i
    }
  }
  return(output)
}


