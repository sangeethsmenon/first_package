euclidean <- function(n, m) {
  stopifnot(is.numeric(n), is.numeric(m))
  while (m != 0) {
    output<-n%%m
    n<-m
    m<-output
  }
  return(n)
}