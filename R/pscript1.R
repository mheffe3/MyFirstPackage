fibonacci <- function(n){
  a = 0
  b = 1
  count = 0
  sequence <- c()
  while(count < n)
  {
    sequence <- append(sequence, a)
    temp <- a
    a <- b
    b <- temp + b
    count <- count + 1
  }
  return(sequence)
}
fibonacci(8)


MatrixMultiplier <- function(a,b){
  result <- a * b
  return(result)
}
MatrixMultiplier(M1 <- matrix(c(1,2,3,4), nrow = 2, byrow = TRUE),M2 <- matrix(c(1,2,3,4), nrow = 2, byrow = TRUE))
