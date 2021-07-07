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

factorial <- function(x){
  if(x == 0){
    return(1)
  }
  else
  {
    return(x * factorial(x-1))
  }
}
factorial(4)

sum <- function(a,b){
  result <- 0
  while(a <= b){
    result <- result + a
    a <- a + 1
  }
  return(result)
}
sum(1,5)
