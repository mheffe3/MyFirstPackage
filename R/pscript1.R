fibonacci <- function(numTerms){
  count = 0
  a = 0
  b = 1
  while(count < numTerms)
  {
    print(a)
    temp = a
    a = b
    b = temp+b
    count = count+1
  }
}
fibonacci(8)


MatrixMultiplier <- function(a,b){
  print("hi")

}
