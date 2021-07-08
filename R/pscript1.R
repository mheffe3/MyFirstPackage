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


matrixMultiplier <- function(a,b){
  result <- a * b
  return(result)
}
matrixMultiplier(M1 <- matrix(c(1,2,3,4), nrow = 2, byrow = TRUE),M2 <- matrix(c(1,2,3,4), nrow = 2, byrow = TRUE))

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


randomNumberGame <- function(leftbound, rightbound){
  if(leftbound > rightbound)
  {
    cat("Upper bound should be greater than lower bound\n")
    return()
  }
  secretNumber<-sample(leftbound:rightbound, 1)
  count <- 1
  cat("Range: ",leftbound,"-",rightbound)
  guess <- readline("Enter a guess : ");
  guess <- as.integer(guess)

  while(guess != secretNumber)
  {
    if(guess < leftbound || guess > rightbound)
    {
      cat("Your guess is outside of the valid range", leftbound,"-",rightbound,". Try Again \n")
    }
    else if(guess < secretNumber)
    {
      cat("The secret number is greater than your guess\n")
    }
    else if(guess > secretNumber)
    {
      cat("The secret number is less than your guess\n")
    }

    count <- count + 1
    guess <- readline("Enter a guess : ");
    guess <- as.integer(guess)

  }
  cat("You got it! The secret number is ", secretNumber,". It took you ",count," guesses")
}
#randomNumberGame(1,10)


writeToFile <- function(filename)
{

  if(file.exists(filename))
  {
    answer <- readline("FILE ALREADY EXISTS. Would you like to 'a' (append), 'o' (overwrite), or 'q' (quit) : ")
    if(answer != 'a' && answer !='o')
    {
      return(cat("QUITTING"))
    }
    else if(answer == 'o')
    {
      a = FALSE
    }
    else
    {
      a = TRUE
    }
  }
  else
  {
    answer <- readline("NEW FILE NAME.  Do you wish to create a new file 'y' (yes) or 'n' (no) : ")
    if(answer != 'y')
    {
      return(cat("QUITTING"))
    }
    else
    {
      cat("NEW FILE CREATED: ", filename, "\n")
      a = TRUE
    }
  }
  name <- readline("Enter your name : ")
  cat(name, file=filename, sep = "\n",append=a)
  cat(format(Sys.time(),usetz=TRUE), file=filename, sep = "\n", append=TRUE)
  line <- readline("Write something ('q' to quit) : ")
  while(line != "q")
  {
    cat(line, file=filename, sep = "\n", append=TRUE)
    line <- readline("Write more ('q' to quit) : ")
  }

  return(cat("DONE"))

}
#writeToFile("output.txt")


arithSeq <- function(d,a1,n)
{
  count <- 0
  sequence <- vector()
  while(count < n)
  {
    sequence <- append(sequence, (a1 + (count)*d))
    count <- count+1
  }

  return(sequence)

}

geoSeq <- function(r,a1,n)
{
  count <- 0
  sequence <- vector()
  while(count < n)
  {
    sequence <- append(sequence, (a1 * r^(count)))
    count <- count+1
  }

  return(sequence)

}




