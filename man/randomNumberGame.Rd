\name{randomNumberGame}
\alias{randomNumberGame}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{randomNumberGame
%%  ~~function to do ... ~~
}
\description{Takes in the bounds used for the random number generation and then asks for the user to guess the random number.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
randomNumberGame(leftbound, rightbound)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{leftbound}{The least value that the random number can hold}
  \item{leftbound}{The greatest value that the random number can hold}
%%     ~~Describe \code{x} here~~

}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{Has a void return type.  Prints out the results instead of returning them
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{Matt Heffernan, University of Illinois at Chicago
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{randomNumberGame(1,10)
Generates a random number between 1 and 10 inclusive and asks for the user to guess it
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
randomNumberGame <- function(leftbound, rightbound){
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
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
