\name{sum}
\alias{sum}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{sum
%%  ~~function to do ... ~~
}
\description{Takes in two numbers and sums from the first to the last inclusive also takes in a third number representing the increment between numbers being summed. If the last number is not within the increment then it goes to the closest number before it.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
sum(a,b,by)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{a}{The starting number for the summation}
  \item{b}{The ending number for the summation}
  \item{by}{The distance between the next number in the sum}
%%     ~~Describe \code{x} here~~
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{Returns an integer that is the sum of numbers from a to b inclusive possibly by a certain increment value
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
\examples{sum(1,5,1)
Returns the sum from 1 to 5 inclusive by 1 which is 15
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
sum <- function(a,b,by){
  if(a == b)
  {
    return(0)
  }
  while((b-a)%%by != 0)
  {
    b <- b - 1
  }
  result <- (((((b-a)/by)+1)*(a+b))/2)
  return(result)
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
