\name{fibonacci}
\alias{fibonacci}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{fibonacci
%%  ~~function to do ... ~~
}
\description{Takes in an integer (n) and returns a vector containing the first n terms of the fibonacci sequence
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
fibonacci(n)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n}{Number of terms of sequence to be returned in the vector
%%     ~~Describe \code{x} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{Returns a vector containing the first n terms of the sequence specified the input
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
\examples{fibonacci(8)
Returns a vector of length 8 containing the first 8 terms of the fibonacci sequence (ie. 0, 1, 1, 2, 3, 5, 8, 13)
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
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
