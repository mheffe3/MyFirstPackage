\name{MatrixMultiplication}
\alias{MatrixMultiplication}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{MatrixMultiplication
%%  ~~function to do ... ~~
}
\description{Multiplies two inputted matrices and prints the product
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
MatrixMultiplication(a,b)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{a}{first matrix}
  \item{b}{second matrix}
%%     ~~Describe \code{x} here~~

}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{Return type is void just prints out the product
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
\examples{MatrixMultiplier(M1 <- matrix(c(1,2,3,4), nrow = 2, byrow = TRUE),
M2 <- matrix(c(1,2,3,4), nrow = 2, byrow = TRUE))
    [,1] [,2]
[1,]  1    4
[2,]  9   16
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
MatrixMultiplication(a,b)
{
  result <- a * b
  print(result)
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
