\name{geoSeq}
\alias{geoSeq}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{geoSeq
%%  ~~function to do ... ~~
}
\description{Takes in the common ratio (r), the first term (a1), and the number of terms to wanted (n) and stores the first n terms of that sequence in a vector
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
geoSeq(r,a1,n)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{r}{the common ratio of the sequence}
  \item{a1}{first term of the series}
  \item{n}{number of terms to be stored in the vector}
%%     ~~Describe \code{x} here~~

}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{Returns a numeric vector containing the first n terms of the sequence
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
\examples{geoSeq(2,1,5)
Returns a vector containg a sequence of 5 terms with a common ratio of 2 and an inital term of 1
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
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
