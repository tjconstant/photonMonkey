\name{jet.colors}
\alias{jet.colors}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Jet Colorscale
}
\description{
"Jet" colorscale.
}
\usage{
jet.colors(n)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n}{
number of breaks in colour scale
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}


\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (n) 
{
    x <- ramp(seq.int(0, 1, length.out = n))
    rgb(x[, 1L], x[, 2L], x[, 3L], maxColorValue = 255)
  }
}

\keyword{ ~kwd1 }
\keyword{ ~kwd2 }
