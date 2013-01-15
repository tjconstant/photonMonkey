\name{splot}
\alias{splot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function to calculate reflectivity from raw Monochromator data
}
\description{
Divides the signal by a reference spectra
}
\usage{
splot(data, ref, dkf)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
raw variable from imported data.dat file
}
  \item{ref}{
raw variable from imported ref.dat file
}
  \item{dkf}{
raw variable from imported dkf.dat file (dark field reading)
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

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (data, ref, dkf) 
{
    d <- data[, 3] - mean(dkf[, 3])
    r <- ref[, 3] - mean(dkf[, 3])
    dr <- data[, 4] - mean(dkf[, 4])
    rr <- ref[, 4] - mean(dkf[, 4])
    normData <- (d/r)
    normRef <- (dr/rr)
    par(mfrow = c(2, 2))
    par(mfrow = c(1, 1))
    return(normData/normRef)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
