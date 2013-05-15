\name{unwrap.phase}
\alias{unwrap.phase}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Phase Unwrapping Function
}
\description{
This unwrap function removes pi or 2*pi phase jumps from an input dataset.
}
\usage{
unwrap.phase(Data, tol = pi, step = (2 * pi))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Data}{
Phase data to be unwrapped as a vector.
}
  \item{tol}{
is the amount between steps which is considered large enough to be a jump, default value is pi.
}
  \item{step}{
indicates the amount size of the jump in the phase data inputted, default value is 2*pi.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Returns the unwrapped dataset as a vector.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
S.J.Berry
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
function (Data, tol = pi, step = (2 * pi)) 
{
    Data_Length <- length(Data)
    for (a in 1:(Data_Length - 1)) {
        b <- a + 1
        Data_Difference <- Data[a] - Data[b]
        if (Data_Difference <= (-tol)) {
            for (c in b:Data_Length) {
                Data[c] <- Data[c] - step
            }
        }
        if (Data_Difference >= (tol)) {
            for (c in b:Data_Length) {
                Data[c] <- Data[c] + step
            }
        }
    }
    return(Data)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
