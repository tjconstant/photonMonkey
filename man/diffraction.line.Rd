\name{diffraction.line}
\alias{diffraction.line}

\title{
Diffraction Line calculation
}
\description{
Return a diffraction line to be added to a plot, given a vector of in-plane wavenumbers (k)
}
\usage{
diffraction.line(k, kgx, kgv, m, n, phi = 0, alpha = pi/2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{k}{
in-plane wavenumbers.
}
  \item{kgx}{
the grating vector of the (phi=0) periodicity (2*pi/pitch)
}
  \item{kgv}{
the grating vector of the out-of-plane periodicity (2*pi/pitch)
}
  \item{m}{
in-plane scattering integer
}
  \item{n}{
out-of-plane scattering integer
}
  \item{phi}{
azimuthal angle away from kgx (phi=0 along kgx) in radians
}
  \item{alpha}{
angle between the two grating vectors in radians
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
function (k, kgx, kgv, m, n, phi = 0, alpha = pi/2) 
{
    return(data.frame(k = k, omega = 3e+08 * (sqrt(((k * cos(phi) + 
        m * kgx - n * kgv * cos(alpha))^2) + ((k * sin(phi) - 
        n * kgv * sin(alpha))^2)))))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
