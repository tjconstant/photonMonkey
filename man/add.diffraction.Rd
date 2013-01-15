\name{add.diffraction}
\alias{add.diffraction}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Add all possible diffraction lines to a dispersion plot.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
add.diffraction(k = seq(par()$usr[1], par()$usr[2], , 100), kgx, kgv, m.range = c(-5, 5), n.range = c(-5, 5), phi = 0, alpha = pi/2, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{k}{
the k range to use. default is the current plotting coordinates}
  \item{kgx}{
in-plane (phi=0) grating vector}
  \item{kgv}{
out-of-plane (phi=alpha) grating vector (if only one grating vector, set this very high so as to not plot in the frequency range)}
  \item{m.range}{
range of m*kgx values to evaluate
}
  \item{n.range}{
range of n*kgv values to evaluate
}
  \item{phi}{
azimuthal angle in radians
}
  \item{alpha}{
angle between grating vectors in radians
}
  \item{\dots}{
parameters to pass to line()
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
function (k = seq(par()$usr[1], par()$usr[2], , 100), kgx, kgv, 
    m.range = c(-5, 5), n.range = c(-5, 5), phi = 0, alpha = pi/2, 
    ...) 
{
    for (n in -5:5) {
        for (m in -5:5) {
            in.plane <- (sqrt(((k * cos(phi) + m * kgx - n * 
                kgv * cos(alpha))^2) + ((k * sin(phi) - n * kgv * 
                sin(alpha))^2)))
            if (any(max(k)^2 > (in.plane)^2)) {
                lines(diffraction.line(k, kgx, kgv, m, n, phi, 
                  alpha), ...)
            }
        }
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
