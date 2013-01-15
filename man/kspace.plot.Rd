\name{kspace.plot}
\alias{kspace.plot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
k-space map from raw scatterometry image
}
\description{
produces k-space scattergram from raw IS image
}
\usage{
kspace.plot(filename, center.correction = NA, radius.correction = NA, phi.correction = 0, lambda0 = as.numeric(substr(filename, 1, 3)), image.colour = NA, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{filename}{
JPEG file. for speed a maximum size is recommened as 600x600 pixels
}
  \item{center.correction}{
The center correction. If none is specified, the function asks for you to select the center of the image.
}
  \item{radius.correction}{
The center correction. If none is specified, the function asks for you to select a point on the radius of the image.
}
  \item{phi.correction}{
azimuthal angle correction in degrees. if none is specified, 0 is assumed.
}
  \item{lambda0}{
The function will attempt to use the filename for the incident wavelength. If not, use this varible to set the wavelength (in nm)
}
  \item{image.colour}{
The function will take maximum colour value of the image for it's upper colour scale value. If this is incorrect (a white pixel will result in a greyscale plot), a colour can be specified using this varible.
}
  \item{\dots}{
Extra graphical parameters passed to image.plot()
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A list containing center.correction, radius.correction, phi.correction and the kx, ky and z matrices.
\references{
%% ~put references to the literature/web site here ~
}
\author{
Tom Constant
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
