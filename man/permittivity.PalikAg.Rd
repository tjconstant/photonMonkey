\name{permittivity.PalikAg}
\alias{permittivity.PalikAg}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Complex Permittivity of Silver in the Visible Regime (function)
}
\description{
Returns the Complex Permittivity of Silver in the Visible Regime according to ref [1], using a spline interpolation of the dataset data(PalikAg).
}
\usage{
permittivity.PalikAg(wavelength)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{wavelength}{
Wavelength in nanometers
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A dataframe containing wavelength, and interpolated complex permittivity as a function of the supplied wavelengths.
}
\references{
[1] E. D. Palik, Handbook of Optical Constants of Solids (Academic Press, 1985), p. 1096.
}
\author{
Tom Constant
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{PalikAg}}
}
\examples{
x<-permittivity.PalikAg(seq(450,850,,100))
plot(x$wavelength,x$epsilon.real,type='l',lwd=2, col=2,xlab="wavelength",ylab="epsilon real")

data(PalikAg)
points(PalikAg$Wavelength,PalikAg$EpsilonReal)
}
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
