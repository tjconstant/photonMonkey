\name{permittivity.NashSamblesAg}
\alias{permittivity.NashSamblesAg}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Complex Permittivity of Silver in the Visible Regime (function)
}
\description{
Returns the Complex Permittivity of Silver in the Visible Regime according to ref [1], using a spline interpolation of the dataset data(NashSamblesAg).
}
\usage{
permittivity.NashSamblesAg(wavelength)
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
[1] D. J. Nash and J. R. Sambles, J. Mod. Op. 43, pp81 (1996).
}
\author{
Tom Constant
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{NashSamblesAg}}
}
\examples{
x<-permittivity.NashSamblesAg(seq(450,850,,100))
plot(x$wavelength,x$epsilon.real,type='l',lwd=2, col=2,xlab="wavelength",ylab="epsilon real")

data(NashSamblesAg)
points(NashSamblesAg$Wavelength,NashSamblesAg$EpsilonReal)
}
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
