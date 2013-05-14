\name{PalikAg}
\alias{PalikAg}
\docType{data}
\title{
Complex Permittivity of Silver in the Visible Regime (dataset)
}
\description{
The measured permittivity of silver in the visible regime
}
\usage{data(PalikAg)}
\format{
  A data frame with 79 observations on the following 3 variables.
  \describe{
    \item{\code{X}}{Row index}
    \item{\code{Wavelength}}{Wavelength of light (nm)}
    \item{\code{EpsilonReal}}{Real part of the complex permittivity}
    \item{\code{EpsilonImaginary}}{Imaginary part of the complex permittivity}
}
}
\details{
Very useful if samples were prepared at Exeter using the same methods as in ref [1].
}
\source{
[1] E. D. Palik, Handbook of Optical Constants of Solids (Academic Press, 1985), p. 1096.
}
\references{
[1] E. D. Palik, Handbook of Optical Constants of Solids (Academic Press, 1985), p. 1096.
}
\examples{
data(NashSamblesAg)
## maybe str(NashSamblesAg) ; plot(NashSamblesAg) ...
}
\keyword{datasets}
