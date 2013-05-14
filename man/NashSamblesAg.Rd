\name{NashSamblesAg}
\alias{NashSamblesAg}
\docType{data}
\title{
Complex Permittivity of Silver in the Visible Regime (dataset)
}
\description{
The measured permittivity of silver in the visible Regime
=======
Complex Permittivity of Silver in the Visible Regieme (dataset)
}
\description{
The measured permittivity of silver in the visible regieme
}
\usage{data(NashSamblesAg)}
\format{
  A data frame with 19 observations on the following 8 variables.
  \describe{
    \item{\code{X}}{Row index}
    \item{\code{Wavelength}}{Wavelength of light (nm)}
    \item{\code{EpsilonReal}}{Real part of the complex permittivity}
    \item{\code{EpsilonRealError}}{Absoloute error on the Real part of the complex permittivity}
    \item{\code{EpsilonImaginary}}{Imaginary part of the complex permittivity}
    \item{\code{EpsilonImaginaryError}}{Absoloute error on the Imaginary part of the complex permittivity}
    \item{\code{DielectricLossTangent}}{The dielectric loss tangent (-EpsilonImaginary/EpsilonReal)}
    \item{\code{DielectricLossTangentError}}{Absolute error on the dielectric loss tangent}
  }
}
\details{
Very useful if samples were prepared at Exeter using the same methods as in ref [1].
}
\source{
[1] D. J. Nash and J. R. Sambles, J. Mod. Op. 43, 81–91 (1996).
}
\references{
[1] D. J. Nash and J. R. Sambles, J. Mod. Op. 43, 81–91 (1996).
}
\examples{
data(NashSamblesAg)
## maybe str(NashSamblesAg) ; plot(NashSamblesAg) ...
}
\keyword{datasets}
