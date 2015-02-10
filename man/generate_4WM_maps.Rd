\name{generate_4WM_maps}
\alias{generate_4WM_maps}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Map Four-Wave Mixing Angles and Wavelengths
}
\description{
Plots a set of maps to show wavelegths generated and angles of emmssion for four wave-mixing experiments.
}
\usage{
generate_4WM_maps(
  lambda_1 = seq(545, 615, 0.5), 
  lambda_2 = seq(545, 615, 0.5), 
  theta_1 = 20, 
  theta_2 = 40, 
  filters = seq(350, 850, 50), 
  suppress_wavelength_plot = F)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lambda_1}{
vector of wavelength ranges used for lambda_1
}
  \item{lambda_2}{
vector of wavelength ranges used for lambda_2
}
  \item{theta_1}{
angle of incidence for light of lambda_1
}
  \item{theta_2}{
angle of incidence for light of lambda_2
}
  \item{filters}{
optional contours are added is a vector of filter values are provided
}
  \item{suppress_wavelength_plot}{
stop the function plotting the wavelegth generated plot
}
}
\details{
This process assumes 2*omega_1-omega_2, with 2 photons provided by the light of wavelength lambda_1 and one from lambda_2.
}
\value{
Returns plots of wavelengths generated and angles of emmission
}
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
generate_4WM_maps()
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
