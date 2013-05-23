\name{labels_photonics}
\alias{labels_photonics}
\alias{label_in_plane_wavevector}
\alias{label_angular_frequency}
\alias{label_polar_angle}
\alias{label_azimuthal_angle}
\alias{label_reflection}
\alias{label_wavelength}
\alias{label_frequency}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Convenient Labelling for Photonics Graphs
}
\description{
Quick expression generation for axes labels, etc.
}
\usage{
label_in_plane_wavevector(subscript,si_prefix)
label_angular_frequency()
label_polar_angle()
label_azimuthal_angle()
label_reflection(subscript)
label_wavelength(si_prefix)
label_frequency(si_prefix)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{subscript}{
A charcter string for using in the relavent subscript of the expression.
}
  \item{si_prefix}{
A character string for the SI unit scale prefix. Greek letters may be typed, for example "mu" will display as the greek symbol mu. The default is "phantom()", which produces no prefix.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A R expression for use in labels and text.
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
plot(NA,NA,xlim=c(0,1),ylim=c(0,1))
text(0.5,1,label_angular_frequency())
text(0.5,0.9,label_azimuthal_angle())
text(0.5,0.8,label_in_plane_wavevector(subscript="gamma",si_prefix="c"))
text(0.5,0.7,label_polar_angle())
text(0.5,0.6,label_reflection("TM"))
text(0.5,0.5,label_wavelength(si_prefix="mu"))
text(0.5,0.4,label_frequency(si_prefix="G"))
}

