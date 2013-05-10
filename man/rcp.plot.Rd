\name{rcp.plot}
\alias{rcp.plot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function to plot the reciprocal lattice of a 2D crystal.
}
\description{
Plot the reciprocal lattice and assosiated diffraction circles for a bigrating.
}
\usage{
rcp.plot(a,b,k0,alpha)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{a}{Period in x direction
}
  \item{b}{Period in v direction
}
  \item{k0}{in-plane wavevector of a grazing photon (for light circles and SPP plot)
}
  \item{alpha}{Angle between x and v in real space (degrees)
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Produces a plot of the lattice and diffraction circles using base
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

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
library(manipulate)
manipulate(rcpPlot(a*1e-9,b*1e-9,k0=2*pi/(lambda0*1e-9),alpha),
           a=slider(200,850,initial=600,step=1,),
           b=slider(200,850,initial=600,step=1),
           alpha=slider(0,90,initial=90),
           lambda0=slider(400,850,initial=633,step=1))
}
