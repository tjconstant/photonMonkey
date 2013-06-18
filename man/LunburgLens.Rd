\name{LunburgLens}
\alias{LunburgLens}
\docType{data}
\title{
Electric Field in a Lunburg Lens (Dataset)
}
\description{
A spatial dataset of electric field magtinude for a Lunburg Lens designed for surface waves.
}
\usage{data(LunburgLens)}
\format{
  A data frame with 29915 observations on the following 4 variables.
  \describe{
    \item{\code{x}}{x-coordinate}
    \item{\code{y}}{y-coordinate}
    \item{\code{z}}{z-coordinate}
    \item{\code{electric_field}}{Electric Field Magnitude}
  }
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
Thin metamaterial Luneburg lens for surface waves - J. A. Dockrey, M. J. Lockyear, S. J. Berry, S. A. R. Horsley, J. R. Sambles, and A. P. Hibbins. Phys. Rev. B 87, 125137 (2013) http://link.aps.org/doi/10.1103/PhysRevB.87.125137

}
\references{
Thin metamaterial Luneburg lens for surface waves - J. A. Dockrey, M. J. Lockyear, S. J. Berry, S. A. R. Horsley, J. R. Sambles, and A. P. Hibbins. Phys. Rev. B 87, 125137 (2013)
}
\examples{
data(LunburgLens)

disp.plot3(LunburgLens$y,LunburgLens$x,LunburgLens$electric_field,xlab="x",ylab="y")

LunburgLensZ<-matrix(LunburgLens$electric_field,nrow=length(unique(LunburgLens$y)),ncol=length(unique(LunburgLens$x)))

library(fields)

nx<-length(unique(LunburgLens$x))
ny<-length(unique(LunburgLens$y))
           
drape.plot(seq(0,1,,ny),seq(0,1,,nx),LunburgLensZ,theta=45,phi=60,r=10,ltheta=0,lphi=90,shade=0.74,border=NA,expand=0.5)

}
\keyword{datasets}
