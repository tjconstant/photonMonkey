\name{disp.plot3}
\alias{disp.plot3}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plotting of irregular spaced matrix data with aribitary transformations
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
disp.plot3(x, y, z, fx = x, fy = y, nx = length(unique(x)), ny = length(unique(y)), ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
x vector
}
  \item{y}{
y vector
}
  \item{z}{
z vector
}
  \item{fx}{
functional transformation of length(x)
}
  \item{fy}{
functional transformation of length(x)
}
  \item{nx}{
length of unique x vector values (usually obtained from length(x))
}
  \item{ny}{
length of unique y vector values (usually obtained from length(y))
}
  \item{\dots}{
graphical parameters to be passed to image.plot
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
\code{\link[image.plot:fields]{image.plot()}}
}
\examples{
library(fields) #required packages

data(SPPdispersion) #load example dataset

wavelength<-SPPdispersion$wavelength
angle<-SPPdispersion$angle
reflection<-SPPdispersion$reflection

disp.plot3(wavelength,angle,reflection) #plot data

kx<-(2*pi/wavelength)*sin(angle*pi/180) #transformation to dispersion plots
omega<-2*pi*3e8/wavelength

disp.plot3(wavelength,angle,reflection,kx,omega) #plot transformed data

}