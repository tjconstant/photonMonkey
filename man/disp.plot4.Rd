\name{disp.plot4}
\alias{disp.plot4}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
New Super-Experimental disp.plot3 function
}
\description{
New Super-Experimental disp.plot3 function including gridded bivariate interpolation for irregular data from the akima package.
}
\usage{
disp.plot4(x,y,z,fx=nothing_x,fy=nothing_y,nx=length(unique(x)),ny=length(unique(y)))
}
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
function of x and y returning the functional transformation of the x-axis
}
  \item{fy}{
function of x and y returning the functional transformation of the y-axis
}
  \item{nx,ny}{
The square image will be constructed from nx by ny polygons. If nx and ny are equal to the defaults, length(unique(x)) and length(unique(y)), the plot produced will be identical to disp.plot3. Higher values of nx and ny will produce smoother plots.
}
  \item{...}{
  The usual arguments to the image function as x,y,or z or as a list with x,y,z as components. One can also include a breaks argument for an unequal color scale with color scale boundaries at the breaks (see example in the image.plot documentation).
  }
}
\details{
A new version of the disp.plot function for plotting non-linearlly spaced data, with the addition of bivariate smoothing of the dataset. Experimental!

Because the smoothing function is always called, disp.plot4 is x5 times slower than disp.plot3 when producing the same image.

Also note, the use of fx and fy is different in this case. In this function fx and fy are passed as functions, not raw vectors. See the examples for how to use this method to plot dispersion.
}
\value{
Returns an image plot of the data
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
\code{\link[fields]{image.plot}}
\code{\link[akima]{interp}}
}
\examples{
data(SPPdispersion)

x<-SPPdispersion$wavelength*1e9
y<-SPPdispersion$angle
z<-SPPdispersion$reflection

kx<-function(x,y) (2*pi/(x*1e-9))*sin(y*pi/180)
omega<-function(x,y) 2*pi*3e8/(x*1e-9)

disp.plot4(x,y,z)

disp.plot4(x,y,z,fx=kx,fy=omega,nx=300,ny=300)
}

