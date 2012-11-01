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
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x, y, z, fx = x, fy = y, nx = length(unique(x)), ny = length(unique(y)), 
    ...) 
{
    require("fields")
    dim(fx) <- c(nx, ny)
    dim(fy) <- c(nx, ny)
    dim(z) <- c(nx, ny)
    if (1 < 2) {
        image.plot(fx, fy, z, nlevel = 100, ...)
    }
    else {
        interpolated_Matrix <- akima:::interp(fx, fy, z, duplicate = "mean", 
            linear = FALSE, extrap = FALSE, xo = seq(min(fx), 
                max(fx), length = 40), yo = seq(min(fy), max(fy), 
                length = 40))
        image.plot(interpolated_Matrix$x, interpolated_Matrix$y, 
            interpolated_Matrix$z, nlevel = 100, col = grey(0:100/100), 
            ...)
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
