\name{disp.plot}
\alias{disp.plot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Original disp.plot
}
\description{
to do
}
\usage{
disp.plot(tk, tf, tr, plotXLim = c(min(tk, na.rm = TRUE), max(tk, na.rm = TRUE)), plotYLim = c((min(tf, na.rm = TRUE)), max(tf, na.rm = TRUE)), ncol = 64, nrow = 64, smooth = 0, smoothY = smooth, zScale = c(0, 1), colScale = grey(0:1000/1000), ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tk}{
%%     ~~Describe \code{tk} here~~
}
  \item{tf}{
%%     ~~Describe \code{tf} here~~
}
  \item{tr}{
%%     ~~Describe \code{tr} here~~
}
  \item{plotXLim}{
%%     ~~Describe \code{plotXLim} here~~
}
  \item{plotYLim}{
%%     ~~Describe \code{plotYLim} here~~
}
  \item{ncol}{
%%     ~~Describe \code{ncol} here~~
}
  \item{nrow}{
%%     ~~Describe \code{nrow} here~~
}
  \item{smooth}{
%%     ~~Describe \code{smooth} here~~
}
  \item{smoothY}{
%%     ~~Describe \code{smoothY} here~~
}
  \item{zScale}{
%%     ~~Describe \code{zScale} here~~
}
  \item{colScale}{
%%     ~~Describe \code{colScale} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
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



\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (tk, tf, tr, plotXLim = c(min(tk, na.rm = TRUE), max(tk, 
    na.rm = TRUE)), plotYLim = c((min(tf, na.rm = TRUE)), max(tf, 
    na.rm = TRUE)), ncol = 64, nrow = 64, smooth = 0, smoothY = smooth, 
    zScale = c(0, 1), colScale = grey(0:1000/1000), ...) 
{
    require("fields")
    require("sfsmisc")
    imageZ <- as.image(tr, cbind(tk, tf), nrow = nrow, ncol = ncol, 
        na.rm = TRUE)
    if (smooth > 0) {
        tempX <- seq(min(tk, na.rm = TRUE), max(tk, na.rm = TRUE), 
            , smooth)
        tempY <- seq(min(tf, na.rm = TRUE), max(tf, na.rm = TRUE), 
            , smoothY)
        obj <- as.list(imageZ)
        loc <- make.surface.grid(list(tempX, tempY))
        look <- interp.surface(obj, loc)
        image.plot(as.surface(loc, look), ylim = plotYLim, xlim = plotXLim, 
            zlim = zScale, col = colScale, xaxt = "n", yaxt = "n", 
            ...)
    }
    else {
        image.plot(imageZ, ylim = plotYLim, xlim = plotXLim, 
            zlim = zScale, col = colScale, xaxt = "n", yaxt = "n", 
            ...)
    }
    lines(c(0, 0), c(0, 0))
    aX <- axTicks(1)
    aY <- axTicks(2)
    axis(1, at = aX, label = axTexpr(1, aX, drop.1 = FALSE))
    axis(2, at = aY, label = axTexpr(2, aY, drop.1 = FALSE))
    box()
    if (smooth > 0) 
        return(as.surface(loc, look))
    else return(imageZ)
  }
}

\keyword{ ~kwd1 }
\keyword{ ~kwd2 }
