\name{semicircle}
\alias{semicircle}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Draw a semi-circle
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
semicircle(x.center, y.center, radius, start_angle, end_angle, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x.center}{
%%     ~~Describe \code{x.center} here~~
}
  \item{y.center}{
%%     ~~Describe \code{y.center} here~~
}
  \item{radius}{
%%     ~~Describe \code{radius} here~~
}
  \item{start_angle}{
%%     ~~Describe \code{start_angle} here~~
}
  \item{end_angle}{
%%     ~~Describe \code{end_angle} here~~
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

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x.center, y.center, radius = r, start_angle, end_angle, 
    ...) 
{
    nseg = 360
    start_angler = start_angle * pi/180
    end_angler = end_angle * pi/180
    xx <- x.center + radius * cos(seq(start_angler, end_angler, 
        length.out = nseg))
    yy <- y.center + radius * sin(seq(start_angler, end_angler, 
        length.out = nseg))
    print(x.center)
    return(cbind(xx, yy))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
