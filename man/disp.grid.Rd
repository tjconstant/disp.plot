% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/disp.grid.R
\name{disp.grid}
\alias{disp.grid}
\title{Wrapper function for akima interpolations}
\usage{
disp.grid(x, y, z, nx = length(unique(x)), ny = length(unique(y)),
  method = "bilinear")
}
\arguments{
\item{x}{x vector}

\item{y}{y vector}

\item{z}{z vector}

\item{nx, ny}{The square image will be constructed from nx by ny polygons. If nx and ny are equal to the defaults, length(unique(x)) and length(unique(y)), the plot produced will be a standard (uninterpolated) colorplot. Higher values of nx and ny will produce smoother plots.}

\item{method}{Specifies the interpolation method to use. Default is 'bilinear' using akima::interp, you can also use 'bicubic' (from akima::bicubic.grid).}
}
\value{
returns a list
x
a nx by ny matrix of interpolated x values
y
a nx by ny matrix of interpolated y values
z
a nx by ny matrix of interpolated z values
}
\description{
This function takes standard long dataset expected by the package's image.plot based functions and returns the appropriate interpolated matrices for the xyz values. Mostly used internally in disp.plot and ggdisp.plot.
}
\examples{
data(SPPdispersion)
a<-disp.grid( SPPdispersion$wavelength*1e9,
              SPPdispersion$angle,
              SPPdispersion$reflection)
image(a$x)
image(a$z)
}
\seealso{
\code{\link[akima]{interp}},
\code{\link[akima]{bicubic.grid}}
}
