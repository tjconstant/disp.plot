#' Wrapper function for akima interpolations
#'
#' @description This function takes standard long dataset expected by the package's image.plot based functions and returns the appropriate interpolated matrices for the xyz values. Mostly used internally in disp.plot and ggdisp.plot.
#' @param x x vector
#' @param y y vector
#' @param z z vector
#' @param nx,ny The square image will be constructed from nx by ny polygons. If nx and ny are equal to the defaults, length(unique(x)) and length(unique(y)), the plot produced will be a standard (uninterpolated) colorplot. Higher values of nx and ny will produce smoother plots.
#' @param method Specifies the interpolation method to use. Default is 'bilinear' using akima::interp, you can also use 'bicubic' (from akima::bicubic.grid).
#'
#' @return
#' returns a list
#' x
#' a nx by ny matrix of interpolated x values
#' y
#' a nx by ny matrix of interpolated y values
#' z
#' a nx by ny matrix of interpolated z values
#'
#' @export
#'
#' @seealso \code{\link[akima]{interp}},
#' \code{\link[akima]{bicubic.grid}}
#'
#' @examples
#' data(SPPdispersion)
#' a<-disp.grid( SPPdispersion$wavelength*1e9,
#'               SPPdispersion$angle,
#'               SPPdispersion$reflection)
#' image(a$x)
#' image(a$z)

disp.grid<-function(x,
                    y,
                    z,
                    nx=length(unique(x)),
                    ny=length(unique(y)),
                    method="bilinear"){

  if(x[2]-x[1]==0 & y[2]-y[1]!=0 & method=="bicubic"){
    warning("x and y values are in unexpected order. Try swapping x & y.")
  }

  if(method=="bilinear"){

    interp<-akima::interp
    int_z<-interp(x,y,z,xo=seq(min(x),max(x),length=nx),yo=seq(min(y),max(y),length=ny),linear=T,duplicate = "mean")

    x2<-matrix(int_z$x,nrow=nx,ncol=ny)
    y2<-matrix(int_z$y,nrow=nx,ncol=ny,byrow=T)
    z2<-int_z$z

    message("Interpolation method: bilinear")

  } else if(method=="bicubic"){

    bicubic.grid<-akima::bicubic.grid

    z.matrix <- matrix(z,ncol = length(unique(y)))

    int_z <- bicubic.grid(x=seq(min(x),max(x),length=length(unique(x))),
                          y=seq(min(y),max(y),length=length(unique(y))),
                          z = z.matrix,
                          xlim = c(min(x),max(x)),
                          ylim=c(min(y),max(y)),
                          dx = (max(x)-min(x))/(nx),
                          dy = (max(y)-min(y))/(ny)
    )

    x2<-matrix(int_z$x,nrow=nx+1,ncol=ny+1)
    y2<-matrix(int_z$y,nrow=nx+1,ncol=ny+1,byrow=T)
    z2<-int_z$z

    message("Interpolation method: bicubic")

  } else {

    stop("Unrecognised interpolation function. Valid methods are 'bilinear' or 'bicubic'")

  }

  return(list(x=x2,y=y2,z=z2))
}
