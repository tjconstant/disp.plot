#' Title
#'
#' @param data
#' @param scales
#' @param params
#' @param fx
#' @param fy
#' @param nx
#' @param ny
#' @param method
#' @param midpoint
#'
#' @return 1
#' @export
#'
#' @examples 1
#' @keywords internal
#'

StatDisp <- ggproto("StatDisp", Stat,


                    compute_group = function(data, scales, params,fx = nothing_x, fy = nothing_y, nx = length(unique(x)),
                                             ny = length(unique(y)), method = "bilinear", midpoint = TRUE) {
                      x<-data$x
                      y<-data$y
                      z<-data$fill

                      interp_grid <- disp.grid(x, y, z, nx, ny, method)
                      x <- interp_grid$x
                      y <- interp_grid$y
                      z <- interp_grid$z
                      poly.image.regrid <- fields::poly.image.regrid
                      x2 <- fx(x, y)
                      y2 <- fy(x, y)
                      Dx <- dim(x2)
                      Dy <- dim(y2)
                      if (any((Dx - Dy) != 0)) {
                        stop(" x and y matrices should have same dimensions")
                      }
                      Dz <- dim(z)
                      if (all((Dx - Dz) == 0) & !midpoint) {
                        x <- poly.image.regrid(x2)
                        y <- poly.image.regrid(y2)
                      }
                      N <- ncol(x2)
                      Nm1 <- N - 1
                      M <- nrow(x2)
                      Mm1 <- M - 1
                      xps<-numeric(length = Nm1*Mm1*5)
                      yps<-numeric(length = Nm1*Mm1*5)
                      zps<-numeric(length(Nm1*Mm1*5))
                      ids<-numeric(length(Nm1*Mm1*5))

                      for (i in (1:Mm1)) {

                        xp <- cbind(x2[i, 1:Nm1], x2[i + 1, 1:Nm1], x2[i + 1,
                                                                       2:N], x2[i, 2:N], rep(NA, Nm1))
                        yp <- cbind(y2[i, 1:Nm1], y2[i + 1, 1:Nm1], y2[i + 1,
                                                                       2:N], y2[i, 2:N], rep(NA, Nm1))
                        id <- i * (length(rep(rep(1:Nm1), each = 5)) + 1) + rep(rep(1:Nm1),
                                                                                each = 5)
                        xps[((i+(i-1)*(Nm1*5)-(i-1))):(i*(Nm1*5))]<-c(t(xp))
                        yps[((i+(i-1)*(Nm1*5)-(i-1))):(i*(Nm1*5))]<-c(t(yp))
                        ids[((i+(i-1)*(Nm1*5)-(i-1))):(i*(Nm1*5))]<-id
                        pcol <- c(z[i, 1:Nm1])
                        zp <- rep(pcol, each = 5)
                        zps[((i+(i-1)*(Nm1*5)-(i-1))):(i*(Nm1*5))]<-zp
                      }

                      data.frame(x=xps, y=yps, fill=zps, colour=zps, group=ids)
                    },

                    required_aes = c("x", "y","fill")
)

#' Title
#'
#' @param mapping
#' @param data
#' @param geom
#' @param position
#' @param na.rm
#' @param show.legend
#' @param inherit.aes
#' @param ...
#'
#' @return 1
#' @export
#'
#' @examples 1
stat_disp <- function(mapping = NULL, data = NULL, geom = "polygon",
                      position = "identity", na.rm = FALSE, show.legend = NA,
                      inherit.aes = TRUE, ...) {
  layer(
    stat = StatDisp, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm,...)
  )
}
