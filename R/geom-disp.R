ggproto<-ggplot2::ggproto

GeomPolygonDisp <- ggproto("GeomPolygonDisp", Geom,

                           draw_panel = function(data, panel_scales, coord) {
                             n <- nrow(data)
                             if (n == 1) return(zeroGrob())

                             munched <- coord_munch(coord, data, panel_scales)
                             # Sort by group to make sure that colors, fill, etc. come in same order
                             munched <- munched[order(munched$group), ]

                             # For gpar(), there is one entry per polygon (not one entry per point).
                             # We'll pull the first value from each group, and assume all these values
                             # are the same within each group.
                             first_idx <- !duplicated(munched$group)
                             first_rows <- munched[first_idx, ]

                             ggplot2:::ggname("geom_disp",
                                              polygonGrob(munched$x, munched$y, default.units = "native",
                                                          id = munched$group,
                                                          gp = gpar(
                                                            col = alpha(first_rows$fill, first_rows$alpha),
                                                            fill = alpha(first_rows$fill, first_rows$alpha),
                                                            lwd = first_rows$size * .pt,
                                                            lty = first_rows$linetype
                                                          )
                                              )
                             )
                           },

                           default_aes = aes(colour = "NA", fill = "grey20", size = 0.5, linetype = 1,
                                             alpha = NA),

                           handle_na = function(data, params) {
                             data
                           },

                           required_aes = c("x", "y"),

                           draw_key = draw_key_polygon
)

#' image plot of irregular spaced matrix data with aribitary transformations
#'
#' @inheritParams ggplot2::geom_polygon
#' @param formula The modelling formula passed to \code{lm}. Should only
#'   involve \code{y} and \code{x}
#' @param n Number of points used for interpolation.
#' @param ... Additional arguments to be passed to the geom or stat
#'
#' @export
#'
#' @examples
#' data("SPPdispersion")
#' SPPdispersion$wavelength<-SPPdispersion$wavelength*1e9
#'
#' q<-ggplot(SPPdispersion, aes(x=wavelength, y=angle,fill=reflection)) +
#'   geom_disp()
#'
#' print(q)
#'
#' kx<-function(x,y) (2*pi/(x*1e-9))*sin(y*pi/180)
#' omega<-function(x,y) 2*pi*3e8/(x*1e-9)
#'
#' q<-ggplot(SPPdispersion, aes(x=wavelength, y=angle,fill=reflection)) +
#'   geom_disp(fx=kx,fy=omega,nx=200,ny=200)
#'
#' print(q)
#' fx <- function(x,y) x*sin(y*pi/180)
#' fy <- function(x,y) x*cos(y*pi/180)
#'
#' q<-ggplot(SPPdispersion, aes(x=wavelength, y=angle,fill=reflection)) +
#'   geom_disp(fx=fx,fy=fy)+coord_equal()
#' print(q)

geom_disp <- function(mapping = NULL, data = NULL,
                      position = "identity", na.rm = FALSE, show.legend = NA,
                      inherit.aes = TRUE, ...) {
  layer(
    stat = StatDisp, geom = GeomPolygonDisp, data = data, mapping = mapping,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}
