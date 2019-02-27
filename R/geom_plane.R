
#' Draw a plane on a steronet
#'
#' @param mapping A mapping
#' @param data Data
#' @param geom Which geometry to use
#' @param position Which position to use
#' @param ... Set aesthetics
#' @param n Number of points to use to generate path
#' @param na.rm Remove points with missing values
#' @param show.legend Show legend
#' @param inherit.aes Inherit aesthetics from the plot
#'
#' @export
#'
geom_plane <- function(mapping = NULL, data = NULL,
                        stat = "identity", position = "identity",
                        ...,
                        n = 101,
                        na.rm = FALSE,
                        show.legend = NA,
                        inherit.aes = TRUE) {

  ggplot2::layer(
    geom = GeomPlane,
    stat = stat,
    data = data,
    mapping = mapping,
    position = position,
    params = list(...),
    inherit.aes = inherit.aes,
    show.legend = show.legend
  )
}

#' @rdname geom_plane
#' @export
GeomPlane <- ggplot2::ggproto(
  "GeomPlane", ggplot2::Geom,

  required_aes = c("strike", "dip"),
  default_aes = ggplot2::aes(colour = "black", size = 0.5, linetype = 1, alpha = NA),

  draw_panel = function(data, panel_params, coord) {

    coordinates <- purrr::imap(purrr::transpose(data), function(item, i) {
      # calculate normal plane vector in azimuth/zenith space
      pole_trend <- bearing_trans$transform(item$strike)
      pole_plunge <- plunge_trans$transform(90 - item$dip)
      pole_xyz <- unit_vector(pole_trend, pole_plunge)

      # calculate the great circle in azimuth/zenith
      gc <- do.call(great_circle, pole_xyz)
      gc <- gc[gc$z <= 0, , drop = FALSE]
      gc_az <- do.call(azimuth_zenith, gc)


      df <- data.frame(
        x = bearing_trans$inverse(gc_az$azimuth),
        y = plunge_trans$inverse(gc_az$zenith)
      )
      item$group <- i
      df[names(item)] <- item

      df
    })
    data <- do.call(rbind, coordinates)
    data$group <- factor(data$group)

    # invoke the magic of GeomPath
    ggplot2::GeomPath$draw_panel(data, panel_params, coord)
  },

  draw_key = ggplot2::draw_key_path
)
