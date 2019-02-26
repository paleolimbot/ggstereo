
#' An azimuth/zenith coordinate system
#'
#' This coordinate system imagines that your data will be plotted on the bottom of a
#' sphere, oriented such that 0 azimuth is at the positive x-axis, and 0 zenith is at the
#' top of the bowl. Increasing azimuth is counterclockwise, and zenith angles that are more
#' negative move to the center of the plot. Positive zenith angles are not relevant in this
#' coordinate system, and are squished to zero with a warning. A more approachable
#' coordinate system is \link{coord_bearing_dip}.
#'
#' @param projection Use stereographic to look at the bottom of the sphere as if you are
#'   situated at the top of the sphere (probably what you want); use orthographic to look
#'   at the bottom of the sphere as if you were very far away from it.
#'
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' df <- data.frame(
#'   azimuth = seq(0, 4*pi, length.out = 40),
#'   zenith = seq(0, -pi / 2, length.out = 40)
#' )
#'
#' ggplot(df, aes(azimuth, zenith)) +
#'   geom_path(col = "red") +
#'   geom_point(col = "blue") +
#'   coord_azimuth_zenith() +
#'   scale_x_continuous(
#'     breaks = radians(seq(0, 330, by = 30)),
#'     labels = degrees
#'   )
#'
#'
coord_azimuth_zenith <- function(projection = c("stereographic", "orthographic")) {
  projection <- match.arg(projection)
  ggplot2::ggproto(NULL, CoordAzimuthZenith, projection = projection)
}


#' A bearing/dip coordinate system
#'
#' This coordinate system imagines that your data will be plotted on the bottom of a
#' sphere, as viewed from the inside, oriented such that 0 bearing is due north
#' and and 0 plunge is horizontal. Increasing plunge values plot towards the centre of
#' the plot, and increasing bearing values plot clockwise from north.
#'
#' @inheritParams coord_azimuth_zenith
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' df <- data.frame(
#'   bearing = seq(0, 4*180, length.out = 40),
#'   plunge = seq(0, 90, length.out = 40)
#' )
#'
#' ggplot(df, aes(bearing, plunge)) +
#'   geom_path(col = "red") +
#'   geom_point(col = "blue") +
#'   coord_bearing_plunge() +
#'   scale_x_continuous(breaks = seq(0, 330, by = 30))
#'
coord_bearing_plunge <- function(projection = c("stereographic", "orthographic")) {
  projection <- match.arg(projection)
  ggplot2::ggproto(
    NULL, CoordAzimuthZenith,
    projection = projection,
    trans_azimuth = scales::trans_new(
      "bearing",
      transform = function(x) radians(90 - x),
      inverse = function(x) 90 - degrees(x)
    ),
    trans_zenith = scales::trans_new(
      "dip",
      transform = function(x) radians(-x),
      inverse = function(x) -degrees(x)
    )
  )
}


#' Pretty breaks for coord_bearing_plunge
#'
#' @param breaks,minor_breaks Where break lines and labels are drawn
#' @param labels Labels or label function that produces labels from breaks
#' @param ... Passed to \link[ggplot2]{scale_x_continuous}.
#'
#' @export
#'
scale_x_bearing <- function(
  breaks = seq(0, 330, by = 30),
  minor_breaks = seq(0, 350, by = 10), ...) {
  ggplot2::scale_x_continuous(
    breaks = breaks,
    limits = c(0, 360),
    minor_breaks = minor_breaks,
    oob = function(x, ...) x,
    ...
  )
}

#' @rdname scale_x_bearing
#' @export
scale_x_compass <- function(breaks = c(0, 90, 180, 270), labels = c("N", "E", "S", "W"),
                            minor_breaks = seq(0, 330, by = 30), ...) {
  ggplot2::scale_x_continuous(
    breaks = breaks,
    limits = c(0, 360),
    minor_breaks = minor_breaks,
    labels = labels,
    oob = function(x, ...) x,
    ...
  )
}

#' Pretty breaks for coord_azimuth_zenith
#'
#' @inheritParams scale_x_bearing
#'
#' @export
#'
scale_x_azimuth <- function(
  breaks = seq(0, 7*pi/4, by = pi/4),
  labels = pi_labels, ...) {
  ggplot2::scale_x_continuous(
    breaks = breaks,
    limits = c(0, 2*pi),
    labels = labels,
    oob = function(x, ...) x,
    ...
  )
}

#' @rdname scale_x_azimuth
#' @export
pi_labels <- function(breaks, ...) {
  labs <- paste(format(breaks / pi, ...), "pi")
  labs[breaks == 0] <- "0"
  labs[breaks == pi] <- gsub("^1(\\.0+)?\\s*", "", labs[breaks == pi])
  labs
}

bearing_trans <- scales::trans_new(
  "bearing",
  transform = function(x) radians(90 - x),
  inverse = function(x) 90 - degrees(x)
)

dip_trans <- scales::trans_new(
  "dip",
  transform = function(x) radians(-x),
  inverse = function(x) -degrees(x)
)

#' @rdname coord_azimuth_zenith
#' @export
CoordAzimuthZenith <- ggplot2::ggproto(
  "CoordAzimuthZenith", ggplot2::Coord,
  aspect = function(ranges) 1,
  # the transformation ensures range 0..1,
  # however with clipping, points on the unit circle aren't guaranteed
  # to not be
  clip = "off",

  setup_panel_params = function(self, scale_x, scale_y, params = list()) {

    # the scale breaks are meaningful here, but they must get transformed to
    # azimuth/zenith space
    azimuth_params <- list(
      azimuth.range = scale_x$get_limits(),
      azimuth.labels = scale_x$get_labels(),
      azimuth.major = as.numeric(self$trans_azimuth$transform(scale_x$get_breaks())),
      azimuth.major_source = as.numeric(scale_x$get_breaks()),
      azimuth.minor = as.numeric(self$trans_azimuth$transform(scale_x$get_breaks_minor())),
      azimuth.minor_source = as.numeric(scale_x$get_breaks_minor())
    )

    # the dummy text is the text that decides how much room is left for the axes
    # even though it will not appear
    if(length(azimuth_params$azimuth.major) > 0) {
      cos_breaks <- cos(azimuth_params$azimuth.major)
      sin_breaks <- sin(azimuth_params$azimuth.major)
      labels <- azimuth_params$azimuth.labels
      most_left <- which((cos_breaks < 0) & (cos_breaks == min(cos_breaks)))[1]
      most_right <- which((cos_breaks > 0) & (cos_breaks == max(cos_breaks)))[1]
      most_bottom <- which((sin_breaks < 0) & (sin_breaks == min(sin_breaks)))[1]
      most_top <- which((sin_breaks > 0) & (sin_breaks == max(sin_breaks)))[1]

      axis_params <- list(
        axis.text_left = labels[most_left],
        axis.text_right = labels[most_right],
        axis.text_bottom = labels[most_bottom],
        axis.text_top = labels[most_top]
      )
    } else {
      axis_params <- list(
        axis.text_left = NA,
        axis.text_right = NA,
        axis.text_bottom = NA,
        axis.text_top = NA
      )
    }

    # combine the two lists
    c(azimuth_params, axis_params)
  },

  labels = function(panel_params) {
    panel_params
  },

  render_axis_h = function(self, panel_params, theme) {
    # render an axis that takes up the right amount of space to render the labels at the extremes
    list(
      top = sized_axis_grob(theme, panel_params$axis.text_top, "h"),
      bottom = sized_axis_grob(theme, panel_params$axis.text_bottom, "h")
    )
  },

  render_axis_v = function(self, panel_params, theme) {
    # render an axis that takes up the right amount of space to render the labels at the extremes
    list(
      left = sized_axis_grob(theme, panel_params$axis.text_left, "v"),
      right = sized_axis_grob(theme, panel_params$axis.text_right, "v")
    )
  },

  render_fg = function(panel_params, theme) {
    ggplot2::zeroGrob()
  },

  render_bg = function(self, panel_params, theme) {
    # panel border makes more sense in the backround here with the graticule

    # render as grobs so that we can steal their gpars for our modified
    # circle grobs, since the panel border isn't a rectangle
    background <- element_render(theme, "panel.background")
    border <- element_render(theme, "panel.border")

    grid::grobTree(

      # panel background
      if(!is.null(background) && !inherits(background, "zeroGrob")) {
        grid::circleGrob(
          x = 0.5, y = 0.5, r = 0.5,
          gp = background$gp,
          default.units = "npc"
        )
      },

      # graticule
      guide_great_circles(panel_params$azimuth.major, theme, self$project_xyz, "panel.grid"),
      guide_great_circles(panel_params$azimuth.minor, theme, self$project_xyz, "panel.grid.minor"),

      # panel border
      if(!is.null(border) && !inherits(border, "zeroGrob")) {
        grid::circleGrob(
          x = 0.5, y = 0.5, r = 0.5,
          gp = border$gp,
          default.units = "npc"
        )
      },

      # ticks and axis labels
      guide_azimuth(panel_params$azimuth.major, panel_params$azimuth.labels, theme, self$project_xyz)
    )
  },

  backtransform_range = function(self, panel_params) {
    self$range(panel_params)
  },

  range = function(self, panel_params) {
    # summarise_layout() expects that the x and y ranges here
    # match the setting from self$theta and self$r
    # setNames(
    #   list(panel_params$theta.range, panel_params$r.range),
    #   c(self$theta, self$r)
    # )
    # fallback until this gets implemented
    warning("Range calc not implemented", call. = FALSE)
    list(x = panel_params$x.range, y = panel_params$y.range)
  },

  # this allows subclasses to rotate/make negative/unit convert from the data
  # for example, north-based degrees that run clockwise
  trans_azimuth = scales::identity_trans(),
  trans_zenith = scales::identity_trans(),

  project_xyz = function(self, data) {
    # this allows different projections, like stereographic, orthographic
    # output is assumed to be ranged on the unit circle
    # input is an xyz unit vector after adjustment

    if(self$projection == "stereographic") {
      result <- do.call(stereo_project, data[c("x", "y", "z")])
    } else if(self$projection == "orthographic") {
      result <- data.frame(x_proj = data$x, y_proj = data$y)
    } else {
      stop("Unrecognized projection in coord_azimuth_zenith: ", self$projection, call. = FALSE)
    }

    # scale from -1..1 to 0..1
    data.frame(x = result$x / 2 + 0.5, y = result$y / 2 + 0.5)
  },

  transform = function(self, data, range) {
    # the range is ignored here, because it doesn't make sense not to include
    # 0..2pi, -pi/2..0 in this coordinate system
    data$azimuth <- self$trans_azimuth$transform(data$x)
    data$zenith <- self$trans_zenith$transform(data$y)
    if(any(data$zenith > 0)) {
      warning("Zenith values > 0; setting to NA", call. = FALSE)
      data$zenith[data$zenith > 0] <- NA
    }

    # trans has range 0..1 for x and y
    trans <- self$project_xyz(unit_vector(data$azimuth, data$zenith))
    data$x <- trans$x
    data$y <- trans$y

    data
  },

  # this coordinate system doesn't make sense with scales that are not
  # x: 0..2pi and y: -pi/2..0
 # should they get modified here?
  modify_scales = function(scales_x, scales_y) {
    invisible()
  }

)

guide_azimuth <- function(breaks, labels, theme, project) {
  if(length(breaks) == 0) return(ggplot2::zeroGrob())

  # get necessary information from theme + elements
  tick_gp <- element_render(theme, "axis.ticks.x")$gp
  text_el <- ggplot2::calc_element("axis.text.x", theme)
  text_gp <- grid::gpar(
    fontsize = text_el$size,
    fontfamily = text_el$family,
    fontface = text_el$face,
    col = text_el$colour
  )

  # calculate positions of axis ticks such that they have constant length
  tick_length <- theme$axis.ticks.length
  tick_location_start <- project(unit_vector(azimuth = breaks, zenith = 0))
  tl_x0 <- grid::unit(tick_location_start$x, "npc")
  tl_y0 <- grid::unit(tick_location_start$y, "npc")
  tl_x1 = tl_x0 + tick_length * cos(breaks)
  tl_y1 = tl_y0 + tick_length * sin(breaks)

  # calculate positions and adjustments of text (respecting margin)
  margin <- max(text_el$margin)
  txt_x = tl_x1 + margin * cos(breaks)
  txt_y = tl_y1 + margin * sin(breaks)
  txt_hjust <- ifelse(cos(breaks) > 0, 0, 1)
  txt_hjust[abs(cos(breaks)) < 1e-5] <- 0.5
  txt_vjust <- ifelse(sin(breaks) > 0, 0, 1)
  txt_vjust[abs(sin(breaks)) < 1e-5] <- 0.5

  grid::grobTree(
    grid::segmentsGrob(
      x0 = tl_x0, y0 = tl_y0,
      x1 = tl_x1, y1 = tl_y1,
      gp = tick_gp
    ),
    grid::textGrob(
      label = labels,
      x = txt_x, y = txt_y,
      hjust = txt_hjust, vjust = txt_vjust,
      gp = text_gp
    )
  )
}

guide_great_circles <- function(breaks, theme, project, element_name) {
  major_gc_grobs <- purrr::map(unique(breaks), function(azimuth) {
    xyz <- unit_vector(azimuth = azimuth, zenith = seq(-pi / 2, 0, length.out = 10))
    coords <- project(xyz)
    element_render(
      theme, element_name,
      x = coords$x, y = coords$y
    )
  })
  do.call(grid::grobTree, major_gc_grobs)
}

# draws a transparent rectangle the right size for an axis
sized_axis_grob <- function(theme, text, orientation = c("v", "h")) {
  if((length(text) == 0) || is.na(text)) return(ggplot2::zeroGrob())
  orientation <- match.arg(orientation)
  text_element <- ggplot2::calc_element("axis.text.x", theme)
  text_grob <- element_render(theme, "axis.text.x", label = text)
  tick_length <- theme$axis.ticks.length
  margin = max(text_element$margin)

  if(orientation == "h") {
    grid::rectGrob(
      width = grid::grobWidth(text_grob),
      height = grid::grobHeight(text_grob) + tick_length + margin,
      gp = grid::gpar(fill = NA, col = "#00000000")
    )
  } else {
    grid::rectGrob(
      width = grid::grobWidth(text_grob) + tick_length + margin,
      height = grid::grobHeight(text_grob),
      gp = grid::gpar(fill = NA, col = "#00000000")
    )
  }
}

element_render <- function(theme, element, ..., name = NULL) {

  # Get the element from the theme, calculating inheritance
  el <- ggplot2::calc_element(element, theme)
  if (is.null(el)) {
    message("Theme element ", element, " missing")
    return(ggplot2::zeroGrob())
  }

  grob <- ggplot2::element_grob(el, ...)
  ggname(paste(element, name, sep = "."), grob)
}

ggname <- function (prefix, grob) {
  grob$name <- grid::grobName(grob, prefix)
  grob
}
