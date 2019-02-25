
#' Convert to/from degrees and radians
#'
#' @param degrees A value in degrees
#' @param radians A value in radians
#'
#' @export
#'
#' @examples
#' radians(180)
#' degrees(pi)
#'
radians <- function(degrees) {
  degrees / 180 * pi
}

#' @rdname radians
#' @export
degrees <- function(radians) {
  radians * 180 / pi
}

unit_vector <- function(azimuth, zenith) {
  data.frame(
    x = cos(azimuth) * cos(zenith),
    y = sin(azimuth) * cos(zenith),
    z = sin(zenith)
  )
}

azimuth_zenith <- function(x, y, z) {
  # r <- sqrt(x^2 + y^2 + z^2)
  data.frame(
    azimuth = atan2(y, x),
    zenith = atan2(z, sqrt(x*x + y*y))
  )
}

stereo_project <- function(x, y, z) {
  r <- sqrt(x^2 + y^2 + z^2)
  data.frame(
    x_proj = (x / r) / (1 - z / r),
    y_proj = (y / r) / (1 - z / r)
  )
}

great_circle <- function(x, y, z, length.out = 400) {
  stopifnot(length(x) == 1, length(y) == 1, length(z) == 1)

  # dot-product = 0 for orthogonality
  # VxU = x v1 + y v2 + z v3 = 0

  theta_out <- seq(-pi, pi, length.out = length.out)

  if(z == 0 && y != 0) {
    # solve for v2
    # v2 = (-x * v1 - z * v3) / y
    v1 <- cos(theta_out)
    v3 <- sin(theta_out)
    v2 <- (-x * v1 - z * v3) / y
  } else if(z == 0 && y == 0) {
    # solve for v1
    # v1 <- (-y * v2 - z * v3) / x
    v2 <- cos(theta_out)
    v3 <- sin(theta_out)
    v1 <- (-y * v2 - z * v3) / x
  } else if(z != 0) {
    # solve for v3
    # v3 = (-x v1 - y v2) / z
    v1 <- cos(theta_out)
    v2 <- sin(theta_out)
    v3 <- (-x * v1 - y * v2) / z
  } else {
    stop("Zero-length vector")
  }

  r <- sqrt(v1*v1 + v2*v2 + v3*v3)
  data.frame(x = v1 / r, y = v2 / r, z = v3 / r)
}

small_circle <- function(x, y, z,  length.out = 400) {
  stopifnot(length(x) == 1, length(y) == 1, length(z) == 1, (x^2 + y^2 + z^2) <= 1)

  # here, x, y, and z describe a pole, and little r describes how far out from the centre
  # the plane is...rc describes the radius of the small circle
  rc <- sqrt(1 - (x^2 + y^2 + z^2))

  # find great circle orthogonal to xyz, scale to radius rc
  gc <- great_circle(x, y, z, length.out = length.out)
  circle <- gc * rc

  # add to x, y, and z
  data.frame(x = x + circle$x, y = y + circle$y, z = z + circle$z)
}

graticule_net <- function(
  # orientation = 0,
  great_circles = radians(seq(0, 170, by = 10)),
  small_circles = radians(seq(10, 170, by = 10))
) {
  gc_spec <- data.frame(
    x = cos(great_circles),
    y = 0,
    z = sin(great_circles),
    id = paste0("great_circle.", seq_along(great_circles)),
    stringsAsFactors = FALSE
  )

  sc_spec <- data.frame(
    x = 0,
    y = cos(small_circles),
    z = 0,
    id = paste0("small_circle.", seq_along(small_circles)),
    stringsAsFactors = FALSE
  )

  gc <- do.call(
    rbind,
    purrr::pmap(gc_spec, function(x, y, z, id) {
      result <- great_circle(x, y, z)
      data.frame(id, result, stringsAsFactors = FALSE)
    })
  )

  sc <- do.call(
    rbind,
    purrr::pmap(sc_spec, function(x, y, z, id) {
      result <- small_circle(x, y, z)
      data.frame(id, result, stringsAsFactors = FALSE)
    })
  )

  rbind(gc, sc)
}
