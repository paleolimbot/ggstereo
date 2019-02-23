context("test-proj")

test_that("azimuth and zenith transformations work on -pi-pi, -pi/2-pi/2", {
  spher <- expand.grid(
    azimuth = seq(-pi, pi, length.out = 4),
    zenith = seq(-pi/2, pi/2, length.out = 4)
  )

  unit <- unit_vector(spher$azimuth, spher$zenith)
  spher2 <- azimuth_zenith(unit$x, unit$y, unit$z)
  error <- cbind(
    spher,
    unit,
    new_az = spher2$azimuth,
    new_zen = spher2$zenith,
    daz = spher$azimuth - spher2$azimuth,
    dzen = spher$zenith - spher2$zenith
  )

  expect_true(all(error$daz < 1e-15))
  expect_true(all(error$dzen < 1e-15))
})

test_that("great circle math works", {
  test_gc <- function(x, y, z, length.out = 431) {
    out <- great_circle(x, y, z, length.out = length.out)
    # plot(y~x, unit_circle_pv, asp = 1, type = "l")
    expect_equal(nrow(out), length.out)
    expect_true(all(is.finite(out$x)))
    expect_true(all(is.finite(out$y)))
    expect_true(all(is.finite(out$z)))
    # all vectors should be length 1
    expect_true(all(abs(1 - (out$x^2 + out$y^2 + out$z^2)) < 1e-15))
    expect_true(!any(duplicated(out)))
    # dot product of all great circle vectors with orthogonal vector is 0
    expect_true(all(abs((out$x*x + out$y*y + out$z*z)) < 1e-15))
  }

  test_gc(1, 0, 0)
  test_gc(0, 1, 0)
  test_gc(0, 0, 1)
  test_gc(1, 1, 1)
})

test_that("stereographic projection works as intended", {
  unit_circle_pv <- great_circle(0, 0, 1)
  unit_circle_proj <- do.call(stereo_project, unit_circle_pv)
  grat <- graticule_net()
  grat_proj <- data.frame(grat, do.call(stereo_project, grat[c("x", "y", "z")]), stringsAsFactors = FALSE)

  # orthographic
  plot(y~x, unit_circle_pv, asp = 1, type = "l", lwd = 2, main = "Orthographic graticule")
  for(item in split(grat, grat$id)) {
    item <- item[item$z <= 0, ]
    item <- item[order(item$y, item$x, item$z), ]
    lines(item$x, item$y, col = "grey30")
  }

  # stereographic (lines intersect at 90 degrees)
  plot(y_proj~x_proj, unit_circle_proj, asp = 1, type = "l", lwd = 2, main = "Steregerphic graticule")
  for(item in split(grat_proj, grat_proj$id)) {
    #item <- item[item$z <= 0, ]
    #item <- item[order(item$y_proj), ]
    lines(item$x_proj, item$y_proj, col = "grey30")
  }

  # graphical test
  expect_true(TRUE)
})
