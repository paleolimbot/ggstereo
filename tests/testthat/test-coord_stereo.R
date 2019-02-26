context("test-coord_stereo")

test_that("coord_bearing_plunge() works with geom_point() and geom_path()", {
  df <- data.frame(
    bearing = seq(0, 4*180, length.out = 40),
    plunge = seq(0, 90, length.out = 40)
  )

  print(
    ggplot2::ggplot(df, ggplot2::aes(bearing, plunge)) +
      ggplot2::geom_path(col = "red") +
      ggplot2::geom_point(col = "blue") +
      coord_bearing_plunge(projection = "stereographic") +
      scale_x_bearing() +
      ggplot2::labs(caption = "spiral to the right with path and points")
  )

})

test_that("bearing_trans and dip_trans work as expected", {
  expect_equal(bearing_trans$transform(90), 0)
  expect_equal(bearing_trans$inverse(0), 90)

  expect_equal(dip_trans$transform(90), -pi/2)
  expect_equal(dip_trans$inverse(-pi/2), 90)
  expect_equal(dip_trans$transform(45), -pi/4)
  expect_equal(dip_trans$inverse(-pi/4), 45)
})

test_that("coord_azimuth_zenith() works with geom_point() and geom_path()", {
  df <- data.frame(
    azimuth = seq(0, 4*pi, length.out = 40),
    zenith = seq(0, -pi / 2, length.out = 40)
  )

  print(
    ggplot2::ggplot(df, ggplot2::aes(azimuth, zenith)) +
      ggplot2::geom_path(col = "red") +
      ggplot2::geom_point(col = "blue") +
      coord_azimuth_zenith(projection = "stereographic") +
      scale_x_azimuth() +
      ggplot2::labs(caption = "spiral to the left with path and points")
  )

  # graphic test
  expect_true(TRUE)

})

test_that("coord_azimuth_zenith() works with different themes", {
  df <- data.frame(
    azimuth = seq(0, 4*pi, length.out = 40),
    zenith = seq(0, -pi / 2, length.out = 40)
  )

  p <- ggplot2::ggplot(df, ggplot2::aes(azimuth, zenith)) +
    ggplot2::geom_path(col = "red") +
    ggplot2::geom_point(col = "blue") +
    coord_azimuth_zenith(projection = "stereographic") +
    ggplot2::scale_x_continuous(
      breaks = seq(0, 2*pi, length.out = 13)[-13],
      labels = degrees
    )

  print(p + ggplot2::theme_gray() + ggplot2::labs(caption = "theme_gray()"))
  print(p + ggplot2::theme_bw() + ggplot2::labs(caption = "theme_bw()"))
  print(p + ggplot2::theme_linedraw() + ggplot2::labs(caption = "theme_linedraw()"))
  print(p + ggplot2::theme_classic() + ggplot2::labs(caption = "theme_classic()"))

  # graphic test
  expect_true(TRUE)

})

test_that("coord_azimuth_zenith() works with stereo and ortho projections", {
  df <- data.frame(
    azimuth = seq(0, 4*pi, length.out = 40),
    zenith = seq(0, -pi / 2, length.out = 40)
  )

  p <- ggplot2::ggplot(df, ggplot2::aes(azimuth, zenith)) +
    ggplot2::geom_path(col = "red") +
    ggplot2::geom_point(col = "blue") +
    ggplot2::scale_x_continuous(
      breaks = seq(0, 2*pi, length.out = 13)[-13],
      labels = degrees
    )

  print(
    p +
      coord_azimuth_zenith(projection = "orthographic") +
      ggplot2::labs(caption = "orthographic projection")
  )

  print(
    p +
      coord_azimuth_zenith(projection = "stereographic") +
      ggplot2::labs(caption = "stereographic projection")
  )

  # graphic test
  expect_true(TRUE)
})

test_that("coord_azimuth_zenith() works with various types of breaks", {
  df <- data.frame(
    azimuth = seq(0, 4*pi, length.out = 40),
    zenith = seq(0, -pi / 2, length.out = 40)
  )
  p <- ggplot2::ggplot(df, ggplot2::aes(azimuth, zenith)) + coord_azimuth_zenith()

  print(
    p +
      ggplot2::scale_x_continuous(breaks = NULL) +
      ggplot2::labs(caption = "NULL breaks")
  )

  print(
    p +
      ggplot2::scale_x_continuous(breaks = seq(0, 2*pi, length.out = 13)[-13]) +
      ggplot2::labs(caption = "30 degree breaks")
  )

  print(
    p +
      ggplot2::scale_x_continuous(
        breaks = seq(0, 2*pi, length.out = 13)[-13],
        minor_breaks = NULL
      ) +
      ggplot2::labs(caption = "no minor breaks")
  )

  print(
    p +
      ggplot2::scale_x_continuous(
        breaks = seq(0, 2*pi, length.out = 13)[-13],
        labels = NULL
      ) +
      ggplot2::labs(caption = "no labels")
  )

  # graphic test
  expect_true(TRUE)
})
