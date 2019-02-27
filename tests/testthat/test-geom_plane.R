context("test-geom_plane")

test_that("geom_plane works", {
  print(
    ggplot2::ggplot(deep_hollow) +
      geom_plane(ggplot2::aes(strike = strike, dip = dip, col = type)) +
      coord_bearing_plunge() +
      scale_x_bearing() +
      ggplot2::labs(caption = "planes as great circles")
  )

  # this one doesn't work yet...lines in some places that there shouldn't be
  expect_true(FALSE)
})
