
test_that("basic plotting works", {
  s2plot(s2::s2_data_countries(), col = "grey90")
  s2plot(s2::s2_data_cities("London"), pch = 16, add = T)
  s2plot("LINESTRING (0 0, 0 45)", add = T)

  expect_true(TRUE)
})

test_that("plotting with manual projection works", {
  s2plot(s2::s2_data_countries(), projection = s2plot_projection_orthographic("POINT (0 0)"))
  s2plot(s2::s2_data_cities("London"), pch = 16, add = T)
  s2plot("LINESTRING (0 0, 0 45)", add = T)

  expect_true(TRUE)
})
