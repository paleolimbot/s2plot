
test_that("basic plotting works", {
  s2plot(libs2::s2data_countries(), col = "grey90")
  s2plot(libs2::s2data_cities("London"), pch = 16, add = T)

  expect_true(TRUE)
})

test_that("plotting with manual projection works", {
  s2plot(libs2::s2data_countries(), projection = s2plot_projection_orthographic("POINT (0 0)"))
  s2plot(libs2::s2data_cities("London"), pch = 16, add = T)

  expect_true(TRUE)
})
