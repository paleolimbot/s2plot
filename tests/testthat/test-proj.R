
test_that("orthographic projections can be constructed", {
  expect_is(s2plot_projection_orthographic("POINT (1 2)"), "s2plot_projection_orthographic")
  expect_equal(s2plot_projection_orthographic("POINT (1 2)")$point, c(1, 2))
})

test_that("default projection is calculated", {
  expect_identical(
    s2plot_projection_default("POINT (1 2)", add = FALSE),
    s2plot_projection_orthographic("POINT (1 2)")
  )

  last_projection_env$last_projection <- s2plot_projection_orthographic("POINT (1 2)")
  expect_identical(
    s2plot_projection_default("POINT (2 3)", add = TRUE),
    s2plot_projection_orthographic("POINT (1 2)")
  )

  rm(last_projection, envir = last_projection_env)
})

test_that("orthographic projection projects correctly", {
  expect_equal(
    s2plot_project(s2plot_projection_orthographic("POINT (1 2)"), "POINT (1 2)"),
    list(x = 0, y = 0)
  )

  expect_warning(
    s2plot_project(s2plot_projection_orthographic("POINT (0 0)"), "POINT (180 0)"),
    "Projection error"
  )
})

test_that("orthographic projection prepares correctly", {
  expect_identical(
    s2::s2_as_text(
      s2plot_prepare(
        s2plot_projection_orthographic("POINT (0 0)"),
        c("POINT (80 0)", "POINT (91 0)")
      )
    ),
    c("POINT (80 0)", "GEOMETRYCOLLECTION EMPTY")
  )
})
