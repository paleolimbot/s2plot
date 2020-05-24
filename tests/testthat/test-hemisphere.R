
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

test_that("hemisphere generation works", {
  hem_front <- make_hemisphere(0, 0, epsilon = 0)
  expect_equal(range(hem_front$lng), c(-90, 90))
  expect_true(all(abs(range(hem_front$lat) - c(-90, 90)) < 0.02))

  hem_top <- make_hemisphere(0, 90, epsilon = 0)
  expect_equal(range(hem_top$lng), c(-180, 180))
  expect_true(all(abs(range(hem_top$lat) - c(0, 0)) < 0.0001))

  hem_bottom <- make_hemisphere(0, -90, epsilon = 0)
  expect_equal(range(hem_bottom$lng), c(-180, 180))
  expect_true(all(abs(range(hem_bottom$lat) - c(0, 0)) < 0.0001))

  hem_kilter <- make_hemisphere(35, 12, epsilon = 0)
  expect_equal(range(hem_kilter$lng), c(-180, 180))
  expect_true(all(abs(range(hem_kilter$lat) - c(12 - 90, 90 - 12)) < 0.01))

  hem_epsilon <- make_hemisphere(0, 0, epsilon = 1)
  expect_equal(range(hem_epsilon$lng), c(-45, 45))
  expect_equal(range(hem_epsilon$lat), c(-45, 45))
})
