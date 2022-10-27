
# lifted from ggstereo (probably a better way to replicate this in s2)
# this is essentially generating a "cap" but with somewhat irregular
# coordinate spacing
great_circle <- function(x, y, z, length.out  = 200) {
  stopifnot(length(x) == 1, length(y) == 1, length(z) == 1)

  # dot-product = 0 for orthogonality
  # VxU = x v1 + y v2 + z v3 = 0

  theta_out <- seq(-pi, pi, length.out = length.out)

  if(!near_zero(y)) {
    # solve for v2
    # v2 = (-x * v1 - z * v3) / y
    v1 <- cos(theta_out)
    v3 <- sin(theta_out)
    v2 <- (-x * v1 - z * v3) / y
  } else if(!near_zero(x)) {
    # solve for v1
    # v1 <- (-y * v2 - z * v3) / x
    v2 <- cos(theta_out)
    v3 <- sin(theta_out)
    v1 <- (-y * v2 - z * v3) / x
  } else if(!near_zero(z)) {
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

near_zero <- function(x, epsilon = 1e-8) {
  abs(x) < epsilon
}

make_hemisphere <- function(lng, lat, detail = 10000, epsilon = 0.1, precision = 2) {

  ref_latlng <- s2::s2_lnglat(lng, lat)
  ref_point <- as.data.frame(s2::as_s2_point(ref_latlng))
  great_circle_df <- great_circle(ref_point$x, ref_point$y, ref_point$z, length.out = detail)

  # move these all a tiny bit in the direction of ref_point to prevent
  # invalid geometries at the edges
  great_circle_df <- great_circle_df + (ref_point * epsilon)[rep(1, nrow(great_circle_df)), ]
  lengths <- with(great_circle_df, sqrt(x * x + y * y + z * z))
  great_circle_df <- great_circle_df / data.frame(x = lengths, y = lengths, z = lengths)

  great_circle_point <- s2::as_s2_point(as.matrix(great_circle_df))
  great_circle_latlng <- unique(round(as.data.frame(s2::as_s2_lnglat(great_circle_point)), precision))
  colnames(great_circle_latlng) <- c("lng", "lat")

  # close the ring
  rbind(great_circle_latlng, great_circle_latlng[1, , drop = FALSE])[c("lng", "lat")]
}

make_hemisphere_wkt <- function(lng, lat, detail = 10000, epsilon = 0.1, precision = 2) {
  coord_df <- make_hemisphere(lng, lat, detail = detail, epsilon = epsilon, precision = precision)
  coords <- paste0(coord_df$lng, " ", coord_df$lat, collapse = ", ")
  sprintf("POLYGON ((%s))", coords)
}
