
#' Define a projection to use with s2plot
#'
#' @param point The centre of the projection
#' @param rotation Rotation
#' @param projection Right now [s2plot_projection_orthographic()]
#'   is the only projection.
#' @inheritParams s2plot
#'
#' @export
#'
s2plot_projection_orthographic <- function(point, rotation = 0) {
  structure(
    list(point = c(libs2::s2_x(point), libs2::s2_y(point)), rotation = rotation),
    class = "s2plot_projection_orthographic"
  )
}

#' @rdname s2plot_projection_orthographic
#' @export
s2plot_projection_default <- function(geog) {
  s2plot_projection_orthographic(libs2::s2_centroid_agg(geog, na.rm = TRUE))
}

#' @rdname s2plot_projection_orthographic
#' @export
s2plot_prepare <- function(projection, geog) {
  UseMethod("s2plot_prepare")
}

#' @rdname s2plot_projection_orthographic
#' @export
s2plot_project <- function(projection, geog) {
  UseMethod("s2plot_project")
}

#' @rdname s2plot_projection_orthographic
#' @export
s2plot_prepare.s2plot_projection_orthographic <- function(projection, geog) {
  hemisphere <- libs2::s2geography(make_hemisphere_wkt(projection$point[1], projection$point[2]))
  libs2::s2_intersection(geog, hemisphere)
}

#' @rdname s2plot_projection_orthographic
#' @export
s2plot_project.s2plot_projection_orthographic <- function(projection, geog) {
  xy <- wk::wkb_coords(libs2::s2_asbinary(geog), sep_na = TRUE)

  projected <- mapproj::mapproject(
    xy$x, xy$y,
    projection = "orthographic",
    orientation = c(projection$point, projection$rotation)
  )

  projected[c("x", "y")]
}
