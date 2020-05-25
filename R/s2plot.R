
#' Plot an object on the sphere
#'
#' @param geog A [libs2::s2geography()]
#' @param add Add to the current plot? Use `FALSE` to create a new plot.
#' @param par Graphical [graphics::par()] to set prior to plotting
#' @param projection Right now [s2plot_projection_orthographic()]
#'   is the only projection. The default is either calculated
#'   based on the [libs2::s2_centroid_agg()] of `geog` or the
#'   last used projection if `add = TRUE`.
#' @param xlim,ylim Limits in projected space.
#' @param ... Passed to graphics functions.
#'
#' @return `x`, invisibly
#' @export
#'
#' @examples
#' s2plot(libs2::s2data_countries())
#' s2plot(libs2::s2data_cities("London"), pch = 16, add = TRUE)
#'
s2plot <- function(geog, ..., projection = s2plot_projection_default(geog, add),
                   xlim = NULL, ylim = NULL, par = s2plot_par_default(), add = FALSE) {
  withr::with_par(par, {
    geog <- s2plot_prepare(projection, geog)

    if (!add) {
      geog_xy  <- s2plot_project(projection, geog)
      graphics::plot(
        double(), double(),
        xlab = "", ylab = "",
        xlim = if (is.null(xlim)) range(geog_xy$x, finite = TRUE) else xlim,
        ylim = if (is.null(ylim)) range(geog_xy$y, finite = TRUE) else ylim,
        asp = 1
      )

      last_projection_env$last_projection <- projection
    }

    geog_split <- split(
      geog,
      factor(libs2::s2_dimension(geog), levels = c("0", "1", "2")),
      drop = FALSE
    )

    if (length(geog_split[[1]]) > 0) {
      xy <- s2plot_project(projection, geog_split[[1]])
      graphics::points(xy$x, xy$y, ...)
    }

    if (length(geog_split[[2]]) > 0) {
      xy <- s2plot_project(projection, geog_split[[2]])
      graphics::lines(xy$x, xy$y, ...)
    }

    if (length(geog_split[[3]]) > 0) {
      xy <- s2plot_project(projection, geog_split[[3]])
      graphics::polypath(xy$x, xy$y, ...)
    }
  })
}

#' @rdname s2plot
#' @export
s2plot_par_default <- function() {
  list(mai = c(0, 0, 0, 0), omi = c(0, 0, 0, 0))
}
