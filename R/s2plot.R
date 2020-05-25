
#' Plot an object on the sphere
#'
#' @param geog A [libs2::s2geography()]
#' @param xlim,ylim Limits in projected space.
#' @param ... Passed to graphics functions.
#'
#' @return `x`, invisibly
#' @export
#'
s2plot <- function(geog, ..., projection = s2plot_projection_default(geog),
                   xlim = NULL, ylim = NULL, par = s2plot_par_default(), add = FALSE) {
  withr::with_par(par, {
    geog <- s2plot_prepare(projection, geog)

    if (!add) {
      geog_xy  <- s2plot_project(projection, geog)
      graphics::plot(
        double(), double(),
        xlab = "", ylab = "",
        xlim = if (is.null(xlim)) range(geog_xy$x, finite = TRUE) else xlim,
        ylim = if (is.null(ylim)) range(geog_xy$y, finite = TRUE) else ylim
      )
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
