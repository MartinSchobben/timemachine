#' Create range plot of tectonic drivers
#'
#' @param lims numeric vector of length 2 giving the limits of the timeplot
#' @param ... not supported
#'
#' returned
#'
#' @return ggplot
#' @export
gg_tect <- function(lims, ...){

  # default ggplot theme
  theme_set(theme_classic())

  gg_els <- lst(
    geom_point(
      data = timemachine::tect_events,
      aes(x = xmid, y = y),
      size = 0.001,
      inherit.aes = FALSE
    ),
    geom_segment(
      data = timemachine::tect_events,
      aes(x = xmin, xend = xmax, y = y, yend = y),
      size = 1,
      inherit.aes = FALSE
      ),
    ggrepel::geom_label_repel(
      data = timemachine::tect_events,
      aes(label = y, x = xmid, y = y),
      force = 8,
      direction = "y",
      inherit.aes = FALSE,
      min.segment.length = 1e-4
      ),
    scale_y_continuous(
      expand = expansion(mult = 0.35)
      ),
    scale_x_continuous(
      trans = "reverse",
      limits = lims,
      expand = c(0, 0)
      ),
    theme(
      axis.text = element_text(color = "transparent"),
      axis.title = element_text(color = "transparent"),
      axis.ticks = element_line(color = "transparent"),
      axis.line = element_line(color = "transparent"),
      legend.position = "none"
      ),
    ggtitle("Tectonic events")
    )


}
