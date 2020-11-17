#' Create proxy timeseries ggplot
#'
#'
#' @param df data
#' @param time variable with time units
#' @param proxy variable with proxy data
#' @param events logical whether to include climate event labels
#' @param explain logical whether the labels include explanatory data
#' @param range_sh the range of the window selected
#'
#' @return ggplot
#' @export
time_plot <- function(df, time, proxy, events = TRUE, explain = FALSE,
                      range_sh = NULL){

  # default ggplot theme
  theme_set(theme_classic())
  # color palette
  dark <- RColorBrewer::brewer.pal(3, "Dark2")
  col_pal <- c(
    "sediments" = dark[3] ,
    "instrumental" =  dark[2],
    "model" = dark[1]
    )

  if (is.null(range_sh)) range_sh <- range(pull(df, {{time}}))
  clim_transients <-filter(timemachine::clim_transients,
                           between(.data$x, range_sh[1], range_sh[2])
                           )

  p <- ggplot(
    df,
    aes(x = {{time}},
        y = {{proxy}},
        color = .data$record,
        group = .data$scenario
        )
    ) +
    geom_line() +
    scale_color_manual("", values = col_pal) +
    ylab(expression('Temperature ('*degree~C*')')) +
    theme(
      legend.key.size = unit(0.1, "npc"),
      legend.text = element_text(size = 11)
    )

  if (events) {
    if (explain) {
    p <- p + ggrepel::geom_label_repel(
      data = clim_transients,
      aes(x = .data$x, y = .data$y, label = .data$label_exp),
      arrow = arrow(length = unit(0.02, "npc")),
      nudge_y = 8,
      inherit.aes = FALSE
      ) +
      geom_errorbarh(
        data = timemachine::clim_trends,
        aes(xmin = .data$xmin,
            xmax = .data$xmax,
            y = .data$y - 6
            ),
        height = 0.3,
        inherit.aes = FALSE
        ) +
      geom_text(
        data = timemachine::clim_trends,
        aes(x = .data$x,
            y = .data$y - 9,
            label = .data$label
            ),
        inherit.aes = FALSE
        )
    return(p)
    } else{
      p <- p + ggrepel::geom_text_repel(
        data = clim_transients,
        aes(x = .data$x, y = .data$y, label = .data$label),
        arrow = arrow(length = unit(0.02, "npc")),
        nudge_y = 8,
        inherit.aes = FALSE
        ) +
        geom_errorbarh(
          data = timemachine::clim_trends,
          aes(xmin = .data$xmin,
              xmax = .data$xmax,
              y = .data$y - 6
              ),
              height = 0.3,
              inherit.aes = FALSE
              ) +
        geom_text(
          data = timemachine::clim_trends,
          aes(x = .data$x,
              y = .data$y - 9,
              label = .data$label
              ),
          inherit.aes = FALSE
          )
      return(p)
    }
  }
  return(p)
}

label_geotime <- function(fctt) function(x) {x * fctt}

