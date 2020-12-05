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
                      range_sh = NULL, ice = FALSE){

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
    p <- p +
      ggrepel::geom_label_repel(
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
        height = 0.6,
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
    } else {
      p <- p +
        ggrepel::geom_text_repel(
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
              height = 0.6,
              inherit.aes = FALSE
              ) +
        ggrepel::geom_text_repel(
          data = timemachine::clim_trends,
          aes(x = .data$x,
              y = .data$y - 9,
              label = .data$label
              ),
          inherit.aes = FALSE
          )
      if(ice) {
        EA_pol <- tibble(
          x = c(0, 14, 14.5, 16.5, 17, 33, 34, 46, 34, 33, 17, 16.5, 14.5, 14, 0, 0),
          y = c(1, 1, 0.7, 0.7, 1, 1, 0.6, 0.5, 0.4, 0, 0, 0.3, 0.3, 0, 0, 1) * 2 + 30,
          label = "East Antarctic"
        )
        WA_pol <- tibble(
          x = c(0, 14, 14.5, 32, 14.5, 14, 0, 0),
          y = c(1, 1, 0.7, 0.5, 0.3, 0, 0, 1) * 2 + 28,
          label = "West Antarctic"
        )

        NH_pol <- tibble(
          x = c(0, 2.4, 2.5, 14, 2.5, 2.4, 0, 0),
          y = c(1, 1, 0.7, 0.5, 0.3, 0, 0, 1) * 2 + 24,
          label = "Arctic"
        )

        ice_pol <- bind_rows(EA_pol, WA_pol, NH_pol)

        p <- p +
          geom_polygon(
            data = ice_pol,
            aes(x = x, y = y),
            fill = "cadetblue3",
            inherit.aes = FALSE
            ) +
         ggrepel::geom_text_repel(
            data = group_by(ice_pol, label) %>%
              summarise(x = min(x), y = mean(y)),
            aes(label = label, x = x, y = y),
            inherit.aes = FALSE,
            #direction = "both",
            size = 3,
            #min.segment.length = 1e-4#,
            nudge_x = 0.3
          ) +
          annotate("text", x = 3.5, y = 34, label = "Ice sheets", size = 5)
        return(p)
      }
      return(p)
    }
  }
  return(p)
}

label_geotime <- function(fctt) function(x) {x * fctt}

