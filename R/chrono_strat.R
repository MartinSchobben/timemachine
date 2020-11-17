#' Chronostratigraphic columns for ggplot
#'
#'
#' @param graph ggplot object
#' @param reverse logical whether the time axis was already reversed
#' @param capture_legend logical indicating whether the legend should be
#' returned
#'
#' @return A list with ggplot, gtable and grob elements
#' @export
chrono_bldr <- function(graph, reverse = FALSE, capture_legend = FALSE) {

  # save legend
  if (capture_legend) {
  g <- ggplotGrob(graph +
                    theme(
                      legend.position = "right",
                      legend.margin = margin(-1, -1, -1, -1, "npc")
                      )
                  )$grobs
  legbox <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  }

  # update original
  original <- graph +
    theme(
      axis.text.x = element_text(color = "transparent"),
      axis.title.x = element_text(color = "transparent"),
      axis.ticks.x = element_line(color = "transparent"),
      axis.line.x = element_line(color = "transparent"),
      legend.position = "none"
      )

  graph <- graph +
    theme(
      axis.text.y = element_text(color = "transparent"),
      axis.title.y = element_text(color = "transparent"),
      axis.ticks.y = element_line(color = "transparent")
    )

  suppressMessages({original <- original + scale_x_continuous(expand = c(0,0))})
  suppressMessages({graph <- graph + scale_x_continuous(expand = c(0,0))})

  # remove geoms
  graph$layers <- NULL

  x.range <- ggplot_build(graph)$layout$panel_params[[1]]$x.range
  y.range <- ggplot_build(graph)$layout$panel_params[[1]]$y.range

  tf <- geotime_finder(x.range, reverse = reverse)
  suppressMessages({
    graph <- graph +
      scale_x_continuous(
        paste0("Age (",  tf[[2]], ")"),
        trans = "reverse",
        labels = label_geotime(tf[[1]]), expand = c(0, 0)
        )
    })
  suppressMessages({
    original <- original +
      scale_x_continuous(
        paste0("Age (",  tf[[2]], ")"),
        trans = "reverse",
        labels = label_geotime(tf[[1]]), expand = c(0, 0)
        )
    })

  pg <- ggplotGrob(graph)

  j.plot <- unique(gtable_filter(pg, "panel", trim = FALSE)$layout$l)
  i.axis <- unique(gtable_filter(pg, "axis-l", trim = FALSE)$layout$t)

  # Filter to only use stratigraphic bounds contained within the plot
  filter.chrono <- chrono_chart %>%
    arrange(desc(.data$top)) %>%
    arrange(.data$type) %>%
    filter(.data$top <= max(x.range), .data$bottom >= min(x.range)) %>%
    rowwise() %>%
    mutate(
      bottom = min(.data$bottom, max(x.range)),
      top = max(.data$top, min(x.range))
      ) %>%
    ungroup() %>%
    mutate(
      width = (.data$bottom - .data$top) /
        (max(.data$bottom) - min(.data$top))
      ) %>%
    select(.data$name, .data$type, .data$R, .data$G, .data$B, .data$width)

  #unique.types <-rev(unique(filter.chrono$type)) %>% as.character()
  # filter unique types based on length of timeseries
  if (reverse) time.select <- diff(rev(x.range)) else time.select <- diff(x.range)
  if (time.select > 10) {
    unique.types <- rev(
      unique(
        pull(
          filter(filter.chrono, .data$type != "Series"),
          .data$type)
        )
      ) %>% as.character()
  }
  if (time.select > 1 & time.select <= 10) {
    unique.types <- rev(
      unique(
        pull(
          filter(filter.chrono, .data$type != "Era"),
          .data$type)
        )
    ) %>% as.character()
  }
  if (time.select <= 1) {
    unique.types <- rev(
      unique(
        pull(
          filter(filter.chrono, .data$type == "Series" |
                   .data$type == "Period"),
          .data$type)
        )
      ) %>%
      as.character()
  }

  # Create empty gtable
  gt <- gtable(
    widths = unit(1, "null"),
    heights = rep(unit(1, "null"), times = length(unique.types))
    )

  # Fill gtable with individual table grobs for each type of geologic time, create dotted lines for unofficial units
  for (i in seq_along(unique.types)){

    period.df <- filter.chrono[filter.chrono$type == unique.types[i],]
    # Fonts
    if (any(period.df$type == "Series")) fonts <- list(fontsize = 8L)
    if (any(period.df$type == "Period")) fonts <- list(fontsize = 8L)
    if (any(period.df$type == "Eon")) fonts <- list(fontsize = 8L)
    if (any(period.df$type == "Era")) fonts <- list(fontsize = 9L, fontface = "bold")


    # Suppress font printing if small box
    period.df  <-  period.df %>%
      mutate(abbr = abbreviate(.data$name, minlength = 1, dot = TRUE),
             name = case_when(
               width < 0.25 ~ abbr,
               width >= 0.25 ~ name
               )
             )

    tt <- gridExtra::tableGrob(
      # transpose to get  horizontal geochronological bar
      d = matrix(period.df$name, ncol = length(period.df$name), nrow = 1),
      cols = NULL,
      rows = NULL,
      heights = unit(1, "null"),
      widths = unit(period.df$width, "null"),
      theme = gridExtra::ttheme_minimal(
        core = list(bg_params = list(
          fill = rgb(period.df$R ,
                     period.df$G,
                     period.df$B,
                     maxColorValue = 255
                     ),
          col = "black"
          ),
          fg_params = fonts
          )
          )
      )

    gt <- gtable_add_grob(x = gt, grobs = tt, t = i, l = 1)
  }

  for(i in i.axis) {
    pg <- gtable_add_grob(x = pg, grobs = gt, t = i, l = j.plot)
  }

  ls_gg <- lst(original, chrono = pg)
  if (capture_legend) ls_gg$legbox <- legbox
  return(ls_gg)
}



label_geotime <- function(fctt) {
  function(x) {x * fctt}
}

geotime_finder <- function(x.range, reverse) {
  if (reverse) x.range <- rev(x.range)
  res_time <- diff(x.range)
  if (res_time  > 1) {return(lst(fct_ch = 1, age_lab = "Mya"))}
  if (between(res_time, 10^-3, 1)) {return(lst(fct_ch = 10^3, age_lab = "Kya"))}
  if (res_time  <
      10^-3) {return(lst(fct_ch = 10^6, age_lab = "years Before Present"))}
}
