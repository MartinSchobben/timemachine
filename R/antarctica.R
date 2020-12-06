#' Create map of Antarctica with Antarctic Circumpolar Current
#'
#' @param event character string of event
#' @param ... not supported
#'
#' returned
#'
#' @return ggplot
#' @export
gg_ACC <- function(event, ...){

  # edge
  circum <- sf::st_point(x = c(0,0)) %>%
    sf::st_buffer(dist = 4e6) %>%
    sf::st_sfc(crs = 3031)
  # map
  ant.map <- maps::map(
    "world",
    "Antarctica",
    wrap = c(0, 360),
    plot = FALSE,
    fill = TRUE
    ) %>%
    sf::st_as_sf() %>%
    sf::st_transform(crs = sf::st_crs(3031))

  arrow_df <- tibble(
    lon = seq(0, 350, 30),
    lon_end = seq(10, 360, 30),
    lat = -60,
    lat_end = -60
    )

 ggplot() +
    geom_sf(data= circum, fill = 'aliceblue') +
    geom_sf(data = ant.map) +
    coord_sf(
      expand = FALSE,
      ndiscr = 1000
      ) +
    ggspatial::geom_spatial_segment(
      data = arrow_df,
      aes(x = lon, y = lat, xend = lon_end, yend = lat_end),
      crs = sf::st_crs(4326),
      arrow = grid::arrow(length = grid::unit(0.05, "npc"), type = "closed"),
      wrap_dateline = T,
      color = "red",
      size = 1,
      inherit.aes = FALSE
      ) +
    theme(
      panel.grid.major = element_line(
        color = gray(.5),
        linetype = 'dashed',
        size = 0.5
        ),
      panel.ontop = TRUE,
      axis.line = element_blank(),
      axis.title = element_blank(),
      panel.background = element_rect(fill = NA)
      ) +
   labs(
     title = "Antarctic Circumpolar Current",
     subtitle = event
       )

}


