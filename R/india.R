#' Create map of India depicting the collision with Eurasia
#'
#' @param event character string of event
#' @param ... not supported
#'
#' returned
#'
#' @return ggplot
#' @export
gg_HIM <- function(event, ...){

  in.map <- maps::map(
    "world",
    plot = FALSE,
    fill = TRUE
    ) %>%
    sf::st_as_sf() %>%
    sf::st_transform(crs = sf::st_crs(4326))

  lon <- c(60, 100)
  lat <- c(5, 50)

  polygon <- sf::st_sfc(
    sf::st_polygon(cbind(rep(lon, each = 2) %>%
                       append(lon[1]),
                     c(5, 50, 50, 5, 5)
                     ) %>%
                 list
               ),
    crs = 4326
    )
  arrow_df <- tibble(
    lon = c(75, 78, 80, 83, 86),
    lon_end = c(75, 79 , 83, 86, 89),
    lat = c(31, 31, 30, 28, 26),
    lat_end = c(35, 35, 33, 31, 29)
  )

  ggplot() +
    geom_sf(data= polygon, fill = 'aliceblue') +
    geom_sf(data =  in.map) +
    coord_sf(
      xlim = lon,
      ylim = lat ,
      expand = FALSE,
      ndiscr = 10
      ) +
    ggspatial::geom_spatial_segment(
      data = arrow_df,
      aes(x = .data$lon, y = .data$lat, xend = .data$lon_end, yend = .data$lat_end),
      crs = sf::st_crs(4326),
      arrow = grid::arrow(length = grid::unit(0.05, "npc"), type = "closed"),
      great_circle = F,
      wrap_dateline = T,
      color = "darkgreen",
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
      title = "Uplift of Himalayas",
      subtitle = event
    )
}
