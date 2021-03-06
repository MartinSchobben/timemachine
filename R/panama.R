#' Create map of Panama and the influence on sea currents
#'
#' @param event character string of event
#' @param ... not supported
#'
#' returned
#'
#' @return ggplot
#' @export
gg_GS <- function(event, ...){

  ma.map <- maps::map(
    "world",
    plot = FALSE,
    fill = TRUE
    ) %>%
    sf::st_as_sf() %>%
    sf::st_transform(crs = sf::st_crs(4326))

  lon <- c(-100, -60)
  lat <- c(0, 30)

  polygon <- sf::st_sfc(
    sf::st_polygon(cbind(rep(lon, each = 2) %>%
                       append(lon[1]),
                     c(0, 30, 30, 0, 0)
                     ) %>%
                 list
               ),
    crs = 4326
    )
  arrow_df <- tibble(
    lon = c(-62, -66, -71, -76, -79),
    lon_end = c(-65, -70, -75, -78, -81),
    lat = c(12, 13, 13.5, 13.5, 15),
    lat_end = c(13, 13.5, 13.5, 14.3, 17)
  )

  ggplot() +
    geom_sf(data= polygon, fill = 'aliceblue') +
    geom_sf(data = ma.map)  +
    coord_sf(
      xlim =c(-100, -60),
      ylim = c(0, 30) ,
      expand = FALSE,
      ndiscr = 10
    ) +
    ggspatial::geom_spatial_segment(
      data = arrow_df,
      aes(x = .data$lon, y = .data$lat, xend = .data$lon_end, yend = .data$lat_end),
      crs = sf::st_crs(4326),
      arrow = grid::arrow(length = grid::unit(0.05, "npc"), type = "closed"),
      great_circle = FALSE,
      wrap_dateline = TRUE,
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
      title = "Gulf Stream Current",
      subtitle = event
      )
}
