# edge
circum <- sf::st_point(x = c(0,0)) %>% sf::st_buffer(dist = 4e6) %>%
  sf::st_sfc(crs = 3031)

ant.map <- maps::map(
  "world",
  "Antarctica",
  wrap = c(0, 360),
  plot = FALSE,
  fill = TRUE
  ) %>%
  sf::st_as_sf() %>%
  sf::st_transform(
    crs = sf::st_crs(3031)
    )

arrow.df <- tibble(lon = seq(0, 350, 30), lon_end = seq(10, 360, 30), lat = -60, lat_end = -60)

gg_ACC <- ggplot() +
  geom_sf(data= circum, fill = 'aliceblue') +
  geom_sf(data = ant.map) +
  coord_sf(
    expand = FALSE,
    ndiscr = 1000
    ) +
  ggspatial::geom_spatial_segment(
    data = arrow.df,
    aes(x = lon, y = lat, xend = lon_end, yend = lat_end),
    crs = sf::st_crs(4326),
    arrow = grid::arrow(),
    wrap_dateline = T,
    great_circle = F,
    color = "red",
    size = 3,
    lineend = "round",
    show.legend = FALSE,
    inherit.aes = FALSE
    )+
  theme(
    panel.grid.major = element_line(
      color = gray(.5),
      linetype = 'dashed',
      size = 0.5
      ),
    panel.ontop = TRUE,
    panel.background = element_rect(fill = NA)
    )


