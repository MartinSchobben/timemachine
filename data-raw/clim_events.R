#-------------------------------------------------------------------------------
# This script list all major climatic events as plot elements
#-------------------------------------------------------------------------------

approx_y <- function(x) {
  filter(temp_curve, abs(Age - x) == min(abs(Age - x))) %>%
    pull(Proxy) # find closed value on curve
  }

clim_trends <- tibble(
  xmin = c(49, 15, 3.025),
  xmax = c(54, 17, 3.264),
  x = purrr::map2_dbl(xmax, xmin, ~{.y + (.x - .y) / 2}),
  y = purrr::map_dbl(x, ~(approx_y(.x))[1]),
  label = c(
    "Early Eocene Climate Optimum",
    "middle Miocene Climate Optimum",
    "mid-Pliocene Warm Period"
    )
)

usethis::use_data(clim_trends, overwrite = TRUE)

clim_transients <-tibble(
  label = c(
    "PETM", # Paleocene-Eocene Thermal Maximum
    "ETM2", # Eocene Thermal Maximum 2 (also known as ELMO)
    "Oi-1", # First major glacial period in the Oligocene
    "Mi-1", # Glacial maximum in the Miocene following late Oligocene warming
    "M2", # First major NH glacial event in the Pliocene
    "LGM" # Last glacial maximum
    ),
  label_exp = c(
    "PETM: Paleocene-Eocene Thermal Maximum",
    "ETM2: Eocene Thermal Maximum 2 \n (also known as ELMO)",
    "Oi-1: First major glacial period in the Oligocene",
    "Mi-1: Glacial maximum in the Miocene \n following late Oligocene warming",
    "M2: First major NH glacial event in the Pliocene",
    "LGM: Last Glacial Maximum"
    ),

  x = c(55.88, 54.05, 33.7, 23.03, 3.3, 0.018),
  y = purrr::map_dbl(x, ~(approx_y(.x))[1])

)

usethis::use_data(clim_transients, overwrite = TRUE, internal = TRUE)

# tectonic events
tect_events <- tibble(
  label = factor(
    c("India-Eurasia collision",
      "opening of the Drake Passage",
      "opening of the Tasman Gateway",
      "closing of the Pananama Gateway"
      ),
    levels = c(
      "India-Eurasia collision",
      "opening of the Drake Passage",
      "opening of the Tasman Gateway",
      "closing of the Pananama Gateway"
      ),
    ordered = TRUE
    ),
  y = 1:4,
  xmin = c(40.4, 19, 33.5 - 1.5, 4.20),
  xmax = c(50.2, 41, 33.5 + 1.5, 4.7),
  ref =  c(
    "Bouilhol et al 2013",
    "Scher et al., 2006; Cramer et al., 2008",
    "Scher et al., 2015",
    "Haug et al., 2001"
    )
  ) %>%
  mutate(
    xrange = xmax - xmin,
    xmid = xmin + (xrange  / 2)
  )


usethis::use_data(tect_events, overwrite = TRUE, internal = TRUE)
