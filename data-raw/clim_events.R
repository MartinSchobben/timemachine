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
  x = map2_dbl(xmax, xmin, ~{.y + (.x -.y) / 2}),
  y = map_dbl(x, ~(approx_y(.x))[1]),
  label = c(
    "EarlyEocene Climate Optimum",
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

  x = c(55.88, 54.05, 33.7, 23.1, 3.3, 0.018),
  y = map_dbl(x, ~(approx_y(.x))[1])

)

usethis::use_data(clim_transients, overwrite = TRUE)

