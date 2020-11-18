
chrono_chart <- tibble(
  name = c(
# Era
    "Phanerozoic", "Precambrian",
# Eon
    "Cenozoic", "Mesozoic", "Paleozoic",
    "Neoproterozoic", "Mesoproterozoic", "Paleoproterozoic",
# Period
    "Quaternary","Neogene", "Paleogene", "Cretaceous", "Jurassic",
    "Triassic", "Permian", " Carboniferous ", "Devonian", "Silurian",
    "Ordovician", "Cambrian", "Ediacaran", "Cryogenian",
    "Tonian", "Stenian","Ectasian", "Calymmian",
# Series
    "Anthropocene", "Holocene", "Pleistocene", "Pliocene", "Miocene",
    "Oligocene", "Eocene",
    "Paleocene", "Upper", "Lower"


           ),
  type = factor(
    c(rep("Era",2),
      rep("Eon", 6),
      rep("Period", 18),
      rep("Series", 10)
      ),
    levels = c("Era", "Eon", "Period", "Series"),
    ordered = TRUE
    ),
  top = c(0, 541, 0, 66.0, 251.902, 541, 1000, 1600, 0, 2.58, 23.03, 66.0,
          145.0, 201.3, 251.902, 298.9, 358.9 , 419.2, 443.8 , 485.4, 541,
          635, 720, 1000, 1200, 1400,
          -150/10^6, 0, 0.0117, 2.58, 5.333, 23.03, 33.9, 56, 66, 100.5
          ),
  bottom = c(541, 4600, 66.0, 251.902, 541, 1000, 1600, 2500, 2.58, 23.03,
             66.0, 145.0, 201.3, 251.902, 298.9, 358.9, 419.2, 443.8, 485.4,
             541, 635, 720, 1000, 1200, 1400, 1600,
             100/10^6, 0.0117, 2.58, 5.333, 23.03, 33.9, 56, 66, 100.5, 145
             ),
  R = c(154, 247, 249,  103, 153, 254 ,253 , 247, 249, 255, 253, 127, 52, 129,
        240, 103, 203, 179, 0, 127, 254, 254, 254, 254, 253, 253,
        255, 254, 255, 255, 255, 253, 253, 253, 166, 140
        ),
  G = c(217, 67, 249, 197, 192, 179, 180, 67, 249, 230, 154, 198, 178, 43, 64,
        165, 104,225,146, 160, 217, 204, 191, 217, 204, 192,
        255, 242, 242, 255, 255, 192, 180, 167, 216, 205
        ),
  B = c(221, 112, 29, 202, 141, 66, 98, 112,   127, 25, 82, 78, 201, 146, 40,
        153, 55, 182, 112, 86, 106, 92, 178, 154, 138, 122,
        255, 224, 174, 153, 0, 122, 108, 95, 74, 87
        )
  )

# create dataset
usethis::use_data(chrono_chart, overwrite = TRUE, internal = TRUE)
