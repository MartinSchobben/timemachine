#-------------------------------------------------------------------------------
# This script accumulates all data
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# Westherold et al 2020 Science magazine del18O based temperature curve
#-------------------------------------------------------------------------------

# Hansen corrections to derive deep and surface temps (Hansen et al 2013)
Hansen_ice_light <- function(x) 5 - 8 * ((x - 1.75) / 3) # Equation 3.5
Hansen_ice_heavy <- function(x) 1 - 4.4 * ((x - 3.25) / 3) # Equation 3.6
Hansen_hot <- function(x) -4 * x + 12 # Equation 3.1

# Pangaea database download
Ceno_doi <- pangaear::pg_search('Cenozoic global reference benthic carbon and oxygen isotope dataset (CENOGRID)')
CD_iso_dat <- pangaear::pg_data(Ceno_doi$doi)

Westherhold2020ScMag <- CD_iso_dat[[24]]$data %>%
  transmute(
    Proxy = `Foram bent δ18O [‰ PDB] (VPDB CorrAdjusted)`,
    Age = `Tuned time [Ma]`,
    Proxy = case_when(
      Age > 0 & Age <= 34.025 & Proxy < 3.25 ~ Hansen_ice_light(Proxy),
      Age > 0 & Age <= 34.025 & Proxy >= 3.25 ~ Hansen_ice_heavy(Proxy),
      Age > 34.025 ~ Hansen_hot(Proxy)
      ),
# surface corrections
    Proxy = case_when(
# Equation 4.1
      Age >= 0 & Age < 2.58 ~ {2 * Proxy + 12.25},
# Equation 4.2
      Age >= 2.5 & Age < 5.33 ~ {2.5 * Proxy + 12.15},
      Age >= 5.33 ~ {Proxy + 14.15}
      ),
    uncer = NA_real_,
    record = "sediments",
    scenario = "0"
    )

# mean Holocene Anomaly
Westherhold2020ScMag <- filter(Westherhold2020ScMag, Age > 0.0117)

#-------------------------------------------------------------------------------
# Marcot et al 2013 Science Magazine Holocene composite curve temperature curve
# behind paywall
#-------------------------------------------------------------------------------

Marcot2013ScMag <- readxl::read_xlsx(
  "https://science-sciencemag.org/highwire/filestream/594506/field_highwire_adjunct_files/1/Marcott.SM.database.S1.xlsx",
  sheet = 3,
  na = "NaN",
  range = cellranger::as.cell_limits("C4:E571"),
  col_names = c("Age", "Proxy", "uncer"),
  col_types = rep("numeric", 3)
  ) %>%
  mutate(
    Age = Age / 10^6,
#splice
    Proxy = Proxy + 14, # Mean 1961–1990 temp Hansen et al 2013,
    record = "sediments",
    scenario = "0"
    )

# mean Recent 1961-90 Anomaly (relative LGM)
meanRecent <- Marcot2013ScMag$Proxy[4]
Marcot2013ScMag <- filter(Marcot2013ScMag, Age > 0.0001)

#-------------------------------------------------------------------------------
# Hadley Center yearly instrumental sea surface anomalies data (HadCRUT4)
#-------------------------------------------------------------------------------

HadCRUT4 <- read_cru_hemi("https://crudata.uea.ac.uk/cru/data/temperature/HadSST3-gl.dat") %>%
  as_tibble() %>%
  # correct age for Bp
  transmute(
    Age = (1950 - year) / 10^6,
    Proxy = annual + meanRecent,
    record = "instrumental",
    scenario = "0"
    )

# for the last 14 years
meanMeasurement <- filter(HadCRUT4, between(Age, -0.000070, -0.000056)) %>%
  pull(Proxy) %>%
  mean()

#-------------------------------------------------------------------------------
# Beijing GMIP 5 GCM model data up to 2100
#-------------------------------------------------------------------------------

CMIP5 <- readRDS("data-raw/clim_model_predict.rds") %>%
  group_by(scenario) %>%
  mutate(base = mean(head(Proxy, 14)),
         Proxy = (Proxy - base) + meanMeasurement,
         Age = (1950 - Age)/10^6,
         record = "model"
         ) %>%
  select(Age, Proxy, scenario, record)

#-------------------------------------------------------------------------------
# combined dataset
#-------------------------------------------------------------------------------

temp_curve <- bind_rows(Westherhold2020ScMag , Marcot2013ScMag, HadCRUT4, CMIP5) %>%
  drop_na(Proxy)

usethis::use_data(temp_curve, overwrite = TRUE)

