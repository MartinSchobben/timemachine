library(tidyverse)
library(RCurl)


#-------------------------------------------------------------------------------
# Tearney et al 2020 Science magazine Cretaceous--Cenozoic del18O based temperature curve
#-------------------------------------------------------------------------------
Tearney2020ScMag <- read_csv(
  getURL("https://raw.githubusercontent.com/jesstierney/PastClimates/master/THansenMethod.csv"),
  skip = 1,
  col_names = c("Age", "Proxy")
  ) %>%
  mutate(uncer = NA_real_,
         record = "sediments")
# mean Holocene Anomaly (relative LGM)
meanHol <- filter(Tearney2020ScMag, between(Age, 0, 0.0117)) %>% pull(Proxy) %>%  mean()
Tearney2020ScMag <- filter(Tearney2020ScMag, Age > 0.0117)

#-------------------------------------------------------------------------------
# Marcot et al 2013 Science Magazine Holocene composite curve temperature curve
#-------------------------------------------------------------------------------

Marcot2013ScMag <- readxl::read_xlsx(
  "data-raw/Marcott.SM.database.S1.xlsx",
  sheet = 3,
  na = "NaN",
  range = cellranger::as.cell_limits("C4:E571"),
  col_names = c("Age", "Proxy", "uncer"),
  col_types = rep("numeric", 3)
  ) %>%
  mutate(
    Age = Age / 10^6,
#splice
    Proxy = Proxy + meanHol,
    record = "sediments"
        )

# mean Recent 1961-90 Anomaly (relative LGM)
meanRecent <- Marcot2013ScMag$Proxy[4]


#-------------------------------------------------------------------------------
# Hadley Center yearly instrumental sea surface anomalies data (HadCRUT4)
#-------------------------------------------------------------------------------

HadCRUT4 <- read_cru_hemi("https://crudata.uea.ac.uk/cru/data/temperature/HadSST3-gl.dat") %>%
  as_tibble() %>%
  # correct age for Bp
  transmute(Age = (1950 - year) / 10^6, Proxy = annual + meanRecent, record = "instrumental")

# for the last 14 years
meanMeasurement <- filter(HadCRUT4, between(Age, -0.000070, -0.000056)) %>% pull(Proxy) %>%  mean()

#-------------------------------------------------------------------------------
# Beijing GMIP 5 GCM model data up to 2100
#-------------------------------------------------------------------------------

CMIP5 <- readRDS("data-raw/clim_model_predict.rds") %>%
  group_by(scenario) %>%
  mutate(base = mean(head(Proxy,14)),
         Proxy = (Proxy - base) + meanMeasurement,
         Age = (1950 - Age)/10^6
         ) %>%
  select(Age, Proxy, scenario, record)

#-------------------------------------------------------------------------------
# combined dataset
#-------------------------------------------------------------------------------

temp_curve <- bind_rows(Tearney2020ScMag , Marcot2013ScMag, HadCRUT4, CMIP5) %>%
  drop_na(Proxy)

saveRDS(temp_curve, file = "data/temp_curve.RDS")
