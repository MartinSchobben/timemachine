library(cubelyr)
library(tidyverse)
library(raster)
library(ncdf4) # package for netcdf manipulation
library(lubridate)

# load data (https://climate4impact.eu/) CMIP5 rcp26 scenario in netcdf format https://rpubs.com/boyerag/297592
nc_rcp26 <- nc_open("data-raw/tas_Amon_bcc-csm1-1-m_rcp26_r1i1p1_200601-210012.nc") # 2.6 W m-2
nc_rcp85 <- nc_open("data-raw/tas_Amon_bcc-csm1-1-m_rcp85_r1i1p1_200601-209912.nc") # 8.5 W m-2
# save dimensions
lon <- ncvar_get(nc_rcp26, "lon")
lat <- ncvar_get(nc_rcp26, "lat", verbose = F)
t <- ncvar_get(nc_rcp26, "time")
# store the data in a 3-dimensional array
ndvi.array <- ncvar_get(nc_rcp26)

# get a slice to check data
ndvi.slice <- ndvi.array[, , 1]
# raster a slice
r <- raster(t(ndvi.slice), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
# flip
r <- flip(r, direction='y')
# plot
plot(r)




flatten_model <- function(model) {

# store data in array
  model <- ncvar_get(model)
# dim sizes
  dim_size <- dim(model)
# provide dim names
  dimnames(model) <- lst(
    "long" = paste("long", 1:dim_size[1], sep = "_"),
    "lat" = paste("lat", 1:dim_size[2], sep = "_"),
    "t" = paste("t", 1:dim_size[3], sep = "_")
    )

# flatten array and summarize global temps
as_tibble(as.tbl_cube(model))  %>%
  group_by(t) %>%
  summarise(mean_T = mean(model)) %>%
  separate(t, into =c("label", "t") , sep = "_") %>%
  mutate(date = make_datetime(year = 2006, month = as.numeric(t))) %>%
  group_by(Age = year(date)) %>%
  summarise(Proxy = mean(mean_T) - 273,15)
}


sum_rcp26 <- flatten_model(nc_rcp26)
sum_rcp85 <- flatten_model(nc_rcp85)
GCMs <- bind_rows(sum_rcp26, sum_rcp85, .id = "scenario")
saveRDS(GCMs, "data-raw/clim_model_predict.rds")

ggplot(GCMs, aes(y = Proxy, x = Age, color = scenario))+
  geom_point() +
  geom_smooth()

