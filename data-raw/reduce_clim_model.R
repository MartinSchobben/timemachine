# load data (https://climate4impact.eu/) CMIP5 scenarios in netcdf format
# 2.6 W m-2
nc_rcp26 <- ncdf4::nc_open(RCurl::getURL("http://esgf.nci.org.au/thredds/fileServer/replica/CMIP5/output1/BCC/bcc-csm1-1-m/rcp26/mon/atmos/Amon/r1i1p1/v20120910/tas/tas_Amon_bcc-csm1-1-m_rcp26_r1i1p1_200601-210012.nc"))
# 8.5 W m-2
nc_rcp85 <- ncdf4::nc_open(RCurl::getURL("http://esgf.nci.org.au/thredds/fileServer/replica/CMIP5/output1/BCC/bcc-csm1-1-m/rcp85/mon/atmos/Amon/r1i1p1/v20130405/tas/tas_Amon_bcc-csm1-1-m_rcp85_r1i1p1_200601-209912.nc"))

# function to flatten the ncdf data
flatten_model <- function(model) {
# store data in array
  model <-  ncdf4::ncvar_get(model)
# dim sizes
  dim_size <- dim(model)
# provide dim names
  dimnames(model) <- lst(
    "long" = paste("long", 1:dim_size[1], sep = "_"),
    "lat" = paste("lat", 1:dim_size[2], sep = "_"),
    "t" = paste("t", 1:dim_size[3], sep = "_")
    )

# flatten array and summarize global temps
as_tibble(cubelyr::as.tbl_cube(model))  %>%
  group_by(t) %>%
  summarise(mean_T = mean(model)) %>%
  separate(t, into =c("label", "t") , sep = "_") %>%
  mutate(date = make_datetime(year = 2006, month = as.numeric(t))) %>%
  group_by(Age = year(date)) %>%
  summarise(Proxy = mean(mean_T) - 273,15)
}


sum_rcp26 <- flatten_model(nc_rcp26)
sum_rcp85 <- flatten_model(nc_rcp85)
clim_model_predict <- bind_rows(sum_rcp26, sum_rcp85, .id = "scenario")

#usethis::use_data(clim_model_predict, overwrite = TRUE)

