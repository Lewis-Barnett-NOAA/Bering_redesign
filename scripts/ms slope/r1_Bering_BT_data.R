####################################################################
##
##    Script #1 
##    Get catch and effort data from bottom trawl survey EBS, NBS and slope
##    for VAST model estimation
##
##    Daniel Vilas (danielvilasgonzalez@gmail.com)
##    Lewis Barnett, Stan Kotwicki, Zack Oyafuso, Megsie Siple, Leah Zacher, 
##    Lukas Defilippo, Andre Punt
##
####################################################################

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  Import packages, connect to Oracle
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
pacman::p_load(c("lubridate", "gapindex", 'ncdf4', 'terra', 'sdmTMB'), 
               character.only = TRUE)
channel <- gapindex::get_connected(check_access = FALSE)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  Pull data from AFSC Groundfish Assessment Program via gapindex 
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
start_year <- 1982; end_year <- 2022
species_list <- read.csv(file = "data/species_list.csv")
cpue_data <- data.frame()

for (iregion in c("bs_slope", "bs_shelf")) { ## loop over regions -- start
  
  ## Initial data pull
  temp_data <- 
    gapindex::get_data(year_set = start_year:end_year, 
                       survey_set = list("bs_slope" = "BSS",
                                         "bs_shelf" = c("NBS", "EBS"))[[iregion]], 
                       spp_codes = species_list,
                       channel = channel, 
                       taxonomic_source = "GAP_PRODUCTS.TAXONOMIC_CLASSIFICATION") 
  temp_cpue <- 
    ## Calculate cpue
    gapindex::calc_cpue(gapdata = temp_data) |>
    ## Select relevant fields
    subset(select = c("HAULJOIN", "SURVEY", "YEAR", 
                      "DEPTH_M", "BOTTOM_TEMPERATURE_C",
                      "LATITUDE_DD_START", "LONGITUDE_DD_START",
                      "SPECIES_CODE", "WEIGHT_KG", "AREA_SWEPT_KM2",
                      "CPUE_KGKM2")) |>
    ## Merge START_TIME from the haul data into the cpue df
    merge(x = temp_data$haul[, c("HAULJOIN", "START_TIME")],
          by = "HAULJOIN") |>
    ## Merge species name to cpue df
    merge(y = temp_data$species |> 
            subset(select = c("SPECIES_CODE", "SCIENTIFIC_NAME")),
          by = "SPECIES_CODE")
  
  ## Extract month and year from haul data START_TIME
  temp_cpue$MONTH <- lubridate::month(as.POSIXlt(temp_cpue$START_TIME, 
                                                 format="%d/%m/%Y"))
  
  ## Lower case the field names in temp_cpue 
  names(x = temp_cpue) <- tolower(x = names(x = temp_cpue))
  names(x = temp_cpue)[names(x = temp_cpue) == "latitude_dd_start"] <-
    "lat"
  names(x = temp_cpue)[names(x = temp_cpue) == "longitude_dd_start"] <-
    "lon"
  names(x = temp_cpue)[names(x = temp_cpue) == "bottom_temperature_c"] <-
    "sbt_c"
  
  ## Shorten name for REBS complex
  temp_cpue$scientific_name[
    temp_cpue$scientific_name == "rougheye and blackspotted rockfish unid."
  ] <- "REBS rockfishes"
  
  ## Remove ATF (10110) and Kams (10112) records prior to 1992
  temp_cpue <- subset(x = temp_cpue,
                      subset = !(species_code %in% c(10110, 10112) 
                                 & year < 1992))
  
  ## Remove Aleutian (472) and AK (471) skate records prior to 1999
  temp_cpue <- subset(x = temp_cpue,
                      subset = !(species_code %in% c(471, 472) 
                                 & year < 1999))
  
  ## rbind to cpue_data
  cpue_data <- rbind( cpue_data, temp_cpue )
  
} ## loop over regions -- end

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  Compile interpolation grid
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
utils::data(bering_sea_slope_grid, package = "VAST")
utils::data(northern_bering_sea_grid, package = "VAST")
utils::data(eastern_bering_sea_grid, package = "VAST")

grid_bs <- rbind(
  eastern_bering_sea_grid |>
    as.data.frame() |> 
    transform(region = "EBS"),
  northern_bering_sea_grid |>
    as.data.frame() |> 
    transform(region = "NBS"),
  bering_sea_slope_grid |>
    as.data.frame() |> 
    transform(region = "BSS") |>
    transform(Stratum = "NA") |>
    subset(select = c("Lat", "Lon", "Area_in_survey_km2", "Stratum", "region")) 
) |>
  setNames(nm = c("lat", "lon", "area_km2", "stratum", "region"))

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  Add depth from akgfmaps to the Bering Sea interpolation grid
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
r <- system.file("extdata", "bathymetry.gpkg", package = "akgfmaps") |>
  terra::rast()

## Extract depth values for each station of grid - using GEBCO data
grid_bs$depth_m <- 
  grid_bs |>
  terra::vect(geom = c("lon", "lat"), crs = "EPSG:4326") |>
  terra::project(terra::crs(x = r)) |>
  terra::extract(x = r) |>
  subset(select = Height) |>
  unlist()

## Constrain grid to those between 0 and 400 m and 
grid_bs <- subset(x = grid_bs, subset = depth_m <= 600 & depth_m >= 1  )
grid_bs$cell <- 1:nrow(x = grid_bs)

## Save interpolation grid
saveRDS(object = grid_bs,
        file = "data/data_processed/grid_bs.RDS")

write.csv(x = grid_bs,
          file = "data/data_processed/grid_bs.csv",
          row.names = FALSE)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  Append Bottom Temperature to Interpolation Grid via Bering 10K ROMS
## Dimensions of the various netcdf files from the ROMS model output 
## 258 rows; 182 cols; 46956 cells; 259 time steps
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# list local netcdf files
base_dir <- "data/data_raw/bering_10k_roms"
hist_dir <- file.path(base_dir, "netcdf_historical")
for_dir  <- file.path(base_dir, "netcdf_forecast")

files.hist <- 
  sort(x = list.files(path = hist_dir, pattern = "\\.nc$", full.names = FALSE))
files.for <- 
  sort(x = list.files(path = for_dir, pattern = "\\.nc$", full.names = FALSE))

#create df to store results
get_roms_file <- function(y, files.hist, files.for) {
  
  if (y >= 1980 && y <= 1984) return(list(type = "hist", file = files.hist[1]))
  if (y >= 1985 && y <= 1989) return(list(type = "hist", file = files.hist[2]))
  if (y >= 1990 && y <= 1994) return(list(type = "hist", file = files.hist[3]))
  if (y >= 1995 && y <= 1999) return(list(type = "hist", file = files.hist[4]))
  if (y >= 2000 && y <= 2004) return(list(type = "hist", file = files.hist[5]))
  if (y >= 2005 && y <= 2009) return(list(type = "hist", file = files.hist[6]))
  if (y >= 2010 && y <= 2014) return(list(type = "hist", file = files.hist[7]))
  if (y >= 2015 && y <= 2019) return(list(type = "hist", file = files.hist[8]))
  if (y == 2020)              return(list(type = "hist", file = files.hist[9]))
  if (y >= 2021 && y <= 2024) return(list(type = "for",  file = files.for[1]))
  
  stop("Year not covered")
}

if (!dir.exists(paths = "data/data_processed/grid_bs_temp_plots/"))
  dir.create(path = "data/data_processed/grid_bs_temp_plots/")

grid_bs_year <- data.frame()
#loop over years to incorporate values into the Bering Sea grid
for (y in 2002:2016) {
  
  #print year to check progress
  cat(paste("    ----- year", y, "-----\n"))  
  
  sel <- get_roms_file(y, files.hist, files.for)
  
  if (sel$type == "hist") {
    file_path <- file.path(hist_dir, sel$file)
  } else {
    file_path <- file.path(for_dir, sel$file)
  }
  
  if (!file.exists(file_path)) {
    stop(paste("Missing file:", file_path))
  }
  
  # open NetCDF
  nc <- ncdf4::nc_open(file_path)
  
  #get latitude
  lats <- ncvar_get(nc,"lat_rho")
  #get longitude
  lons <- ncvar_get(nc,"lon_rho")
  #get SBT
  temp<-ncvar_get(nc,'temp')
  #get time
  t_axis <- ncvar_get(nc,"ocean_time")
  
  #convert time, filter to May - August
  time_axis <- as.POSIXct(t_axis, origin = "1900-01-01", tz = "GMT") 
  time_axis <-
    time_axis[format(x = time_axis, format = "%m") |> as.integer() %in% 5:8]
  
  
  #get weekly temp slices from specific year y
  nc_y <- ncvar_get(nc, "temp")[,,which(grepl(paste0(y,'-'),time_axis))]
  
  #get mean matrix for this year. ##SHOULD WE FURTHER SUBSET TO JUST 
  #SUMMER MONTHS?
  mean_nc <- apply(X = nc_y, MARGIN = c(1, 2), FUN = mean, na.rm = TRUE)
  
  #Create spatial object from ROMS data
  roms_obj <- 
    data.frame(lat = as.vector(lats),
               lon = as.vector(lons),
               temp = as.vector(mean_nc)) |>
    subset(subset = lon >= 180 & !is.na(x = temp)) |>
    transform(lon = lon - 360) |>
    subset(subset = lat >= min(grid_bs$lat) 
           & lat <= max(grid_bs$lat) 
           & lon >= min(grid_bs$lon) 
           & lon <= max(grid_bs$lon)) |>
    terra::vect(geom = c("lon", "lat"), crs = "EPSG:4326") |>
    terra::project("EPSG:3338")
  
  ## create spatial object from bs_grid
  grid_bs_obj <- 
    grid_bs |>
    transform(year = y) |>
    terra::vect(geom = c("lon", "lat"), crs = "EPSG:4326") |>
    terra::project("EPSG:3338")
  
  
  ## For each bs_grid_obj cell, extract the ROMS mean summer temperature value 
  ## of the nearest point in the ROMS spatial object
  grid_bs_obj$roms_sbt_c <- roms_obj$temp[terra::nearest(x = grid_bs_obj, 
                                                         y = roms_obj)$to_id]
  
  ## Add coordinates in lat/lon and utms
  grid_bs_obj[, c("lon", "lat")] <-
    grid_bs_obj |> 
    terra::project("EPSG:4326") |> 
    terra::crds() |> 
    as.data.frame() |> setNames(nm = c("lon", "lat"))
  
  grid_bs_obj <-
    sdmTMB::add_utm_columns(
      dat = grid_bs_obj,
      ll_names = c("lon", "lat"),
      utm_names = c("x_utm_km", "y_utm_km"),
      units = "km"
    )
  
  ## Plot
  ggplot() +
    geom_spatvector(data = grid_bs_obj, aes(color = roms_sbt_c), size = 0.5) +
    scale_color_viridis_c(limits = c(-3, 12)) +
    theme_minimal() +
    labs(title = paste(y),
         color = "sbt_c Value")
  ggsave(filename = paste0("data/data_processed/grid_bs_temp_plots/",
                           "grid_bs_roms_sbt_c_", y, ".jpeg"), 
         width = 5, height = 5, units = "in")
  
  ## rbind to grid_bs_year
  grid_bs_year <- rbind(grid_bs_year, 
                        as.data.frame(grid_bs_obj) |>
                          subset(select = c(cell, year, region, stratum, 
                                            lon, lat, x_utm_km, y_utm_km,
                                            area_km2, depth_m, roms_sbt_c)) )
}

#save grid Bering Sea with SBT and depth as dataframe
saveRDS(object = grid_bs_year, file = 'data/data_processed/grid_bs_year.RDS')
write.csv(x = grid_bs_year,
          file = "data/data_processed/grid_bs_year.csv",
          row.names = FALSE)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  For each species, extract sea bottom temperature to observed data 
##  from the Bering 10K ROMS model
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
cpue_data_with_sbt <- data.frame()

for (y in sort(x = unique(x = cpue_data$year))) {
  
  #print year to check progress
  cat(paste("    ---- year", y, "----\n"))
  
  ## Get the correct roms file depending on the year y
  sel <- get_roms_file(y, files.hist, files.for)
  
  if (sel$type == "hist") {
    file_path <- file.path(hist_dir, sel$file)
  } else {
    file_path <- file.path(for_dir, sel$file)
  }
  
  if (!file.exists(file_path)) {
    stop(paste("Missing file:", file_path))
  }
  
  # open NetCDF
  nc <- ncdf4::nc_open(file_path)
  #get latitude
  lats <- ncvar_get(nc,"lat_rho")
  #get longitude
  lons <- ncvar_get(nc,"lon_rho")
  #get SBT
  temp<-ncvar_get(nc,'temp')
  #get time
  t_axis<-ncvar_get(nc,"ocean_time")
  #convert time
  time_axis <- as.POSIXct(t_axis, origin = "1900-01-01", tz = "GMT") 
  
  ## create spatial object from dataset
  cpue_obj <- 
    subset(x = cpue_data, subset = year == y) |>
    terra::vect(geom = c("lon", "lat"), crs = "EPSG:4326") |>
    terra::project("EPSG:3338")
  
  # unique months observed
  obs_months <- sort(x = unique(x = cpue_obj$month))
  for (imonth in obs_months) { ## Loop over months -- start
    
    ## Index the correct year and month 
    time_axis_month <- grepl(paste0(y, '-', sprintf("%02d", imonth)), time_axis)
    
    ## Get monthly average of sbt across the region
    nc_y <- apply(X = ncvar_get(nc, "temp")[, , time_axis_month],
                  MARGIN = c(1, 2), 
                  FUN = mean)
    
    ## create roms spatial object
    roms_obj <- data.frame(lat = as.vector(x = lats),
                           lon = as.vector(x = lons),
                           temp = as.vector(x = nc_y)) |>
      subset(subset = lon >= 180 & !is.na(x = temp)) |>
      transform(lon = lon - 360) |>
      subset(subset = lat >= min(cpue_data$lat)
             & lat <= max(cpue_data$lat)
             & lon >= min(cpue_data$lon)
             & lon <= max(cpue_data$lon)) |>
      terra::vect(geom = c("lon", "lat"), crs = "EPSG:4326") |>
      terra::project("EPSG:3338")
    
    ## create temp cpue spatial object
    temp_cpue_obj <- subset(x = cpue_obj, subset = cpue_obj$month == imonth)
    
    ## For each temp_cpue_obj record, extract the ROMS month-averaged sbt 
    ## of the nearest point in the ROMS spatial object
    temp_cpue_obj$roms_sbt_c <- roms_obj$temp[terra::nearest(x = temp_cpue_obj,
                                                             y = roms_obj)$to_id]
    
    ## Add coordinates in lat/lon and utms
    temp_cpue_obj[, c("lon", "lat")] <-
      temp_cpue_obj |> 
      terra::project("EPSG:4326") |> 
      terra::crds() |> 
      as.data.frame() |> setNames(nm = c("lon", "lat"))
    
    temp_cpue_obj <- 
      sdmTMB::add_utm_columns(
        dat = temp_cpue_obj,
        ll_names = c("lon", "lat"),
        utm_names = c("x_utm_km", "y_utm_km"),
        units = "km"
      )
    
    ## Save sbt temperature monthly average map
    ggplot() +
      geom_spatvector(data = roms_obj, aes(color = temp), size = 3) +
      scale_color_viridis_c(limits = c(-3, 16)) +
      theme_minimal() +
      labs(title = paste(y),
           color = "sbt_c Value")
    ggsave(filename = paste0("data/data_processed/grid_bs_temp_plots/",
                             "grid_bs_roms_sbt_c_", y, sprintf("%02d", imonth),
                             ".jpeg"), 
           width = 5, height = 5, units = "in")
    
    ## Append to cpue_data_with_sbt 
    cpue_data_with_sbt <- 
      rbind(cpue_data_with_sbt,
            as.data.frame(temp_cpue_obj) |>
              subset(select = c(hauljoin, survey, start_time, year, month, 
                                lon, lat, x_utm_km, y_utm_km, area_swept_km2, 
                                depth_m, sbt_c, roms_sbt_c, 
                                species_code, scientific_name, 
                                weight_kg, cpue_kgkm2)))
  } ## Loop over months -- end
}

## save data_geostat file for all species
saveRDS(object = cpue_data_with_sbt, 
        file = "data/data_processed/cpue_bs_allspp.RDS")

write.csv(x = cpue_data_with_sbt,
          file = "data/data_processed/cpue_bs_allspp.csv",
          row.names = FALSE)

## Plot correlation between observed and ROMS-derived SBTs
jpeg(filename = "data/data_processed/grid_bs_temp_plots/obs_v_roms.jpeg", 
     width = 5, height = 5, units = "in", res = 300)
par(mar = c(5, 6, 1, 1))
plot(roms_sbt_c ~ sbt_c, data = cpue_data_with_sbt, las = 1, cex = 0.5,
     xlim = c(-2, 15), ylim = c(-2, 15),
     xlab = "Observed Sea Bottom Temperature (degrees C)",
     ylab = "Sea Bottom Temperature Extracted from\nBering 10K ROMS (degrees C)")
abline(a = 0, b = 1)
text(13, 0, 
     label = paste0("r = ", cor(cpue_data_with_sbt$roms_sbt_c, 
                                cpue_data_with_sbt$sbt_c,
                                use = "complete.obs") |> round(2) ))
dev.off()

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  Mesh for slope and shelf models
##  Shelf models have 300 spatial knots, slope models have 200 spatial knots
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
if (!dir.exists(paths = "data/data_processed/sdmtmb_mesh/"))
  dir.create(path = "data/data_processed/sdmtmb_mesh/")

cpue_data_with_sbt |>
  subset(subset = species_code == 21740 & survey %in% c("EBS", "NBS")) |>
  sdmTMB::make_mesh(xy_cols = c("x_utm_km", "y_utm_km"), n_knots = 300) |>
  saveRDS(file  = "data/data_processed/sdmtmb_mesh/bs_shelf_mesh.RDS")

cpue_data_with_sbt |>
  subset(subset = species_code == 30020 & survey %in% c("BSS")) |>
  sdmTMB::make_mesh(xy_cols = c("x_utm_km", "y_utm_km"), n_knots = 200)|>
  saveRDS(file = "data/data_processed/sdmtmb_mesh/bs_slope_mesh.RDS")
