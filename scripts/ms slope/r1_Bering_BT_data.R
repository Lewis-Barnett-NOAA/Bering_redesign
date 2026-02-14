####################################################################
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
####################################################################

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  Install packages
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
pacman::p_load(c("lubridate", "gapindex", 'ncdf4', 'terra', "FNN", "sp"), 
               character.only = TRUE)
channel <- gapindex::get_connected(check_access = FALSE)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  Pull data from AFSC Groundfish Assessment Program via gapindex 
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
start_year <- 1982; end_year <- 2022
species_list <- read.csv(file = "data/species_list.csv")
cpue_data <- data.frame()
for (iregion in c("BSS", "NBS", "EBS")) { ## loop over regions -- start
  
  ## Initial data pull
  temp_data <- 
    gapindex::get_data(year_set = start_year:end_year, 
                       survey_set = iregion, 
                       spp_codes = species_list,
                       channel = channel) 
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
  temp_cpue$month <- lubridate::month(as.POSIXlt(temp_cpue$START_TIME, 
                                                 format="%d/%m/%Y"))
  # temp_cpue$year <- lubridate::year(as.POSIXlt(temp_cpue$START_TIME, 
  #                                              format="%d/%m/%Y"))
  
  ## Lower case the field names in temp_cpue 
  names(x = temp_cpue) <- tolower(x = names(x = temp_cpue))
  names(x = temp_cpue)[names(x = temp_cpue) == "latitude_dd_start"] <-
    "Lat"
  names(x = temp_cpue)[names(x = temp_cpue) == "longitude_dd_start"] <-
    "Lon"
  names(x = temp_cpue)[names(x = temp_cpue) == "bottom_temperature_c"] <-
    "bottom_temp_c"
  
  ## Shorten names of complexes
  temp_cpue$scientific_name[
    temp_cpue$scientific_name == "rougheye and blackspotted rockfish unid."
  ] <- "REBS rockfishes"
  
  temp_cpue$scientific_name[
    temp_cpue$scientific_name == "Lepidopsetta sp."
  ] <- "Lepidopsetta sp"
  
  ## Remove arrowtooth flounder (10110) and Kamchatka flounder (10112) records
  ## prior to 1992 (just applies to EBS data)
  temp_cpue <- subset(x = temp_cpue,
                      subset = !(species_code %in% c(10110, 10112) 
                                 & year < 1992))
  
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
    transform(Region = "EBS"),
  northern_bering_sea_grid |>
    as.data.frame() |> 
    transform(Region = "NBS"),
  bering_sea_slope_grid |>
    as.data.frame() |> 
    transform(Region = "BSS") |>
    transform(Stratum = "NA") |>
    subset(select = c("Lat", "Lon", "Area_in_survey_km2", "Stratum", "Region")) 
) |>
  setNames(nm = c("Lat", "Lon", "Area_km2", "Stratum", "Region"))

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  Add GEBCO depth (downloaded on August 2022, https://download.gebco.net/)
##  to the Bering Sea interpolation grid
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
r <- terra::rast('data/data_raw/gebco_2022_n70.0_s50.0_w-180.0_e-155.0.asc')
names(r) <- "depth_raster"

## Extract depth values for each station of grid - using GEBCO data
grid_bs$depth_m <- 
  -terra::extract(x = r,
                  y = terra::vect(x = grid_bs, 
                                  geom = c("Lon", "Lat"), 
                                  crs = terra::crs(r)))$depth_raster

## Constrain grid to those between 0 and 400 m and 
grid_bs <- subset(x = grid_bs, subset = depth_m <= 400 & depth_m >= 1  )

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
grid_bs_year <- data.frame(matrix(nrow = 0,
                                  ncol = ncol(x = grid_bs) + 2))
colnames(grid_bs_year)<-c(colnames(grid_bs),
                          'Temp',
                          'Year')
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

#loop over years to incorporate values into the Bering Sea grid
for (y in start_year:end_year) {
  
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
  
  #convert time
  time_axis <- as.POSIXct(t_axis, origin = "1900-01-01", tz = "GMT") 
  
  #get weekly temp slices from specific year y
  nc_y <- ncvar_get(nc, "temp")[,,which(grepl(paste0(y,'-'),time_axis))]
  
  #get mean matrix for this year. ##SHOULD WE FURTHER SUBSET TO JUST 
  #SUMMER MONTHS?
  mean_nc <- apply(X = nc_y, MARGIN = c(1, 2), FUN = mean, na.rm = TRUE)
  
  #create dataframe with lats, lons and mean year SBT
  df_nc <- data.frame(Lat = as.vector(lats),
                      Lon = as.vector(lons),
                      temp = as.vector(mean_nc))
  
  #longitude are in eastern. get SBT for the western hemisphere (Bering Sea). 
  #longitude greater than 180 degrees
  df_nc1 <- df_nc[which(df_nc$Lon >= 180), ]
  
  #convert eastern longitude to western values (higher). 
  #longitude should be negative
  df_nc1$Lon <- df_nc1$Lon - 360
  
  #filter values from the grid 
  df_nc2 <- subset(x = df_nc1, 
                   subset = Lat >= min(grid_bs$Lat) 
                   & Lat <= max(grid_bs$Lat) 
                   & Lon >= min(grid_bs$Lon) 
                   & Lon <= max(grid_bs$Lon) )
  
  #remove rows with NAs
  df_nc3 <- df_nc2[complete.cases(df_nc2),]
  
  #create spatial object from df
  coordinates(df_nc3) <- ~ Lon + Lat
  
  #create spatial object from grid
  spg <- grid_bs
  coordinates(spg) <- ~ Lon + Lat
  
  #get the nearests points from one df to other df
  nn <- FNN::get.knnx(coordinates(df_nc3), coordinates(spg),1)
  nc_index <- nn$nn.index[,1]
  
  #get SBT
  temps <- as.data.frame(df_nc3)$temp[nc_index]
  grid_bs$Temp <- temps
  grid_bs$Year <- y
  
  #incorporate SBT
  grid_bs_year <- rbind(grid_bs_year, as.data.frame(grid_bs))
  
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
  
  #subset df by year
  df2 <- subset(x = cpue_data, year == y)
  
  #if no data for that year, use stations file
  # if (nrow(x = df2) == 0 ) {
  #   
  #   #filter year and remove negative depth values
  #   st_year1 <- subset(grid_bs_year, Year == y)
  #   df3 <- data.frame(matrix(nrow = nrow(st_year1), ncol = ncol(df2)))
  #   colnames(df3)<-colnames(df2)
  #   df3$scientific_name <- ispp
  #   df3$year<-y
  #   df3$lat_start<-st_year1$Lat
  #   df3$lat_end<-st_year1$Lat
  #   df3$lon_start<-st_year1$Lon
  #   df3$lon_end<-st_year1$Lon
  #   df3$depth_m<-st_year1$depth_m
  #   df3$Temp<-st_year1$Temp
  #   df3$bottom_temp_c<-st_year1$Temp
  #   df2<-df3
  #   
  # }
  
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
  
  #get weekly temp slices from specific year y
  nc_y<-ncvar_get(nc, "temp")[,,which(grepl(paste0(y,'-'),time_axis))]
  
  #get mean matrix for this year
  mean_nc<-apply(nc_y,c(1,2),mean,na.rm=TRUE)
  
  #create dataframe with lats, lons and mean year SBT
  df_nc <- data.frame(Lat = as.vector(x = lats),
                      Lon = as.vector(x = lons),
                      temp = as.vector(x = mean_nc))
  
  #longitude are in eastern. get SBT for the western hemisphere (Bering Sea). 
  #longitude greater than 180 degrees
  df_nc1 <- df_nc[which(df_nc$Lon>=180),]
  
  #convert eastern longitude to western values (higher). 
  #longitude should be negative
  df_nc1$Lon <- df_nc1$Lon-360
  
  #filter values from the grid 
  df_nc2 <- subset(x = df_nc1, 
                   subset = Lat >= min(cpue_data$Lat) 
                   & Lat <= max(cpue_data$Lat) 
                   & Lon >= min(cpue_data$Lon) 
                   & Lon <= max(cpue_data$Lon))
  
  #remove NA rows
  df_nc3 <- df_nc2[complete.cases(df_nc2),]
  
  #create a spatial object
  coordinates(df_nc3) <- ~ Lon + Lat
  spg <- df2
  coordinates(spg) <- ~ Lon + Lat
  
  #get the nearests points from one df to other df
  nn<-get.knnx(coordinates(df_nc3),coordinates(spg),1)
  nc_index<-nn$nn.index[,1]
  
  #get SBT
  temps<-as.data.frame(df_nc3)$temp[nc_index]
  df2$Temp<-temps
  
  #rbind cpue data with SBT
  cpue_data_with_sbt <- rbind(cpue_data_with_sbt, df2)
}

## save data_geostat file for all species
saveRDS(object = cpue_data_with_sbt, 
        file = "data/data_processed/cpue_bs_allspp.RDS")

write.csv(x = cpue_data_with_sbt,
          file = "data/data_processed/cpue_bs_allspp.csv",
          row.names = FALSE)
