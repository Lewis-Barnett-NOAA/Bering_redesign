#################################################
# CREATE DATA SAMPLING SCENARIO BASELINE (existing)
#################################################

#load grid of NBS and EBS
load('data/extrapolation_grids/northern_bering_sea_grid.rda')
load('data/extrapolation_grids/eastern_bering_sea_grid.rda')
grid <- rbind(data.frame(northern_bering_sea_grid, region = 'NBS'),
              data.frame(eastern_bering_sea_grid, region = 'EBS'))
grid$cell <- 1:nrow(grid)
#df to spatialpoint df
coordinates(grid) <- ~ Lon + Lat
crs(grid)<-c(crs="EPSG:4326")
#reproject coordinates for plotting purposes
D2_1<-grid
D2_2<-data.frame(D2_1)
#x and y cells
xycells<-as.integer(sqrt(dim(D2_1)[1]))
# create a template raster
r1 <- raster(ext = extent(D2_1), ncol = xycells, nrow = xycells) 
#create raster
r2 <- rasterize(D2_1, r1 , field = 'cell')

#baseline strata areas
strata_areas <- sf::st_drop_geometry(ebs_layers$survey.strata)
strata_areas <- data.frame('Stratum' = strata_areas$STRATUM,
                           'Area_in_survey_km2' = strata_areas$AREA_M2/1000000)

#strata polygon
strata_pol<-as(ebs_layers$survey.strata, 'Spatial')
proj4string(strata_pol) <- CRS('EPSG:3338') 
strata_pol<-spTransform(strata_pol,CRSobj = CRS("EPSG:4326"))

#locations
coords <- ebs_layers$survey.grid |>
  st_centroid(of_largest_polygon = TRUE) |>
  st_coordinates()
baseline <- data.frame('Lon' = coords[, 1],
                       'Lat' = coords[, 2],
                       "Stratum" = ebs_layers$survey.grid$STRATUM,
                       "corner" = ebs_layers$survey.grid$CORNER,
                       'stationid'= ebs_layers$survey.grid$STATION)

#locations of stations
locations <- as.data.frame(baseline)
st<-baseline
coordinates(st)<- ~ Lon + Lat
proj4string(st) <- CRS('EPSG:3338') 
st<-spTransform(st,CRSobj = CRS("EPSG:4326"))
#cell
locations$cell<-extract(r2,st)
st1 <- as.data.frame(st)[, c("coords.x1","coords.x2")]
names(st1) <- c('x','y')
xy <- st1
sampled <- apply(X = xy, 
                 MARGIN = 1, 
                 FUN = function(xy) r2@data@values[which.min(replace(distanceFromPoints(r2,xy), 
                                                                     is.na(r2), NA))])
locations$cell<-sampled
locations$Stratum <- over(st, strata_pol)[,'STRATUM']

#number of samples per strata for random sampling
yc <- aggregate(STATION ~ STRATUM, 
                data = subset(x = ebs_layers$survey.grid, 
                              subset = CORNER == FALSE), 
                FUN = length)
y <- aggregate(STATION ~ STRATUM, 
               data = ebs_layers$survey.grid,
               FUN = length)
n_samples <- data.frame('stratum'= yc$STRATUM, 
                        'scnbase'= y$STATION, 
                        'scnbase_bis'= yc$STATION)

#list baseline strata
baseline_strata <- list(strata_areas = strata_areas,
                        locations = locations,
                        n_samples = n_samples,
                        cell_strata = as.data.frame(grid))

#save data
save(baseline_strata, file = 'output/baseline_strata.RData')
