####################################################################
####################################################################
##
##    Script #0
##    Plot sampling stations for each region
##    Figure 1
##    Baseline strata - existing sampling design
##    Daniel Vilas (danielvilasgonzalez@gmail.com)
##    Lewis Barnett, Stan Kotwicki, Zack Oyafuso, Megsie Siple, Leah Zacher,
##    Lukas DeFilippo, Andre Punt
##    
####################################################################
####################################################################

#####################################
# Settings
#####################################

#clear all objects
rm(list = ls(all.names = TRUE)) 

#libraries from cran to call or install/load
pack_cran<-c('cowplot','ggspatial','raster','rasterVis','rgeos','scales',
             'rnaturalearth','grid','ggplot2','lubridate','ragg','rgdal', "pak",
             "tidyterra")

#install pacman to use p_load function
if (!('pacman' %in% installed.packages())) {
  install.packages("pacman")}

#install akgfmaps to extract shapefile of Alaska
if (!('akgfmaps' %in% installed.packages())) {
  devtools::install_github('afsc-gap-products/akgfmaps')};library(akgfmaps)

#load/install packages
pacman::p_load(pack_cran, character.only = TRUE)

# Define plot extent (through trial end error) units km
panel_extent <- data.frame(x = c(-1716559.21, -77636.05), 
                           y = c(483099.5, 2194909.7)) 

if (!dir.exists(paths = "figures/")) dir.create(path = "figures/")

################################################
# Alaska land shapefile from afgfmaps package
################################################
ebs_layers <- akgfmaps::get_base_layers(select.region = "bs.all", 
                                        set.crs = "EPSG:3338",
                                        design.year = 2022)

ebs_layers$survey.grid <- 
  sf::st_intersection(
    x = ebs_layers$survey.grid[, c("STATION", "AREA_M2")] |> 
      sf::st_centroid(of_largest_polygon = T), 
    y = ebs_layers$survey.strata[, "STRATUM"]
  )
ebs_layers$survey.grid$CORNER <- grepl(pattern = 'GF|HG|JI|IH|ON|QP|PO',
                                       x = ebs_layers$survey.grid$STATION)
ak_land <- ebs_layers$akland

#####################################
# Depth raster (from gebco) - downloaded in december 2022
# Crop and Mask to EBS/NBS survey footprint
# truncate positive values to 0 and reverse sign so that values are positive
#####################################
ak_bathy <- 
  terra::rast(x = 'data/data_raw//ak_bathy_NAD83.tiff') |> 
  terra::crop(y = ebs_layers$survey.area, mask = TRUE)
ak_bathy[ak_bathy > 0] <- 0 
ak_bathy <- -ak_bathy

#########################################################
# Plot sampling stations in EBS, NBS and slope in 2010
#########################################################
#segment for pointing islands in the zoomin plot
seg <- data.frame('x' = c(-170,-169,-172.9,-176.7,-179,-168.9,-170.9,-169,-169.5),
                  'y' = c(56.8,55,60.5,62.3, 62.1,54.4,63.5,67,67.5))
coordinates(seg)<- ~x + y
proj4string(seg) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0") 
seg1<-spTransform(seg, CRSobj = CRS('EPSG:3338'))
seg2<-as.data.frame(seg1)

#plot
ggplot() +
  geom_sf(data = ebs_layers$survey.strata, fill = NA) +
  geom_sf(data = ebs_layers$survey.grid |> sf::st_centroid(), shape = 4,
          aes(color = CORNER)) +
  scale_color_manual(values = c("TRUE" = "red", "FALSE" = "black")) +
  geom_sf(data = ak_land, fill = 'grey60') +
  scale_x_continuous(
    expand = c(0,0), 
    position = 'bottom',
    breaks = c(-175,-170,-165,-160,-155), 
    sec.axis = dup_axis()
  ) +
  geom_sf(data = ebs_layers$survey.area, 
          fill = NA, col = 'black', linewidth = 1.) +
  coord_sf(crs = "EPSG:3338",
           xlim = c(panel_extent$x[1] + 200000, panel_extent$x[2] + 100000),
           ylim = c(panel_extent$y[1] - 100000, panel_extent$y[2] - 200000),
           label_axes = "-NE-") +
  ## Add Place Names
  annotate("text", x = -256559, y = 1354909, 
           label = "Alaska", parse=TRUE, size=7) +
  annotate("text", x = -1376559, y = 2049090, 
           label = "Russia", parse=TRUE, size=7) +
  annotate("text", x = -816559, y = 1454909, 
           label = "NBS", parse=TRUE,size=7) +
  annotate("text", x = -816559, y = 1024909, 
           label = "EBS", size = 7) +
  annotate("text", x = -1264892, y = 1550000, 
           label = "St. Matthew\nIsland", size = 5) +
  annotate("text", x = -1376559, y = 744900, 
           label = "italic('Bering Sea')", parse = TRUE, size = 9) +
  annotate("text", x = -970000, y = 600000, 
           label = "Pribilof\nIslands", size = 5, lineheight = 0.9) +
  annotate("text", x = -616559, y = 1950000, 
           label = "St. Lawrence\nIsland", size = 5, lineheight = 0.9) +
  annotation_scale(location='tr') +
  theme(aspect.ratio = 1, 
        panel.grid.major = element_line(
          color = rgb(0, 0, 0, 20, 
                      maxColorValue = 285), 
          linetype = 'dashed', 
          linewidth =  0.5
        ),
        panel.background = element_rect(fill = NA), 
        panel.ontop = TRUE, 
        text = element_text(size = 10),
        legend.background = element_rect(fill = "transparent", 
                                         colour = "transparent"),
        legend.key.height = unit(25, 'points'),
        legend.key.width = unit(25, 'points'),
        axis.title = element_blank(),
        legend.position = 'none', 
        panel.border = element_rect(fill = NA, colour = 'black'),
        legend.key = element_rect(color="black"),
        axis.text = element_text(color='black'),
        legend.spacing.y = unit(10, 'points'),
        axis.text.y.right = element_text(
          hjust= 0.1,
          margin = margin(0, 7, 0, -25, unit = 'points'),
          color='black'
        ),
        axis.text.x = element_text(
          vjust = 6, 
          margin = margin(-7, 0, 7, 0, unit = 'points'),
          color='black'
        ),
        axis.ticks.length = unit(-5, "points")) 

## Pull USA, Russia, Canada, Mexico and associated lake polygons for zoomout plot
count <- rnaturalearth::ne_countries(
  scale = 'large',
  country = c('united states of america',"russia","canada",'mexico'),
  returnclass = c( "sf")
) |>
  sf::st_transform(crs = "EPSG:3338")
lakes <- rnaturalearth::ne_download(
  category = "physical", 
  type = "lakes", 
  returnclass = "sf", 
  scale = 'small'
)|>
  sf::st_transform(crs = "EPSG:3338")

#zoomout plot
# zoomout<-
ggplot() + 
  geom_sf(data = count,
          color='black',
          linewidth = 0.2,
          fill = 'grey80') + 
  geom_sf(data = lakes,
          color='black',
          linewidth=0.2,
          fill='white') + 
  scale_y_continuous(expand = c(0,0)) +
  scale_x_continuous(expand = c(0,0),
                     breaks = c(160, 170, -180, -170, -160, -150, -140, -130)) +
  coord_sf(crs ='EPSG:3338',
           xlim=c(-3500000, 3500000),
           ylim = c(-2000000, 3000000)) +
  theme(
    panel.grid.major = element_line(
      color = rgb(0, 0, 0, 20, maxColorValue = 285),
      linetype = 'dashed', 
      linewidth =  0.5
    ),
    panel.ontop = TRUE,
    text = element_text(size = 10),
    legend.background = element_rect(fill = "transparent", 
                                     colour = "transparent"),
    legend.key.height = unit(20, 'points'),
    legend.key.width = unit(20, 'points'),
    axis.title = element_blank(),
    legend.position = c(0.12, 0.155),
    panel.border = element_rect(fill = NA, colour = 'black'),
    axis.text = element_blank(),
    axis.ticks.length = unit(-3, "points"),
    panel.background = element_rect(fill = NA),
    plot.margin=grid::unit(c(0, 0, 0, 0), "mm")
  ) +
  geom_rect(aes(xmin = panel_extent$x[1], 
                xmax = panel_extent$x[2], 
                ymin = panel_extent$y[1], 
                ymax = panel_extent$y[2]),
            colour = '#800000', 
            linetype = 'solid', 
            fill = NA,
            linewidth = 0.7) +
  annotate("text", x = -2300000, y = 2600000, 
           label = "Russia", parse = TRUE, size = 4) +
  annotate("text", x = 0, y = -1000000, 
           label = "italic('Pacific Ocean')", parse = TRUE, size = 4) +
  annotate("text", x = 2000000, y = 1400000, 
           label = "Canada", parse = TRUE, size = 4) +
  annotate("text", x = 3000000, y = 0, 
           label = "USA", parse = TRUE, size = 4)

#save plot
# dir.create('./figures/')
# agg_png(paste0('./figures/map_bering6.png'), width = 7, height = 7, units = "in", res = 300)
# grid.newpage()
# vp_b <- viewport(width = 1, height = 1, x = 0.5, y = 0.5)  # the larger map
# vp_a <- viewport(width = 0.4, height = 0.3, x = 0.217, y = 0.846)  # the inset in upper left
# print(zoomin , vp = vp_b)
# print(zoomout , vp = vp_a)
# dev.off()

#depth miniplot
# pd<-
ggplot() +
  tidyterra::geom_spatraster(data = ak_bathy) +
  geom_sf(data = ak_land, color = 'black', 
          linewidth = 0.2, fill = 'grey80') +
  scale_x_continuous(expand = c(0, 0), position = 'bottom',
                     breaks = c(-180, -175, -170, -165, -160, -155), 
                     sec.axis = dup_axis()) +
  geom_sf(data = ebs_layers$survey.area, linewidth = 0.3, 
          fill = NA, col = 'black') +
  coord_sf(crs = 'EPSG:3338',
           xlim = panel_extent$x,
           ylim = c(panel_extent$y[1], panel_extent$y[2] - 300000),
           label_axes = "-NE-") +
  scale_fill_gradient2(
    high = '#10715e', mid='#1ABC9C', low = 'white', midpoint = 100,
    limits = c(5, 200), oob = scales::squish,
    breaks = c(5, 50, 100, 200), labels = c('0', '50', '100', '200'),
    na.value = NA, name = 'depth (m)',
    guide = guide_colorbar(frame.colour = 'black', ticks.colour = 'black')
  ) +
  theme(
    aspect.ratio = 1,
    panel.grid.major = element_line(
      color = rgb(0, 0, 0,20, maxColorValue = 285), 
      linetype = 'dashed', 
      linewidth =  0.5
    ),
    panel.background = element_rect(fill = NA),
    panel.ontop = TRUE,
    text = element_text(size = 12),
    legend.background = element_rect(fill = "transparent", 
                                     colour = "transparent"),
    legend.key.height= unit(20, 'points'),
    legend.key.width= unit(15, 'points'),
    axis.title = element_blank(),
    legend.position = c(0.13,0.55), 
    panel.border = element_rect(fill = NA, colour = 'black'),
    legend.key = element_rect(color="black"),
    axis.text = element_text(color='black'),
    legend.spacing.y = unit(10, 'points'),
    axis.text.y.right = element_text(
      hjust= 0.1,
      margin = margin(0, 0, 0, -28, unit = 'points'),
      color='black'
    ),
    axis.text.x = element_text(
      vjust = 6, margin = margin(-4, 0, 0, 0, unit = 'points'),
      color='black'
    ),
    axis.ticks.length = unit(-5, "points")
  ) +
  scale_y_continuous(expand = c(0, 0),
                     position = 'right',
                     sec.axis = dup_axis(),
                     breaks = c(65, 60, 55))

#load optim file (in script #6 to get meanTemp and varSBT)
load('output/optimization/multisp_optimization_static_data.RData')
head(df)
coordinates(df) <- ~ Lon + Lat
proj4string(df) <- CRS("EPSG:4326")
df1<-spTransform(df, CRSobj = CRS("EPSG:3338"))
df1<-as.data.frame(df1, xy=TRUE)

df1 <- sf::st_as_sf(x = df, coords = c("Lon", "Lat"), crs = "EPSG:4326") |>
  sf::st_transform(crs = "EPSG:3338")

#varSBT miniplot
# pv<-
ggplot()+
  geom_sf(data=df1, aes(color = varTemp), size = 0.3) +
  geom_sf(data = ak_land, color='black', linewidth = 0.2, fill = 'grey80') +
  scale_x_continuous(expand = c(0,0),position = 'bottom',
                     breaks = c(-180,-175,-170,-165,-160,-155), 
                     sec.axis = dup_axis()) +
  geom_sf(data = ebs_layers$survey.area, 
          linewidth = 0.3, fill = NA, col = 'black') +
  coord_sf(crs = "EPSG:3338",
           xlim = panel_extent$x,
           ylim = c(panel_extent$y[1], panel_extent$y[2] - 300000),
           label_axes = "-NE-") +
  scale_color_gradient2(
    high = '#3498DB',
    name='varSBT (°C)',
    guide = guide_colorbar(frame.colour = 'black', ticks.colour = 'black')
  ) +
  theme(
    aspect.ratio = 1,
    panel.grid.major = element_line(
      color = rgb(0, 0, 0,20, maxColorValue = 285), 
      linetype = 'dashed', 
      linewidth =  0.5
    ),
    panel.background = element_rect(fill = NA),
    panel.ontop = TRUE,
    text = element_text(size = 12),
    legend.background =  element_rect(
      fill = "transparent", 
      colour = "transparent"
    ),
    legend.key.height = unit(20, 'points'),
    legend.key.width = unit(15, 'points'),
    axis.title = element_blank(),
    legend.position = c(0.16, 0.55), 
    panel.border = element_rect(fill = NA, colour = 'black'),
    legend.key = element_rect(color="black"),
    axis.text = element_text(color='black'),
    legend.spacing.y = unit(10, 'points'),
    axis.text.y.right = element_text(
      hjust = 0.1, 
      margin = margin(0, 0, 0, -28, unit = 'points'),
      color='black'
    ),
    axis.text.x = element_text(
      vjust = 6, 
      margin = margin(-4, 0, 0, 0, unit = 'points'),
      color = 'black'
    ),
    axis.ticks.length = unit(-5,"points") 
  ) +
  scale_y_continuous(expand = c(0, 0),
                     position = 'right',
                     sec.axis = dup_axis(),
                     breaks = c(65, 60, 55))

