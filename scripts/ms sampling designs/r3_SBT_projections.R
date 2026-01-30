####################################################################
####################################################################
##
##    Create raster of SBT for species projections
##    Plot SBT projection figures - scenarios annual maps and SBT change
##    Daniel Vilas (danielvilasgonzalez@gmail.com/dvilasg@uw.edu/daniel.vilas@noaa.gov)
##    Lewis Barnett, Stan Kotwicki, Zack Oyafuso, Megsie Siple, Leah Zacher, Lukas Defilippo, Andre Punt
##
####################################################################
####################################################################

#clear all objects
rm(list = ls(all.names = TRUE)) 
#free up memrory and report the memory usage
gc() 

#libraries from cran to call or install/load
pack_cran<-c('ncdf4','raster','FNN','lubridate','rgeos','scales','rnaturalearth','grid','ggplot2','rasterVis','ggthemes','reshape2','ragg',"wesanderson")

#install pacman to use p_load function - call library and if not installed, then install
if (!('pacman' %in% installed.packages())) {
  install.packages("pacman")}

#load/install packages
pacman::p_load(pack_cran,character.only = TRUE)

#setwd - depends on computer using
#out_dir<-'C:/Users/Daniel.Vilas/Work/Adapting Monitoring to a Changing Seascape/' #NOAA laptop  
out_dir<-'/Users/daniel/Work/Adapting Monitoring to a Changing Seascape/' #mac
#out_dir<-'/Users/daniel/Work/VM' #VM
setwd(out_dir)

#range years of data
sta_y<-1982
end_y<-2022

#selected species
spp<-c('Limanda aspera',
       'Gadus chalcogrammus',
       'Gadus macrocephalus',
       'Atheresthes stomias',
       'Reinhardtius hippoglossoides',
       'Lepidopsetta polyxystra',
       'Hippoglossoides elassodon',
       'Pleuronectes quadrituberculatus',
       'Hippoglossoides robustus',
       'Boreogadus saida',
       'Eleginus gracilis',
       'Anoplopoma fimbria',
       'Chionoecetes opilio',
       'Paralithodes platypus',
       'Paralithodes camtschaticus',
       #'Lepidopsetta sp.',
       'Chionoecetes bairdi',
       'Sebastes alutus',
       'Sebastes melanostictus',
       'Atheresthes evermanni',
       'Sebastes borealis',
       'Sebastolobus alascanus',
       'Glyptocephalus zachirus',
       'Bathyraja aleutica')

######################################
# Create SBT rasters (2023-2027) for projections
######################################

#create directory
dir.create('./tables/',showWarnings = FALSE)
  
#create SBT projections
df_sbt<-data.frame(y2022=c(0,0,0,0,0,0,0,0,0,0,0,0),
                   y2023=c(0,-0.5,0.5,1,1,-1,2,-2,3,-3,4,5),
                   y2024=c(0,-1,1,2,0,0,0,0,0,0,0,0),
                   y2025=c(0,-1.5,1.5,3,1,-1,2,-2,3,-3,4,5),
                   y2026=c(0,-2,2,4,0,0,0,0,0,0,0,0),
                   y2027=c(0,-2.5,2.5,5,1,-1,2,-2,3,-3,4,5),
                   Scenario=c('status quo','gradually cold','gradually warm','severe gradually warm',
                              'warm low variation','cold low variation','warm moderate variation',
                              'cold moderate variation','warm high variation','cold high variation',
                              'warm very high variation','warm extreme variation'))
  
#remove cold SBT scenarios
df_sbt<-df_sbt[-grep('cold',df_sbt$Scenario),]    
df_sbt$sbt_n<-1:nrow(df_sbt)

#save SBT table
save(df_sbt,file = './tables/SBT_projection.RData')

#load grid
load('./data processed/grid_EBS_NBS.RData')

#subset to year 2022
df2022<-subset(grid.ebs_year,Year==2022)

#create spatial object from df
coordinates(df2022) <- ~ Lon + Lat
proj4string(df2022) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0") 
df2022<-spTransform(df2022,CRSobj = CRS('+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs'))

#n cells
xycells<-as.integer(sqrt(dim(df2022)[1]))

# create a template raster
r1 <- raster(ext=extent(df2022),nrow=xycells,ncol=xycells)

#create raster
r2<-rasterize(df2022, r1 ,field='Temp')
crs(r2) <- CRS('+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs')
#plot(r1)

#color palette
pal<-wesanderson::wes_palette('Zissou1',21,type='continuous')

#year
yrs<-2023:2027

#dir.create
dir.create('./data processed/SBT projections/')

#create folder
dir.create('./figures/SBT/')

#Alaska layer
ebs_layers <- akgfmaps::get_base_layers(select.region = "ebs", set.crs = "EPSG:3338")
ak_sppoly<-as(ebs_layers$akland, 'Spatial')

# Define plot extent (through trial end error) units km
panel_extent <- data.frame(x = c(-1516559.21, -77636.05), #x = c(-1326559.21, -87636.05),
                           y = c(483099.5, 1894909.7)) #y = c(533099.5, 1894909.7))

#list to store plots
plot_sbt<-list()

#loop over scenarios and projected years
for (sbt in unique(df_sbt$sbt_n)) {

  #sbt<-7
  
  #list for plots
  plot_list<-list()
  
  #plot 2022 SBT
  p<-
    gplot(r2)+
    geom_tile(aes(fill=value))+
    geom_polygon(data=ak_sppoly,aes(x=long,y=lat,group=group),color='black',linewidth=0.2,fill = 'grey80')+
    #geom_polygon(data=eez_sh33,aes(x=long,y=lat,group=group),fill=NA,color='grey40')+
    scale_x_continuous(expand = c(0,0),position = 'bottom',
                       breaks = c(-175,-170,-165,-160,-155),sec.axis = dup_axis())+
    coord_sf(crs = '+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs',
             xlim = panel_extent$x,
             ylim = panel_extent$y,
             #lim = c(panel_extent$x[1]+200000,panel_extent$x[2]),
             #ylim = c(panel_extent$y[1],panel_extent$y[2]-200000),
             label_axes = "-NE-")+
    scale_fill_gradient2(low = 'darkblue',midpoint = 3,#c1f2fe',
                         high = 'red',#'#007c9b',
                         limits=c(-1,15),#oob = scales::squish,breaks=c(0,50,100,200),
                         #labels=c('0','50','100',paste0('200 - ',round(maxValue(ak_bathy_4)))),
                         na.value = 'white',
                         name='SBT (°C)',
                         guide = guide_colorbar(frame.colour = 'black',ticks.colour = 'black'))+
    theme(aspect.ratio = 1,panel.grid.major = element_line(color = rgb(0, 0, 0,20, maxColorValue = 285), linetype = 'dashed', linewidth =  0.5),
          panel.background = element_rect(fill = NA),panel.ontop = TRUE,text = element_text(size=10),
          legend.background =  element_rect(fill = "white", colour = "white"),#legend.key.height= unit(25, 'points'),
          #legend.key.width= unit(25, 'points'),
          legend.key.height= unit(35, 'points'),
          legend.key.width= unit(35, 'points'),
          legend.text = element_text(size=14),
          legend.title = element_text(size=16),
          legend.direction = "horizontal", #legend.position = "bottom",
          axis.title = element_blank(),#legend.position = 'none',
          panel.border = element_rect(fill = NA, colour = 'black'),#legend.key = element_rect(color="black"),
          axis.text = element_text(color='black'),legend.spacing.y = unit(10, 'points'),
          axis.text.y.right = element_text(hjust= 0.1 ,margin = margin(0,7,0,-25, unit = 'points'),color='black'),
          axis.text.x = element_text(vjust = 6, margin = margin(-7,0,7,0, unit = 'points'),color='black'),
          axis.ticks.length = unit(-5,"points"),plot.title = element_text(hjust = 0.01, vjust = -7, size = 14),
          plot.margin = margin(0.01,0.01,0.01,0.01))+
    labs(title = '2022')+
    guides(fill=guide_colorbar(title.position='top',ticks.colour = 'black',frame.colour = 'black'))+
    scale_y_continuous(expand = c(0,0),position = 'right',sec.axis = dup_axis())
  #+
    # theme(legend.key.height= unit(35, 'points'),
    #       legend.key.width= unit(35, 'points'),
    #       legend.text = element_text(size=14),
    #       legend.title = element_text(size=16),
    #       legend.position = "bottom")+ 
    # guides(fill=guide_colorbar(title.position='top', title.hjust = 0.5,ticks.colour = 'black',frame.colour = 'black'))
  
  
  # #store plot in list
  # plot_list[[1]]<-p
    
  #get legend
  legend1 <- cowplot::get_legend(p)
  plot(legend1)
  
  #create stack
  sbt_stack<-stack()
  
  #add layer to stack
  sbt_stack<-addLayer(sbt_stack,r2)
  names(sbt_stack)<-'y2022'
  
  #loop over projected years
  for (y in 1:5) {
    #y<-1
    
    #print species to check progress
    cat(paste(" #############  Scenario", sbt, "year",y, " #############\n"))
    
    #modify raster y2022
    r3<-r2
    names(r3)<-paste0('y',yrs[y])
    values(r3)<-values(r3)+df_sbt[which(df_sbt$sbt_n==sbt),paste0('y',yrs[y])]
    
    #add layer to stack
    sbt_stack<-addLayer(sbt_stack,r3)

    #plot SBT map 2023:2027
    p1<-
    gplot(r3)+
      geom_tile(aes(fill=value))+
      geom_polygon(data=ak_sppoly,aes(x=long,y=lat,group=group),color='black',linewidth=0.2,fill = 'grey80')+
      #geom_polygon(data=eez_sh33,aes(x=long,y=lat,group=group),fill=NA,color='grey40')+
      scale_x_continuous(expand = c(0,0),position = 'bottom',
                         breaks = c(-170,-160),sec.axis = dup_axis())+
      coord_sf(crs = '+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs',
               xlim = panel_extent$x,
               ylim = c(panel_extent$y[1]-50000,panel_extent$y[2]),
               #lim = c(panel_extent$x[1]+200000,panel_extent$x[2]),
               #ylim = c(panel_extent$y[1],panel_extent$y[2]-200000),
               label_axes = "-NE-")+
      scale_fill_gradient2(low = 'darkblue',midpoint = 3,#c1f2fe',
                           high = 'red',#'#007c9b',
                           limits=c(-1,15),#oob = scales::squish,breaks=c(0,50,100,200),
                           #labels=c('0','50','100',paste0('200 - ',round(maxValue(ak_bathy_4)))),
                           na.value = 'white',
                           name='SBT',
                           guide = guide_colorbar(frame.colour = 'black',ticks.colour = 'black'))+
      theme(panel.grid.major = element_line(color = rgb(0, 0, 0,20, maxColorValue = 285), linetype = 'dashed', linewidth =  0.5),
            #panel.background = element_rect(fill = NA),
            panel.background = element_rect(fill='transparent'),
            plot.background = element_rect(fill='transparent', color=NA),
            panel.ontop = TRUE,text = element_text(size=14),
            legend.background =  element_rect(fill = "transparent", colour = "transparent"),legend.key.height= unit(25, 'points'),
            legend.key.width= unit(25, 'points'),axis.title = element_blank(),legend.position = 'none', #c(0.12,0.47)
            panel.border = element_rect(fill = NA, colour = 'black'),legend.key = element_rect(color="black"),
            axis.text = element_text(color='black'),legend.spacing.y = unit(10, 'points'),
            plot.margin = margin(0,0.05,0.05,0.05), 
            axis.text.y.right = element_text(hjust= -0.1 ,margin = margin(0,0,0,-33, unit = 'points'),color='black'),
            #axis.text.y = element_text(hjust = -0.1, margin = margin(0,0,0,0, unit = 'points'), color = 'black'),
            axis.text.x = element_text(vjust = 6, margin = margin(-5,0,0,0, unit = 'points'),color='black'),axis.ticks.length.y = unit(-5,"points"),
            axis.ticks.length.x = unit(-5,"points"),plot.title = element_text(hjust = 0.03, vjust = -7, size = 16))+
      labs(title = yrs[y])+
      scale_y_continuous(expand = c(0,0),position = 'right',sec.axis = dup_axis(), breaks = c(64,60,56))
      
    #store plot
    plot_list[[y]]<-p1
    
  }
  
  pgrid1<-cowplot::plot_grid(plotlist = plot_list, nrow = 1)
  #pgrid2<-cowplot::plot_grid(plotlist = plot_list_n, nrow = 2)
  title <- cowplot::ggdraw() + cowplot::draw_label(paste0(df_sbt[sbt,'Scenario'],' (SBT',sbt,')'), fontface='bold',vjust=1.75)
  
  #plot_sbt[[sbt]]<-
    cowplot::plot_grid(title,pgrid1, nrow = 2, rel_heights = c(.07,1))
  
  #save plots
  ragg::agg_png(paste0('./figures/SBT/sbt',sbt,'.png'), width = 12.2, height = 3, units = "in", res = 300)
  print(cowplot::plot_grid(title,pgrid1, nrow = 2, rel_heights = c(.10,1)))
  dev.off()
  
  #sbt remove 2022
  sbt_stack<-sbt_stack[[paste0('y',2023:2027)]]
  
  #save sbt rasters 
  #save raster
  writeRaster(sbt_stack, paste0('data processed/SBT projections/SBT_',sbt,'_',paste0(range(yrs)[1],'-',range(yrs)[2]),'.grd'),overwrite=TRUE)
}

#pgrid2<-cowplot::plot_grid(plotlist = plot_sbt, ncol = 1)

ragg::agg_png(paste0('./figures/SBT_future.png'), width = 11, height = 24, units = "in", res = 300)
print(cowplot::plot_grid(pgrid2,legend1, nrow = 2, rel_heights = c(1,.05)))
dev.off()



######################################
# Plot SBT projections SBT trend
######################################

#projection name
df_sbt$Scenario<-paste0('SBT',1:nrow(df_sbt),'_',df_sbt$Scenario)

#reshape df
df_sbt1<-reshape2::melt(df_sbt[,1:7],id='Scenario')
df_sbt1$Scenario <- factor(df_sbt1$Scenario, levels = df_sbt$Scenario)

#plot
p<-
  ggplot()+
  geom_line(data=df_sbt1,aes(x=variable,y=value,group=Scenario,color=Scenario),size=0.8)+
  #scale_color_tableau(palette = 'Tableau 10')+
  scale_color_manual(values=c('SBT1_status quo'="grey50", 'SBT2_gradually warm'="#a25db4", 'SBT3_severe gradually warm'="#740094",
                              'SBT4_warm low variation'="#f5aab4", 'SBT5_warm moderate variation'="#ec6174", 'SBT6_warm high variation'="#e5203a",
                              'SBT7_warm very high variation'="#c5162d", 'SBT8_warm extreme variation'="#8a1020"))+
  theme_bw()+
  scale_x_discrete(labels=c(2022:2027))+
  labs(color = 'SBTn_scenario name', x = '', y = expression('SBT anomaly (SBT'[y]*'-SBT'[2022]*') (°C)'))

#save plot
ragg::agg_png(paste0('./figures/SBT_projections_v2.png'), width = 7, height = 4, units = "in", res = 300)
print(p)
dev.off()

