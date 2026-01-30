####################################################################
####################################################################
##
##    Script #1 
##    Get raw data from bottom trawl survey EBS, NBS and slope
##    Plot sea bottom temperature time series in the regions
##    Create data_geostat file to fit OM VAST 
##    *sea bottom temperature is appended in the next script (#3)
##    Daniel Vilas (danielvilasgonzalez@gmail.com/dvilasg@uw.edu/daniel.vilas@noaa.gov)
##    Lewis Barnett, Stan Kotwicki, Zack Oyafuso, Megsie Siple, Leah Zacher, Lukas Defilippo, Andre Punt
##
####################################################################
####################################################################

#libraries from cran to call or install/load
pack_cran<-c('googledrive','lubridate','ggplot2','fishualize','sp','raster')

#install pacman to use p_load function - call library and if not installed, then install
if (!('pacman' %in% installed.packages())) {
  install.packages("pacman")}

#load/install packages
pacman::p_load(pack_cran,character.only = TRUE)

#install akgfmaps to extract shapefile of Alaska
if (!('akgfmaps' %in% installed.packages())) {
  devtools::install_github('afsc-gap-products/akgfmaps')};library(akgfmaps)

#setwd - depends on computer using
#out_dir<-'C:/Users/Daniel.Vilas/Work/Adapting Monitoring to a Changing Seascape/' #NOAA laptop  
#out_dir<-'/Users/daniel/Work/Adapting Monitoring to a Changing Seascape/' #mac
#out_dir<-'/Users/daniel/Work/VM' #VM
#setwd(out_dir)

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
           'Lepidopsetta sp.',
           'Chionoecetes bairdi',
           'Sebastes alutus',
           'Sebastes melanostictus',
           'Atheresthes evermanni',
           'Sebastes borealis',
           'Sebastolobus alascanus',
           'Glyptocephalus zachirus',
           'Bathyraja aleutica')

#####################################
# Get haul (sampling stations)
#####################################

#read csv file
#haul<-readRDS(paste0('./data raw/',file$name))
#haul<-readRDS(paste0('./data raw/afsc_haul_raw_2023_2_21.rds'))
haul<-readRDS('Data/data_raw/afsc_haul_raw_2023_2_21.rds')

dim(haul);length(unique(haul$hauljoin))

#####################################
# Bering Sea grid
#####################################

#https://github.com/James-Thorson-NOAA/FishStatsUtils/tree/main/data
#load grids
load('Data/extrapolation_grids/eastern_bering_sea_grid.rda')
dim(eastern_bering_sea_grid)
load('Data/extrapolation_grids/northern_bering_sea_grid.rda')
dim(northern_bering_sea_grid)
load('Data/extrapolation_grids/bering_sea_slope_grid.rda')
dim(bering_sea_slope_grid)

#####################################
# Catch data
#####################################

catch<-readRDS('Data/data_raw/afsc_catch_raw_2023_2_21.rds')

#check for names - based on important spp for the slope (all that we have + Blackspotted/rougheye and POP)
unique(catch$common_name)[grep('perch',unique(catch$common_name))] #Pacific ocean perch
unique(catch$common_name)[grep('blackspotted',unique(catch$common_name))] #"rougheye and blackspotted rockfish unid." "blackspotted rockfish"
unique(catch[which(catch$common_name=='Pacific ocean perch'),'scientific_name']) #Sebastes alutus
unique(catch[which(catch$common_name=='rougheye and blackspotted rockfish unid.'),'scientific_name']) #NA - to arrange
unique(catch[which(catch$common_name=='blackspotted rockfish'),'scientific_name']) #Sebastes melanostictus

#add scientific_name to 'rougheye and blackspotted rockfish unid.'
catch$scientific_name[catch$common_name == 'rougheye and blackspotted rockfish unid.'] <- 'Sebastes melanostictus'

#most northern rock sole was missidentified before 1996
unique(catch$common_name)[grepl('rock sole',unique(catch$common_name))]
subset(catch, common_name=='rock sole unid.')

#check other spp
unique(catch$common_name)[grep('shortraker',unique(catch$common_name))] #shortraker rockfish - #Sebastes borealis
#catch[which(catch$common_name=='shortraker rockfish'),]
unique(catch$common_name)[grep('shortspine',unique(catch$common_name))] #shortspine thornyhead - #Sebastolobus alascanus
#catch[which(catch$common_name=='shortspine thornyhead'),]
unique(catch$common_name)[grep('sole',unique(catch$common_name))] #rex sole - #Glyptocephalus zachirus
#catch[which(catch$common_name=='rex sole'),]
unique(catch$common_name)[grep('Aleutian skate',unique(catch$common_name))] #Aleutian skate - #"Bathyraja aleutica"
catch[which(catch$common_name=='Aleutian skate'),]
sort(unique(catch$common_name))

#filter by species
catch1<-subset(catch,scientific_name %in% spp)

#sum blackspotted rockfish and blackspotted rockfish unid
catch2<-catch1[which(catch1$scientific_name=='Sebastes melanostictus'),]
catch21<-aggregate(catch2[, c('cpue_kgha','cpue_kgkm2','cpue_noha','cpue_nokm2','count','weight_kg')], 
                   by = list('hauljoin'=catch2$hauljoin), FUN = sum)
catch3<-cbind('hauljoin'=catch21$hauljoin,
              'species_code'=unique(catch[which(catch$common_name=='blackspotted rockfish'),'species_code']),
              catch21[,-1],
              'taxon_confidence'='Unassessed',
              'scientific_name'='Sebastes melanostictus',
              'common_name'='rougheye and blackspotted rockfish',
              'worms'=unique(catch[which(catch$common_name=='blackspotted rockfish'),'worms']),
              'itis'=NA)
catch1<-subset(catch1,scientific_name != "Sebastes melanostictus")
catch1<-rbind(catch1,catch3)

length(unique(catch1$scientific_name))==length(spp)

#####################################
# Merge catch and haul dataframes
#####################################
#if there are 19 selected spp and 16693 hauls in slope EBS, shelf EBS and NBS
#then 19*16693=317167 rows for the dataframe

#create the empty df 
haul1<-do.call("rbind", replicate(length(spp), haul, simplify = FALSE))
dim(haul1)

#replicate spp for each station
spp1<-rep(spp,each=nrow(haul))

#join dataframe
all<-data.frame(haul1,'scientific_name'=spp1)
head(all);dim(all)

#merge haul and catch
all1<-merge(all,catch1,all.x=T)
dim(all1)

#cpue_kgha,cpue_kgkm2,cpue_noha,cpue_nokm2,count,weight_kg columns need to replace NA by 0s
all1[c('cpue_kgha','cpue_kgkm2','cpue_noha','cpue_nokm2','count','weight_kg')][is.na(all1[c('cpue_kgha','cpue_kgkm2','cpue_noha','cpue_nokm2','count','weight_kg')])] <- 0
head(all1)
summary(all1)

#####################################
# Create data_geostat file that fit OM
#####################################

#create folder
dir.create('./data processed/',showWarnings = FALSE)
dir.create('./data processed/species/',showWarnings = FALSE)

#add year and month
all1$month<-month(as.POSIXlt(all1$date, format="%d/%m/%Y"))
all1$year<-year(as.POSIXlt(all1$date, format="%d/%m/%Y"))

#check Lepidopsetta sp.
mm<-subset(all1,scientific_name=='Lepidopsetta sp.')
mm$year<-lubridate::year(mm$date)
tapply(mm$count,mm$year,summary)

#remove Lepidopsetta sp. >=1996
#all1<-all1[-which(all1$scientific_name=='Lepidopsetta sp.' & all1$year>=1996),]

#remove Lepidopsetta sp. <=1995
all1<-all1[-which(all1$scientific_name=='Lepidopsetta polyxystra' & all1$year<=1995),]

#replace spp rock sole unid
all1$scientific_name[all1$scientific_name == 'Lepidopsetta sp.'] <- 'Lepidopsetta polyxystra'

#remove from spp vector
spp<-spp[spp!='Lepidopsetta sp.']

#save data_geostat file
saveRDS(all1, paste0('./data processed/species/slope_shelf_EBS_NBS_data_geostat.rds'))
#all1<-readRDS(paste0('./data processed/species/slope_shelf_EBS_NBS_data_geostat.rds'))

#loop over species to create data_geostat df
for (sp in spp) {

  #sp<-spp[16]
  
  #print species to check progress
  cat(paste("    -----", sp, "-----\n"))
  
  #create folder to store results
  dir.create(paste0('./data processed/species/',sp),
             showWarnings = FALSE)
  
  #filter by sp
  all2<-subset(all1, scientific_name == sp)
  all2<-subset(all2, year %in% sta_y:end_y)
  cat(paste("    ----- ", nrow(all2) , "samples -----\n"))
  
  #xx<-all2[which(is.na(all2$bottom_temp_c)),]
  #summary(xx)
  #save data_geostat file
  saveRDS(all2, paste0('./data processed/species/',sp,'/data_geostat.rds'))
  
}

#####################################
# Plot SBT distribution
#####################################

#get haul year
haul$year<-year(as.POSIXlt(haul$date, format="%d/%m/%Y"))

#mean SBT by year
yearagg.df <- aggregate(data = haul, bottom_temp_c ~ year, mean)
yearagg.df$TempAnomaly<-NA

#calculate SBT anomaly
for (i in 2:nrow(yearagg.df)) {
  yearagg.df[i,'TempAnomaly']<-yearagg.df[i,'bottom_temp_c']-yearagg.df[i-1,'bottom_temp_c']
}

#plot 1
#print(
p<-
  ggplot() +
    geom_line(data=yearagg.df, aes(x=year, y=bottom_temp_c),linetype='dashed')+
    geom_point(data=yearagg.df, aes(x=year, y=bottom_temp_c,color=bottom_temp_c),size=2)+
    geom_bar(data=yearagg.df, aes(x=year, y=TempAnomaly,fill=TempAnomaly),color='black',stat="identity",position = position_dodge(0.9))+
    scale_colour_gradient2(low = 'darkblue',high='darkred',midpoint = 2.5)+
    scale_fill_gradient2(low = 'darkblue',high='darkred',midpoint = 0,breaks=c(-2,-1,0,1,2),limits=c(NA,2))+
    #xlab(label = 1982:2022)+
    scale_x_continuous(breaks=c(1982:2022),expand = c(0,0.1))+
    theme_bw()+
    labs(y='°C',color='SBT',fill='SBT anomaly',x='')+
    guides(fill=guide_legend(order = 2),color=guide_legend(order = 1))+
    theme(panel.grid.minor.x = element_blank(),legend.spacing  = unit(1,'cm'),
          panel.grid.minor.y = element_line(linetype=2,color='grey90'),axis.text.x = element_text(angle=90,vjust=0.5))
#)

  #save plot
  ragg::agg_png(paste0('./figures/SBT anomaly.png'), width = 13, height = 5, units = "in", res = 300)
  p
  dev.off()
  
#plot 2
print(
  ggplot() +
    geom_density(data=haul, aes(x=bottom_temp_c, group=year))+
    geom_vline(data=yearagg.df,aes(xintercept=bottom_temp_c,group=year, color=bottom_temp_c),
               linetype="dashed", size=1)+
    scale_x_continuous(limits = c(-2,8),breaks =c(-1,1,3,5,7))+
    scale_colour_gradient2(low = 'darkblue',high='darkred',midpoint = 2.5)+
    #geom_text(data=yearagg.df,aes(label=paste0('mean BotTemp = ',round(bottom_temp_c,digits = 2)),x = 10,y=0.43))+
    facet_wrap(~year,nrow = 3)+
    labs(x='°C',y='',color='SBT')+
    theme_bw())


#' 
#' 
#' ################################################################
#' # Plot species abundance over time in the EBS, NBS and slope
#' #################################################################
#' 
#' #selected species
#' spp<-c('Limanda aspera',
#'        'Gadus chalcogrammus',
#'        'Gadus macrocephalus',
#'        'Atheresthes stomias',
#'        'Reinhardtius hippoglossoides',
#'        'Lepidopsetta polyxystra',
#'        'Hippoglossoides elassodon',
#'        'Pleuronectes quadrituberculatus',
#'        'Hippoglossoides robustus',
#'        'Boreogadus saida',
#'        'Eleginus gracilis',
#'        'Anoplopoma fimbria',
#'        'Chionoecetes opilio',
#'        'Paralithodes platypus',
#'        'Paralithodes camtschaticus',
#'        #'Lepidopsetta sp.',
#'        'Chionoecetes bairdi',
#'        'Sebastes alutus',
#'        'Sebastes melanostictus',
#'        'Atheresthes evermanni',
#'        'Sebastes borealis',
#'        'Sebastolobus alascanus',
#'        'Glyptocephalus zachirus',
#'        'Bathyraja aleutica')
#' 
#' #remove Anoploma and Reinhardtius because habitat preference reasons
#' #spp<-setdiff(spp, c('Anoplopoma fimbria','Reinhardtius hippoglossoides'))
#' 
#' #remove two species because of habitat preferece reasons
#' all1<-all1[all1$scientific_name %in% spp,]
#' 
#' #common names
#' spp1<-c('Yellowfin sole',
#'         'Alaska pollock',
#'         'Pacific cod',
#'         'Arrowtooth flounder',
#'         'Greenland turbot',
#'         'Northern rock sole',
#'         'Flathead sole',
#'         'Alaska plaice',
#'         'Bering flounder',
#'         'Arctic cod',
#'         'Saffron cod',
#'         'Sablefish',
#'         'Snow crab',
#'         'Blue king crab',
#'         'Red king crab',
#'         'Tanner crab',
#'         'Pacific ocean perch',
#'         'Rougheye and blackspotted rockfish',
#'         'Kamchatka flounder',
#'         'Shortraker rockfish',
#'         'Shortspine thornyhead',
#'         'Rex sole',
#'         'Aleutian skate')
#' 
#' #df sp scientific and common
#' df_spp<-data.frame('spp'=spp,
#'                    'common'=spp1)
#' 
#' #merge both df
#' all2<-merge(all1,df_spp,by.x='scientific_name',by.y = 'spp',all.x = 'TRUE')
#' 
#' #get sci + common name
#' all2$sp<-paste0(all2$scientific_name,'\n(',all2$common,')')
#' all2$scientific_name2<-gsub(' ','_',all2$scientific_name)
#' 
#' #plot CPUE in log+1 for better visualization
#' p<-
#'   ggplot()+
#'   geom_boxplot(data=all2,aes(x=year,y=log(cpue_kgha+1),group=interaction(year,survey_name),color=survey_name),alpha=0.7,position = position_dodge2(preserve = "single"))+
#'   facet_wrap(~sp,scales = 'free_y',nrow=5)+
#'   #add_fishape(data=all2,aes(option = scientific_name2))+
#'   scale_color_manual(values=c("Northern Bering Sea Crab/Groundfish Survey - Eastern Bering Sea Shelf Survey Extension"="#4682B4",
#'                               "Eastern Bering Sea Crab/Groundfish Bottom Trawl Survey"="#B4464B",
#'                               "Eastern Bering Sea Slope Bottom Trawl Survey"="#B4AF46"),
#'                      labels = c('EBS shelf','EBS slope','NBS'),name='survey')+
#'   scale_x_continuous(breaks = c(1985,1990,1995,2000,2005,2010,2015,2020),
#'                      minor_breaks = setdiff(1982:2022,c(1982,1985,1990,1995,2000,2005,2010,2015,2020,2022)))+
#'   scale_y_continuous(limits = c(0,NA),labels = scales::comma)+
#'   theme_bw()+
#'   labs(y='log(CPUE+1)',x='')+
#'   theme(panel.grid.minor = element_line(linetype=2,color='grey90'),strip.background = element_rect(fill='white'),
#'         legend.position=c(.80,.08),legend.key.size = unit(20, 'points'),legend.text = element_text(size=10),
#'         legend.title = element_text(size=14),strip.text = element_text(size=12))+ #axis.text.x = element_text(angle=90,vjust=0.5),
#'   expand_limits(y = 0)
#' 
#' #save plot
#' ragg::agg_png(paste0('./figures/CPUE_survey_year_v2.png'), width = 15, height = 14, units = "in", res = 300)
#' p
#' dev.off()
