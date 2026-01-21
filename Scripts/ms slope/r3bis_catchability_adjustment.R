####################################################################
####################################################################
##
##    Catchability adjustment for the BS slope hauls
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
pack_cran<-c('ncdf4','raster','FNN','lubridate','ggpubr')

#install pacman to use p_load function - call library and if not installed, then install
if (!('pacman' %in% installed.packages())) {
  install.packages("pacman")}

#load/install packages
pacman::p_load(pack_cran,character.only = TRUE)

#setwd - depends on computer using
#out_dir<-'C:/Users/Daniel.Vilas/Work/Adapting Monitoring to a Changing Seascape/' #NOAA laptop  
# out_dir<-'/Users/daniel/Work/UW-NOAA/Adapting Monitoring to a Changing Seascape/' #mac
# setwd(out_dir)

#range years of data
# sta_y<-1982
# end_y<-2022

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

#remove Anoploma and Reinhardtius because habitat preference reasons
#spp<-setdiff(spp, c('Anoplopoma fimbria','Reinhardtius hippoglossoides'))

#common names
spp1<-c('Yellowfin sole',
        'Alaska pollock',
        'Pacific cod',
        'Arrowtooth flounder',
        'Greenland turbot',
        'Northern rock sole',
        'Flathead sole',
        'Alaska plaice',
        'Bering flounder',
        'Arctic cod',
        'Saffron cod',
        'Sablefish',
        'Snow crab',
        'Blue king crab',
        'Red king crab',
        'Tanner crab',
        'Pacific ocean perch',
        'Rougheye and blackspotted rockfish',
        'Kamchatka flounder',
        'Shortraker rockfish',
        'Shortspine thornyhead',
        'Rex sole',
        'Aleutian skate')

#read catchability data
#s12 Selectivity ratio > 1 means the slope gear and protocol had higher selectivity
#so, we need to divide slope data by the sr index
#471 is for Alaska skate - which we will use to represent SR values for
#Aleutian skate 472 given lack of species-specific data but similar morphology
data_sratio<-readRDS('Data/data_raw/shelf_slope_sratio_bootstrap.rds')
#data_sratio<-readRDS('./data raw/shelf_slope_sratio_bootstrap.rds')
unique(data_sratio$SPECIES_CODE)

#convert SR of one speccies into another
data_sratio[which(data_sratio$SPECIES_CODE=='471'),'SPECIES_CODE']<-'472'

#read length raw data
#data_length<-readRDS('Data/data_raw/ak_bts_ebs_nbs_slope.rds') #data_length
data_length<-readRDS('Data/data_raw/ak_bts_ebs_nbs_slope.rds') #data_length
head(data_length)
head(data_length$specimen)
dim(data_length$specimen)
head(data_length$catch)

unique(data_length$size$SPECIES_CODE)
head(data_length$specimen)

#get cruisejoin for the ebs
head(data_length$cruise)
cruisejoin_ebs<-subset(data_length$cruise,SURVEY=='EBS')[,'CRUISEJOIN']

#get hauls for the ebs
hauls_ebs<-subset(data_length$haul,CRUISEJOIN %in% cruisejoin_ebs)

#convert time
time_axis <- as.POSIXct(hauls_ebs$START_TIME, origin = "1900-01-01", tz = "GMT") 
hauls_ebs$YEAR <- format(time_axis, "%Y")

#check hauls number per year in ebs
aggregate(HAULJOIN ~ YEAR,hauls_ebs,FUN=length)

#code species
spp_code<-unique(data_length$species[,c('SPECIES_CODE',"REPORT_NAME_SCIENTIFIC")])
names(spp_code)<-c('species_code',"scientific_name")
spp_code1<-spp_code[which(spp_code$scientific_name %in% 
                            spp),]
#add Alaska skate
spp_code1<-rbind(spp_code1,c(472,'Bathyraja aleutica'))

#merge sr data to species
data_sratio<-merge(data_sratio,spp_code1,by.x='SPECIES_CODE',by.y='species_code')
#1000 samples per size and species combination
aggregate(SPECIES_CODE ~ SIZE_BIN +scientific_name,data_sratio,FUN=length)

#get mean
data_sratio<-subset(data_sratio,s12<100)

# Custom function to calculate mean, 90th, and 10th percentiles
custom_summary <- function(x) {
  c(
    mean = mean(x, na.rm = TRUE), 
    p90 = quantile(x, 0.9, na.rm = TRUE), 
    p10 = quantile(x, 0.1, na.rm = TRUE)
  )
}

# Apply the custom function using aggregate
data_sratio_summary <- aggregate(
  s12 ~ SIZE_BIN + scientific_name,
  data = data_sratio,
  FUN = function(x) unlist(custom_summary(x))
)

# Transform the result to a more usable format
data_sratio_summary <- do.call(data.frame, data_sratio_summary)

# Rename the columns for clarity
names(data_sratio_summary) <- c("SIZE_BIN", "scientific_name", "mean", "p90", "p10")

# View the result
print(data_sratio_summary)

#plot
ggplot()+
  #geom_point(data=data_sratio,aes(x=SIZE_BIN,y=s12))+
  geom_point(data=data_sratio_summary,aes(x=SIZE_BIN,y=mean,group=SIZE_BIN))+
  geom_point(data=data_sratio_summary,aes(x=SIZE_BIN,y=p90,group=SIZE_BIN),color='red')+
  geom_point(data=data_sratio_summary,aes(x=SIZE_BIN,y=p10,group=SIZE_BIN),color='blue')+
  facet_wrap(~scientific_name)


p<-
ggplot(data_sratio_summary, aes(x = SIZE_BIN, y = mean)) +
  geom_hline(yintercept = 1,linetype='dashed',alpha=0.5)+
  
  # Add ribbon for uncertainty between p10 and p90
  geom_ribbon(aes(ymin = p10, ymax = p90),fill='grey30', alpha = 0.3) +
  # Add smoothed line over mean values
  #geom_smooth(se = FALSE, color = "black") +
  
  # Optionally, show the original mean points
  geom_line() +
  
  # Facet by species
  facet_wrap(~scientific_name,nrow=2,scales='free_x') +
  scale_y_continuous(limits = c(0,5.5))+
  # Optional theme
  theme_bw() +
  theme(
    strip.text = element_text(size = 12),
    strip.background = element_blank(),
    text = element_text(size = 12)
  ) +
  labs(x = "length (cm)", y = "SR")


#save plot
ragg::agg_png(paste0('./figures slope/sr2.png'), width = 10, height = 5, units = "in", res = 300)
#ragg::agg_png(paste0('./figures/ms_hist_indices_cv_box_EBSNBS_suppl.png'), width = 13, height = 8, units = "in", res = 300)
p
dev.off()


#plot
#ggplot()+
#  #geom_point(data=data_sratio,aes(x=SIZE_BIN,y=s12))+
#  geom_point(data=data_sratio1,aes(x=SIZE_BIN,y=s12,group=SIZE_BIN))+
#  facet_wrap(~scientific_name)

#to convert cm to mm
data_sratio_summary$LENGTH<-data_sratio_summary$SIZE_BIN*10

#data input 
coef_wl<-expand.grid('spp'=spp_code1$scientific_name,
                     'sex'=c('1','2','all'),
                     'year'=as.character(c(2002,2004,2008,2010,2012,2016)))

#to add values parms
coef_wl$log_a<-NA
coef_wl$b<-NA

for (r in 1:nrow(coef_wl)) {
  
  #r<-1
  
  #species
  sp<-coef_wl[r,'spp']
  sp_code<-spp_code1[which(spp_code1$scientific_name==sp),'species_code']
  
  #sex
  sex<-coef_wl[r,'sex']
  sex<-ifelse(sex=='1',1,
              ifelse(sex=='2',2,c(1,2)))
  
  #year
  y<-coef_wl[r,'year']
  
  #hauls per year
  hauls<-subset(hauls_ebs,YEAR %in% y)[,'HAULJOIN']
  
  #data weight-length
  data<-subset(data_length$specimen,
               SPECIES_CODE %in% sp_code  & 
                 HAULJOIN %in% hauls  &
                 SEX %in% sex)
  
  #remove NAs and zeros
  data<-data[complete.cases(data),]
  data<-subset(data,LENGTH != 0 & WEIGHT!=0)
  
  #jump if no more than 3 obs
  if (nrow(data)<3) {
    next
  }
  
  #fit lm log space
  m <- stats::lm(log(WEIGHT) ~ log(LENGTH), data = data)
  pars <- as.list(coef(m))
  pars <- stats::setNames(pars, c("log_a", "b"))
  
  #store parms
  coef_wl[r,'log_a']<-pars$log_a
  coef_wl[r,'b']<- pars$b 
  
}

#add coeff of Aleutian skate
lwgoa<-read.csv('Data/data_raw/LenWtParams_GOA.csv')
lwai<-read.csv('Data/data_raw/LenWtParams_AI.csv')
ilwgoa<-lwgoa[which(lwgoa$species=='472'),]
ilwai<-lwai[which(lwai$species=='472'),]

#rbind skate
coef_wl<-
rbind(coef_wl,
      data.frame('spp'=c('Bathyraja aleutica','Bathyraja aleutica'),
                 'sex'=c('all','all'),
                 'year'=c('2015','2010'),
                 'log_a'=c(log(ilwgoa$alpha.kg),log(ilwai$alpha.kg)),
                 'b'=c(ilwgoa$beta,ilwai$beta)))

#remove species with no coeffs
sp_data<-unique(coef_wl[complete.cases(coef_wl),'spp'])
sp_rem<-setdiff(spp_code1$scientific_name,sp_data)

#get cruisejoin for the slope
head(data_length$cruise)
cruisejoin_bss<-subset(data_length$cruise,SURVEY=='BSS')[,'CRUISEJOIN']

#get hauls for the ebs
hauls_bss<-subset(data_length$haul,CRUISEJOIN %in% cruisejoin_bss)

#convert time
time_axis <- as.POSIXct(hauls_bss$START_TIME, origin = "1900-01-01", tz = "GMT") 
hauls_bss$YEAR <- format(time_axis, "%Y")

#length data slope
length_bss<-data_length$size[which(data_length$size$HAULJOIN %in% unique(hauls_bss$HAULJOIN)),]
length_bss<-merge(length_bss,hauls_bss[,c('CRUISEJOIN','HAULJOIN','YEAR')],by=c('CRUISEJOIN','HAULJOIN'))
length_bss<-merge(length_bss,spp_code1,by.x='SPECIES_CODE',by.y='species_code')

#only for species with coeffs
length_bss<-length_bss[which(length_bss$scientific_name %in% sp_data),]

#add weight
length_bss$WEIGHT<-NA

#add weight
length_bss$SR<-NA
length_bss$SR90<-NA
length_bss$SR10<-NA

#data s12
data_sratio2<-data_sratio_summary[,c('mean','p90','p10','scientific_name','LENGTH')]

#loop over combinations
for (r in 1:nrow(length_bss)) {
  
  #r<-21000
  cat(paste('#',r,'-',nrow(length_bss),'\n'))
  
  #length
  l<-length_bss[r,'LENGTH']
  
  #year
  y<-length_bss[r,'YEAR']
  
  #sex
  sex<-as.character(length_bss[r,'SEX'])
  sex<-ifelse(sex=='3','all',sex)
  
  #spp
  sp<-length_bss[r,'scientific_name']
  
  #filter
  coef_wl1<-coef_wl[which(coef_wl$spp==sp & coef_wl$sex==sex & coef_wl$year==y),]
  data_sratio3<-data_sratio2[which(data_sratio2$scientific_name==sp),]
  
  #if no data, get the average
  if (all(is.na(coef_wl1$log_a))) {
    coef_wl1<-coef_wl[which(coef_wl$spp==sp & coef_wl$sex==sex ),]
    
    if (all(is.na(coef_wl1$log_a))) {
      coef_wl1<-coef_wl[which(coef_wl$spp==sp ),]
    }
    
    coef_wl1<-data.frame('spp'=sp,'sex'=sex,'year'=NA,'log_a'=mean(coef_wl1$log_a,na.rm=TRUE),'b'=mean(coef_wl1$b,na.rm=TRUE))
  }
  
  #convert length to weight  
  length_bss[r,'WEIGHT'] <- exp(coef_wl1$log_a + coef_wl1$b * log(l))
  
  if (sp %in% unique(data_sratio3$scientific_name) & l %in% data_sratio3$LENGTH) {
    sr<-data_sratio3[which(data_sratio3$scientific_name==sp & data_sratio3$LENGTH==l),'mean']
    sr90<-data_sratio3[which(data_sratio3$scientific_name==sp & data_sratio3$LENGTH==l),'p90']
    sr10<-data_sratio3[which(data_sratio3$scientific_name==sp & data_sratio3$LENGTH==l),'p10']
    length_bss[r,'SR'] <-sr
    length_bss[r,'SR90'] <-sr90
    length_bss[r,'SR10'] <-sr10
  } else if (sp %in% unique(data_sratio3$scientific_name) & (l+5) %in% data_sratio3$LENGTH) { #some species have different bins (X5 instead of X0)
    sr<-data_sratio3[which(data_sratio3$scientific_name==sp & data_sratio3$LENGTH==(l+5)),'mean']
    sr90<-data_sratio3[which(data_sratio3$scientific_name==sp & data_sratio3$LENGTH==(l+5)),'p90']
    sr10<-data_sratio3[which(data_sratio3$scientific_name==sp & data_sratio3$LENGTH==(l+5)),'p10']
    length_bss[r,'SR'] <-sr
    length_bss[r,'SR90'] <-sr90
    length_bss[r,'SR10'] <-sr10
  } else if (sp %in% unique(data_sratio3$scientific_name) & (l-5) %in% data_sratio3$LENGTH) { #some species have different bins (X5 instead of X0)
    sr<-data_sratio3[which(data_sratio3$scientific_name==sp & data_sratio3$LENGTH==(l-5)),'mean']
    sr90<-data_sratio3[which(data_sratio3$scientific_name==sp & data_sratio3$LENGTH==(l-5)),'p90']
    sr10<-data_sratio3[which(data_sratio3$scientific_name==sp & data_sratio3$LENGTH==(l-5)),'p10']
    length_bss[r,'SR'] <-sr
    length_bss[r,'SR90'] <-sr90
    length_bss[r,'SR10'] <-sr10
  } else if (sp %in% unique(data_sratio3$scientific_name) & l < min(data_sratio3$LENGTH)) { #if smaller than min, then get SR of min length
    sr<-data_sratio3[which(data_sratio3$scientific_name==sp & data_sratio3$LENGTH==min(data_sratio3$LENGTH)),'mean']
    sr90<-data_sratio3[which(data_sratio3$scientific_name==sp & data_sratio3$LENGTH==min(data_sratio3$LENGTH)),'p90']
    sr10<-data_sratio3[which(data_sratio3$scientific_name==sp & data_sratio3$LENGTH==min(data_sratio3$LENGTH)),'p10']
    length_bss[r,'SR'] <-sr
    length_bss[r,'SR90'] <-sr90
    length_bss[r,'SR10'] <-sr10
  } else if (sp %in% unique(data_sratio3$scientific_name) & l > max(data_sratio3$LENGTH)) { #if bigger than min, then get SR of max length
    sr<-data_sratio3[which(data_sratio3$scientific_name==sp & data_sratio3$LENGTH==max(data_sratio3$LENGTH)),'mean']
    sr90<-data_sratio3[which(data_sratio3$scientific_name==sp & data_sratio3$LENGTH==max(data_sratio3$LENGTH)),'p90']
    sr10<-data_sratio3[which(data_sratio3$scientific_name==sp & data_sratio3$LENGTH==max(data_sratio3$LENGTH)),'p10']
    length_bss[r,'SR'] <-sr
    length_bss[r,'SR90'] <-sr90
    length_bss[r,'SR10'] <-sr10
  } else {
    next
  }
}

#check NAs in SR
length_bss[is.na(length_bss$SR),]
unique(length_bss[is.na(length_bss$SR),'scientific_name'])

#get subsample info
freq_subsamp<-aggregate(FREQUENCY ~ HAULJOIN + SPECIES_CODE,length_bss,FUN=sum)
freq_subsamp1<-merge(data_length$catch,freq_subsamp,by=c('SPECIES_CODE','HAULJOIN'))
names(freq_subsamp1)[5]<-'FREQ_SUBSAMP'

#merge total haul
length_bss<-merge(length_bss,freq_subsamp1[,c(1,2,4,5)],by=c('HAULJOIN','SPECIES_CODE'))

#expanded frequency
length_bss$FREQ_EXP<-length_bss$FREQUENCY*length_bss$NUMBER_FISH/length_bss$FREQ_SUBSAMP

#Adjusted frequency (frequency * SR)
length_bss$FREQ_ADJ<-length_bss$FREQ_EXP/length_bss$SR
length_bss$FREQ_ADJ90<-length_bss$FREQ_EXP/length_bss$SR90
length_bss$FREQ_ADJ10<-length_bss$FREQ_EXP/length_bss$SR10
length_bss$WEIGHT_FREQ<-length_bss$WEIGHT*length_bss$FREQ_EXP

#Adjusted frequency over weight to get adjusted WEIGHT
length_bss$ADJ_WEIGHT_FREQ<-length_bss$FREQ_ADJ*length_bss$WEIGHT
length_bss$ADJ_WEIGHT_FREQ90<-length_bss$FREQ_ADJ90*length_bss$WEIGHT
length_bss$ADJ_WEIGHT_FREQ10<-length_bss$FREQ_ADJ10*length_bss$WEIGHT

# Filter out non-finite values
length_bss_clean <- length_bss[is.finite(length_bss$SR), ]

# Replot with cleaned data
ggplot() +
  geom_boxplot(data = length_bss_clean, aes(x = scientific_name, y = SR))

#mean values of species over lengths classes
meansr<-unique(length_bss_clean[,c("scientific_name",'LENGTH','SR')])
aggregate(SR ~ scientific_name,meansr,mean)

#weight by species for each haul
wl<-aggregate(cbind(WEIGHT_FREQ,ADJ_WEIGHT_FREQ,ADJ_WEIGHT_FREQ90,ADJ_WEIGHT_FREQ10) ~ scientific_name + YEAR + HAULJOIN,length_bss,FUN=sum)

#total per year
wl1<-aggregate(cbind(WEIGHT_FREQ,ADJ_WEIGHT_FREQ) ~ scientific_name + YEAR ,length_bss,FUN=sum)
wl2<-reshape2::melt(wl1)

#check how different these estimates are
ggplot()+
  geom_point(data=wl2,aes(x=YEAR,y=value/1000000,color=variable))+
  facet_wrap(~scientific_name,scales='free_y')+
  labs(y='total t',x='')+
  theme_minimal()+
  scale_color_discrete(labels=c('observed','adjusted'),name='estimates')

 #check data_catch slope
catch_bss<-data_length$catch[which(data_length$catch$HAULJOIN %in% unique(hauls_bss$HAULJOIN)),]
catch_bss<-merge(catch_bss,hauls_bss[,c('HAULJOIN','YEAR')],by=c('HAULJOIN'))
catch_bss<-merge(catch_bss,spp_code1,by.x='SPECIES_CODE',by.y='species_code')

#weigth by species for each haul
wc<-aggregate(WEIGHT ~ scientific_name + YEAR + HAULJOIN,catch_bss,FUN=sum)
wc$WEIGHT<-wc$WEIGHT*1000

#merge both to check how similar
merge(wc,wl,by=c('scientific_name','YEAR','HAULJOIN'))

#plot list
plots<-list()

#species with SR data
spp_vect<-c("Atheresthes evermanni","Atheresthes stomias",
            "Gadus chalcogrammus","Gadus macrocephalus",
            "Hippoglossoides elassodon","Reinhardtius hippoglossoides",'Bathyraja aleutica')

#loop over species
for (sp in spp_vect) {
  
  #species
  #sp<-'Reinhardtius hippoglossoides'
  #sp<-spp_vect[7]
  
  #add new estimates per haul
  #data_geostat<-readRDS(paste0('./data processed/species/',sp,'/data_geostat.rds'))
  data_geostat<-readRDS(paste0('Data/data_processed/',sp,'/data_geostat.rds'))
  data_geostat1<-subset(data_geostat,survey_name=='Eastern Bering Sea Slope Bottom Trawl Survey')
  #unique(data_geostat1$hauljoin)
  #unique(wl$HAULJOIN)
  
  #weigth adjusted SR
  wl1<-subset(wl,scientific_name==sp)[,c('scientific_name' ,'HAULJOIN' , 'ADJ_WEIGHT_FREQ', 'ADJ_WEIGHT_FREQ90', 'ADJ_WEIGHT_FREQ10')]
  
  #merge
  names(data_geostat1);names(wl1)
  data_geostat2<-merge(data_geostat1,wl1,by.x=c('hauljoin','scientific_name'),by.y=c('HAULJOIN','scientific_name'),all.x=TRUE)
  data_geostat2$ADJ_WEIGHT_FREQ[is.na(data_geostat2$ADJ_WEIGHT_FREQ)] <- 0
  
  #convert grams to kg/ha
  data_geostat2$ADJ_KG_HA<-data_geostat2$ADJ_WEIGHT_FREQ/data_geostat2$effort/1000
  data_geostat2$ADJ_KG_HA90<-data_geostat2$ADJ_WEIGHT_FREQ90/data_geostat2$effort/1000
  data_geostat2$ADJ_KG_HA10<-data_geostat2$ADJ_WEIGHT_FREQ10/data_geostat2$effort/1000
  
  #if bathyraja because of adjustments
  if (sp=='Bathyraja aleutica') {
    data_geostat2$ADJ_KG_HA<-data_geostat2$ADJ_KG_HA/1000
    data_geostat2$ADJ_KG_HA90<-data_geostat2$ADJ_KG_HA90/1000
    data_geostat2$ADJ_KG_HA10<-data_geostat2$ADJ_KG_HA10/1000
    
  }
  
  #save data
  #saveRDS(data_geostat2,paste0('Data/data_processed/',sp,'/data_geostat_slope_adj.rds'))
  
  #remove high value on greenland turbot for visualization purposes
  if (sp=='Reinhardtius hippoglossoides') {
    data_geostat2<-subset(data_geostat2,cpue_kgha<700)
  }
  
  #plot
  p <- 
    ggplot() +
    geom_point(data = subset(data_geostat2, cpue_kgha != 0), aes(x = cpue_kgha, y = ADJ_KG_HA)) +
    #scale_x_continuous(limits = c(0, max(data_geostat2$cpue_kgha) * 0.9)) +
    #scale_y_continuous(limits = c(0, max(data_geostat2$cpue_kgha) * 0.9)) +
    geom_smooth(data = subset(data_geostat2, cpue_kgha != 0), aes(x = cpue_kgha, y = ADJ_KG_HA), method = "lm", color = "grey", se = FALSE) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
    coord_cartesian(xlim = c(0, max(data_geostat2$cpue_kgha)), 
                    ylim = c(0, max(data_geostat2$cpue_kgha))) +
    theme_minimal() +
    labs(title = sp, x = 'observed CPUE kgha', y = 'adjusted CPUE kgha')
  
  plots[[sp]]<-p  
}

#multiplot of all species cpue ratio by haul
cowplot::plot_grid(plotlist = plots,nrow=2)

