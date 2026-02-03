####################################################################
####################################################################
##
##    Script #1 
##    Get raw data from bottom trawl survey EBS, NBS and slope
##    Plot in situ sea bottom temperature time series in the regions
##    Create data_geostat file to fit OM VAST 
##    sea bottom temperature is appended in the next script (#3)
##    Daniel Vilas (danielvilasgonzalez@gmail.com)
##    Lewis Barnett, Stan Kotwicki, Zack Oyafuso, Megsie Siple, Leah Zacher, 
##    Lukas Defilippo, Andre Punt
##
####################################################################
####################################################################

#libraries from cran to call or install/load
pack_cran <- c("FishStatsUtils", "lubridate")

#install pacman to use p_load function - call library and if not installed, then install
if (!('pacman' %in% installed.packages())) {
  install.packages("pacman")}

#load/install packages
pacman::p_load(pack_cran,character.only = TRUE)

#create folder
if (!dir.exists(paths = "output/")) dir.create(path = 'output/')
if (!dir.exists(paths = "figures/")) dir.create(path = 'figures/')
if (!dir.exists(paths = "data/data_processed/")) 
  dir.create('data/data_processed/')
if (!dir.exists(paths = "data/data_processed/species/")) 
  dir.create('data/data_processed/species/')

#range years of data
sta_y <- 1982
end_y <- 2022

#selected species
spp <- c('Limanda aspera',
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
         'Sebastes aleutianus',
         'Atheresthes evermanni',
         'Sebastes borealis',
         'Sebastolobus alascanus',
         'Glyptocephalus zachirus',
         'Bathyraja aleutica')

#####################################
# Get haul data (sampling stations)
#####################################
haul <- readRDS(file = 'data/data_raw/afsc_haul_raw_2023_2_21.rds')

#####################################
## Catch data
#####################################
catch <- readRDS(file = 'data/data_raw/afsc_catch_raw_2023_2_21.rds')

#combine rougheye and blackspotted by assigning all three the backspotted scientific name
catch$scientific_name[catch$common_name == 'rougheye and blackspotted rockfish unid.'] <- 'Sebastes melanostictus'
catch$scientific_name[catch$common_name == 'rougheye rockfish'] <- 'Sebastes melanostictus'

#filter by species
catch1 <- subset(x = catch, subset = scientific_name %in% spp)

#sum blackspotted rockfish and blackspotted rockfish unid
catch2 <- catch1[which(catch1$scientific_name == 'Sebastes melanostictus'), ]
catch21 <- aggregate(catch2[, c('cpue_kgha','cpue_kgkm2','weight_kg',
                                'cpue_noha','cpue_nokm2','count')], 
                     by = list('hauljoin'=catch2$hauljoin), FUN = sum)
catch3 <- 
  cbind('hauljoin' = catch21$hauljoin,
        'species_code' = unique(x = catch[
          which(catch$common_name == 'blackspotted rockfish'),'species_code'
        ]),
        catch21[,-1],
        'taxon_confidence' = 'Unassessed',
        'scientific_name' = 'Sebastes melanostictus',
        'common_name' = 'rougheye and blackspotted rockfish',
        'worms' = unique(x = catch[
          which(catch$common_name=='blackspotted rockfish'), 'worms'
        ]),
        'itis' = NA)
catch1 <- subset(x = catch1, 
                 subset = scientific_name != "Sebastes melanostictus")
catch1 <- rbind(catch1, catch3)

#replace rock sole unid with northern rock sole since assessment goes to 1982 and most of
#unidentified are northern rock sole except for the far southern boundary
catch1$scientific_name[catch1$scientific_name == 'Lepidopsetta sp.'] <- 
  'Lepidopsetta polyxystra'
catch1 <- subset(x = catch1, subset = common_name != "rock sole unid.")

#remove lumped species from spp vector
spp <- spp[spp != 'Sebastes aleutianus']
spp <- spp[spp != 'Lepidopsetta sp.']

#check 
length(unique(catch1$scientific_name))==length(spp)

#####################################
# Merge catch and haul dataframes
#####################################
#create the empty df 
haul1 <- do.call("rbind", replicate(length(x = spp), haul, simplify = FALSE))

#replicate spp for each station
spp1 <- rep(x = spp, each = nrow(x = haul))

#join dataframe
all <- data.frame(haul1, 'scientific_name' = spp1)

#merge haul and catch
all1 <- merge(all, catch1, all.x = T)

## Zero-fill these fields: cpue_kgha, cpue_kgkm2, cpue_noha, cpue_nokm2, 
## count, and weight_kg 
all1[
  c('cpue_kgha','cpue_kgkm2','cpue_noha','cpue_nokm2','count','weight_kg')
][
  is.na(x = all1[c('cpue_kgha','cpue_kgkm2','cpue_noha','cpue_nokm2',
                   'count','weight_kg')])
] <- 0

#####################################
# Create data_geostat file as input to fit OM ----
#####################################
#add year and month
all1$month <- lubridate::month(as.POSIXlt(all1$date, format="%d/%m/%Y"))
all1$year <- lubridate::year(as.POSIXlt(all1$date, format="%d/%m/%Y"))

#save data_geostat file for all species
saveRDS(object = all1, 
        file = 'data/data_processed/species/slope_shelf_EBS_NBS_data_geostat.rds')

#loop over species to create data_geostat df for each species
for (sp in spp) {
  
  #print species to check progress
  cat(paste("    -----", sp, "-----\n"))
  
  #create folder to store results
  dir.create(paste0('data/data_processed/species/', sp))
  
  #filter by sp
  all2 <- subset(x = all1, 
                 subset = scientific_name == sp & year %in% sta_y:end_y)
  
  # Save
  cat(paste("    ----- ", nrow(all2) , "samples -----\n"))
  saveRDS(object = all2, 
          file = paste0('data/data_processed/species/', sp, 
                        '/data_geostat.rds'))
}
