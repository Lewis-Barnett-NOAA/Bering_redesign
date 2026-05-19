####################################################################
####################################################################
##
##    Get true index from the OM, prepare data for optimization
##    (using devtools::install_github(repo = "zoyafuso-NOAA/SamplingStrata"))
##    Daniel Vilas (daniel.vilas@noaa.gov/dvilasg@uw.edu)
##
####################################################################
####################################################################
rm(list = ls(all.names = TRUE))
gc()

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  Import data and constants
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Species List
species_list <- read.csv(file = "data/species_list.csv") |>
  subset(subset = SPECIES_CODE %in% 
           unique(x = sort(x = GROUP_CODE)))
n_spp <- nrow(x = species_list)

## classify cold and warm years between 2002 and 2016
cyrs <- c(2006:2013)
wyrs <- c(2002:2005, 2014:2016)
yrs <- sort(x = c(cyrs, wyrs))
n_yrs <- length(x = yrs)

## Model convergence checks
df_conv <- read.csv(file = "output/df_conv.csv")

## Interpolation grid with depth and sea bottom temperature
grid_bs_year <-  readRDS(file = "data/data_processed/grid_bs_year.RDS")
grid_bs <- readRDS(file = "data/data_processed/grid_bs.RDS")
n_cells <- length(x = unique(x = grid_bs$cell))

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  Collate predicted densities (D_gct) from each region/species VAST model
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
D_gct <- array(data = 0,
               dim = c(n_cells, n_spp, n_yrs ),
               dimnames = list(NULL, species_list$SCIENTIFIC_NAME, yrs))
for (iregion in c("bs_shelf", "bs_slope")) { ## Loop over regions -- start
  
  region_idx <- 
    grid_bs$Region %in% list("bs_slope" = "BSS", 
                             "bs_shelf" = c("EBS", "NBS"))[[iregion]]
  
  for (ispp in 1:nrow(x = df_conv)) { ## Loop over species -- start
    
    species_name <- df_conv$species_name[ispp]
    cat(paste("##################", iregion, species_name, "################\n"))
    
    if (df_conv[df_conv$species_name == species_name, iregion] == "converged") {
      fit <- readRDS(file = paste0("output/", iregion, "/vast/", species_name, 
                                   "/initial_fit.RDS"))
      
      D_gct[region_idx, species_name, ] <- fit$Report$D_gct[, 1, paste(yrs)] 
    }
    
  } ## Loop over species -- start
} ## Loop over regions -- end

saveRDS(object = D_gct, file = "output/D_gct.RDS")

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  Calculate variance of sea bottom temperature for each cell across
##  static: all years from 2002-2016; cold: "cold" years; and warm: "warm" years
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
optim_data_variables <- rbind(
  grid_bs |>
    transform(varTemp = with(subset(x = grid_bs_year, 
                                    subset = Year %in% yrs),
                             tapply(X = Temp, 
                                    INDEX = cell, 
                                    FUN = var)),
              regime = "static"),
  
  cbind(grid_bs, 
        varTemp = with(subset(x = grid_bs_year, 
                              subset = Year %in% cyrs),
                       tapply(X = Temp, 
                              INDEX = cell, 
                              FUN = var)),
        regime = "cold" ) ,
  
  cbind(grid_bs, 
        varTemp = with(subset(x = grid_bs_year, 
                              subset = Year %in% wyrs),
                       tapply(X = Temp, 
                              INDEX = cell, 
                              FUN = var)),
        regime = "warm" )
) |>
  ## Transform longitude so that it is positive
  transform(LonE = Lon + 360,
            Stratum = ifelse(test = Stratum == "NA",
                             yes = 999, no = Stratum))

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  Attach density data with the stratum variables
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
optim_data <- 
  cbind(
    optim_data_variables,
    rbind(
      ## Density data for the static scenario
      cbind(apply(X = D_gct[, , paste(yrs)],
                  MARGIN = 1:2,
                  FUN = function(x) sum(x, na.rm = TRUE)) |>
              as.data.frame() |>
              setNames(nm = paste0("Y", 1:n_spp)),
            apply(X = D_gct[, , paste(yrs)],
                  MARGIN = 1:2,
                  FUN = function(x) sum(x^2, na.rm = TRUE)) |>
              as.data.frame() |>
              setNames(nm = paste0("Y", 1:n_spp, "_SQ_SUM"))),
      
      ## Density data for the dynamic scenario: cold years
      cbind(apply(X = D_gct[, , paste(cyrs)],
                  MARGIN = 1:2,
                  FUN = function(x) sum(x, na.rm = TRUE)) |>
              as.data.frame() |>
              setNames(nm = paste0("Y", 1:n_spp)),
            apply(X = D_gct[, , paste(cyrs)],
                  MARGIN = 1:2,
                  FUN = function(x) sum(x^2, na.rm = TRUE)) |>
              as.data.frame() |>
              setNames(nm = paste0("Y", 1:n_spp, "_SQ_SUM"))),
      
      ## Density data for the dynamic scenario: warm years
      cbind(apply(X = D_gct[, , paste(wyrs)],
                  MARGIN = 1:2,
                  FUN = function(x) sum(x, na.rm = TRUE)) |>
              as.data.frame() |>
              setNames(nm = paste0("Y", 1:n_spp)),
            apply(X = D_gct[, , paste(wyrs)],
                  MARGIN = 1:2,
                  FUN = function(x) sum(x^2, na.rm = TRUE)) |>
              as.data.frame() |>
              setNames(nm = paste0("Y", 1:n_spp, "_SQ_SUM")))
    ) 
  )

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  Save
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
saveRDS(object = optim_data, file = "output/optim_data.RDS")
write.csv(x = optim_data, file = "output/optim_data.csv", row.names = FALSE)
