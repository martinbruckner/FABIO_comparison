##############################################################################################
##  FABIO Footprints
##############################################################################################

library(Matrix)
library(tidyverse)

rm(list=ls()); gc()

is.finite.data.frame <- function(x) do.call(cbind, lapply(x, is.finite))
agg <- function(x) { x <- as.matrix(x) %*% sapply(unique(colnames(x)),"==",colnames(x));  return(x) }

year <- 2012

#-------------------------------------------------------------------------
# Read data
#-------------------------------------------------------------------------
load(file=paste0("W:/WU/Projekte/GRU/04_Daten/MRIO/IO data/EXIOBASE/EXIOBASE 3.6/parsed/pxp/",year,"_L.RData"))
load(file=paste0("W:/WU/Projekte/GRU/04_Daten/MRIO/IO data/EXIOBASE/EXIOBASE 3.6/parsed/pxp/",year,"_x.RData"))
load(file=paste0("W:/WU/Projekte/GRU/04_Daten/MRIO/IO data/EXIOBASE/EXIOBASE 3.6/parsed/pxp/",year,"_Y.RData"))
load(file=paste0("W:/WU/Projekte/GRU/04_Daten/MRIO/IO data/EXIOBASE/EXIOBASE 3.6/parsed/pxp/",year,"_E.RData"))
load(file=paste0("W:/WU/Projekte/GRU/04_Daten/MRIO/IO data/EXIOBASE/EXIOBASE 3.6/parsed/pxp/",year,"_F_hh.RData"))

load(file="W:/WU/Projekte/GRU/04_Daten/MRIO/IO data/EXIOBASE/EXIOBASE 3.6/parsed/Y.codes.RData")
load(file="W:/WU/Projekte/GRU/04_Daten/MRIO/IO data/EXIOBASE/EXIOBASE 3.6/parsed/Q.codes.RData")
load(file="W:/WU/Projekte/GRU/04_Daten/MRIO/IO data/EXIOBASE/EXIOBASE 3.6/parsed/pxp/IO.codes.RData")


footprint <- function(country = "EU", extension = "Forest area - Forestry"){
  #-------------------------------------------------------------------------
  # Prepare Multipliers
  #-------------------------------------------------------------------------
  ext <- as.vector(E[Q.codes$Stressor==extension,]) / x
  ext[!is.finite(ext)] <- 0
  MP <- ext * L
  #-------------------------------------------------------------------------
  # Prepare Final Demand
  #-------------------------------------------------------------------------
  if(country=="EU"){
    Y_country <- Y[,Y.codes$`Region Name` %in% unique(Y.codes$`Region Name`)[1:28]]
    colnames(Y_country) <- Y.codes$`Final Demand Category`[Y.codes$`Region Name` %in% unique(Y.codes$`Region Name`)[1:28]]
    Y_country <- agg(Y_country)
  } else {
    Y_country <- Y[,Y.codes$`Region Name` == country]
    colnames(Y_country) <- Y.codes$`Final Demand Category`[Y.codes$`Region Name` == country]
  }
  #-------------------------------------------------------------------------
  # Prepare F_hh
  #-------------------------------------------------------------------------
  if(country=="EU"){ F_hh_country <- F_hh[Q.codes$Stressor==extension,Y.codes$`Region Name` %in% unique(Y.codes$`Region Name`)[1:28]]
  } else { F_hh_country <- F_hh[Q.codes$Stressor==extension,Y.codes$`Region Name` == country] }
  F_hh_country <- sum(F_hh_country)
  
  if(country=="EU"){ F_hh_country_marginal <- F_hh[Q.codes$Stressor=="Forest area - Marginal use",Y.codes$`Region Name` %in% unique(Y.codes$`Region Name`)[1:28]]
  } else { F_hh_country_marginal <- F_hh[Q.codes$Stressor=="Forest area - Marginal use",Y.codes$`Region Name` == country] }
  F_hh_country_marginal <- sum(F_hh_country_marginal)
  
  #-------------------------------------------------------------------------
  # Calculate detailed Footprints
  #-------------------------------------------------------------------------
  FP <- t(t(MP) * rowSums(Y_country))
  colnames(FP) <- rownames(FP) <- paste0(IO.codes$Country.Code, "_", IO.codes$Product.Name)
  
  results <- FP %>%
    as.matrix() %>% 
    as_tibble() %>% 
    mutate(country_origin = IO.codes$Country.Code) %>% 
    gather(target, value, -country_origin) %>% 
    mutate(item_target = substr(target,4,100)) %>% 
    select(-target) %>% 
    group_by(item_target,country_origin) %>% 
    summarise(value = round(sum(value))) %>% 
    ungroup() %>% 
    add_row(item_target = "Households", country_origin = country, value = F_hh_country) %>% 
    add_row(item_target = "Households - marginal use", country_origin = country, value = F_hh_country_marginal) %>% 
    filter(value != 0)
  
  data.table::fwrite(results, file=paste0("./results/EXIOBASE_",country,"_Forest area.csv"), sep=",")
  
  return(data)
}


#-------------------------------------------------------------------------
# Calculate detailed footprints
#-------------------------------------------------------------------------
extensions <- c("Forest area - Forestry","Forest area - Marginal use")
consumption_categories <- Y.codes$`Final Demand Category`[1:7]
countries <- c("US","CA","AU","EU")

# calculate footprints
for(country in countries){
  data <- footprint(country = country, extension = extensions[1])
}



