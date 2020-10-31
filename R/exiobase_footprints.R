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
load(file=paste0("/mnt/nfs_fineprint/tmp/exiobase/pxp/",year,"_L.RData"))
load(file=paste0("/mnt/nfs_fineprint/tmp/exiobase/pxp/",year,"_x.RData"))
load(file=paste0("/mnt/nfs_fineprint/tmp/exiobase/pxp/",year,"_Y.RData"))
load(file=paste0("/mnt/nfs_fineprint/tmp/exiobase/pxp/",year,"_E.RData"))
load(file=paste0("/mnt/nfs_fineprint/tmp/exiobase/pxp/",year,"_F_hh.RData"))

load(file="/mnt/nfs_fineprint/tmp/exiobase/Y.codes.RData")
load(file="/mnt/nfs_fineprint/tmp/exiobase/Q.codes.RData")
load(file="/mnt/nfs_fineprint/tmp/exiobase/pxp/IO.codes.RData")


footprint <- function(country = "EU", extension = "cropland", extensions = extensions){
  #-------------------------------------------------------------------------
  # Prepare Multipliers
  #-------------------------------------------------------------------------
  # ext <- as.vector(E[Q.codes$Stressor==extension,]) / x
  ext <- as.vector(colSums(E[Q.codes$Index %in% extensions[[extension]],])) / x
  ext[!is.finite(ext)] <- 0
  MP <- ext * L
  #-------------------------------------------------------------------------
  # Prepare Final Demand
  #-------------------------------------------------------------------------
  if(country=="EU"){
    Y_country <- Y[,Y.codes$`Region Name` %in% unique(Y.codes$`Region Name`)[1:28]]
    colnames(Y_country) <- Y.codes$`Final Demand Category`[Y.codes$`Region Name` %in% unique(Y.codes$`Region Name`)[1:28]]
    Y_country <- agg(Y_country)
  } else if(country=="EU27"){
    Y_country <- Y[,Y.codes$`Region Name` %in% unique(Y.codes$`Region Name`)[1:27]]
    colnames(Y_country) <- Y.codes$`Final Demand Category`[Y.codes$`Region Name` %in% unique(Y.codes$`Region Name`)[1:27]]
    Y_country <- agg(Y_country)
  } else {
    Y_country <- Y[,Y.codes$`Region Name` == country]
    colnames(Y_country) <- Y.codes$`Final Demand Category`[Y.codes$`Region Name` == country]
  }
  # #-------------------------------------------------------------------------
  # # Prepare F_hh
  # #-------------------------------------------------------------------------
  # if(country=="EU"){ F_hh_country <- F_hh[Q.codes$Index %in% extensions[[extension]],Y.codes$`Region Name` %in% unique(Y.codes$`Region Name`)[1:28]]
  # } else if(country=="EU27"){ F_hh_country <- F_hh[Q.codes$Index %in% extensions[[extension]],Y.codes$`Region Name` %in% unique(Y.codes$`Region Name`)[1:27]]
  # } else { F_hh_country <- F_hh[Q.codes$Index %in% extensions[[extension]],Y.codes$`Region Name` == country] }
  # F_hh_country <- sum(F_hh_country)
  # 
  # if(country=="EU"){ F_hh_country_marginal <- F_hh[Q.codes$Stressor=="Forest area - Marginal use",Y.codes$`Region Name` %in% unique(Y.codes$`Region Name`)[1:28]]
  # } else { F_hh_country_marginal <- F_hh[Q.codes$Stressor=="Forest area - Marginal use",Y.codes$`Region Name` == country] }
  # F_hh_country_marginal <- sum(F_hh_country_marginal)
  
  #-------------------------------------------------------------------------
  # Calculate detailed Footprints
  #-------------------------------------------------------------------------
  FP <- t(t(MP) * rowSums(Y_country))
  colnames(FP) <- rownames(FP) <- paste0(IO.codes$Country.Code, "_", IO.codes$Product.Name)
  
  results <- FP %>%
    as.matrix() %>% 
    as_tibble() %>% 
    mutate(origin = paste0(IO.codes$Country.Code, "_", IO.codes$Product.Name)) %>% 
    gather(index, value, -origin) %>%
    mutate(country_origin = substr(origin,1,2)) %>% 
    mutate(item_origin = substr(origin,4,100)) %>% 
    mutate(country_target = substr(index,1,2)) %>% 
    mutate(final_product = substr(index,4,100)) %>% 
    select(-index, -origin) %>% 
    filter(value != 0)
  
  results$group_origin <- IO.codes$Sector.Group[match(results$item_origin, IO.codes$Product.Name)]
  results$group_target <- IO.codes$Sector.Group[match(results$final_product, IO.codes$Product.Name)]
  results$continent_origin <- IO.codes$Region.Code[match(results$country_origin, IO.codes$Country.Code)]
  results$continent_origin[results$country_origin==country] <- country
  
  data <- results %>%
    group_by(final_product, group_origin, country_origin, continent_origin) %>%
    summarise(value = round(sum(value))) %>%
    filter(value != 0) %>%
    spread(group_origin, value, fill = 0)
  fwrite(data, file=paste0("./output/EXIOBASE_",country,"_",year,"_",extension,"_detailed.csv"), sep=",")
  
  data <- results %>% 
    group_by(final_product, group_origin) %>% 
    summarise(value = round(sum(value))) %>% 
    filter(value != 0) %>% 
    spread(group_origin, value, fill = 0)
  data$index <- IO.codes$Index[IO.codes$Country.Code=="AT"][match(substr(data$final_product,1,50), substr(IO.codes$Product.Name[IO.codes$Country.Code=="AT"],1,50))]
  data <- data[,c(ncol(data),1:(ncol(data)-1))]
  data <- data[order(data$index),]
  fwrite(data, file=paste0("./output/EXIOBASE_",country,"_",year,"_",extension,"_products-inputs.csv"), sep=",")
  
  data <- results %>% 
    group_by(item_origin, continent_origin) %>% 
    filter(value != 0) %>% 
    summarise(value = round(sum(value))) %>% 
    spread(continent_origin, value, fill = 0)
  data.table::fwrite(data, file=paste0("./output/EXIOBASE_",country,"_",year,"_",extension,"_inputs-continent.csv"), sep=",")
  
  
    # add_row(final_product = "Households", country_origin = country, value = F_hh_country) %>% 
    # add_row(final_product = "Households - marginal use", country_origin = country, value = F_hh_country_marginal) %>% 
  
}


#-------------------------------------------------------------------------
# Calculate detailed footprints
#-------------------------------------------------------------------------
cropland <- 447:459
grazing <- 465:467
blue <- 1158:1182
green <- 1145:1157
extensions <- list(cropland, grazing, blue, green)
names(extensions) <- c("cropland", "grazing", "blue", "green")
consumption_categories <- Y.codes$`Final Demand Category`[1:7]
countries <- c("US","CA","AU","EU")

country="EU27"
# calculate footprints
for(country in countries){
  for(extension in names(extensions)){
    footprint(country = country, extension = extension, extensions = extensions)
  }
}



