##############################################################################################
##  FABIO Footprints
##############################################################################################

library(Matrix)
library(tidyverse)

rm(list=ls()); gc()

is.finite.data.frame <- function(x) do.call(cbind, lapply(x, is.finite))
agg <- function(x) { x <- as.matrix(x) %*% sapply(unique(colnames(x)),"==",colnames(x));  return(x) }


#-------------------------------------------------------------------------
# Make intitial settings
#-------------------------------------------------------------------------
# read region classification
regions <- read.csv(file="./input/fabio_countries.csv", header=TRUE)
# read commodity classification
items <- read.csv(file="./input/fabio_items.csv", header=TRUE)
nrreg <- nrow(regions)
nrcom <- nrow(items)
index <- data.frame(ISO = rep(regions$ISO, each = nrcom),
                    country = rep(regions$Country, each = nrcom),
                    item = rep(items$Item, nrreg),
                    group = rep(items$Group, nrreg))


allocation <- c("mass","value")[1]
year <- 2012

#-------------------------------------------------------------------------
# Read data
#-------------------------------------------------------------------------
if(allocation=="mass") L <- readRDS(file=paste0("/mnt/nfs_fineprint/tmp/fabio/",year,"_L_mass.rds"))
if(allocation=="value") L <- readRDS(file=paste0("/mnt/nfs_fineprint/tmp/fabio/",year,"_L_price.rds"))

X <- readRDS(file=paste0("/mnt/nfs_fineprint/tmp/fabio/",year,"_X.rds"))
Y <- readRDS(file=paste0("/mnt/nfs_fineprint/tmp/fabio/",year,"_Y.rds"))
E <- readRDS(file=paste0("/mnt/nfs_fineprint/tmp/fabio/",year,"_E.rds"))

grazing <- read.csv("input/grazing.csv")
E$Landuse[E$Item.Code==2001] <- E$Biomass[E$Item.Code==2001] / grazing$t_per_ha

Y_codes <- data.frame(ISO = substr(colnames(Y),1,3))
Y_codes$Continent = regions$EU27[match(Y_codes$ISO,regions$ISO)]
Y_codes$FD <- substr(colnames(Y),5,100)


footprint <- function(country = "EU", extension = "Landuse", consumption = "Food", allocation = "value"){
  #-------------------------------------------------------------------------
  # Prepare Multipliers
  #-------------------------------------------------------------------------
  ext <- as.vector(E[,extension]) / X
  ext[!is.finite(ext)] <- 0
  # ext[ext < 0] <- 0         # eliminate negative values
  MP <- ext * L
  
  #-------------------------------------------------------------------------
  # Calculate detailed Footprints
  #-------------------------------------------------------------------------
  if(country=="EU"){
    Y_country <- Y[,Y_codes$Continent == "EU"]
    colnames(Y_country) <- Y_codes$FD[Y_codes$Continent == "EU"]
    Y_country <- agg(Y_country)
  } else {
    Y_country <- Y[,Y_codes$ISO == country]
    colnames(Y_country) <- Y_codes$FD[Y_codes$ISO == country]
  }
  FP <- t(t(MP) * Y_country[,consumption])
  colnames(FP) <- rownames(FP) <- paste0(index$ISO, "_", index$item)
  results <- FP %>% 
    as_tibble() %>% 
    mutate(origin = paste0(index$ISO, "_", index$item)) %>% 
    gather(index, value, -origin) %>% 
    mutate(country_origin = substr(origin,1,3)) %>% 
    mutate(item_origin = substr(origin,5,100)) %>% 
    mutate(country_target = substr(index,1,3)) %>% 
    mutate(final_product = substr(index,5,100)) %>% 
    select(-index, -origin) %>% 
    filter(value != 0)
  
  results$group_origin <- items$Com.Group[match(results$item_origin,items$Item)]
  results$final_product_group <- items$Com.Group[match(results$final_product,items$Item)]
  
  data.table::fwrite(results, file=paste0("./output/FABIO_",country,"_",extension,"_",consumption,"_",allocation,"-alloc.csv"), sep=",")
  
  # data <- results %>% 
  #   group_by(final_product,item_origin,country_origin) %>% 
  #   summarise(value = round(sum(value))) %>% 
  #   filter(value != 0)
  # data.table::fwrite(data, file=paste0("./output/FABIO_",country,"_",extension,"_",consumption,"_",allocation,"-alloc_summary.csv"), sep=",")
  
  data <- results %>% 
    group_by(final_product, group_origin) %>% 
    filter(value != 0) %>% 
    summarise(value = round(sum(value))) %>% 
    spread(group_origin, value, fill = 0)
  data.table::fwrite(data, file=paste0("./output/FABIO_",country,"_",extension,"_",consumption,"_",allocation,"-alloc_summary.csv"), sep=",")
  
  return(data)
}


#-------------------------------------------------------------------------
# Calculate detailed footprints
#-------------------------------------------------------------------------
extensions <- colnames(E)[7:10]
consumption_categories <- c("Food","OtherUses","StockVariation","Balancing")
countries <- c("USA","CAN","AUS","EU")

country <- "EU"
# calculate footprints
# for(country in countries){
  for(extension in extensions[-2]){
    for(consumption in consumption_categories){
      data <- footprint(country = country, extension = extension, consumption = consumption, allocation = allocation)
    }
  }
# }




#-------------------------------------------------------------------------
# Calculate aggregate footprints
#-------------------------------------------------------------------------
footprint <- function(region = character(), Y = matrix(), year=integer(), MP = matrix(), type=character()){
  FP <- Y[,1:2] * colSums(MP)
  FP[FP<0] <- 0     # filter negative values
  FP[rep(items$Item,nrreg)=="Cocoa Beans and products",][FP[rep(items$Item,nrreg)=="Cocoa Beans and products",] > 
                                                           mean(FP)*1000] <- 0   # filter outliers
  FP <- t(FP)
  colnames(FP) <- rep(c(rep("crop",96),rep("lvst",130-96)), nrreg)
  FP <- agg(FP)
  FP <- reshape2::melt(FP)
  FP$year <- year
  FP$region <- region
  FP$type <- type
  return(FP)
}
results <- rbind(results, footprint(region = "EU", Y = Y_EU, year = year, MP = MP, allocation))
results <- rbind(results, footprint(region = "EU", Y = Y_EU, year = year, MP = MP, allocation))

imports <- rbind(imports, footprint(region = "EU", Y = Y_EU, year = year, MP = MP[rep(regions$Continent, each = nrow(items)) != "EU", ], allocation))
imports <- rbind(imports, footprint(region = "EU", Y = Y_EU, year = year, MP = MP[rep(regions$Continent, each = nrow(items)) != "EU", ], allocation))

data.table::fwrite(results, file="./results/FABIO_land_results.csv", sep=",")
data.table::fwrite(imports, file="./results/FABIO_land_imports.csv", sep=",")




