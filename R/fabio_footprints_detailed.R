##############################################################################################
##  FABIO Detailed Footprints
##############################################################################################

library(Matrix)
library(tidyverse)
library(data.table)

rm(list=ls()); gc()

is.finite.data.frame <- function(x) do.call(cbind, lapply(x, is.finite))
agg <- function(x) { x <- as.matrix(x) %*% sapply(unique(colnames(x)),"==",colnames(x));  return(x) }


#-------------------------------------------------------------------------
# Make intitial settings
#-------------------------------------------------------------------------
# read region classification
regions <- fread(file="/mnt/nfs_fineprint/tmp/fabio/v2/regions.csv")
# read commodity classification
items <- fread(file="/mnt/nfs_fineprint/tmp/fabio/v2/items.csv")
nrreg <- nrow(regions)
nrcom <- nrow(items)
index <- data.table(code = rep(regions$code, each = nrcom),
                    iso3c = rep(regions$iso3c, each = nrcom),
                    country = rep(regions$name, each = nrcom),
                    comm_code = rep(items$comm_code, nrreg),
                    item = rep(items$item, nrreg),
                    group = rep(items$group, nrreg))

X <- readRDS(file=paste0("/mnt/nfs_fineprint/tmp/fabio/v2/losses/X.rds"))
Y <- readRDS(file=paste0("/mnt/nfs_fineprint/tmp/fabio/v2/losses/Y.rds"))
E <- readRDS(file=paste0("/mnt/nfs_fineprint/tmp/fabio/v2/E.rds"))

year <- 2013
Xi <- X[, as.character(year)]
Yi <- Y[[as.character(year)]]
Ei <- E[[as.character(year)]]

Y_codes <- data.frame(code = substr(colnames(Yi), 1, str_locate(colnames(Yi), "_")[,1]-1))
Y_codes$iso3c = regions$iso3c[match(Y_codes$code,regions$code)]
Y_codes$continent = regions$continent[match(Y_codes$iso3c,regions$iso3c)]
Y_codes$fd <- substr(colnames(Yi), str_locate(colnames(Yi), "_")[,1]+1, 100)

extensions <- colnames(Ei)[c(8,10:11)]
allocations = c("mass","value")
consumption_categories <- c("food","other","stock_addition","balancing")
allocation = "value"
country = "AUT"
extension = "landuse"
consumption = "food"


#-------------------------------------------------------------------------
# Prepare Multipliers
#-------------------------------------------------------------------------
ext <- as.vector(as.matrix(as.vector(Ei[, ..extension]) / as.vector(Xi)))
ext[!is.finite(ext)] <- 0
# ext[ext < 0] <- 0         # eliminate negative values
L <- readRDS(file=paste0("/mnt/nfs_fineprint/tmp/fabio/v2/losses/",year,"_L_value.rds"))
MP <- ext * L

#-------------------------------------------------------------------------
# Calculate detailed Footprints
#-------------------------------------------------------------------------
if(country=="EU27"){
  Y_country <- Yi[, (Y_codes$continent == "EU" & Y_codes$iso3c != "GBR"), with=FALSE]
  colnames(Y_country) <- Y_codes$fd[Y_codes$continent == "EU" & Y_codes$iso3c != "GBR"]
  Y_country <- agg(Y_country)
} else if(country=="EU"){
  Y_country <- Yi[, Y_codes$continent == "EU", with=FALSE]
  colnames(Y_country) <- Y_codes$fd[Y_codes$continent == "EU"]
  Y_country <- agg(Y_country)
} else {
  Y_country <- Yi[, Y_codes$iso3c == country]
  colnames(Y_country) <- Y_codes$fd[Y_codes$iso3c == country]
}


fp <- lapply(consumption_categories, function(x) {
  FP <- as.matrix(t(t(MP) * as.vector(as.matrix(Y_country[,x]))))
  colnames(FP) <- rownames(FP) <- paste0(index$iso3c, "_", index$item)
  results <- FP %>% 
    as_tibble() %>% 
    mutate(origin = paste0(index$iso3c, "_", index$item)) %>% 
    gather(index, value, -origin) %>% 
    mutate(country_origin = substr(origin,1,3)) %>% 
    mutate(item_origin = substr(origin,5,100)) %>% 
    mutate(country_final = substr(index,1,3)) %>% 
    mutate(item_final = substr(index,5,100)) %>% 
    select(-index, -origin) %>% 
    mutate(category = x) %>% 
    mutate(value = round(value)) %>% 
    filter(value != 0)
  
  results$group_origin <- items$comm_group[match(results$item_origin,items$item)]
  results$group_final <- items$comm_group[match(results$item_final,items$item)]
  results$continent_origin <- regions$continent[match(results$country_origin, regions$iso3c)]
  
  return(results[, c(9,2,7,3,4,8,5,6,1)])
})

fp <- rbindlist(fp)
fp <- fp %>% 
  spread(category, value, fill = 0)

write_csv(fp, "./output/FABIO_AUT_2013_landuse_value-alloc_detailed.csv")


data <- fp %>% 
  mutate(country_origin = ifelse(country_origin=="AUT", "Austria", "Others")) %>% 
  group_by(country_origin, item_origin, group_final, item_final) %>% 
  summarize(value = sum(balancing) + sum(food)) %>% 
  ungroup() %>% 
  rename(country = country_origin, crop = item_origin, product = group_final) %>% 
  mutate(product = ifelse(crop == item_final, "crop and products", product)) %>% 
  select(-item_final) %>% 
  group_by(country, crop, product) %>% 
  summarize(value = sum(value)) %>% 
  ungroup() %>% 
  filter(value != 0) %>% 
  mutate(value = round(value / 8451860 * 10000, 1)) %>% 
  spread(product, value, fill = 0)
view(data)
