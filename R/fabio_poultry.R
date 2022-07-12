##############################################################################################
##  FABIO Poultry checks
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
Z <- readRDS(file=paste0("/mnt/nfs_fineprint/tmp/fabio/v2/losses/Z_mass.rds"))

year <- 2013
Xi <- X[, as.character(year)]
Yi <- Y[[as.character(year)]]
Ei <- E[[as.character(year)]]
Zi <- Z[[as.character(year)]]

Y_codes <- data.frame(code = substr(colnames(Yi), 1, str_locate(colnames(Yi), "_")[,1]-1))
Y_codes$iso3c = regions$iso3c[match(Y_codes$code,regions$code)]
Y_codes$continent = regions$continent[match(Y_codes$iso3c,regions$iso3c)]
Y_codes$fd <- substr(colnames(Yi), str_locate(colnames(Yi), "_")[,1]+1, 100)

data <- as.matrix(Zi[, index$item=="Poultry Birds"])
colnames(data) <- regions$iso3c
data <- cbind(index, data)

data <- data %>% 
  gather(key = "iso", "value", -code, -iso3c, -country, -comm_code, -item, -group) %>% 
  group_by(iso) %>% 
  summarize(value = sum(value)) %>% 
  ungroup()

regions$poultry_feed <- data$value[match(regions$iso3c, data$iso)]

data <- rowSums(as.matrix(Zi[index$item=="Poultry Birds",])) + rowSums(as.matrix(Yi[index$item=="Poultry Birds",]))
regions$poultry_production <- as.numeric(data)

data <- colSums(as.matrix(Zi[, index$item=="Poultry Meat"]))
regions$poultry_slaughtering <- as.numeric(data)

data <- rowSums(as.matrix(Zi[index$item=="Poultry Meat",])) + rowSums(as.matrix(Yi[index$item=="Poultry Meat",]))
regions$poultry_meat <- as.numeric(data)
