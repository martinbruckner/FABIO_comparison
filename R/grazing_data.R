library(tidyverse)
library(data.table)
# library(velox) # install from github https://github.com/hunzikp/velox -- devtools::install_github("hunzikp/velox")
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(raster)

# dir.create("./input", showWarnings = FALSE, recursive = TRUE)
# dir.create("./output", showWarnings = FALSE, recursive = TRUE)
# unzip("/mnt/nfs_fineprint/tmp/ssp585.zip", exdir = "./input/")
# unzip("/mnt/nfs_fineprint/tmp/historical.zip", exdir = "./input/")
# file_path_yields <- c(dir("./input/historical", full.names = TRUE), 
#                       dir("./input/ssp585", full.names = TRUE))

unzip("./input/grazing_water_use_20052009.zip", exdir = "./input/")

# # --------------------------------------------------------------------------------------
# # label crop types ---------------------------------------------------------------------
# #Soft White Wheat - swh
# #Western White Wheat - wwh
# crop_types <- c(ri1 = "rice", soy = "soy", mai = "maize", wwh = "western white wheat", swh = "soft white wheat")
# climate_scenarios <- c("historical", "ssp585")
# irrigation_system <- c("firr", "noirr")

# --------------------------------------------------------------------------------------
# get world map ------------------------------------------------------------------------
world <- rnaturalearth::ne_countries(scale = "small", returnclass = "sf") %>% 
  dplyr::select(COUNTRY_NAME = admin, ISO_A3 = iso_a3)

# --------------------------------------------------------------------------------------
# aggregate yields per coutry, crop type, and year -------------------------------------
f <- "./input/grazing_water_use_20052009.zip"
col_names <- stringr::str_match(f, "annual_\\s*(.*?)\\s*.nc")[,2] %>% 
  stringr::str_split("_") %>% 
  unlist() %>% 
  as.numeric() 
col_names <- c(col_names[1]:col_names[2]) %>%
  paste0("Y", .)
irrigation_system <- stringr::str_match(f, "_yield-\\s*(.*?)\\s*_global_annual")[,2] %>% 
  stringr::str_split("-") %>% 
  unlist() %>% 
  .[2]
crop_name <- stringr::str_match(f, "_yield-\\s*(.*?)\\s*_global_annual")[,2] %>% 
  stringr::str_split("-") %>% 
  unlist() %>% 
  .[1]
scenario <- stringr::str_match(f, "w5e5_\\s*(.*?)\\s*_2015soc")[,2] %>% 
  stringr::str_split("_") %>%
  unlist()

countries <- world %>% 
  sf::st_drop_geometry() %>% 
  tibble::as_tibble()

vx <- velox::velox(f)

yield_mean <- vx$extract(world, fun = function(x) mean(x, na.rm=TRUE) ) %>% 
  tibble::as_tibble(.name_repair = ~col_names) %>% 
  dplyr::bind_cols(countries) %>% 
  tidyr::gather("YEAR", "MEAN", -COUNTRY_NAME, -ISO_A3)

yield_sd <- vx$extract(world, fun = sd) %>% 
  tibble::as_tibble(.name_repair = ~col_names) %>% 
  dplyr::bind_cols(countries) %>% 
  tidyr::gather("YEAR", "SD", -COUNTRY_NAME, -ISO_A3)

yield_median <- vx$extract(world, fun = median) %>% 
  tibble::as_tibble(.name_repair = ~col_names) %>% 
  dplyr::bind_cols(countries) %>% 
  tidyr::gather("YEAR", "MEDIAN", -COUNTRY_NAME, -ISO_A3)

yields_summary <- dplyr::left_join(yield_mean, yield_sd) %>% 
  dplyr::left_join(yield_median) %>% 
  dplyr::mutate(YEAR = as.numeric(stringr::str_remove(YEAR, "Y")),
                CROP_NAME = crop_name, 
                SCENARIO = scenario, 
                IRRIGATION_SYSTEM = irrigation_system)  

  


readr::write_csv(country_level_yield, file = "./output/country_level_yields_summary.csv")

# --------------------------------------------------------------------------------------
# plot global yield maps ---------------------------------------------------------------
country_level_yield <- readr::read_csv("./output/country_level_yields_summary.csv")
map_ids <- expand.grid(names(crop_types), irrigation_system)
y <- 2010 # NOTE: that the year 2015 seems to have an issue 
var <- "MEAN"
for (k in 1:nrow(map_ids)){
  yields <- dplyr::filter(country_level_yield, 
                          CROP_NAME == map_ids[k,1],
                          YEAR == y, 
                          IRRIGATION_SYSTEM == map_ids[k,2])
  
  map_title <- stringr::str_glue("{stringr::str_to_title(var)} {map_ids[k,1]} yield with {map_ids[k,2]} in {y}")
  
  gp <- world %>% 
    dplyr::left_join(yields) %>% 
    dplyr::select(value = !!var) %>% 
    # sf::st_transform(crs = "+proj=robin") %>% 
    ggplot() +
    geom_sf(aes(fill = value)) +
    theme_bw() +
    coord_sf(crs = "+proj=robin") + 
    scale_fill_viridis_c(option = "plasma", na.value = "grey90") + 
    ggtitle(map_title)
  
  stringr::str_replace_all(map_title, " ", "_") %>% 
    stringr::str_glue(".pdf") %>% 
    stringr::str_glue("./output/", .) %>%
    ggsave(plot = gp, width = 12, height =6, scale = 1)
  
}

