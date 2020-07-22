# Estimate which census block groups are in (greedy match) Redwood City

# Author: Kate Ham
# Version: 2020-02-14

# Libraries
library(tidyverse)

# Parameters
boundaries_city_redwood_file <- "/Users/katea/GitHub/dcl-2020-01/kate/c01-own/data/boundaries_city_redwood.rds"
boundaries_cbg_san_mateo_file <- "C:/Users/katea/GitHub/dcl-2020-01/kate/c01-own/data/boundaries_cbg_san_mateo.rds"

#===============================================================================

# Read in files
boundaries_cbg_san_mateo <- read_rds(boundaries_cbg_san_mateo_file)
boundaries_city_redwood <- read_rds(boundaries_city_redwood_file)

# slight problem because city boundary includes a lot of water

boundaries_cbg_san_mateo %>% 
  st_filter(boundaries_city_redwood, .predicate = st_intersects) %>% 
  st_filter(boundaries_city_redwood, .predicate = function(x, y){!st_touches(x, y)}) %>% 
  filter(`ALAND` > 0) %>% 
  write_rds(path = "/Users/katea/GitHub/dcl-2020-01/kate/c01-own/data/boundaries_cbg_city_redwood.rds")


# ggplot() +
#   geom_sf(data = boundaries_cbg_redwood, color = "red", fill = NA, size = 0.2) +
#   geom_sf(data = boundaries_city_redwood, color = "blue", fill = NA, size = 0.2) 

  
