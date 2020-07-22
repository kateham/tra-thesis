# Download county census block group (cbg) boundaries using tigris

# Author: Kate Ham
# Version: 2020-02-14

# Libraries
library(tidyverse)
library(tigris)

# Parameters

# 9-county Bay Area
bay_area <- 
  c(
    "alameda",
    "contra_costa", 
    "marin",
    "napa", 
    "san_francisco",
    "san_mateo",
    "santa_clara",
    "solano",
    "sonoma"
  )

recode_county_fips <- function(county_name) {
  county_name %>% 
    recode(
      "alameda" = '06001',
      "contra_costa" = '06013', 
      "marin" = '06041',
      "napa" = '06055', 
      "san_francisco" = '06075',
      "san_mateo" = '06081',
      "santa_clara" = '06085',
      "solano" = '06095',
      "sonoma" = '06097' 
    )
}

get_cbg_boundaries <- function(county_name) {
  tigris::block_groups(
    state = "CA", 
    county = recode_county_fips(county_name) %>% str_sub(., -3), 
    year = 2018, 
    class = "sf"
    ) %>%
    write_rds(str_c("/Users/katea/GitHub/dcl-2020-01/kate/c01-own/data/boundaries_cbg_", county_name, ".rds"))
}

#===============================================================================

walk(bay_area, get_cbg_boundaries)
