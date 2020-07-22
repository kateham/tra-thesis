# Download city (census designated place) boundaries using tigris

# Author: Kate Ham
# Version: 2020-03-10

# Libraries
library(tidyverse)
library(tigris)

# Parameters

# NOTE: to add a new city, add to (1) `cities` parameter and (2) in the `recode_place_fips` function

cities <- 
  c(
    "alameda",
    "san_leandro",
    "emeryville",
    "oakland",
    "berkeley",
    "san_francisco",
    "east_palo_alto",
    "redwood_city",
    "menlo_park",
    "mountain_view",
    "palo_alto",
    "san_jose",
    "richmond",
    "healdsburg"
  )

recode_place_fips <- function(city_name) {
  city_name %>% 
    recode(
      "alameda" = "00562",
      "san_leandro" = "68084",
      "emeryville" = "22594",
      "oakland" = "53000",
      "berkeley" = "06000",
      "san_francisco" = "67000",
      "east_palo_alto" = "20956",
      "redwood_city"  = "60102",
      "menlo_park" = "46870",
      "mountain_view" = "49670",
      "palo_alto" = "55282",
      "san_jose" = "68000",
      "richmond" = "60620",
      "healdsburg" = "33056"
    )
}

get_place_boundaries <- function(city_name) {
  tigris::places("CA", year = 2018, class = "sf") %>% 
    filter(`PLACEFP` == recode_place_fips({{city_name}})) %>% 
    write_rds(
      str_c(
        "/Users/katea/GitHub/dcl-2020-01/kate/c01-own/data/boundaries_city/boundaries_city_", 
        {{city_name}}, 
        ".rds")
    )
}

#===============================================================================

walk(cities, get_place_boundaries)

