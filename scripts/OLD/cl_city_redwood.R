# Read CoreLogic San Mateo County data and Redwood City boundaries to filter for Redwood City

# Author: Kate Ham
# Version: 2020-02-01

# Libraries
library(tidyverse)
library(sf)

# Parameters
  # CoreLogic data in San Mateo County
cl_san_mateo_file <- "/Users/katea/GitHub/dcl-2020-01/kate/c01-own/data/cl_sanmateo.rds"
  # Boundaries data for Redwood City
boundaries_city_redwood_file <- "/Users/katea/GitHub/dcl-2020-01/kate/c01-own/data/boundaries_city_redwood.rds"

# NOTE: boundaries data in NAD83 datum but corelogic seems to be in WGS84 data so it is reprojected
# Need to confirm what crs for original corelogic data is

# idea: make this script input a list of cities

#===============================================================================

# Read in boundaries
boundaries_city_redwood <-
  read_rds(boundaries_city_redwood_file) %>% 
  st_as_sf()

# problem with sf filtering:
# "although coordinates are longitude/latitude, st_* assumes that they are planar"

cl_city_redwood <- 
  read_rds(cl_san_mateo_file) %>% 
  mutate(
    lat = `parcel_level_latitude__2_6_`,
    long = `parcel_level_longitude__3_6_`
  ) %>% 
  drop_na(lat, long) %>% 
  st_as_sf(coords = c("long", "lat"), crs = 4326, agr = "constant") %>% 
  st_transform(st_crs(boundaries_city_redwood)) %>% 
  st_filter(boundaries_city_redwood, .predicate = st_within) %>% 
  write_rds(path = "/Users/katea/GitHub/dcl-2020-01/kate/c01-own/data/cl_city_redwood.rds")


# ggplot() +
#   geom_sf(data = cl_san_mateo, size = 1, shape = 23, fill = "darkred") +
#   geom_sf(data = cl_city_redwood, size = 1, shape = 23, fill = "green") +
#   geom_sf(data = boundaries_city_redwood, color = "blue", fill = NA, size = 0.2) +
#   coord_sf(xlim = c(-122.26, -122.22), ylim = c(37.46, 37.48))



