# Get ACS data for given Bay Area city by tract
# Produces 2 files:
# 1) household income data for renters (B25118)
# 2) units number for renters (B25032)
# uses spatial filter with municipal boundaries

# NOTE: Before running, first run 'boundaries_city.R' script in the 'scripts' folder.

# Author: Kate Ham
# Version: 2020-03-10

# Libraries
library(tidyverse)
library(tidycensus)
library(sf)

# Parameters

  # ACS variables: Household Income renter-occupied
vars_hi_rent <- 
  c(
    'B25118_014', #	Estimate Total Renter occupied
    'B25118_015', #	Less than $5,000	
    'B25118_016', #	$5,000 to $9,999	
    'B25118_017', #	$10,000 to $14,999
    'B25118_018', #	$15,000 to $19,999
    'B25118_019', #	$20,000 to $24,999
    'B25118_020', #	$25,000 to $34,999
    'B25118_021', #	$35,000 to $49,999
    'B25118_022', #	$50,000 to $74,999
    'B25118_023', #	$75,000 to $99,999
    'B25118_024', #	$100,000 to $149,999
    'B25118_025'  #	$150,000 or more
  )

  # ACS variables: Tenure Units in Structure (selecing renter-occupied only)
vars_built_units <- 
  c(
    'B25032_013', # Total
    'B25032_014', # 1 unit, detached	
    'B25032_015', # 1 unit, attached	
    'B25032_016', # 2 units	
    'B25032_017', # 3 or 4 units	
    'B25032_018', # 5 to 9 units	
    'B25032_019', # 10 to 19 units	
    'B25032_020', # 20 to 49 units	
    'B25032_021', # 50 or more units	
    'B25032_022', # Mobile homes	
    'B25032_023' # Boat, RV, van, etc.	
  )


  # Average household size of renter occupied units
var_hsize <- 'B25010_003'	

#   # Tenure by Year Householder Moved Into Unit
# vars_tenancy <- 
#   c(
#     'B25038_009', # Estimate Total Renter Occupied
#     'B25038_010', # Moved in 2017 or later
#     'B25038_011', # Moved in 2015 to 2016	
#     'B25038_012', # Moved in 2010 to 2014	
#     'B25038_013', # Moved in 2000 to 2009	
#     'B25038_014', # Moved in 1990 to 1999	
#     'B25038_015' # Moved in 1989 or earlier
#   )

  # Select cities with TRA policies
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

# Functions

  # City-county case when
county_city <- function(city){
  case_when(
    city %in% 
      c(
        "alameda",
        "san_leandro",
        "emeryville",
        "oakland",
        "berkeley"
      ) 
    ~ "001", # Alameda
    city %in% c("san_francisco") ~ "075", # SF
    city %in%
      c(
        "east_palo_alto",
        "redwood_city",
        "menlo_park"
      ) 
    ~ "081", # San Mateo
    city %in%
      c(
        "mountain_view",
        "palo_alto",
        "san_jose"
      ) 
    ~ "085", # Santa Clara
    city %in% c("richmond") ~ "013", # Contra Costa
    city %in% c("healdsburg") ~ "097" # Sonoma
  )
}

  # Download city boundary
boundaries_city <- function(city) {
  read_rds(
    str_c(
      here::here("c01-own/data/boundaries_city/boundaries_city_"),
      {{city}}, 
      ".rds"
    )
  )
}

  # Get ACS vars and download
get_acs_city <- function(city) {
  get_acs(
    geography = "tract", # smallest geography for these ACS tables
    variables = c(vars_hi_rent, vars_built_units, var_hsize),
    state = "CA",
    county = county_city({{city}}),
    geometry = TRUE,
    year = 2018,
    survey = "acs5"
  ) %>% 
    # currently pivot_wider() doesn't work with sf objects
    select(-moe) %>%
    group_by(GEOID, NAME) %>% 
    spread(variable, estimate) %>% 
    ungroup() %>% 
    st_filter(
      boundaries_city({{city}}), 
      .predicate = st_intersects
    ) %>% 
    st_filter(
      boundaries_city({{city}}), 
      .predicate = function(x, y){!st_touches(x, y)}
    ) %>% 
    write_rds(
      path = 
        str_glue(
          here::here("c01-own/data/acs/acs_"),
          {{city}},
          ".rds"
        )
    )
}

#===============================================================================

walk(cities, get_acs_city)

