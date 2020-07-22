# Download ACS 2018 5-year Household Income Data for renters
# by Census Tract and spatial filter for Redwood City

# Author: Kate Ham
# Version: 2020-02-27

# Libraries
library(tidyverse)
library(tidycensus)
library(sf)

# Parameters

boundaries_city_redwood <- "C:/Users/katea/GitHub/dcl-2020-01/kate/c01-own/data/boundaries_city_redwood.rds"

# HUD FY 2018 Income Limits
# Median Income
# $115,300
# Low Income (80%)
# $105,350

# ACS Household Income Variables
# TENURE BY HOUSEHOLD INCOME IN THE PAST 12 MONTHS (IN 2018 INFLATION-ADJUSTED DOLLARS)
vars_hi_rent <- 
  c(
    'B25118_014', #	Estimate!!Total!!Renter occupied
    'B25118_015', #	Estimate!!Total!!Renter occupied!!Less than $5,000	
    'B25118_016', #	Estimate!!Total!!Renter occupied!!$5,000 to $9,999	
    'B25118_017', #	Estimate!!Total!!Renter occupied!!$10,000 to $14,999
    'B25118_018', #	Estimate!!Total!!Renter occupied!!$15,000 to $19,999
    'B25118_019', #	Estimate!!Total!!Renter occupied!!$20,000 to $24,999
    'B25118_020', #	Estimate!!Total!!Renter occupied!!$25,000 to $34,999
    'B25118_021', #	Estimate!!Total!!Renter occupied!!$35,000 to $49,999
    'B25118_022', #	Estimate!!Total!!Renter occupied!!$50,000 to $74,999
    'B25118_023', #	Estimate!!Total!!Renter occupied!!$75,000 to $99,999
    'B25118_024', #	Estimate!!Total!!Renter occupied!!$100,000 to $149,999
    'B25118_025'  #	Estimate!!Total!!Renter occupied!!$150,000 or more
  )

# San Mateo County FIPS
SMC <- '081'

# NOTE: Currently spatially filtering by Censustracts
# and overestimated boundaries.

#===============================================================================
# v18 <- load_variables(2018, "acs5", cache = TRUE)
# test <- 
#   v18 %>% 
#   filter(str_detect(name, "B25127_0[4-8][\\d]$"))

acs_rwc <-
  get_acs(
    geography = "tract", # smallest geography for this ACS table
    variables = vars_hi_rent,
    state = "CA",
    county = SMC,
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
    boundaries_city_redwood %>% read_rds(), 
    .predicate = st_intersects
  ) %>% 
  st_filter(
    boundaries_city_redwood %>% read_rds(), 
    .predicate = function(x, y){!st_touches(x, y)}
  ) %>% 
  write_rds(path = "C:/Users/katea/GitHub/dcl-2020-01/kate/c01-own/data/acs_rwc.rds")
