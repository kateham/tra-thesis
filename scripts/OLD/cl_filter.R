# Read CoreLogic file and filter for 9 counties

# Author: Kate Ham
# Version: 2020-02-01

# Libraries
library(tidyverse)

# Parameters

# Z drive is AFS server - connect via Internet and reauthenticate
rds_file <- "Z:/.ir.stanford.edu/users/k/h/kham101/Documents/dcl-c01-kham101/corelogic_tax_bayarea.rds"
best_vars <- "C:/Users/katea/GitHub/dcl-2020-01/kate/c01-own/data/best_vars.rds"


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

# select CoreLogic variables
vars <- 
  read_rds(best_vars)

name_to_convention <- function(name) {
  name %>% 
    str_to_lower() %>% 
    str_replace_all(pattern = "[ ]+", "_") 
}

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

corelogic_filter <- function(county_name) {
  corelogic %>% 
    filter(`FIPS CODE` == recode_county_fips(county_name)) %>% 
    select_at(vars, name_to_convention) %>% 
    write_rds(path = str_c("/Users/katea/GitHub/dcl-2020-01/kate/c01-own/data/cl_", county_name, ".rds"))
}

#===============================================================================

corelogic <- 
  read_rds(rds_file)

# # Sample data of variables with most missing values
# nas <- corelogic_SMC %>% 
#   summarise_all(~sum(is.na(.))) %>%  
#   pivot_longer(
#     cols = everything(),
#     names_to = "variable",
#     values_to = "NAs"
#   ) %>% 
#   arrange(desc(NAs))

walk(bay_area, corelogic_filter)


