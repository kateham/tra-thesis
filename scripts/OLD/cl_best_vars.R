# Assessing Corelogic Data Quality
# Get best vars, 25% or less missing values from all bay area

# Author: Kate Ham
# Version: 2020-02-28

# Libraries
library(tidyverse)

# Parameters

## Files

cl_alameda <- "C:/Users/katea/GitHub/dcl-2020-01/kate/c01-own/data/cl_alameda.rds"
cl_contracosta <- "C:/Users/katea/GitHub/dcl-2020-01/kate/c01-own/data/cl_contracosta.rds"
cl_marin <- "C:/Users/katea/GitHub/dcl-2020-01/kate/c01-own/data/cl_marin.rds"
cl_napa <- "C:/Users/katea/GitHub/dcl-2020-01/kate/c01-own/data/cl_napa.rds"
cl_sanfrancisco <- "C:/Users/katea/GitHub/dcl-2020-01/kate/c01-own/data/cl_sanfrancisco.rds"
cl_sanmateo <- "C:/Users/katea/GitHub/dcl-2020-01/kate/c01-own/data/cl_sanmateo.rds"
cl_santaclara <- "C:/Users/katea/GitHub/dcl-2020-01/kate/c01-own/data/cl_santaclara.rds"
cl_solano <- "C:/Users/katea/GitHub/dcl-2020-01/kate/c01-own/data/cl_solano.rds"
cl_sonoma <- "C:/Users/katea/GitHub/dcl-2020-01/kate/c01-own/data/cl_sonoma.rds"

## Variables

BAY_AREA <- 
  c(
    cl_alameda,
    cl_contracosta,
    cl_marin,
    cl_napa,
    cl_sanfrancisco,
    cl_sanmateo,
    cl_santaclara,
    cl_solano,
    cl_sonoma
  )

## Functions

recode_county2 <- function(file) {
  file %>% 
    recode(
      "C:/Users/katea/GitHub/dcl-2020-01/kate/c01-own/data/cl_alameda.rds" = "alameda",
      "C:/Users/katea/GitHub/dcl-2020-01/kate/c01-own/data/cl_contracosta.rds" = "contracosta",
      "C:/Users/katea/GitHub/dcl-2020-01/kate/c01-own/data/cl_marin.rds" = "marin",
      "C:/Users/katea/GitHub/dcl-2020-01/kate/c01-own/data/cl_napa.rds" = "napa",
      "C:/Users/katea/GitHub/dcl-2020-01/kate/c01-own/data/cl_sanfrancisco.rds" = "sanfrancisco",
      "C:/Users/katea/GitHub/dcl-2020-01/kate/c01-own/data/cl_sanmateo.rds" = "sanmateo",
      "C:/Users/katea/GitHub/dcl-2020-01/kate/c01-own/data/cl_santaclara.rds" = "santaclara",
      "C:/Users/katea/GitHub/dcl-2020-01/kate/c01-own/data/cl_solano.rds" = "solano",
      "C:/Users/katea/GitHub/dcl-2020-01/kate/c01-own/data/cl_sonoma.rds" = "sonoma"
    )
}

cl_filter_na <- function(cl_county) {
  cl_county %>% 
    read_rds() %>% 
    summarize_all(~ sum(is.na(.))) %>% 
    pivot_longer(cols = everything(), names_to = "variable", values_to = "na_count") %>% 
    mutate(na_prop = na_count / nrow(cl_county %>% read_rds())) %>% 
    filter(na_prop <= 0.25) %>% 
    arrange(na_prop) %>% 
    write_rds(
      str_glue(
        "C:/Users/katea/GitHub/dcl-2020-01/kate/c01-own/data/cl_na_", 
        "{recode_county2(cl_county)}", 
        ".rds"
      )
    )
}

#===============================================================================

walk(BAY_AREA, cl_filter_na)

# haven't run this yet...
BAY_AREA %>% 
  map(read_rds) 
# or this one...
all9counties <- 
  rbind(cl_alameda, cl_contracosta, cl_marin, cl_napa, cl_sanfrancisco, cl_sanmateo, cl_santaclara, cl_solano, cl_sonoma) %>% 
  write_rds("C:/Users/katea/GitHub/dcl-2020-01/kate/c01-own/data-raw/all9counties.rds")


best_vars <- 
  all9counties %>% 
  summarize_all(~ sum(is.na(.))) %>% 
  pivot_longer(cols = everything(), names_to = "variable", values_to = "na_count") %>% 
  mutate(na_prop = na_count / nrow(all9)) %>% 
  filter(na_prop <= 0.25) %>% 
  group_by(variable) %>% 
  summarize(
    na_count = sum(na_count)
  ) %>% 
  arrange(na_count) %>% 
  .$variable %>%
  write_rds("C:/Users/katea/GitHub/dcl-2020-01/kate/c01-own/data/best_vars.rds")
  
  

