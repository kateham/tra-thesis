# Read CoreLogic .txt file into .rds using Stanford RICE server

# NOTE: Run this script on Stanford Mac desktop and connect to RICE server

# Author: Kate Ham
# Version: 2020-02-23

# Libraries

library(tidyverse)
  # library(vroom)
  # vroom was not reading in correctly past about 15 observations, so read_delim_chunked() is used instead

# Parameters

# Input file
  # Downloaded and unzipped via Stanford CoreLogic shared Google Drive
  # text file saved on AFS in Downloads folder
file_txt <- 
  "Z:/ir.stanford.edu/users/k/h/kham101/Documents/dcl-c01-kham101/StanfordUniversity_TAX_201906_CA.txt"

# 9-County Bay Area FIPs County Codes
BAYAREA <- 
  c(
    '06001', 
    '06013',
    '06075', 
    '06081', 
    '06085', 
    '06095', 
    '06055', 
    '06097', 
    '06041'
  )


#===============================================================================

f <- function(x, pos) {
  subset(x, `FIPS CODE` %in% BAYAREA)
}

corelogic_tax <- 
  read_delim_chunked(
    file_txt, 
    delim = "|", 
    col_names = TRUE, 
    DataFrameCallback$new(f), 
    chunk_size = 10000
  ) %>% 
  write_rds("Z:/ir.stanford.edu/users/k/h/kham101/Documents/dcl-c01-kham101/corelogic_tax_bayarea.rds")
