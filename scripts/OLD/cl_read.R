# Reading and Parsing CoreLogic csv for 9 counties

# Author: Kate Ham
# Version: 2020-02-27

# Libraries
library(tidyverse)
#library(readxl) # part of the tidyverse

# Parameters

## Files

file_alameda <- "C:/Users/katea/GitHub/dcl-2020-01/kate/c01-own/data-raw/alameda.csv"
file_contracosta <- "C:/Users/katea/GitHub/dcl-2020-01/kate/c01-own/data-raw/contracosta.csv"
file_marin <- "C:/Users/katea/GitHub/dcl-2020-01/kate/c01-own/data-raw/marin.csv"
file_napa <- "C:/Users/katea/GitHub/dcl-2020-01/kate/c01-own/data-raw/napa.csv"
file_sanfrancisco <- "C:/Users/katea/GitHub/dcl-2020-01/kate/c01-own/data-raw/sanfrancisco.csv"
file_sanmateo <- "C:/Users/katea/GitHub/dcl-2020-01/kate/c01-own/data-raw/sanmateo.csv"
file_santaclara <- "C:/Users/katea/GitHub/dcl-2020-01/kate/c01-own/data-raw/santaclara.csv"
file_solano <- "C:/Users/katea/GitHub/dcl-2020-01/kate/c01-own/data-raw/solano.csv"
file_sonoma <- "C:/Users/katea/GitHub/dcl-2020-01/kate/c01-own/data-raw/sonoma.csv"

best_vars <- "C:/Users/katea/GitHub/dcl-2020-01/kate/c01-own/data/best_vars.rds"

## Variables

FILES_BAY_AREA <- 
  c(
    file_alameda, 
    file_contracosta, 
    file_marin,
    file_napa,
    file_sanfrancisco,
    file_sanmateo, 
    file_santaclara,
    file_solano, 
    file_sonoma
  )

vars <- 
  read_rds(best_vars)

## Functions

name_to_convention <- function(name) {
  name %>%
    str_to_lower() %>%
    str_replace_all(pattern = "[ ]+", "_")
}

recode_county <- function(file) {
  file %>% 
    recode(
      "C:/Users/katea/GitHub/dcl-2020-01/kate/c01-own/data-raw/alameda.csv" = "alameda",
      "C:/Users/katea/GitHub/dcl-2020-01/kate/c01-own/data-raw/contracosta.csv" = "contracosta",
      "C:/Users/katea/GitHub/dcl-2020-01/kate/c01-own/data-raw/marin.csv" = "marin",
      "C:/Users/katea/GitHub/dcl-2020-01/kate/c01-own/data-raw/napa.csv" = "napa",
      "C:/Users/katea/GitHub/dcl-2020-01/kate/c01-own/data-raw/sanfrancisco.csv" = "sanfrancisco",
      "C:/Users/katea/GitHub/dcl-2020-01/kate/c01-own/data-raw/sanmateo.csv" = "sanmateo",
      "C:/Users/katea/GitHub/dcl-2020-01/kate/c01-own/data-raw/santaclara.csv" = "santaclara",
      "C:/Users/katea/GitHub/dcl-2020-01/kate/c01-own/data-raw/solano.csv" = "solano",
      "C:/Users/katea/GitHub/dcl-2020-01/kate/c01-own/data-raw/sonoma.csv" = "sonoma"
    )
}

cl_parse_write <- function(file) {
  file %>% 
    read_csv(
      guess_max = 2000,
      col_types = 
        cols(
          FIPS_CODE = col_character(),
          UNFORMATTED_APN = col_character(),
          CENSUS_TRACT = col_character(),
          TITLE_COMPANY_CODE = col_character(),
          TITLE_COMPANY_NAME = col_character(),
          RESIDENTIAL_MODEL_INDICATOR = col_character(),
          `_1st_MORTGAGE_AMOUNT` = col_double(),
          MORTGAGE_DATE = col_double(),
          MORTGAGE_LOAN_TYPE_CODE = col_character(),
          MORTGAGE_DEED_TYPE = col_character(),
          MORTGAGE_TERM_CODE = col_character(),
          MORTGAGE_TERM = col_double(),
          MORTGAGE_DUE_DATE = col_double(),
          BLOCK_NUMBER = col_character(),
          MORTAGE_ASSUMPTION_AMOUNT = col_double(),
          LENDER_CODE = col_character(),
          LENDER_NAME = col_character(),
          `_2nd_MORTGAGE_AMOUNT` = col_double(),
          `_2nd_MORTGAGE_LOAN_TYPE_CODE` = col_character(),
          `_2nd_DEED_TYPE` = col_character(),
          FRONT_FOOTAGE = col_double(),
          DEPTH_FOOTAGE = col_double(),
          ACRES = col_double(),
          LAND_SQUARE_FOOTAGE = col_double(),
          LOT_NUMBER = col_character(),
          UNIVERSAL_BUILDING_SQUARE_FEET = col_double(),
          BUILDING_SQUARE_FEET_IND = col_character(),
          BASEMENT_SQUARE_FEET = col_double(),
          GARAGE_PARKING_SQUARE_FEET = col_double(),
          YEAR_BUILT = col_double(),
          RANGE = col_character(),
          EFFECTIVE_YEAR_BUILT = col_double(),
          BEDROOMS = col_double(),
          TOTAL_ROOMS = col_double(),
          TOTAL_BATHS_CALCULATED = col_double(),
          TOTAL_BATHS = col_double(),
          FULL_BATHS = col_double(),
          HALF_BATHS = col_double(),
          `_1QTR_BATHS` = col_double(),
          `_3QTR_BATHS` = col_double(),
          BATH_FIXTURES = col_double(),
          TOWNSHIP = col_character(),
          AIR_CONDITIONING = col_character(),
          BASEMENT_FINISH = col_character(),
          BASEMENT_DESCRIPTION = col_character(),
          BLDG_CODE = col_character(),
          BLDG_IMPV_CODE = col_character(),
          IMPROVEMENT_VALUE_CALCULATED = col_double(),
          TOTAL_VALUE_CALCULATED_IND = col_character(),
          LAND_VALUE_CALCULATED_IND = col_character(),
          IMPROVEMENT_VALUE_CALCULATED_IND = col_character(),
          ASSD_TOTAL_VALUE = col_double(),
          ASSD_LAND_VALUE = col_double(),
          ASSD_IMPROVEMENT_VALUE = col_double(),
          MKT_TOTAL_VALUE = col_double(),
          MKT_LAND_VALUE = col_logical(),
          MAP_REFERENCE1 = col_character(),
          MKT_IMPROVEMENT_VALUE = col_double(),
          APPR_TOTAL_VALUE = col_double(),
          APPR_LAND_VALUE = col_double(),
          APPR_IMPROVEMENT_VALUE = col_double(),
          TAX_AMOUNT = col_double(),
          TAX_YEAR = col_double(),
          ASSESSED_YEAR = col_double(),
          TAX_CODE_AREA = col_character(),
          BATCH_ID = col_character(),
          BATCH_SEQ = col_character(),
          MAP_REFERENCE2 = col_character(),
          BUILDING_SQUARE_FEET = col_double(),
          LIVING_SQUARE_FEET = col_double(),
          GROUND_FLOOR_SQUARE_FEET = col_double(),
          GROSS_SQUARE_FEET = col_double(),
          ADJUSTED_GROSS_SQUARE_FEET = col_double(),
          CONDITION = col_character(),
          CONSTRUCTION_TYPE = col_character(),
          EXTERIOR_WALLS = col_character(),
          FIREPLACE_IND = col_character(),
          FIREPLACE_NUMBER = col_double(),
          SECTION = col_character(),
          FIREPLACE_TYPE = col_character(),
          FOUNDATION = col_character(),
          FLOOR = col_character(),
          FRAME = col_character(),
          GARAGE = col_character(),
          HEATING = col_character(),
          PARKING_SPACES = col_double(),
          PARKING_TYPE = col_character(),
          POOL = col_character(),
          POOL_CODE = col_character(),
          QUARTER_SECTION = col_character(),
          QUALITY = col_character(),
          ROOF_COVER = col_character(),
          ROOF_TYPE = col_character(),
          STORIES_CODE = col_character(),
          STORIES_NUMBER = col_double(),
          STYLE = col_character(),
          VIEW = col_character(),
          LOCATION_INFLUENCE = col_character(),
          NUMBER_OF_UNITS = col_double(),
          UNITS_NUMBER = col_double(),
          FLOOD_ZONE_COMMUNITY_PANEL_ID = col_character(),
          ELECTRIC_ENERGY = col_character(),
          FUEL = col_character(),
          SEWER = col_character(),
          UTILITIES = col_character(),
          WATER = col_character(),
          LEGAL1 = col_character(),
          LEGAL2 = col_character(),
          LEGAL3 = col_character(),
          LAND_USE = col_character(),
          COUNTY_USE1 = col_character(),
          APN_SEQUENCE_NBR = col_double(),
          COUNTY_USE2 = col_character(),
          MOBILE_HOME_IND = col_character(),
          ZONING = col_character(),
          PROPERTY_INDICATOR = col_character(),
          MUNICIPALITY_NAME = col_character(),
          MUNICIPALITY_CODE = col_character(),
          SUBDIVISION_TRACT_NUMBER = col_character(),
          SUBDIVISION_PLAT_BOOK = col_character(),
          SUBDIVISION_PLAT_PAGE = col_character(),
          SUBDIVISION_NAME = col_character(),
          FORMATTED_APN = col_character(),
          PARCEL_LEVEL_LATITUDE__2_6_ = col_double(),
          PARCEL_LEVEL_LONGITUDE__3_6_ = col_double(),
          SITUS_HOUSE_NUMBER_PREFIX = col_character(),
          SITUS_HOUSE_NUMBER = col_character(),
          SITUS_HOUSE_NUMBER__2 = col_character(),
          SITUS_HOUSE_NUMBER_SUFFIX = col_character(),
          SITUS_DIRECTION = col_character(),
          SITUS_STREET_NAME = col_character(),
          SITUS_MODE = col_character(),
          SITUS_QUADRANT = col_character(),
          ORIGINAL_APN = col_character(),
          SITUS_UNIT_NUMBER = col_character(),
          SITUS_CITY = col_character(),
          SITUS_STATE = col_character(),
          SITUS_ZIP_CODE = col_character(),
          SITUS_CARRIER_CODE = col_character(),
          OWNER_CORPORATE_INDICATOR = col_character(),
          OWNER1_LAST_NAME = col_character(),
          OWNER1_FIRST_NAME___MI = col_character(),
          OWNER2_LAST_NAME = col_character(),
          OWNER2_FIRST_NAME___MI = col_character(),
          PREVIOUS_PARCEL_NUMBER = col_character(),
          ABSENTEE_OWNER_STATUS = col_character(),
          HOMESTEAD_EXEMPT = col_character(),
          OWNER_ETAL_INDICATOR = col_character(),
          OWNER_OWNERSHIP_RIGHTS_CODE = col_character(),
          OWNER_RELATIONSHIP_TYPE = col_character(),
          MAIL_HOUSE_NUMBER_PREFIX = col_character(),
          MAIL_HOUSE_NUMBER = col_character(),
          MAIL_HOUSE_NUMBER__2 = col_character(),
          MAIL_HOUSE_NUMBER_SUFFIX = col_character(),
          MAIL_DIRECTION = col_character(),
          P_ID_IRIS_FRMTD = col_character(),
          MAIL_STREET_NAME = col_character(),
          MAIL_MODE = col_character(),
          MAIL_QUADRANT = col_character(),
          MAIL_UNIT_NUMBER = col_character(),
          MAIL_CITY = col_character(),
          MAIL_STATE = col_character(),
          MAIL_ZIP_CODE = col_character(),
          MAIL_CARRIER_CODE = col_character(),
          MAILING_OPT_OUT_CODE = col_character(),
          TOTAL_VALUE_CALCULATED = col_double(),
          ACCOUNT_NUMBER = col_character(),
          LAND_VALUE_CALCULATED = col_double(),
          MULTI_APN_FLAG = col_character(),
          DOCUMENT_NO_ = col_character(),
          BOOK___PAGE = col_character(),
          DOCUMENT_TYPE = col_character(),
          RECORDING_DATE = col_double(),
          SALE_DATE = col_double(),
          SALE_PRICE = col_double(),
          SALE_CODE = col_character(),
          SELLER_NAME = col_character(),
          TRANSACTION_TYPE = col_character()
        )
    ) %>% 
    select_at(vars, name_to_convention) %>%
    write_rds(
      path = 
        str_glue(
          "C:/Users/katea/GitHub/dcl-2020-01/kate/c01-own/data/cl_",
          "{recode_county(file)}.rds"
        )
    )
}

#===============================================================================

walk(FILES_BAY_AREA, cl_parse_write)
