Corporate Landlords EDA - Avalon
================
Kate Ham
7/17/2020

  - [Preface](#preface)
      - [Packages](#packages)
  - [Avalon](#avalon)
      - [Webscraping with Rvest](#webscraping-with-rvest)
      - [Parsing Addresses with
        Postmastr](#parsing-addresses-with-postmastr)
  - [CoreLogic](#corelogic)
      - [Subsetting](#subsetting)
      - [Finding Owners](#finding-owners)
  - [Conclusion](#conclusion)

## Preface

Idea for this EDA came from Tenants Together’s manual guide for
corporate landlord ownership lookup
[pdf](https://www.tenantstogether.org/sites/tenantstogether.org/files/Wall_St_Landlord_Participatory_Action_Research_Guide.pdf).
In short, the guide says to find a landlord’s website and get all the
listing addresses. Then, go to a proptech data source like Property
Radar to check ownership information.

### Packages

This EDA tries to speed up this process by scraping all the listing
addresses from a landlord site with
[rvest](https://github.com/tidyverse/rvest), then using dyplr with
pre-downloaded CoreLogic data to quickly get owner information.

While using rvest, use the [SelectorGadget](https://selectorgadget.com/)
Chrome extension to get the CSS selector for the addresses. Save the
result as `css_selector`.

We will also use the
[postmastr](https://github.com/slu-openGIS/postmastr) package to parse
the addresses. This package is still in development so needs to be
downloaded remotely.

*Limitations: The CoreLogic data downloaded is from 2019 and is only for
the 9-county Bay Area. In addition, this analysis will exclusively look
at [Avalon’s properties in Northern
California](https://www.avaloncommunities.com/northern-california).*

*Warning: The `corelogic` file, which contains the CoreLogic data for
the 9 counties in the Bay Area, is 3.6 GB.*

``` r
## Libraries
library(tidyverse)
library(rvest)
library(lubridate)
# install.packages("remotes")
#remotes::install_github("slu-openGIS/postmastr")
library(postmastr)
```

``` r
## Parameters

### Webscraping
url_data <- "https://www.avaloncommunities.com/northern-california" # Accessed 7-17-2020
css_selector <- ".address"
```

``` r
### CoreLogic
corelogic <- read_rds(here::here("data-raw/all9counties.rds"))
best_vars <-   read_rds(here::here("data/best_vars.rds"))
file_data_dic <- here::here("data-raw/Bulk_Tax_Current_Layout w parcel level lat long_03102016.xlsx")
  # Property Indicator filter
RESIDENTIAL <- c(0:199)
  # recode function
recode_county <- function(county_fips) {
  county_fips %>% 
    recode(
      '06001' = "alameda",
      '06013' = "contra_costa", 
      '06041' = "marin",
      '06055' = "napa", 
      '06075' = "san_francisco",
      '06081' = "san_mateo",
      '06085' = "santa_clara",
      '06095' = "solano",
      '06097' = "sonoma" 
    )
}
```

## Avalon

### Webscraping with Rvest

First, scrape the data from the Avalon website using rvest and the
proper CSS selector. Then, save the read in as HTML text.

Note: If the html elements are in lists, check out this
[thread](https://stackoverflow.com/questions/52650604/scrapping-li-elements-with-rvest).

``` r
address_data <- 
  url_data %>% 
  read_html() %>% 
  html_nodes(css = css_selector) %>% 
  html_text()
```

Next, save the address data in a tibble to make it easier to manage.

``` r
avalon <- 
  tibble(
    owner = "avalon",
    address = str_to_upper(address_data),
    access_date = Sys.Date()
  )
avalon
```

    ## # A tibble: 37 x 3
    ##    owner  address                                    access_date
    ##    <chr>  <chr>                                      <date>     
    ##  1 avalon 55 NINTH STREET SAN FRANCISCO, CA 94103    2020-07-21 
    ##  2 avalon 965 SUTTER STREET SAN FRANCISCO, CA 94109  2020-07-21 
    ##  3 avalon 754 THE ALAMEDA SAN JOSE, CA 95126         2020-07-21 
    ##  4 avalon 255 KING STREET SAN FRANCISCO, CA 94107    2020-07-21 
    ##  5 avalon 651 ADDISON STREET BERKELEY, CA 94710      2020-07-21 
    ##  6 avalon 508 RAILWAY AVENUE CAMPBELL, CA 95008      2020-07-21 
    ##  7 avalon 800 INDIANA STREET SAN FRANCISCO, CA 94107 2020-07-21 
    ##  8 avalon 5200 IRON HORSE PARKWAY DUBLIN, CA 94568   2020-07-21 
    ##  9 avalon 39939 STEVENSON COMMON FREMONT, CA 94538   2020-07-21 
    ## 10 avalon 325 OCTAVIA STREET SAN FRANCISCO, CA 94102 2020-07-21 
    ## # ... with 27 more rows

### Parsing Addresses with Postmastr

According to the postmastr
[guide](https://slu-opengis.github.io/postmastr/articles/postmastr.html),
We first need to create the appropriate dictionaries for elements of the
address.

``` r
dict_ca <- 
  pm_dictionary(
    type = "state",
    filter = "CA",
    case = "upper",
    locale = "us"
  )

dict_cities <- 
  pm_dictionary(
    type = "city",
    filter = "CA",
    case = "upper",
    locale = "us"
  )

dict_dir <- 
  pm_dictionary(
    type = "directional",
    case = "upper"
  )

# I can't figure out how to tidy this...
dict_mode <- 
  pm_dictionary(
    type = "suffix",
    case = "upper"
  )
# get abbreviated street modes
dict_mode <- 
  dict_mode %>% 
  bind_rows(
    map(
      1, 
      ~ dict_mode %>% 
        mutate(suf.input = str_c(.$suf.input, "."))
    )
  )
```

We then parse out each element of the address and clean up the table a
bit so it’s easier to join with the CoreLogic data. Be sure to modify
the function as necessary depending on the elements that are missing
from the scraped addresses. There’s definitely already some parsing
errors that might be able to be fixed upon further examination.

``` r
avalon <- 
  avalon %>% 
  pm_identify(var = "address") %>% 
  pm_parse(
    input = "full",
    address = "address",
    output = "full",
    keep_parsed = "yes",
    side = "right",
    keep_ids = TRUE,
    dir_dict = dict_dir,
    suffix_dict = dict_mode,
    city_dict = dict_cities,
    state_dict = dict_ca,
    locale = "us"
  ) %>%
  transmute(
    owner,
    address,
    access_date,
    house_num = pm.house,
    dir = str_to_upper(pm.preDir),
    street = str_to_upper(pm.street),
    mode = str_to_upper(pm.streetSuf),
    city = pm.city,
    zipcode = pm.zip
  )
avalon
```

    ## # A tibble: 37 x 9
    ##    owner  address        access_date house_num dir   street mode  city   zipcode
    ##    <chr>  <chr>          <date>      <chr>     <chr> <chr>  <chr> <chr>  <chr>  
    ##  1 avalon 55 NINTH STRE~ 2020-07-21  55        <NA>  9TH    ST    SAN F~ 94103  
    ##  2 avalon 965 SUTTER ST~ 2020-07-21  965       <NA>  SUTTER ST    SAN F~ 94109  
    ##  3 avalon 754 THE ALAME~ 2020-07-21  754       <NA>  THE A~ <NA>  SAN J~ 95126  
    ##  4 avalon 255 KING STRE~ 2020-07-21  255       <NA>  KING   ST    SAN F~ 94107  
    ##  5 avalon 651 ADDISON S~ 2020-07-21  651       <NA>  ADDIS~ ST    BERKE~ 94710  
    ##  6 avalon 508 RAILWAY A~ 2020-07-21  508       <NA>  RAILW~ AVE   CAMPB~ 95008  
    ##  7 avalon 800 INDIANA S~ 2020-07-21  800       <NA>  INDIA~ ST    SAN F~ 94107  
    ##  8 avalon 5200 IRON HOR~ 2020-07-21  5200      <NA>  IRON ~ PKWY  DUBLIN 94568  
    ##  9 avalon 39939 STEVENS~ 2020-07-21  39939     <NA>  STEVE~ CMN   FREMO~ 94538  
    ## 10 avalon 325 OCTAVIA S~ 2020-07-21  325       <NA>  OCTAV~ ST    SAN F~ 94102  
    ## # ... with 27 more rows

## CoreLogic

### Subsetting

The following code chunks clean and create a file of all the residential
properties in the 9-county Bay Area with select variables (less than 25%
missing values).

NOTE: Parsing failures are for properties without sales transaction
recording dates.

``` r
cl_select <-
  corelogic %>%
  select(all_of(best_vars)) %>% 
  filter(LAND_USE %in% RESIDENTIAL) %>% 
  mutate(RECORDING_DATE = ymd(RECORDING_DATE)) %>% 
  mutate(county = recode_county(FIPS_CODE))
```

    ## Warning: 17168 failed to parse.

Feel free to check the `file_data_dic` for more information. There are
67 different variables, though these variables will be of most interest
in joining with CL data:

``` r
cl_select %>% 
  select(contains("OWNER"), starts_with("SITUS"), starts_with("PARCEL_LEVEL")) %>% 
  head()
```

    ## # A tibble: 6 x 12
    ##   OWNER1_LAST_NAME ABSENTEE_OWNER_~ OWNER1_FIRST_NA~ SITUS_STATE SITUS_CITY
    ##   <chr>            <chr>            <chr>            <chr>       <chr>     
    ## 1 CATELLUS DEVELO~ A                <NA>             CA          FREMONT   
    ## 2 800 WEST TOWER ~ O                <NA>             CA          ALAMEDA   
    ## 3 HOUSING AUTHORI~ A                <NA>             CA          ALAMEDA   
    ## 4 HOUSING AUTHORI~ A                <NA>             CA          ALAMEDA   
    ## 5 CITY ALAMEDA HO~ <NA>             <NA>             CA          ALAMEDA   
    ## 6 HOUSING AUTHORI~ A                <NA>             CA          HAYWARD   
    ## # ... with 7 more variables: SITUS_ZIP_CODE <chr>, SITUS_STREET_NAME <chr>,
    ## #   SITUS_HOUSE_NUMBER <chr>, SITUS_MODE <chr>, SITUS_CARRIER_CODE <chr>,
    ## #   PARCEL_LEVEL_LATITUDE__2_6_ <dbl>, PARCEL_LEVEL_LONGITUDE__3_6_ <dbl>

### Finding Owners

Join the Avalon properties data `avalon` with the CoreLogic data
`cl_select` to get more information about the properties.

``` r
avalon_cl <- 
  avalon %>% 
  inner_join(
    cl_select, 
    by = 
      c(
        "house_num" = "SITUS_HOUSE_NUMBER", 
        "street" = "SITUS_STREET_NAME",
        "city" = "SITUS_CITY"
      )
  )
head(avalon_cl)
```

    ## # A tibble: 6 x 73
    ##   owner address access_date house_num dir   street mode  city  zipcode
    ##   <chr> <chr>   <date>      <chr>     <chr> <chr>  <chr> <chr> <chr>  
    ## 1 aval~ 55 NIN~ 2020-07-21  55        <NA>  9TH    ST    SAN ~ 94103  
    ## 2 aval~ 965 SU~ 2020-07-21  965       <NA>  SUTTER ST    SAN ~ 94109  
    ## 3 aval~ 754 TH~ 2020-07-21  754       <NA>  THE A~ <NA>  SAN ~ 95126  
    ## 4 aval~ 255 KI~ 2020-07-21  255       <NA>  KING   ST    SAN ~ 94107  
    ## 5 aval~ 255 KI~ 2020-07-21  255       <NA>  KING   ST    SAN ~ 94107  
    ## 6 aval~ 255 KI~ 2020-07-21  255       <NA>  KING   ST    SAN ~ 94107  
    ## # ... with 64 more variables: APN_SEQUENCE_NBR <dbl>, FIPS_CODE <chr>,
    ## #   FORMATTED_APN <chr>, IMPROVEMENT_VALUE_CALCULATED_IND <chr>,
    ## #   LAND_VALUE_CALCULATED_IND <chr>, ORIGINAL_APN <chr>, P_ID_IRIS_FRMTD <chr>,
    ## #   TOTAL_VALUE_CALCULATED_IND <chr>, UNFORMATTED_APN <chr>, SITUS_STATE <chr>,
    ## #   OWNER1_LAST_NAME <chr>, PARCEL_LEVEL_LATITUDE__2_6_ <dbl>,
    ## #   PARCEL_LEVEL_LONGITUDE__3_6_ <dbl>, MAIL_CITY <chr>, MAIL_STATE <chr>,
    ## #   MAIL_ZIP_CODE <chr>, LAND_USE <chr>, PROPERTY_INDICATOR <chr>,
    ## #   COUNTY_USE1 <chr>, MAIL_STREET_NAME <chr>, TAX_CODE_AREA <chr>,
    ## #   CENSUS_TRACT <chr>, LAND_SQUARE_FOOTAGE <dbl>, ACRES <dbl>,
    ## #   MAIL_CARRIER_CODE <chr>, SITUS_ZIP_CODE <chr>, ASSESSED_YEAR <dbl>,
    ## #   ASSD_TOTAL_VALUE <dbl>, TOTAL_VALUE_CALCULATED <dbl>, TAX_AMOUNT <dbl>,
    ## #   TAX_YEAR <dbl>, ASSD_LAND_VALUE <dbl>, LAND_VALUE_CALCULATED <dbl>,
    ## #   MAIL_HOUSE_NUMBER <chr>, SITUS_MODE <chr>, SITUS_CARRIER_CODE <chr>,
    ## #   MAIL_MODE <chr>, ASSD_IMPROVEMENT_VALUE <dbl>,
    ## #   IMPROVEMENT_VALUE_CALCULATED <dbl>, ABSENTEE_OWNER_STATUS <chr>,
    ## #   NUMBER_OF_UNITS <dbl>, UNIVERSAL_BUILDING_SQUARE_FEET <dbl>,
    ## #   BUILDING_SQUARE_FEET_IND <chr>, YEAR_BUILT <dbl>,
    ## #   BUILDING_SQUARE_FEET <dbl>, LOT_NUMBER <chr>, BATCH_ID <chr>,
    ## #   BATCH_SEQ <chr>, DOCUMENT_TYPE <chr>, TRANSACTION_TYPE <chr>,
    ## #   TOTAL_BATHS_CALCULATED <dbl>, RECORDING_DATE <date>, TOTAL_BATHS <dbl>,
    ## #   STORIES_NUMBER <dbl>, TOTAL_ROOMS <dbl>, BEDROOMS <dbl>,
    ## #   OWNER1_FIRST_NAME___MI <chr>, LIVING_SQUARE_FEET <dbl>, SALE_CODE <chr>,
    ## #   DOCUMENT_NO_ <chr>, SALE_PRICE <dbl>, SELLER_NAME <chr>,
    ## #   RESIDENTIAL_MODEL_INDICATOR <chr>, county <chr>

There are 73 Avalon properties in the CoreLogic dataset.

Many properties have seemingly duplicate data. This is because the
Avalon data is by property building and the CL data is by individual
unit. For the purposes of this analysis, we assume that all of a
building’s units have the same owner. However, I left them all in the
tibble to maximize the property information available.

Only Avalon properties that matched with the CL data were kept. Ones
that did not match were likely due to parsing errors with postmastr. We
will ignore the unmatched properties, but you can see them from the
chunk below.

``` r
avalon %>% 
  anti_join(
    cl_select, 
    by = 
      c(
        "house_num" = "SITUS_HOUSE_NUMBER", 
        "street" = "SITUS_STREET_NAME",
        "city" = "SITUS_CITY"
      )
  )
```

    ## # A tibble: 19 x 9
    ##    owner  address       access_date house_num dir   street  mode  city   zipcode
    ##    <chr>  <chr>         <date>      <chr>     <chr> <chr>   <chr> <chr>  <chr>  
    ##  1 avalon 651 ADDISON ~ 2020-07-21  651       <NA>  ADDISON ST    BERKE~ 94710  
    ##  2 avalon 508 RAILWAY ~ 2020-07-21  508       <NA>  RAILWAY AVE   CAMPB~ 95008  
    ##  3 avalon 5200 IRON HO~ 2020-07-21  5200      <NA>  IRON H~ PKWY  DUBLIN 94568  
    ##  4 avalon 325 OCTAVIA ~ 2020-07-21  325       <NA>  OCTAVIA ST    SAN F~ 94102  
    ##  5 avalon 899 MORRISON~ 2020-07-21  899       <NA>  MORRIS~ DR    SAN J~ 95126  
    ##  6 avalon 1600 VILLA S~ 2020-07-21  1600      <NA>  VILLA   ST    MOUNT~ 94041  
    ##  7 avalon 1200 OCEAN A~ 2020-07-21  1200      <NA>  OCEAN   AVE   SAN F~ 94112  
    ##  8 avalon 6301 SHELLMO~ 2020-07-21  6301      <NA>  SHELLM~ ST    EMERY~ 94608  
    ##  9 avalon 1099 ADMIRAL~ 2020-07-21  1099      <NA>  ADMIRA~ <NA>  SAN B~ 94066  
    ## 10 avalon EIGHT LOCKSL~ 2020-07-21  <NA>      <NA>  EIGHT ~ AVE   SAN F~ 94122  
    ## 11 avalon 24 UNION SQU~ 2020-07-21  24        <NA>  UNION   SQ    UNION~ 94587  
    ## 12 avalon 1001 HARVEY ~ 2020-07-21  1001      <NA>  HARVEY~ <NA>  WALNU~ 94597  
    ## 13 avalon 121 ROBLE RD~ 2020-07-21  121       <NA>  ROBLE ~ <NA>  WALNU~ 94597  
    ## 14 avalon 3200 RUBINO ~ 2020-07-21  3200      <NA>  RUBINO~ <NA>  SAN J~ 95125  
    ## 15 avalon 555 WEST MID~ 2020-07-21  555       W     MIDDLE~ <NA>  MOUNT~ 94043  
    ## 16 avalon 265 GATEWAY ~ 2020-07-21  265       <NA>  GATEWAY DR    PACIF~ 94044  
    ## 17 avalon 3650 ANDREWS~ 2020-07-21  3650      <NA>  ANDREWS DR    PLEAS~ 94588  
    ## 18 avalon 2175 DECOTO ~ 2020-07-21  2175      <NA>  DECOTO  RD    UNION~ 94587  
    ## 19 avalon 1445 TREAT B~ 2020-07-21  1445      <NA>  TREAT ~ <NA>  WALNU~ 94597

The following is a list of all the owner names that Avalon goes by.

``` r
owners <- 
  avalon_cl %>% 
  distinct(OWNER1_LAST_NAME) %>% 
  pull()
owners
```

    ##  [1] "AVA NINTH"                     "AVALON BAY COMMUNITIES INC"   
    ##  [3] "BAY APARTMENT COMMUNITIES INC" "MISSION BAY NORTH FINCG"      
    ##  [5] "AVB OPERA WAREHOUSE"           "PI"                           
    ##  [7] "ALAMEDA FINANCING"             "SILICON VALLEY FINANCING LLC" 
    ##  [9] "AVALONBAY COMMUNITIES INC"     "BAY APARTMENT COMNS INC"      
    ## [11] "BAY COUNTRYBROOK"              "TISHMAN"

There are 12 different names.

The following are units owned by Avalon that were not listed on their
website.

``` r
avalon_unlisted <- 
  cl_select %>% 
  filter(OWNER1_LAST_NAME %in% owners) %>% 
  anti_join(avalon_cl, by = "FORMATTED_APN") %>% 
  select(contains("SITUS"))
avalon_unlisted
```

    ## # A tibble: 68 x 7
    ##    SITUS_STATE SITUS_CITY SITUS_ZIP_CODE SITUS_STREET_NA~ SITUS_HOUSE_NUM~
    ##    <chr>       <chr>      <chr>          <chr>            <chr>           
    ##  1 CA          BERKELEY   947022323      WARD             1311            
    ##  2 CA          FREMONT    945381904      BURR             39864           
    ##  3 CA          FREMONT    945395245      OLIVE            964             
    ##  4 CA          LIVERMORE  945505908      EL CAMINITO      945             
    ##  5 CA          FREMONT    945364278      STONINGTON       38778           
    ##  6 CA          PLEASANTON 945663261      BROOKLINE        1308            
    ##  7 CA          DUBLIN     945682900      MONTIANO         5251            
    ##  8 CA          BERKELEY   947042456      CHANNING         2727            
    ##  9 CA          UNION CITY 94587          DECOTO           2261            
    ## 10 CA          WALNUT CR~ 945977953      TREAT            1445            
    ## # ... with 58 more rows, and 2 more variables: SITUS_MODE <chr>,
    ## #   SITUS_CARRIER_CODE <chr>

There are 68 units. If we remove units with the same property address,
we find that there are 61different properties.

## Conclusion

This basic process of scraping and joining with CoreLogic data is a
method of speeding up the manual process as detailed in the original
Tenants Together guide. Certainly the methods would need to be tweaked
for individual websites. In further EDAs, I will need to explore using
the `RSelenium` package, as many residential property company websites
use cookies.
