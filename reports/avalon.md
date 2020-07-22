Corporate Landlords - Avalon
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

Idea for this report came from Tenants Together’s manual guide for
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
library(knitr)
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
best_vars <- read_rds(here::here("data/best_vars.rds"))
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
avalon %>% head() %>% kable()
```

| owner  | address                                   | access\_date |
| :----- | :---------------------------------------- | :----------- |
| avalon | 55 NINTH STREET SAN FRANCISCO, CA 94103   | 2020-07-21   |
| avalon | 965 SUTTER STREET SAN FRANCISCO, CA 94109 | 2020-07-21   |
| avalon | 754 THE ALAMEDA SAN JOSE, CA 95126        | 2020-07-21   |
| avalon | 255 KING STREET SAN FRANCISCO, CA 94107   | 2020-07-21   |
| avalon | 651 ADDISON STREET BERKELEY, CA 94710     | 2020-07-21   |
| avalon | 508 RAILWAY AVENUE CAMPBELL, CA 95008     | 2020-07-21   |

From this webscrape, there are 37 listings on the Avalon site.

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
avalon %>% head() %>% kable()
```

| owner  | address                                   | access\_date | house\_num | dir | street      | mode | city          | zipcode |
| :----- | :---------------------------------------- | :----------- | :--------- | :-- | :---------- | :--- | :------------ | :------ |
| avalon | 55 NINTH STREET SAN FRANCISCO, CA 94103   | 2020-07-21   | 55         | NA  | 9TH         | ST   | SAN FRANCISCO | 94103   |
| avalon | 965 SUTTER STREET SAN FRANCISCO, CA 94109 | 2020-07-21   | 965        | NA  | SUTTER      | ST   | SAN FRANCISCO | 94109   |
| avalon | 754 THE ALAMEDA SAN JOSE, CA 95126        | 2020-07-21   | 754        | NA  | THE ALAMEDA | NA   | SAN JOSE      | 95126   |
| avalon | 255 KING STREET SAN FRANCISCO, CA 94107   | 2020-07-21   | 255        | NA  | KING        | ST   | SAN FRANCISCO | 94107   |
| avalon | 651 ADDISON STREET BERKELEY, CA 94710     | 2020-07-21   | 651        | NA  | ADDISON     | ST   | BERKELEY      | 94710   |
| avalon | 508 RAILWAY AVENUE CAMPBELL, CA 95008     | 2020-07-21   | 508        | NA  | RAILWAY     | AVE  | CAMPBELL      | 95008   |

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
  head() %>% kable()
```

| OWNER1\_LAST\_NAME                         | ABSENTEE\_OWNER\_STATUS | OWNER1\_FIRST\_NAME\_\_\_MI | SITUS\_STATE | SITUS\_CITY | SITUS\_ZIP\_CODE | SITUS\_STREET\_NAME | SITUS\_HOUSE\_NUMBER | SITUS\_MODE | SITUS\_CARRIER\_CODE | PARCEL\_LEVEL\_LATITUDE\_*2\_6* | PARCEL\_LEVEL\_LONGITUDE\_*3\_6* |
| :----------------------------------------- | :---------------------- | :-------------------------- | :----------- | :---------- | :--------------- | :------------------ | :------------------- | :---------- | :------------------- | ------------------------------: | -------------------------------: |
| CATELLUS DEVELOPMENT CORP                  | A                       | NA                          | CA           | FREMONT     | 945383155        | BOSCELL             | 41652                | RD          | C046                 |                        37.51031 |                       \-121.9826 |
| 800 WEST TOWER AVENUE LLC                  | O                       | NA                          | CA           | ALAMEDA     | 945015048        | TOWER               | 800                  | AVE         | C077                 |                        37.78230 |                       \-122.3013 |
| HOUSING AUTHORITY OF THE CITY OF ALAMEDA   | A                       | NA                          | CA           | ALAMEDA     | 94501            | CLEMENT             | 2100                 | AVE         | C008                 |                        37.77233 |                       \-122.2454 |
| HOUSING AUTHORITY OF THE CITY OF ALAMEDA   | A                       | NA                          | CA           | ALAMEDA     | 945011437        | CLEMENT             | 2102                 | AVE         | C008                 |                        37.77233 |                       \-122.2454 |
| CITY ALAMEDA HOUSING                       | NA                      | NA                          | CA           | ALAMEDA     | 94501            | WILLIE STARGELL     | NA                   | AVE         | NA                   |                        37.78549 |                       \-122.2837 |
| HOUSING AUTHORITY OF THE COUNTY OF ALAMEDA | A                       | NA                          | CA           | HAYWARD     | 945413872        | SUNSET              | 260                  | BLVD        | C020                 |                        37.67339 |                       \-122.1014 |

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
avalon_cl %>% head() %>% kable()
```

| owner  | address                                   | access\_date | house\_num | dir | street      | mode | city          | zipcode | APN\_SEQUENCE\_NBR | FIPS\_CODE | FORMATTED\_APN | IMPROVEMENT\_VALUE\_CALCULATED\_IND | LAND\_VALUE\_CALCULATED\_IND | ORIGINAL\_APN | P\_ID\_IRIS\_FRMTD | TOTAL\_VALUE\_CALCULATED\_IND | UNFORMATTED\_APN | SITUS\_STATE | OWNER1\_LAST\_NAME            | PARCEL\_LEVEL\_LATITUDE\_*2\_6* | PARCEL\_LEVEL\_LONGITUDE\_*3\_6* | MAIL\_CITY | MAIL\_STATE | MAIL\_ZIP\_CODE | LAND\_USE | PROPERTY\_INDICATOR | COUNTY\_USE1 | MAIL\_STREET\_NAME | TAX\_CODE\_AREA | CENSUS\_TRACT | LAND\_SQUARE\_FOOTAGE |  ACRES | MAIL\_CARRIER\_CODE | SITUS\_ZIP\_CODE | ASSESSED\_YEAR | ASSD\_TOTAL\_VALUE | TOTAL\_VALUE\_CALCULATED | TAX\_AMOUNT | TAX\_YEAR | ASSD\_LAND\_VALUE | LAND\_VALUE\_CALCULATED | MAIL\_HOUSE\_NUMBER | SITUS\_MODE | SITUS\_CARRIER\_CODE | MAIL\_MODE | ASSD\_IMPROVEMENT\_VALUE | IMPROVEMENT\_VALUE\_CALCULATED | ABSENTEE\_OWNER\_STATUS | NUMBER\_OF\_UNITS | UNIVERSAL\_BUILDING\_SQUARE\_FEET | BUILDING\_SQUARE\_FEET\_IND | YEAR\_BUILT | BUILDING\_SQUARE\_FEET | LOT\_NUMBER | BATCH\_ID | BATCH\_SEQ | DOCUMENT\_TYPE | TRANSACTION\_TYPE | TOTAL\_BATHS\_CALCULATED | RECORDING\_DATE | TOTAL\_BATHS | STORIES\_NUMBER | TOTAL\_ROOMS | BEDROOMS | OWNER1\_FIRST\_NAME\_\_\_MI | LIVING\_SQUARE\_FEET | SALE\_CODE | DOCUMENT\_NO\_ | SALE\_PRICE | SELLER\_NAME                    | RESIDENTIAL\_MODEL\_INDICATOR | county         |
| :----- | :---------------------------------------- | :----------- | :--------- | :-- | :---------- | :--- | :------------ | :------ | -----------------: | :--------- | :------------- | :---------------------------------- | :--------------------------- | :------------ | :----------------- | :---------------------------- | :--------------- | :----------- | :---------------------------- | ------------------------------: | -------------------------------: | :--------- | :---------- | :-------------- | :-------- | :------------------ | :----------- | :----------------- | :-------------- | :------------ | --------------------: | -----: | :------------------ | :--------------- | -------------: | -----------------: | -----------------------: | ----------: | --------: | ----------------: | ----------------------: | :------------------ | :---------- | :------------------- | :--------- | -----------------------: | -----------------------------: | :---------------------- | ----------------: | --------------------------------: | :-------------------------- | ----------: | ---------------------: | :---------- | :-------- | :--------- | :------------- | :---------------- | -----------------------: | :-------------- | -----------: | --------------: | -----------: | -------: | :-------------------------- | -------------------: | :--------- | :------------- | ----------: | :------------------------------ | :---------------------------- | :------------- |
| avalon | 55 NINTH STREET SAN FRANCISCO, CA 94103   | 2020-07-21   | 55         | NA  | 9TH         | ST   | SAN FRANCISCO | 94103   |                  1 | 06075      | 3701-066       | A                                   | A                            | 3701 066      | 3701-066           | A                             | 3701 066         | CA           | AVA NINTH                     |                        37.77710 |                       \-122.4151 | ARLINGTON  | VA          | 222032138       | 106       | 22                  | A15          | GLEBE              | 1000            | 176013008     |                 35800 | 0.8219 | C014                | 941031443        |       20180000 |          169242834 |                169242834 |  2019796.96 |      2018 |          16006645 |                16006645 | 671                 | ST          | C001                 | RD         |                153236189 |                      153236189 | A                       |                 1 |                            241907 | B                           |        2014 |                 241907 | 66          | 20120611  | 1582       | GD             | 1                 |                       NA | 2012-06-01      |           NA |              17 |           NA |       NA | NA                          |                   NA | F          | J424775        |    14500000 | MACQUARIE 55 NINTH ST INC       | NA                            | san\_francisco |
| avalon | 965 SUTTER STREET SAN FRANCISCO, CA 94109 | 2020-07-21   | 965        | NA  | SUTTER      | ST   | SAN FRANCISCO | 94109   |                  1 | 06075      | 0300-022       | A                                   | A                            | 0300 022      | 0300-022           | A                             | 0300 022         | CA           | AVALON BAY COMMUNITIES INC    |                        37.78813 |                       \-122.4162 | ARLINGTON  | VA          | 222032138       | 106       | 22                  | A15          | GLEBE              | 1000            | 120002000     |                 13320 | 0.3058 | C014                | 941096068        |       20180000 |            9139922 |                  9139922 |   108740.44 |      2018 |           1314038 |                 1314038 | 671                 | ST          | C050                 | RD         |                  7825884 |                        7825884 | A                       |                 1 |                             77800 | B                           |        1990 |                  77800 | 22          | 19380102  | 55536      | GD             | 1                 |                       97 | 1995-10-19      |           97 |               9 |          250 |       NA | NA                          |                   NA | NA         | F871657        |          NA | LAMPASAS ASSET INVESTORS L L C  | NA                            | san\_francisco |
| avalon | 754 THE ALAMEDA SAN JOSE, CA 95126        | 2020-07-21   | 754        | NA  | THE ALAMEDA | NA   | SAN JOSE      | 95126   |                  1 | 06085      | 261-33-049     | A                                   | A                            | 26133049      | 261-33-049         | A                             | 26133049         | CA           | BAY APARTMENT COMMUNITIES INC |                        37.33065 |                       \-121.9051 | ARLINGTON  | VA          | 222032138       | 133       | 22                  | 04           | GLEBE              | 17108           | 5003002014    |                170836 | 3.9219 | C014                | 951263164        |       20180000 |           47384741 |                 47384741 |   651719.24 |      2018 |           5068445 |                 5068445 | 671                 | NA          | C034                 | RD         |                 42316296 |                       42316296 | A                       |                 1 |                                NA | NA                          |        2002 |                     NA | NA          | 19380106  | 18347      | GD             | 1                 |                       NA | 1998-04-10      |           NA |               3 |           NA |       NA | NA                          |                   NA | N          | 14134014       |          NA | VITALE GEORGE A & ADA A TRUSTEE | NA                            | santa\_clara   |
| avalon | 255 KING STREET SAN FRANCISCO, CA 94107   | 2020-07-21   | 255        | NA  | KING        | ST   | SAN FRANCISCO | 94107   |                  1 | 06075      | 8706-013       | A                                   | A                            | 8706 013      | 8706-013           | A                             | 8706 013         | CA           | MISSION BAY NORTH FINCG       |                        37.77683 |                       \-122.3926 | ARLINGTON  | VA          | 222032138       | 106       | 22                  | A            | GLEBE              | 1013            | 607001004     |                  5205 | 0.1195 | C014                | 941071700        |       20180000 |            1594147 |                  1594147 |    19238.40 |      2018 |            224072 |                  224072 | 671                 | ST          | C032                 | RD         |                  1370075 |                        1370075 | A                       |                NA |                                NA | NA                          |          NA |                     NA | 13          | NA        | NA         | NA             | NA                |                       NA | NA              |           NA |              NA |           NA |       NA | NA                          |                   NA | NA         | NA             |          NA | NA                              | NA                            | san\_francisco |
| avalon | 255 KING STREET SAN FRANCISCO, CA 94107   | 2020-07-21   | 255        | NA  | KING        | ST   | SAN FRANCISCO | 94107   |                  1 | 06075      | 8706-012       | A                                   | A                            | 8706 012      | 8706-012           | A                             | 8706 012         | CA           | MISSION BAY NORTH FINCG       |                        37.77647 |                       \-122.3930 | ARLINGTON  | VA          | 222032138       | 106       | 22                  | A            | GLEBE              | 1013            | 607001004     |                 60740 | 1.3944 | C014                | 941071700        |       20180000 |            7461631 |                  7461631 |    89698.56 |      2018 |            893116 |                  893116 | 671                 | ST          | C032                 | RD         |                  6568515 |                        6568515 | A                       |                NA |                                NA | NA                          |          NA |                     NA | 12          | NA        | NA         | NA             | NA                |                       NA | NA              |           NA |              NA |           NA |       NA | NA                          |                   NA | NA         | NA             |          NA | NA                              | NA                            | san\_francisco |
| avalon | 255 KING STREET SAN FRANCISCO, CA 94107   | 2020-07-21   | 255        | NA  | KING        | ST   | SAN FRANCISCO | 94107   |                  1 | 06075      | 8706-011       | A                                   | A                            | 8706 011      | 8706-011           | A                             | 8706 011         | CA           | MISSION BAY NORTH FINCG       |                        37.77647 |                       \-122.3930 | ARLINGTON  | VA          | 222032138       | 106       | 22                  | A            | GLEBE              | 1013            | 607001004     |                 60740 | 1.3944 | C014                | 941071700        |       20180000 |           20791854 |                 20791854 |   242507.74 |      2018 |           2922518 |                 2922518 | 671                 | ST          | C032                 | RD         |                 17869336 |                       17869336 | A                       |                NA |                                NA | NA                          |          NA |                     NA | 11          | NA        | NA         | NA             | NA                |                       NA | NA              |           NA |              NA |           NA |       NA | NA                          |                   NA | NA         | NA             |          NA | NA                              | NA                            | san\_francisco |

There are 73 Avalon properties in the CoreLogic dataset.

Many properties have seemingly duplicate data. This is because the
Avalon data is by property building and the CL data is by individual
unit. For the purposes of this analysis, we assume that all of a
building’s units have the same owner. However, I left them all in the
tibble to maximize the property information available.

Only Avalon properties that matched with the CL data were kept. Ones
that did not match were likely due to parsing errors with postmastr. We
will ignore the unmatched properties, but you can see how many there are
from the chunk below.

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
  ) %>% 
  nrow()
```

    ## [1] 19

The following is a list of all the owner names that Avalon goes by.

``` r
owners <- 
  avalon_cl %>% 
  distinct(OWNER1_LAST_NAME) %>% 
  pull()
owners %>% kable()
```

| x                             |
| :---------------------------- |
| AVA NINTH                     |
| AVALON BAY COMMUNITIES INC    |
| BAY APARTMENT COMMUNITIES INC |
| MISSION BAY NORTH FINCG       |
| AVB OPERA WAREHOUSE           |
| PI                            |
| ALAMEDA FINANCING             |
| SILICON VALLEY FINANCING LLC  |
| AVALONBAY COMMUNITIES INC     |
| BAY APARTMENT COMNS INC       |
| BAY COUNTRYBROOK              |
| TISHMAN                       |

There are 12 different names.

The following are units owned by Avalon that were not listed on their
website.

``` r
avalon_unlisted <- 
  cl_select %>% 
  filter(OWNER1_LAST_NAME %in% owners) %>% 
  anti_join(avalon_cl, by = "FORMATTED_APN") %>% 
  select(contains("SITUS"))
avalon_unlisted %>% head() %>% kable()
```

| SITUS\_STATE | SITUS\_CITY | SITUS\_ZIP\_CODE | SITUS\_STREET\_NAME | SITUS\_HOUSE\_NUMBER | SITUS\_MODE | SITUS\_CARRIER\_CODE |
| :----------- | :---------- | :--------------- | :------------------ | :------------------- | :---------- | :------------------- |
| CA           | BERKELEY    | 947022323        | WARD                | 1311                 | ST          | C055                 |
| CA           | FREMONT     | 945381904        | BURR                | 39864                | AVE         | C090                 |
| CA           | FREMONT     | 945395245        | OLIVE               | 964                  | AVE         | C090                 |
| CA           | LIVERMORE   | 945505908        | EL CAMINITO         | 945                  | NA          | C066                 |
| CA           | FREMONT     | 945364278        | STONINGTON          | 38778                | TER         | C030                 |
| CA           | PLEASANTON  | 945663261        | BROOKLINE           | 1308                 | LOOP        | C081                 |

There are 68 units. If we remove units with the same property address,
we find that there are 61 different properties.

## Conclusion

This basic process of scraping and joining with CoreLogic data is a
method of speeding up the manual process as detailed in the original
Tenants Together guide. Certainly the methods would need to be tweaked
for individual websites. In further EDAs, I will need to explore using
the `RSelenium` package, as many residential property company websites
use cookies.
