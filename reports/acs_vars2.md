Methodology for ACS Variables
================
Kate Ham
2020-06-28

  - [Prior Script](#prior-script)
  - [Parameters](#parameters)
  - [Eligibility Factors](#eligibility-factors)
      - [Units in Structure](#units-in-structure)
          - [Units Variables](#units-variables)
          - [Units Variable Groups](#units-variable-groups)
      - [Household Income](#household-income)
          - [Household Income Variables](#household-income-variables)
          - [AMI Variable Groups](#ami-variable-groups)
  - [Output](#output)

``` r
# Libraries
library(tidyverse)
```

# Prior Script

This section provides recoding methodology using ACS variables. The
appropriate ACS variables for each of the selected Bay Area cities is
downloaded using the ‘tidycensus’ package in the ‘acs\_all.R’ script in
the ‘c01-own/scripts’ folder. The script should be run prior to running
this report.

# Parameters

The following sections explain the methodology in choosing the American
Community Survey (ACS) 2018 5-year variables to represent eligibility
for tenant relocation assistance (TRA) policies. At times, the ACS does
not have the same level of precision that the policy does, so some
generalizations are made transparent.

The following 14 Bay Area cities were chosen because they all have
municipal TRA policies. Please see this [Google
Sheet](https://docs.google.com/spreadsheets/d/1WF56CGRvEL8fDPcb3Y8BoHV1E_GiOYXJRQQgLvrIRjM/edit?usp=sharing)
that I’ve created detailing the policies in the Bay Area. The sheet is
updated to May 2020.

``` r
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
    "healdsburg",
    "richmond"
  )
```

-----

# Eligibility Factors

The two most common observed eligibility restrictions in TRA policies
examined in this report were by AMI (area median income) and units in
structure. The following sections create recode functions for
eligibility factors.

## Units in Structure

One of the two main factors of TRA policies is the number of units in
the structure. For example, Redwood City’s relocation assistance policy
details the following according to the city’s [renter protections
webpage](https://www.redwoodcity.org/departments/community-development-department/planning-housing/renter-protections)
under “Relocation Assistance Highlights”:

> Rental units not included in the City’s ordinance include a room or
> any portion of a residential unit which is occupied by the landlord, a
> mobile home, properties of **four or fewer dwelling units** located on
> one lot, including single-family, duplex, tri-plex, or four-plex homes
> and accessory dwellings, mobile homes or housing accommodation in
> hotels, motels, etc.

Since the ACS data does not have the same level of precision in units, I
make a generalization: I round the actual units requirement detailed by
the policy to the units cutoff in the ACS data (see ACS variables
below). The differences for each city are included as notes.

### Units Variables

The following variables were pulled from the ACS 2018 (5-year) table
‘B25032’ for estimates of total renter-occupied households by units in
structure using the ‘tidycensus’ package:

  - B25032\_013 - Total
  - B25032\_014 - 1 unit, detached
  - B25032\_015 - 1 unit, attached
  - B25032\_016 - 2 units  
  - B25032\_017 - 3 or 4 units
  - B25032\_018 - 5 to 9 units
  - B25032\_019 - 10 to 19 units  
  - B25032\_020 - 20 to 49 units  
  - B25032\_021 - 50 or more units
  - B25032\_022 - Mobile homes
  - B25032\_023 - Boat, RV, van, etc.

### Units Variable Groups

The following code chunks create variable lists for each city and save
them as .rds files, and each city includes an explanation of what the
policy says.

#### Units Group 1 - 2 or more units, excluding mobile homes

1 city meets this criteria: \* San Leandro

``` r
vars_units1 <- 
  vars(
    B25032_016, 
    B25032_017, 
    B25032_018, 
    B25032_019, 
    B25032_020,
    B25032_021
  ) %>% 
  write_rds(path = here::here("c01-own/data/acs_vars/vars_units1.rds"))
```

#### Units Group 2 - 3 or more units, excluding mobile homes

3 cities meet this criteria: \* Mountain View \* San Jose \* Richmond

``` r
vars_units2 <- 
  vars(
    B25032_017, 
    B25032_018, 
    B25032_019, 
    B25032_020,
    B25032_021
  ) %>% 
  write_rds(path = here::here("c01-own/data/acs_vars/vars_units2.rds"))
```

#### Units Group 3 - all units, excluding mobile homes

7 cities meet this criteria: \* Alameda \* Emeryville \* Berkeley \* San
Francisco \* East Palo Alto \* Healdsburg \* Oakland

``` r
vars_units3 <- 
  vars(
    B25032_014,
    B25032_015,
    B25032_016, 
    B25032_017, 
    B25032_018, 
    B25032_019, 
    B25032_020,
    B25032_021
  ) %>% 
  write_rds(path = here::here("c01-own/data/acs_vars/vars_units3.rds"))
```

#### Units Group 4 - 5 units or more, excluding mobile homes

2 cities meet this criteria: \* Redwood City \* Menlo Park

``` r
vars_units4 <- 
  vars(
    B25032_018, 
    B25032_019, 
    B25032_020,
    B25032_021
  ) %>% 
  write_rds(path = here::here("c01-own/data/acs_vars/vars_units4.rds"))
```

#### Units Group 5 - 50 units or more, excluding mobile homes

1 city meets this criteria: \* Palo Alto

``` r
vars_units5 <- 
  vars(
    B25032_021
  ) %>% 
  write_rds(path = here::here("c01-own/data/acs_vars/vars_units5.rds"))
```

#### Units Recode Function

This function will apply the correct variable list to the correct city.

``` r
recode_units <- function(city){
  if (city %in% c("san_leandro")) {
    vars_units1
  } else if (city %in% c("mountain_view", "san_jose", "richmond")) {
    vars_units2
  } else if (
    city %in% 
    c(
      "alameda",
      "emeryville",
      "berkeley",
      "san_francisco", 
      "east_palo_alto",
      "healdsburg",
      "oakland"
    )
  ) {
    vars_units3
  } else if (city %in% c("redwood_city", "menlo_park")) {
    vars_units4
  } else if (city %in% c("palo_alto")) {
    vars_units5
  } else {
    NA
  }
}

recode_units %>% 
  write_rds(path = here::here("c01-own/data/acs_vars/recode_units.rds"))
```

-----

## Household Income

The other main factor in TRA policies is means-testing, which is
traditionally done by percentage of Area Median Income (AMI).

For example, Redwood City’s relocation assistance policy details the
following according to the city’s [renter protections
webpage](https://www.redwoodcity.org/departments/community-development-department/planning-housing/renter-protections)
under “Relocation Assistance Highlights”:

> Eligible residential households include a displaced residential
> household whose annual income **does not exceed eighty percent of the
> area median household income** for San Mateo County as adjusted for
> household size according to the United States Department of Housing
> and Urban Development and whose rental payments to the landlord remain
> current through the date of displacement. \[^1\]

Each city’s AMI income limits calculations are determined by HUD and can
be found [here](https://www.huduser.gov/portal/datasets/il.html#2018).
All selecteted cities are grouped by HUD FMR Areas. Income limits are
determined by household size in addition to AMI.

For each city, I use 2018 ACS data for average household size (table
‘B25010’) and round to the nearest integer to generalize household
size across the entire city, which is necessary for the AMI calculation.
Since the ACS data does not have the same level of precision in incomes,
I make another generalization: I include the difference between the
actual AMI income limit at the average household size and the income
cutoff in the ACS data (see ACS variables below).

Note: While the “CHAS” data (Comprehensive Housing Affordability
Strategy) from HUD might seem useful for its precise AMI calculations
rather than using my estimations, its most recent data is from 2012 -
2016. I conclude that this time frame is too far from 2018 to be useful.

### Household Income Variables

The following variables are estimates number of households based on
household income in the past 12 months (in 2018 inflation-adjusted
dollars) and pulled from the ACS 2018 (5-year) table ‘B25118’ for
renter-occupied household units:

  - ‘B25118\_014’ - Total
  - ‘B25118\_015’ - Less than $5,000  
  - ‘B25118\_016’ - $5,000 to $9,999  
  - ‘B25118\_017’ - $10,000 to $14,999
  - ‘B25118\_018’ - $15,000 to $19,999
  - ‘B25118\_019’ - $20,000 to $24,999
  - ‘B25118\_020’ - $25,000 to $34,999
  - ‘B25118\_021’ - $35,000 to $49,999
  - ‘B25118\_022’ - $50,000 to $74,999
  - ‘B25118\_023’ - $75,000 to $99,999
  - ‘B25118\_024’ - $100,000 to $149,999
  - ‘B25118\_025’ - $150,000 or more

### AMI Variable Groups

The following code chunks create variable lists for each city and save
them as .rds files, and each city includes an explanation of what the
policy says. See [TRA Ordinance Chart]() for more details. Relocation
assistance estimates are based on 2018 data and dollars.

  - Mountain View
      - 120% AMI
      - San Jose-Sunnyvale-Santa Clara, CA HUD Metro
      - 2.3 persons/household ⇒ 2 persons/household
      - $120,180 (120% AMI for 2-person household) ⇒ $100,000
  - Redwood City
      - 80% AMI
      - San Francisco, CA HUD Metro FMR Area
      - 2.8 persons/household ⇒ 3 persons/household
      - $105,700 (80% AMI for 3-person household) ⇒ $100,000
  - Menlo Park
      - 80% AMI
      - San Jose-Sunnyvale-Santa Clara, CA HUD Metro FMR Area
      - 2.55 persons/household ⇒ 3 persons/household
      - $85,050 (80% AMI for 3-person household) ⇒ $75,000
  - Healdsburg
      - 120% AMI
      - Santa Rosa, CA MSA

#### AMI Group 1 - Less than $150,000 household income

``` r
vars_ami150 <- 
  vars(
    B25118_015,
    B25118_016,
    B25118_017,
    B25118_018,
    B25118_019,
    B25118_020,
    B25118_021,
    B25118_022,
    B25118_023,
    B25118_024
  ) %>% 
  write_rds(path = here::here("c01-own/data/acs_vars/vars_ami150.rds"))
```

#### AMI Group 2 - Less than $100,000 household income

``` r
vars_ami100 <- 
  vars(
    B25118_015,
    B25118_016,
    B25118_017,
    B25118_018,
    B25118_019,
    B25118_020,
    B25118_021,
    B25118_022,
    B25118_023
  ) %>% 
  write_rds(path = here::here("c01-own/data/acs_vars/vars_ami100.rds"))
```

#### AMI Group 3 - Less than $75,000 household income

``` r
vars_ami75 <- 
  vars(
    B25118_015,
    B25118_016,
    B25118_017,
    B25118_018,
    B25118_019,
    B25118_020,
    B25118_021,
    B25118_022
  ) %>% 
  write_rds(path = here::here("c01-own/data/acs_vars/vars_ami75.rds"))
```

#### AMI Group 4 - Less than $50,000 household income

``` r
vars_ami50 <- 
  vars(
    B25118_015,
    B25118_016,
    B25118_017,
    B25118_018,
    B25118_019,
    B25118_020,
    B25118_021
  ) %>% 
  write_rds(path = here::here("c01-own/data/acs_vars/vars_ami50.rds"))
```

#### AMI Group 5 - No income limits

The following cities do not specify income requirements:

  - Alameda
  - San Leandro
  - Emeryville
  - Oakland
  - Berkeley
  - San Francisco
  - East Palo Alto
  - Palo Alto
  - San Jose
  - Richmond

<!-- end list -->

``` r
vars_ami_all <- 
  vars(B25118_014) %>% 
  write_rds(path = here::here("c01-own/data/acs_vars/vars_ami_all.rds"))
```

#### Income Limits

Based on 2018 official data from California Housing and Community
Development Department (see
[here](https://www.hcd.ca.gov/grants-funding/income-limits/state-and-federal-income-limits/docs/inc2k18.pdf)),
I manually create tibbles for each city based on its AMI restriction and
2018 income limits for various household sizes (‘hhsize’). This tibble
will be used to match the rounded household size of each census tract to
the appropriate income limit.

``` r
ami_mountain_view <- 
  tribble(
    ~ami, ~hhsize, ~incomelimit,
    120, 1, 105200,
    120, 2, 120200,
    120, 3, 135250,
    120, 4, 150250,
    120, 5, 162250
  )

ami_redwood_city <- 
  tribble(
    ~ami, ~hhsize, ~incomelimit,
    80, 1, 82200,
    80, 2, 93950,
    80, 3, 105700,
    80, 4, 117400,
    80, 5, 126800,
    80, 6, 136200
  )

ami_menlo_park <- 
  tribble(
    ~ami, ~hhsize, ~incomelimit,
    80, 1, 66150,
    80, 2, 75600,
    80, 3, 85050,
    80, 4, 94450,
    80, 5, 102050,
    80, 6, 109600
  )

ami_healdsburg <- 
  tribble(
    ~ami, ~hhsize, ~incomelimit,
    120, 1, 70650,
    120, 2, 80700,
    120, 3, 90800,
    120, 4, 100900,
    120, 5, 108950,
    120, 6, 117050
  )
```

#### Joining AMI, Household Size, and Income Limit Data

For each of the 4 cities with AMI limits, three new columns are appended
for household size (by tract), AMI limit (by city), and income limit
(which depends on both). In addition, another column ‘acs\_ami’ is added
that rounds the income limit to the nearest ACS household income
variable. The results overwrite the ACS .rds files for the cities.

``` r
hhi_acs <-
  c(5000, 10000, 15000, 20000, 25000, 35000, 50000, 75000, 100000, 150000)

join_ami <- function(df, ami_tibble){
  str_glue(here::here("c01-own/data/acs/{df}.rds")) %>%
    read_rds() %>% 
    mutate(hhsize = round(B25010_003)) %>% # B25032_013
    left_join(
      {{ami_tibble}},
      by = "hhsize"
    ) %>% 
    mutate(
      ami_acs = map_int(incomelimit, ~ which.min(abs(hhi_acs - .))),
      ami_acs = hhi_acs[ami_acs]
    ) %>% 
    write_rds(path = str_glue(here::here("c01-own/data/acs/{df}.rds")))
}

# acs_redwood_city <- join_ami("acs_redwood_city", ami_redwood_city)
# acs_menlo_park <- join_ami("acs_menlo_park", ami_menlo_park)
# acs_mountain_view <- join_ami("acs_mountain_view", ami_mountain_view)
# acs_healdsburg <- join_ami("acs_healdsburg", ami_healdsburg)
```

#### AMI Recode Function

This function will apply the correct variable list to the correct city.

``` r
recode_ami <- function(ami_acs, city){
  if (
    city %in%
    c(
      "mountain_view",
      "redwood_city",
      "menlo_park",
      "healdsburg"
    )
  ) {
    if (ami_acs == 150000) {vars_ami150}
    else if (ami_acs == 100000) {vars_ami100}
    else if (ami_acs == 75000) {vars_ami75}
    else if (ami_acs == 50000) {vars_ami50}
    else {NA}
  } else if (
    city %in%
    c(
      "alameda",
      "san_leandro",
      "emeryville",
      "oakland",
      "berkeley",
      "san_francisco",
      "east_palo_alto",
      "palo_alto",
      "san_jose",
      "richmond"
    )
  ) {
    vars_ami_all
  } else {
    NA
  }
}

recode_ami %>% 
  write_rds(path = here::here("c01-own/data/acs_vars/recode_ami.rds"))
```

# Output

The following is a list of the object outputs produced from this file,
which are saved in `data/acs_vars`:

    ## C:/Users/katea/GitHub/dcl-2020-01/kate/c01-own/data/acs_vars/recode_ami.rds
    ## C:/Users/katea/GitHub/dcl-2020-01/kate/c01-own/data/acs_vars/recode_units.rds
    ## C:/Users/katea/GitHub/dcl-2020-01/kate/c01-own/data/acs_vars/vars_ami1.rds
    ## C:/Users/katea/GitHub/dcl-2020-01/kate/c01-own/data/acs_vars/vars_ami100.rds
    ## C:/Users/katea/GitHub/dcl-2020-01/kate/c01-own/data/acs_vars/vars_ami150.rds
    ## C:/Users/katea/GitHub/dcl-2020-01/kate/c01-own/data/acs_vars/vars_ami2.rds
    ## C:/Users/katea/GitHub/dcl-2020-01/kate/c01-own/data/acs_vars/vars_ami3.rds
    ## C:/Users/katea/GitHub/dcl-2020-01/kate/c01-own/data/acs_vars/vars_ami50.rds
    ## C:/Users/katea/GitHub/dcl-2020-01/kate/c01-own/data/acs_vars/vars_ami75.rds
    ## C:/Users/katea/GitHub/dcl-2020-01/kate/c01-own/data/acs_vars/vars_ami_all.rds
    ## C:/Users/katea/GitHub/dcl-2020-01/kate/c01-own/data/acs_vars/vars_units1.rds
    ## C:/Users/katea/GitHub/dcl-2020-01/kate/c01-own/data/acs_vars/vars_units2.rds
    ## C:/Users/katea/GitHub/dcl-2020-01/kate/c01-own/data/acs_vars/vars_units3.rds
    ## C:/Users/katea/GitHub/dcl-2020-01/kate/c01-own/data/acs_vars/vars_units4.rds
    ## C:/Users/katea/GitHub/dcl-2020-01/kate/c01-own/data/acs_vars/vars_units5.rds
