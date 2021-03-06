Methodology for ACS Variables
================
Kate Ham
2020-03-18

  - [Parameters](#parameters)
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

# Parameters

The following sections explain the methodology in choosing the American
Community Survey (ACS) variables to represent eligibility for tenant
relocation assistance (TRA) policies. At times, the ACS does not have
the same level of precision that the policy does, so some
generalizations are made transparent.

The following 11 Bay Area cities were chosen because they all have
municipal TRA policies. Please see this [Google
Sheet](https://docs.google.com/spreadsheets/d/1WF56CGRvEL8fDPcb3Y8BoHV1E_GiOYXJRQQgLvrIRjM/edit?usp=sharing)
that I’ve created detailing the policies in the Bay Area. The sheet is
still being cleaned and updated.

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
    "san_jose"
  )
```

-----

## Units in Structure

One of the two main factors of TRA policies and that will be examined in
this report is the number of units in the structure.

For example, Redwood City’s relocation assistance policy details the
following according to the city’s [renter protections
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
‘B25127’ for renter-occupied household units:

  - ‘B25127\_045’ - Estimate Total Renter occupied
  - ‘B25127\_046’ - Built 2010 or later  
  - ‘B25127\_047’ - Built 2010 or later 1, detached or attached  
  - ‘B25127\_048’ - Built 2010 or later 2 to 4
  - ‘B25127\_049’ - Built 2010 or later 5 to 19
  - ‘B25127\_050’ - Built 2010 or later 20 to 49  
  - ‘B25127\_051’ - Built 2010 or later 50 or more
  - ‘B25127\_052’ - Built 2010 or later Mobile home, boat, RV, van,
    etc.  
  - ‘B25127\_053’ - Built 2000 to 2009
  - ‘B25127\_054’ - Built 2000 to 2009 1, detached or attached
  - ‘B25127\_055’ - Built 2000 to 2009 2 to 4  
  - ‘B25127\_056’ - Built 2000 to 2009 5 to 19
  - ‘B25127\_057’ - Built 2000 to 2009 20 to 49  
  - ‘B25127\_058’ - Built 2000 to 2009 50 or more  
  - ‘B25127\_059’ - Built 2000 to 2009 Mobile home, boat, RV, van, etc.
  - ‘B25127\_060’ - Built 1980 to 1999
  - ‘B25127\_061’ - Built 1980 to 1999 1, detached or attached
  - ‘B25127\_062’ - Built 1980 to 1999 2 to 4  
  - ‘B25127\_063’ - Built 1980 to 1999 5 to 19
  - ‘B25127\_064’ - Built 1980 to 1999 20 to 49  
  - ‘B25127\_065’ - Built 1980 to 1999 50 or more  
  - ‘B25127\_066’ - Built 1980 to 1999 Mobile home, boat, RV, van,
    etc.  
  - ‘B25127\_067’ - Built 1960 to 1979
  - ‘B25127\_068’ - Built 1960 to 1979 1, detached or attached
  - ‘B25127\_069’ - Built 1960 to 1979 2 to 4
  - ‘B25127\_070’ - Built 1960 to 1979 5 to 19
  - ‘B25127\_071’ - Built 1960 to 1979 20 to 49  
  - ‘B25127\_072’ - Built 1960 to 1979 50 or more  
  - ‘B25127\_073’ - Built 1960 to 1979 Mobile home, boat, RV, van,
    etc.  
  - ‘B25127\_074’ - Built 1940 to 1959
  - ‘B25127\_075’ - Built 1940 to 1959 1, detached or attached
  - ‘B25127\_076’ - Built 1940 to 1959 2 to 4  
  - ‘B25127\_077’ - Built 1940 to 1959 5 to 19
  - ‘B25127\_078’ - Built 1940 to 1959 20 to 49  
  - ‘B25127\_079’ - Built 1940 to 1959 50 or more
  - ‘B25127\_080’ - Built 1940 to 1959 Mobile home, boat, RV, van,
    etc.  
  - ‘B25127\_081’ - Built 1939 or earlier  
  - ‘B25127\_082’ - Built 1939 or earlier 1, detached or attached  
  - ‘B25127\_083’ - Built 1939 or earlier 2 to 4  
  - ‘B25127\_084’ - Built 1939 or earlier 5 to 19  
  - ‘B25127\_085’ - Built 1939 or earlier 20 to 49
  - ‘B25127\_086’ - Built 1939 or earlier 50 or more  
  - ‘B25127\_087’ - Built 1939 or earlier Mobile home, boat, RV, van,
    etc.

### Units Variable Groups

The following code chunks create variable lists for each city and save
them as .rds files, and each city includes an explanation of what the
policy says.

#### Units Group 1 - 2 or more units, excluding mobile homes

  - Mountain View
      - 3 units or more - *difference of 1 unit*
  - San Leandro
      - 2 units or more - *no difference*
  - San Jose
      - 3 units or more - *difference of 1 unit*

<!-- end list -->

``` r
vars_units1 <- 
  vars(
    B25127_048,     
    B25127_049, 
    B25127_050,     
    B25127_051,     
    B25127_055, 
    B25127_056,     
    B25127_057,     
    B25127_058, 
    B25127_062, 
    B25127_063,     
    B25127_064,     
    B25127_065,     
    B25127_069,    
    B25127_070,     
    B25127_071,     
    B25127_072,     
    B25127_076, 
    B25127_077,     
    B25127_078,     
    B25127_079, 
    B25127_083,     
    B25127_084,     
    B25127_085, 
    B25127_086  
  ) %>% 
  write_rds(path = here::here("c01-own/data/acs_vars/vars_units1.rds"))
```

#### Units Group 2 - all units, excluding mobile homes

This seems to be exact for all the following cities:

  - Alameda
  - Emeryville
  - Berkeley
  - San Francisco
  - East Palo Alto

<!-- end list -->

``` r
vars_units2 <- 
  vars(
    B25127_047, 
    B25127_048,     
    B25127_049, 
    B25127_050,     
    B25127_051,
    B25127_054, 
    B25127_055,     
    B25127_056, 
    B25127_057, 
    B25127_058, 
    B25127_061,
    B25127_062,     
    B25127_063,
    B25127_064, 
    B25127_065,
    B25127_068, 
    B25127_069, 
    B25127_070, 
    B25127_071,
    B25127_072,     
    B25127_075,
    B25127_076,     
    B25127_077,
    B25127_078,
    B25127_079,
    B25127_082, 
    B25127_083,
    B25127_084,
    B25127_085,
    B25127_086
  ) %>% 
  write_rds(path = here::here("c01-own/data/acs_vars/vars_units2.rds"))
```

#### Units Group 3 - 5 units or more, excluding mobile homes, built before 1999

Note to self: need to revisit this section’s policies. \* Oakland \*
Built before 1996 - *difference of 3 years* \* Redwood City \* *no
difference in units* \* Menlo Park \* *no difference in units*

``` r
vars_units3 <- 
  vars(
    B25127_063,     
    B25127_064,     
    B25127_065,     
    B25127_070, 
    B25127_071, 
    B25127_072, 
    B25127_077, 
    B25127_078, 
    B25127_079,
    B25127_084, 
    B25127_085, 
    B25127_086  
  ) %>% 
  write_rds(path = here::here("c01-own/data/acs_vars/vars_units3.rds"))
```

#### Units Group 4 - 50 units or more, excluding mobile homes

  - Palo Alto
      - 50 units or more - *no difference*

<!-- end list -->

``` r
vars_units4 <- 
  vars(
    B25127_051,     
    B25127_058, 
    B25127_065,
    B25127_072, 
    B25127_079,
    B25127_086
  ) %>% 
  write_rds(path = here::here("c01-own/data/acs_vars/vars_units4.rds"))
```

#### Units Recode Function

This function will apply the correct variable list to the correct city.

``` r
recode_units <- function(city){
  if (
    city %in% 
    c(
      "mountain_view",
      "san_leandro",
      "san_jose"
    )
  ) {
    vars_units1
  } else if (
    city %in% 
    c(
      "alameda",
      "berkeley",
      "san_francisco", 
      "emeryville",
      "east_palo_alto"
    )
  ) {
    vars_units2
  } else if (city %in% c("oakland", "redwood_city", "menlo_park")) {
    vars_units3
  } else if (city %in% c("palo_alto")) {
    vars_units4
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
All selecteted cities are grouped by HUD Metro FMR Areas. Income limits
are determined by household size in addition to AMI.

For each city, I use 2018 ACS data for average household size (table
‘B25010’) and round to the nearest integer to generalize household
size across the entire city, which is necessary for the AMI calculation.
Since the ACS data does not have the same level of precision in incomes,
I make another generalization: I include the difference between the
actual AMI income limit at the average household size and the income
cutoff in the ACS data (see ACS variables below).

NOTE TO SELF: EDA to figure out if it’s worth it to get household size
by census tract.

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
policy says.

#### AMI Group 1 - Less than $100,000 household income

  - Mountain View
      - 120% AMI
      - San Francisco, CA HUD Metro FMR Area
      - 2.3 persons/household ⇒ 2 persons/household
      - $120,180 (120% AMI for 2-person household) ⇒ $100,000
  - Redwood City
      - 80% AMI
      - San Francisco, CA HUD Metro FMR Area
      - 2.8 persons/household ⇒ 3 persons/household
      - $105,700 (80% AMI for 3-person household) ⇒ $100,000

<!-- end list -->

``` r
vars_ami1 <- 
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
  write_rds(path = here::here("c01-own/data/acs_vars/vars_ami1.rds"))
```

#### AMI Group 2 - Less than $75,000 household income

  - Menlo Park
      - 80% AMI
      - San Jose-Sunnyvale-Santa Clara, CA HUD Metro FMR Area
      - 2.55 persons/household ⇒ 3 persons/household
      - $85,050 (80% AMI for 3-person household) ⇒ $75,000

<!-- end list -->

``` r
vars_ami2 <- 
  vars(
    B25118_015,
    B25118_016,
    B25118_017,
    B25118_018,
    B25118_019,
    B25118_020,
    B25118_021,
    B25118_022
  )%>% 
  write_rds(path = here::here("c01-own/data/acs_vars/vars_ami2.rds"))
```

#### AMI Group 3 - No income limits

The following cities do not specify income requirements:

  - Alameda
  - San Leandro
  - Oakland
  - Berkeley
  - San Francisco
  - East Palo Alto
  - Palo Alto
  - San Jose

<!-- end list -->

``` r
vars_ami3 <- 
  vars(B25118_014 )%>% 
  write_rds(path = here::here("c01-own/data/acs_vars/vars_ami3.rds"))
```

#### AMI Recode Function

This function will apply the correct variable list to the correct city.

``` r
recode_ami <- function(city){
  if(
    city %in%
    c(
      "mountain_view",
      "redwood_city",
      "menlo_park"
    )
  ) {
    vars_ami1
  } else if (
    city %in%
    c(
      "menlo_park"
    )
  ) {
    vars_ami2
  } else if (
    city %in%
    c(
      "alameda",
      "san_leandro",
      "oakland",
      "berkeley",
      "san_francisco",
      "east_palo_alto",
      "san_jose"
    )
  ) {
    vars_ami3
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
    ## C:/Users/katea/GitHub/dcl-2020-01/kate/c01-own/data/acs_vars/vars_ami2.rds
    ## C:/Users/katea/GitHub/dcl-2020-01/kate/c01-own/data/acs_vars/vars_ami3.rds
    ## C:/Users/katea/GitHub/dcl-2020-01/kate/c01-own/data/acs_vars/vars_units1.rds
    ## C:/Users/katea/GitHub/dcl-2020-01/kate/c01-own/data/acs_vars/vars_units2.rds
    ## C:/Users/katea/GitHub/dcl-2020-01/kate/c01-own/data/acs_vars/vars_units3.rds
    ## C:/Users/katea/GitHub/dcl-2020-01/kate/c01-own/data/acs_vars/vars_units4.rds
