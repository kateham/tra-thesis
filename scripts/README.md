# Scripts

# cl_filter.R
# Takes corelogic_tax_bayarea.rds located in Kate's personal AFS folder
# and filters it into smaller rds files for each of the Bay Area's 9 counties.

# boundaries_city.R
# Gets selected group of city boundaries using tigris package.

# boundaries_cbg.R
# Gets all census block groups from 9 Bay Area counties using tigris package.

# boundaries_cbg_city_redwood.R
# Spatially estimates which block groups are within municipal boundaries. Currently
# only reproducible for Redwood City.

# cl_city_redwood.R
# Filters for Redwood City Corelogic data using spatial filter (st_within) on
# county Corelogic data and city boundaries.

# acs_rwc.R
# Downloads ACS 2018 5-year Household Income Data for Redwood City. Currently
# only functions for Redwood City. Smallest geography level is block groups.
