
# CENSUS KEY: get a free key at https://api.census.gov/data/key_signup.html

library(tidycensus)
library(tidyverse)


CENSUS_KEY <- # Paste Census API key here
census_api_key(CENSUS_KEY, overwrite = TRUE)

# All 50 state FIPS codes
fips_codes <- c(
  "01","02","04","05","06","08","09","10","12","13","15","16",
  "17","18","19","20","21","22","23","24","25","26","27","28",
  "29","30","31","32","33","34","35","36","37","38","39","40",
  "41","42","44","45","46","47","48","49","50","51","53","54",
  "55","56"
)

# Tract-level total population 
# P1_001N = total population from 2020 Census PL 94-171 redistricting file
# geometry = TRUE so we can spatially join to the tract geometry already used by the redistricting map ensuring a perfect 1:1 match by GEOID
cat("=== Downloading tract populations ===\n")
cat("Expected time: ~10 minutes.\n\n")

tract_pops <- map_dfr(fips_codes, function(fips) {
  cat(sprintf("[....] FIPS %s...", fips)); flush.console()

  result <- tryCatch({
    get_decennial(
      geography = "tract",
      variables = "P1_001N",      # total population, PL 94-171
      state     = fips,
      year      = 2020,
      geometry  = TRUE            # keeps TIGER tract geometry for verification
    ) %>%
      rename(pop_tract = value) %>%
      select(GEOID, pop_tract, geometry)
  }, error = function(e) {
    cat(sprintf("\n[FAIL] FIPS %s: %s\n", fips, e$message))
    NULL
  })

  if (!is.null(result)) cat(sprintf(" done (%d tracts)\n", nrow(result)))
  Sys.sleep(1)
  result
})

saveRDS(tract_pops, "tract_pops.rds")
cat(sprintf("\ntract_pops.rds saved (%d tracts, %.1f MB)\n",
            nrow(tract_pops),
            file.size("tract_pops.rds") / 1e6))

# Tract-level minority VAP
# Same geography as tract_pops — joins cleanly on GEOID with no spatial join
# P4_001N = Total VAP
# P4_004N = Non-Hispanic Black or African American alone VAP
# P4_010N = Hispanic or Latino VAP
cat("\n=== Downloading minority VAP by tract ===\n")
cat("Expected time: ~10 minutes.\n\n")

minority_vap <- map_dfr(fips_codes, function(fips) {
  cat(sprintf("[....] FIPS %s...", fips)); flush.console()

  result <- tryCatch({
    get_decennial(
      geography  = "tract",
      variables  = c(total_vap    = "P4_001N",
                     black_vap    = "P4_004N",
                     hispanic_vap = "P4_010N"),
      state      = fips,
      year       = 2020,
      output     = "wide",
      geometry   = FALSE        
    ) %>%
      mutate(
        minority_vap      = black_vap + hispanic_vap,
        minority_vap_frac = minority_vap / pmax(total_vap, 1)
      ) %>%
      select(GEOID, total_vap, black_vap, hispanic_vap,
             minority_vap, minority_vap_frac)
  }, error = function(e) {
    cat(sprintf("\n[FAIL] FIPS %s: %s\n", fips, e$message))
    NULL
  })

  if (!is.null(result)) cat(sprintf(" done (%d tracts)\n", nrow(result)))
  Sys.sleep(1)
  result
})

saveRDS(minority_vap, "minority_vap.rds")
cat(sprintf("\nminority_vap.rds saved (%d tracts, %.1f MB)\n",
            nrow(minority_vap),
            file.size("minority_vap.rds") / 1e6))
