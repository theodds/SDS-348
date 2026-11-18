## install.packages(c("sf", "mgcv", "patchwork", "ggrepel"))
library(tidyverse)
library(sf)
library(patchwork)

## Each source is downloaded once and saved as a small Texas-only file
tx_dir <- "data/texas-covid"
dir.create(tx_dir, showWarnings = FALSE, recursive = TRUE)
options(timeout = 600)

download_temp <- function(url) {
  path <- tempfile()
  download.file(url, path, mode = "wb", quiet = TRUE)
  path
}

## Read a saved file, creating it with make_file() if it doesn't exist yet
read_saved <- function(file, make_file) {
  path <- file.path(tx_dir, file)
  if (!file.exists(path)) write_csv(make_file(), path)
  read_csv(path, col_types = cols(fips = "c"))
}

## (1) Cumulative COVID-19 deaths by county on Dec 31, 2020 (NYT)
tx_deaths <- read_saved("tx-covid-deaths-2020.csv", function() {
  "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties-2020.csv" %>%
    download_temp() %>%
    read_csv(col_types = cols(fips = "c")) %>%
    filter(state == "Texas", date == as.Date("2020-12-31"), !is.na(fips)) %>%
    select(fips, deaths)
})

## (2) County population by age group, July 1, 2020 (Census Bureau)
tx_age <- read_saved("tx-population-by-age-2020.csv", function() {
  paste0(
    "https://www2.census.gov/programs-surveys/popest/datasets/",
    "2020-2021/counties/asrh/cc-est2021-agesex-48.csv"
  ) %>%
    download_temp() %>%
    read_csv(col_types = cols(STATE = "c", COUNTY = "c")) %>%
    filter(YEAR == 2) %>%  # YEAR = 2 is the July 1, 2020 estimate
    transmute(
      fips = paste0(STATE, COUNTY),
      name = str_remove(CTYNAME, " County$"),
      `0-24` = AGE04_TOT + AGE59_TOT + AGE1014_TOT + AGE1519_TOT + AGE2024_TOT,
      `25-34` = AGE2529_TOT + AGE3034_TOT,
      `35-44` = AGE3539_TOT + AGE4044_TOT,
      `45-54` = AGE4549_TOT + AGE5054_TOT,
      `55-64` = AGE5559_TOT + AGE6064_TOT,
      `65-74` = AGE6569_TOT + AGE7074_TOT,
      `75-84` = AGE7579_TOT + AGE8084_TOT,
      `85+` = AGE85PLUS_TOT
    ) %>%
    pivot_longer(-c(fips, name), names_to = "age", values_to = "n")
})

## (3) Texas COVID-19 deaths in 2020 by age group (CDC/NCHS). Deaths under
## age 5 are suppressed because there were so few, so "0-24" uses ages 5-24.
tx_age_deaths <- read_saved("tx-covid-deaths-by-age-2020.csv", function() {
  age_lookup <- tibble(
    age_group = c("5-14 years", "15-24 years", "25-34 years", "35-44 years",
                  "45-54 years", "55-64 years", "65-74 years", "75-84 years",
                  "85 years and over"),
    age = c("0-24", "0-24", "25-34", "35-44", "45-54", "55-64", "65-74",
            "75-84", "85+")
  )
  paste0(
    "https://data.cdc.gov/resource/9bhg-hcku.csv?state=Texas&year=2020",
    "&sex=All%20Sexes&$where=%60group%60%3D%27By%20Year%27"
  ) %>%
    download_temp() %>%
    read_csv(show_col_types = FALSE) %>%
    inner_join(age_lookup, by = "age_group") %>%
    group_by(age) %>%
    summarise(deaths = sum(covid_19_deaths))
})

## (4) County characteristics (County Health Rankings, 2022 release)
tx_chr <- read_saved("tx-county-health-rankings-2022.csv", function() {
  paste0(
    "https://www.countyhealthrankings.org/sites/default/files/",
    "media/document/analytic_data2022.csv"
  ) %>%
    download_temp() %>%
    read_csv(  # the first row holds long variable names, so skip it
      skip = 1,
      col_types = cols(statecode = "c", countycode = "c", fipscode = "c")
    ) %>%
    filter(statecode == "48", countycode != "000") %>%
    transmute(
      fips = fipscode,
      uninsured = v085_rawvalue,  # fraction of people under 65 uninsured
      rural = v058_rawvalue,      # fraction of residents in rural areas
      income = v063_rawvalue,     # median household income
      hispanic = v056_rawvalue    # fraction Hispanic
    )
})

## (5) County boundaries (Census cartographic boundary file)
tx_shape_file <- file.path(tx_dir, "tx-counties.gpkg")
if (!file.exists(tx_shape_file)) {
  shape_dir <- tempfile()
  "https://www2.census.gov/geo/tiger/GENZ2020/shp/cb_2020_us_county_20m.zip" %>%
    download_temp() %>%
    unzip(exdir = shape_dir)
  st_read(file.path(shape_dir, "cb_2020_us_county_20m.shp"), quiet = TRUE) %>%
    filter(STATEFP == "48") %>%
    transmute(fips = GEOID) %>%
    st_write(tx_shape_file, quiet = TRUE)
}

## Put everything together, one row per county
tx_pop <- tx_age %>%
  group_by(fips, name) %>%
  summarise(pop = sum(n), .groups = "drop")

tx <- st_read(tx_shape_file, quiet = TRUE) %>%
  left_join(tx_pop, by = "fips") %>%
  left_join(tx_deaths, by = "fips") %>%
  left_join(tx_chr, by = "fips") %>%
  mutate(rate = 1e5 * deaths / pop) %>%
  arrange(fips) %>%
  st_transform(3083)  # Texas-centric Albers equal-area projection

## Shared pieces for the maps
tx_cities <- tibble(
  city = c("Houston", "Dallas", "Austin", "San Antonio", "El Paso",
           "Lubbock", "Amarillo", "Laredo", "McAllen"),
  long = c(-95.37, -96.80, -97.74, -98.49, -106.49, -101.86, -101.83,
           -99.48, -98.23),
  lat = c(29.76, 32.78, 30.27, 29.42, 31.76, 33.58, 35.22, 27.53, 26.20)
) %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
  st_transform(3083)

city_layers <- list(
  geom_sf(data = tx_cities, size = 0.8),
  geom_sf_text(data = tx_cities, aes(label = city), size = 2.6,
               nudge_y = 30000, fontface = "bold")
)

county_layer <- function(mapping, ...) {
  geom_sf(mapping, color = "grey60", linewidth = 0.1, ...)
}

## Ratios like the SMR are mapped on the log scale, clipped to [1/4, 4]
clip_log <- function(x) log(pmin(pmax(x, 1 / 4), 4))

smr_scale <- function(name = "SMR") {
  scale_fill_gradient2(
    low = "#2166AC",
    mid = "grey97",
    high = "#B2182B",
    midpoint = 0,
    limits = log(c(1 / 4, 4)),
    breaks = log(c(1 / 4, 1 / 2, 1, 2, 4)),
    labels = c("1/4", "1/2", "1", "2", "4"),
    name = name
  )
}
