library(tidytable)

if(!dir.exists(here::here("data"))) {dir.create(here::here("data"))}


options(timeout = 600)
if(!file.exists(here::here("data", "npdes_downloads.zip"))) { 
  download.file("https://echo.epa.gov/files/echodownloads/npdes_downloads.zip",
                destfile = here::here("data", "npdes_downloads.zip"))
  
  unzip(here::here("data", "npdes_downloads.zip"),
        files = c("ICIS_PERMITS.csv", "NPDES_PERM_COMPONENTS.csv"),
        exdir = here::here("data"))
}

if(!file.exists(here::here("data", "ICIS_FACILITIES.csv"))) {
  
  unzip(here::here("data", "npdes_downloads.zip"),
        files = c("ICIS_FACILITIES.csv"),
        exdir = here::here("data"))
  
}

npdes_perm_components <- fread(here::here("data", "NPDES_PERM_COMPONENTS.csv"))

npdes_perm_components |>
  group_by(COMPONENT_TYPE_CODE, COMPONENT_TYPE_DESC) |>
  count(sort = TRUE)

npdes_perm_components <- npdes_perm_components |>
  filter(COMPONENT_TYPE_CODE %in% c("SWS", "SWM"))

icis_permits <- fread(here::here("data", "ICIS_PERMITS.csv"))

icis_permits <- icis_permits |>
  left_join(npdes_perm_components) |>
  filter(!is.na(COMPONENT_TYPE_CODE))

icis_permits |>
  group_by(EXTERNAL_PERMIT_NMBR) |>
  summarise(num_versions = n()) |>
  ungroup() |>
  summarise(mean_versions = mean(num_versions))

icis_permits_large <- icis_permits |>
  filter(COMPONENT_TYPE_CODE == "SWM") |>
  select(EXTERNAL_PERMIT_NMBR, ORIGINAL_ISSUE_DATE) |>
  distinct() |>
  mutate(original_date = lubridate::mdy(ORIGINAL_ISSUE_DATE)) |>
  arrange(original_date)


icis_permits_small <- icis_permits |>
  filter(COMPONENT_TYPE_CODE == "SWS") |>
  select(EXTERNAL_PERMIT_NMBR, ORIGINAL_ISSUE_DATE) |>
  distinct() |>
  mutate(original_date = lubridate::mdy(ORIGINAL_ISSUE_DATE)) |>
  arrange(original_date)

icis_facilities <- fread(here::here("data", "ICIS_FACILITIES.csv"))

icis_facilities <- icis_facilities |>
  left_join(npdes_perm_components, by = c("NPDES_ID" = "EXTERNAL_PERMIT_NMBR")) |>
  filter(!is.na(COMPONENT_TYPE_CODE))

sum(is.na(icis_facilities$GEOCODE_LATITUDE))
sum(is.na(icis_facilities$GEOCODE_LONGITUDE))


## now get discharge point data

if(!file.exists(here::here("data", "npdes_outfalls_layer.zip"))) {
  
  download.file("https://echo.epa.gov/files/echodownloads/npdes_outfalls_layer.zip",
                destfile = here::here("data", "npdes_outfalls_layer.zip"))
  
}

npdes_outfalls <- fread(here::here("data", "npdes_outfalls_layer.zip"))

npdes_outfalls <- npdes_outfalls |>
  left_join(npdes_perm_components) |>
  filter(!is.na(COMPONENT_TYPE_CODE))


sum(is.na(npdes_outfalls$GEOCODE_LATITUDE))
sum(is.na(npdes_outfalls$GEOCODE_LONGITUDE))
length(unique(npdes_outfalls$EXTERNAL_PERMIT_NMBR))
