# Join hourly Water Quality (DP1.20288.001) + Nitrate (DP1.20033.001)
library(dplyr)
library(lubridate)
library(usethis)

# Load processed datasets produced by prepare_* scripts
load("data/wq_hourly.rda")
load("data/nitrate_hourly.rda")

# Standardise keys and remove any duplicates (paranoia-safety)
wq_hourly <- wq_hourly %>%
  mutate(
    site = as.character(site),
    sensor = as.character(sensor),
    date_time = force_tz(as.POSIXct(date_time, tz = "UTC"), tzone = "UTC")
  ) %>%
  distinct()

nitrate_hourly <- nitrate_hourly %>%
  mutate(
    site = as.character(site),
    sensor = as.character(sensor),
    date_time = force_tz(as.POSIXct(date_time, tz = "UTC"), tzone = "UTC")
  ) %>%
  distinct()

# Full join (keep hours where either dataset has values)
wq_nitrate <- full_join(
  wq_hourly,
  nitrate_hourly,
  by = c("site", "sensor", "date_time")
) %>%
  arrange(site, sensor, date_time)

# If any accidental duplicates slipped in, collapse them
wq_nitrate <- wq_nitrate %>%
  group_by(site, sensor, date_time) %>%
  summarise(
    cond_uScm     = mean(cond_uScm, na.rm = TRUE),
    do_mgL        = mean(do_mgL,    na.rm = TRUE),
    turb_FNU      = mean(turb_FNU,  na.rm = TRUE),
    nitrate_umolL = mean(nitrate_umolL, na.rm = TRUE),
    .groups = "drop"
  )

# Save for the installed package
usethis::use_data(wq_nitrate, overwrite = TRUE)

message(
  "Saved data/wq_nitrate.rda  rows=", nrow(wq_nitrate),
  "sites=", dplyr::n_distinct(wq_nitrate$site),
  "sensors=", dplyr::n_distinct(wq_nitrate$sensor),
  "coverage: ",
  round(mean(!is.na(wq_nitrate$nitrate_umolL)), 3), "nitrate present;",
  round(mean(!is.na(wq_nitrate$cond_uScm)), 3), "WQ present."
)
