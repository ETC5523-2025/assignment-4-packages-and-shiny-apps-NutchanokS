# Build compact hourly NEON Nitrate (DP1.20033.001) dataset
library(dplyr)
library(readr)
library(stringr)
library(lubridate)
library(purrr)
library(usethis)

# Adjust this path if you want to rebuild the dataset locally:
base_dir_nitrate <- "C:/Users/User/2025_S02/ETC5523/NEON_nitrate-surfacewater/NEON_nitrate-surfacewater"
if (!dir.exists(base_dir_nitrate)) {
  stop("Raw NEON nitrate folders not found. Please update 'base_dir_nitrate' before re-running.")
}

# Find Nitrate CSVs (NSW_15_minute/basic)
nitrate_all_csv <- list.files(base_dir_nitrate, pattern = "\\.csv$", recursive = TRUE, full.names = TRUE)
nitrate_csv <- nitrate_all_csv[str_detect(nitrate_all_csv, regex("NSW_15_minute.*basic", ignore_case = TRUE))]
if (!length(nitrate_csv)) stop("No 'NSW_15_minute.*basic' nitrate CSVs found under: ", base_dir_nitrate)
message("Found ", length(nitrate_csv), " nitrate CSVs")

# Robust reader and tag site + sensor
read_nitrate <- function(fp){
  site <- str_match(fp, "NEON\\.D\\d{2}\\.(\\w{4})\\.")[,2]
  sensor <- case_when(
    str_detect(fp, "\\.101\\.") ~ "S1_upstream",
    str_detect(fp, "\\.102\\.") ~ "S2_downstream",
    TRUE ~ "S_unknown"
  )

  dat <- read_csv(
    fp,
    na = c("", "NA", "NaN", "nan", "-9999", "-9999.0"),
    guess_max = 100000,
    col_types = cols(
      startDateTime        = col_character(),
      surfWaterNitrateMean = col_double(),
      finalQF              = col_integer()
    )
  )

  transmute(
    dat,
    site,
    sensor,
    datetime      = ymd_hms(startDateTime, tz = "UTC", quiet = TRUE),
    nitrate_umolL = surfWaterNitrateMean,
    nitrate_qf    = finalQF
  )
}

nitrate_raw <- map_dfr(nitrate_csv, read_nitrate)
message("Rows combined: ", nrow(nitrate_raw), " across ", n_distinct(nitrate_raw$site), " site(s)")

# Conditional QC (keep QF==0 when available)
good_nitrate <- any(nitrate_raw$nitrate_qf == 0, na.rm = TRUE)

nitrate_qc <- nitrate_raw |> filter(!is.na(datetime))
if (good_nitrate) {
  nitrate_qc <- nitrate_qc |> filter(is.na(nitrate_qf) | nitrate_qf == 0)
}
nitrate_qc <- select(nitrate_qc, -nitrate_qf)

# Aggregate to hourly and rename time column
nitrate_hourly <- nitrate_qc |>
  mutate(date_time = floor_date(datetime, "hour")) |>
  group_by(site, sensor, date_time) |>
  summarise(
    nitrate_umolL = mean(nitrate_umolL, na.rm = TRUE),
    .groups = "drop"
  )

# Save for the package
usethis::use_data(nitrate_hourly, overwrite = TRUE)
message("Saved data/nitrate_hourly.rda (rows=", nrow(nitrate_hourly),
        ", sites=", n_distinct(nitrate_hourly$site),
        ", sensors=", n_distinct(nitrate_hourly$sensor), ").")
