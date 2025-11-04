# Build compact hourly NEON Water Quality (DP1.20288.001) dataset
library(dplyr)
library(readr)
library(stringr)
library(lubridate)
library(purrr)
library(usethis)

# Adjust this path if you want to rebuild the dataset locally:
base_dir_wq <- "C:/Users/User/2025_S02/ETC5523/NEON_water-quality/NEON_water-quality"
if (!dir.exists(base_dir_wq)) {
  stop("Raw NEON folders not found. Please update 'base_dir_wq' to your local path before re-running.")
}

# Find WQ CSVs (instantaneous/basic)
wq_all_csv <- list.files(base_dir_wq, pattern = "\\.csv$", recursive = TRUE, full.names = TRUE)
wq_csv <- wq_all_csv[str_detect(wq_all_csv, regex("waq_instantaneous.*basic", ignore_case = TRUE))]
if (!length(wq_csv)) stop("No 'waq_instantaneous.*basic' CSVs found under: ", base_dir_wq)
message("Found ", length(wq_csv), " water-quality CSVs")

# Robust reader (lock types + NA tokens) and tag site + sensor
read_wq <- function(fp){
  site <- str_match(fp, "NEON\\.D\\d{2}\\.(\\w{4})\\.")[,2]
  sensor <- case_when(
    str_detect(fp, "\\.101\\.") ~ "S1_upstream",
    str_detect(fp, "\\.102\\.") ~ "S2_downstream",
    TRUE ~ "S_unknown"
  )

  dat <- read_csv(
    fp,
    na = c("", "NA", "NaN", "nan", "-9999", "-9999.0", "-7777"),
    guess_max = 100000,
    col_types = cols(
      startDateTime              = col_character(),
      specificConductance        = col_double(),
      specificCondFinalQF        = col_integer(),
      dissolvedOxygen            = col_double(),
      dissolvedOxygenFinalQF     = col_integer(),
      turbidity                  = col_double(),
      turbidityFinalQF           = col_integer()
    )
  )

  transmute(
    dat,
    site,
    sensor,
    datetime  = ymd_hms(startDateTime, tz = "UTC", quiet = TRUE),
    cond_uScm = specificConductance,
    cond_qf   = specificCondFinalQF,
    do_mgL    = dissolvedOxygen,
    do_qf     = dissolvedOxygenFinalQF,
    turb_FNU  = turbidity,
    turb_qf   = turbidityFinalQF
  )
}

wq_raw <- map_dfr(wq_csv, read_wq)
message("Rows combined: ", nrow(wq_raw), " across ", n_distinct(wq_raw$site), " site(s)")

# Conditional QC: if any good (0) flags exist, enforce them; else keep all data
good_cond <- any(wq_raw$cond_qf == 0, na.rm = TRUE)
good_do   <- any(wq_raw$do_qf   == 0, na.rm = TRUE)
good_turb <- any(wq_raw$turb_qf == 0, na.rm = TRUE)

wq_qc <- wq_raw |> filter(!is.na(datetime))
if (good_cond || good_do || good_turb) {
  wq_qc <- wq_qc |>
    filter(
      (is.na(cond_qf) | cond_qf == 0 | !good_cond),
      (is.na(do_qf)   | do_qf   == 0 | !good_do),
      (is.na(turb_qf) | turb_qf == 0 | !good_turb)
    )
}
wq_qc <- select(wq_qc, -cond_qf, -do_qf, -turb_qf)

# Aggregate to hourly and rename time column
wq_hourly <- wq_qc |>
  mutate(date_time = floor_date(datetime, "hour")) |>
  group_by(site, sensor, date_time) |>
  summarise(
    cond_uScm = mean(cond_uScm, na.rm = TRUE),
    do_mgL    = mean(do_mgL,    na.rm = TRUE),
    turb_FNU  = mean(turb_FNU,  na.rm = TRUE),
    .groups = "drop"
  )

# 5) Save for the package
usethis::use_data(wq_hourly, overwrite = TRUE)
message("Saved data/wq_hourly.rda (rows=", nrow(wq_hourly),
        ", sites=", n_distinct(wq_hourly$site),
        ", sensors=", n_distinct(wq_hourly$sensor), ").")
