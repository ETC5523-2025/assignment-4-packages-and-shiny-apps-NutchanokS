#' Hourly water quality by site
#'
#' High-frequency water-quality time series collected from in-situ sensors.
#'
#' @format A tibble/data frame with N rows and 6 variables:
#' \describe{
#'   \item{date_time}{POSIXct. Timestamp in local time or UTC (state which one).}
#'   \item{site}{chr. Site identifier (e.g., "XYZ").}
#'   \item{sensor}{chr. Sensor identifier or type.}
#'   \item{cond_uScm}{dbl. Specific conductivity (µS/cm).}
#'   \item{do_mgL}{dbl. Dissolved oxygen (mg/L).}
#'   \item{turb_FNU}{dbl. Turbidity (FNU).}
#' }
#' @source NEON products (list product codes + your processing steps).
#' @docType data
#' @name wq_hourly
#' @usage data(wq_hourly)
#' @keywords datasets
NULL

#' Hourly nitrate by site
#'
#' @format A tibble/data frame with N rows and 4 variables:
#' \describe{
#'   \item{date_time}{POSIXct. Timestamp.}
#'   \item{site}{chr. Site identifier.}
#'   \item{sensor}{chr. Sensor identifier or type.}
#'   \item{nitrate_umolL}{dbl. Nitrate concentration (µmol/L).}
#' }
#' @source NEON products (codes ...)
#' @docType data
#' @name nitrate_hourly
#' @usage data(nitrate_hourly)
#' @keywords datasets
NULL

#' Joined hourly water quality and nitrate data
#'
#' Combines NEON water-quality (DP1.20288.001) and nitrate (DP1.20033.001) products.
#' Nitrate data are available primarily for downstream sensors; upstream sensors may have missing values.
#'
#' @format A data frame with columns:
#' \describe{
#'   \item{site}{Site code (4-letter).}
#'   \item{sensor}{Sensor position: "S1_upstream" or "S2_downstream".}
#'   \item{date_time}{Hourly timestamp (UTC).}
#'   \item{cond_uScm}{Specific conductivity (µS/cm).}
#'   \item{do_mgL}{Dissolved oxygen (mg/L).}
#'   \item{turb_FNU}{Turbidity (FNU).}
#'   \item{nitrate_umolL}{Nitrate concentration (µmol/L). May be missing for upstream sensors.}
#' }
#' @source NEON data products DP1.20288.001 and DP1.20033.001.
#' @docType data
#' @name wq_nitrate
NULL
