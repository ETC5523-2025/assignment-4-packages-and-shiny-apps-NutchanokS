## code to prepare `example_data` dataset goes here

set.seed(1)
example_data <- data.frame(
  time    = seq.Date(Sys.Date()-199, Sys.Date(), by = "day"),
  nitrate = round(runif(200, 0, 20), 2),
  site    = factor(sample(c("A","B","C"), 200, TRUE))
)


usethis::use_data(example_data, overwrite = TRUE)
