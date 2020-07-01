## code to prepare `das_sample_stratum` dataset goes here
# Note will need to update this is a new das_sample file is created because of random numbers

df.out <- data.frame(
  Lon = c(-138, -138, -136.5, -136.5, -138),
  Lat = c(39, 40.7, 40.7, 39, 39)
)

write.csv(df.out, file = "inst/das_sample_stratum.csv", row.names = FALSE)

# usethis::use_data(das_sample_stratum, overwrite = TRUE)
