## code to prepare `das_sample_stratum` dataset goes here
# Note will need to update this is a new das_sample file is created because of random numbers

df.out <- data.frame(
  Lon = c(-137.2, -137.2, -135, -135, -137.2),
  Lat = c(40, 42, 42, 40, 40)
)

write.csv(df.out, file = "inst/das_sample_stratum.csv", row.names = FALSE)



# usethis::use_data(das_sample_stratum, overwrite = TRUE)
