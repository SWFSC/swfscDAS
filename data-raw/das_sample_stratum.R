## code to prepare `das_sample_stratum` dataset goes here

df.out <- data.frame(
  Lon = c(-142, -142, -138, -138, -142),
  Lat = c(42, 44.5, 44.5, 42, 42)
)

write.csv(df.out, file = "inst/das_sample_stratum.csv", row.names = FALSE)

# usethis::use_data(das_sample_stratum, overwrite = TRUE)
