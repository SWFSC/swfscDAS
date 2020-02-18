# Generate randpicks file to include in package
library(swfscDAS)

d.read <- das_read(system.file("das_sample.das", package = "swfscDAS"))
d.proc <- das_process(d.read, reset.event = FALSE)

d.eff <- das_effort(
  d.proc, method = "equallength", sp.codes = c("016", "018"),
  seg.km = 10
)
d.eff.randpicks <- d.eff[[3]]

write.csv(d.eff.randpicks, file = "inst/das_sample_randpicks.csv", row.names = FALSE)
