temp_hadcrut1 <- read.csv("inst/extdata/hadcrut/HadCRUT.5.0.1.0.analysis.summary_series.global.annual.csv",
                          sep = ",", header = TRUE)
hadcrut5 <- tibble::tibble(year = temp_hadcrut1$Time,
                           temperature = temp_hadcrut1$Anomaly..deg.C.)
hadcrut = hadcrut5
usethis::use_data(hadcrut, overwrite = TRUE)
