## code to prepare `hadcrut` dataset goes here
file <- system.file(
  "extdata",
  "hadcrut/HadCRUT.4.6.0.0.annual_ns_avg.txt",
  package = "ClimBayes"
)
temp_hadcrut1 <- read.csv(file, sep = "", header = FALSE)
hadcrut <- tibble::tibble(year = temp_hadcrut1$V1,
                               temperature = temp_hadcrut1$V2)

usethis::use_data(hadcrut, overwrite = TRUE)
