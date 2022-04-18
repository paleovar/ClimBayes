## code to prepare `schmidt` dataset goes here

load_schmidt <- function(input_volc, input_solar, input_landuse) {
  path = "extdata/schmidt"

  file_ghg <- system.file(
    path,
    "ghg_forcing.txt",
    package = "ClimBayes"
  )

  # Forcing
  # CO2
  forc_data_co2 <- tibble::as_tibble(read.delim(file_ghg,
                                                skip = 1, header = TRUE, sep=""))

  #Volcanic
  file_volc <- system.file(
    path,
    "volcanic_forcing.txt",
    package = "ClimBayes"
  )
  volc <- read.table(file_volc,
                     skip = 1, header = TRUE)
  volc$Year <- volc$year
  volc$year <- NULL
  forc_data_volc <- tibble::as_tibble(volc)

  # Join
  forc_data <- dplyr::left_join(forc_data_co2, forc_data_volc, by = "Year")

  # Solar
  file_solar <- system.file(
    path,
    "solar_forcing.txt",
    package = "ClimBayes"
  )
  forc_data_solar <- tibble::as_tibble(read.delim(file_solar,
                                                  skip = 2, header = TRUE, sep=""))
  # join
  forc_data <- dplyr::left_join(forc_data, forc_data_solar, by = "Year")

  # Land use change
  file_landuse <- system.file(
    path,
    "landuse_forcing.txt",
    package = "ClimBayes"
  )
  land <- read.table(file_landuse, skip = 1, header = TRUE)
  land$Year <- as.double(rownames(land))
  forc_data_land <- tibble::as_tibble(land)

  # Join
  forc_data <- dplyr::left_join(forc_data, forc_data_land, by = "Year")

  # change s.t. baseline is Year 850
  for (col in colnames(forc_data)) {
    if (col == "Year") {
      next
    }
    init = forc_data[[col]][1]
    col_symb <- rlang::sym(col)
    forc_data %>%
      dplyr::mutate("{col}_base850" := (!!col_symb) - init) ->
      forc_data
  }


  forc_ghg <- forc_data[["WMGHG_base850"]]

  if(input_solar == "none") {
    forc_solar <- 0
  } else {
    input_solar <- paste0(input_solar, "_base850")
    forc_solar <- forc_data[[input_solar]]
  }

  if(input_volc == "none") {
    forc_volc <- 0
  } else {
    input_volc <- paste0(input_volc, "_base850")
    forc_volc <- forc_data[[input_volc]]
  }

  if(input_landuse == "none") {
    forc_landuse <- 0
  } else {
    input_landuse <- paste0(input_landuse, "_base850")
    forc_landuse <- forc_data[[input_landuse]]
  }

  forc_df <- tibble::tibble(year = 850:2000,
                            forcing = forc_volc + forc_solar + forc_ghg + forc_landuse)
  schmidt <- forc_df
  return(schmidt)
}

input_volc = "CEA"
input_solar = "SBF"
input_landuse = "none"
schmidt <- load_schmidt(input_volc, input_solar, input_landuse)
usethis::use_data(schmidt, overwrite = TRUE)
