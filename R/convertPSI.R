#' Convert PSI data to iso country.
#'
#' @param x a magpie data object
#' @param subtype One of the possible subtypes, see default argument.
#' @return magpie object
#'
#' @examples
#' \dontrun{
#' a <- readSource("PSI", subtype = "costs")
#' }
#' @author Johanna Hoppe
#' @seealso [madrat::readSource()]
#' @import data.table
#' @importFrom magclass getItems getItems<-
#' @importFrom rmndt magpie2dt
convertPSI <- function(x, subtype) {
  region <- NULL

  #PSI data is mapped on iso countries - Note that we do not have regionally differentiated data
  PSI2isoMapFile <- system.file("extdata", "regionmappingISOto21to12.csv",
                                package = "mrtransport", mustWork = TRUE)
  PSI2iso <- fread(PSI2isoMapFile, skip = 0)
  setnames(PSI2iso, c("countryCode"), c("iso"))
  PSI2iso <- PSI2iso[, c("iso")][, region := "GLO"]
  #Here data is mapped on all iso countries -> same value according to vehicle class/technology/year
  x <- toolAggregate(x, rel = PSI2iso)

  switch(
    subtype,
    "CAPEX" = {

      # PSI CAPEX need to be transformed from EUR 2017 to USD
      x <- GDPuc::toolConvertGDP(
        gdp = x,
        unit_in = "constant 2017 EUR",
        unit_out = mrdrivers::toolGetUnitDollar(),
        replace_NAs = "with_USA"
      )
      monUnit <- gsub(".*?(\\d{4}).*", "US$\\1", mrdrivers::toolGetUnitDollar())
      getItems(x, dim = "unit") <- paste0(monUnit, "/veh")
    },
    "energyIntensity" = {
      #PSI energy intensity needs to be transformed to MJ/vehkm
      kJPerVehkmtoMJperVehkm <- 1e-3
      x <- x * kJPerVehkmtoMJperVehkm
      getItems(x, dim = "unit") <- "MJ/vehkm"
    }
  )
  return(x)
}
