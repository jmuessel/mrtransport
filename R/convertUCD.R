#' Convert UCD road transportation data to iso country.
#'
#' @param x a magpie data object
#' @param subtype One of the possible subtypes, see default argument.
#' @return magpie object
#'
#' @examples
#' \dontrun{
#' a <- readSource("UCD")
#' }
#' @author Johanna Hoppe, Alois Dirnaichner
#' @seealso [madrat::readSource()]
#' @importFrom data.table fread
#' @importFrom magclass getItems getSets getItems<- getSets<-
#'
convertUCD <- function(x, subtype) {

  gdp <- calcOutput("GDP", scenario = "SSP2", aggregate = FALSE)[, getYears(x), ]

  UCD2isoMapFile <- system.file("extdata", "isoUCD.csv", package = "mrtransport", mustWork = TRUE)
  UCD2iso <- fread(UCD2isoMapFile, skip = 0)

  if (subtype == "feDemand") {
    x <- toolAggregate(x, rel = UCD2iso, weight = gdp)
    # Convert PJ to MJ
    PJToMJ <- 1e9
    x <- x * PJToMJ
    getItems(x, dim = "unit") <- "MJ"

  } else if (subtype == "nonMotorizedDemand") {
    x <- toolAggregate(x, rel = UCD2iso, weight = gdp)

  } else if (subtype %in% c("CAPEX", "nonFuelOPEX", "CAPEXandNonFuelOPEX", "OperatingSubsidies")) {
    x <- toolAggregate(x, rel = UCD2iso)
    x <- GDPuc::toolConvertGDP(
      gdp = x,
      unit_in = "constant 2005 US$MER",
      unit_out = mrdrivers::toolGetUnitDollar(),
      replace_NAs = "with_USA"
    )

    monUnit <- gsub(".*?(\\d{4}).*", "US$\\1", mrdrivers::toolGetUnitDollar())
    magclass::getNames(x) <- gsub("US\\$2005", monUnit, magclass::getNames(x))

  } else {
    x <- toolAggregate(x, rel = UCD2iso)
  }

  getSets(x)["d1.1"] <- "region"
  x
}
