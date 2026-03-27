#' Convert Eurostat road transportation data to iso country.
#'
#' @param x a magpie data object
#' @param subtype One of the possible subtypes, see default argument.
#' @return magpie object
#'
#' @examples
#' \dontrun{
#' a <- readSource("Eurostat")
#' }
#' @author Johanna Hoppe
#' @seealso [madrat::readSource()]
#' @importFrom magclass getItems getSets getItems<- getSets<-

convertEurostatEnergyCountryDataSheets <- function(x, subtype) {
  getItems(x, dim = 1) <- toolCountry2isocode(getItems(x, dim = 1), mapping = c("EL" = "GRC"))
  getSets(x)["d1.1"] <- "region"
  #Convert Mtoe to MJ#
  if (subtype == "feDemand") {
    MtoeToMJ <- 41868000000
    x <- x * MtoeToMJ
    getItems(x, dim = "unit") <- "MJ"
  }
  if (subtype == "LDVfleet") {
    x <- suppressMessages(toolCountryFill(x, fill = 0))
  } else {
    x <- suppressMessages(toolCountryFill(x, fill = NA))
  }

  return(x)
}
