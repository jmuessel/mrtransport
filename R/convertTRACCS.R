#' Convert TRACCS road transportation data to iso country.
#'
#' @param x a magpie data object
#' @param subtype One of the possible subtypes, see default argument.
#' @return magpie object
#'
#' @examples
#' \dontrun{
#' a <- readSource("TRACCS")
#' }
#' @author Johanna Hoppe, Alois Dirnaichner
#' @seealso [madrat::readSource()]
#' @importFrom magclass getItems getSets getItems<- getSets<-

convertTRACCS <- function(x, subtype) {

  getItems(x, dim = 1) <- toolCountry2isocode(getItems(x, dim = 1))
  getSets(x)["d1.1"] <- "region"

  if (subtype == "histESdemand") {
    # Convert unit from million (t|p)km to billion (t|p)km
    millionToBillion <- 0.001
    x <- x * millionToBillion
    getItems(x, dim = "unit") <- c("billion tkm/yr", "billion pkm/yr")
  }
  #TRACCS data is provided only for EUR -> toolCountryFill notes that important countries are filled with NA.
  #This is considered, data fo these important countries is taken from other sources. Therefore, the note is supressed
  x <- suppressMessages(toolCountryFill(x, fill = NA))
  return(x)
}
