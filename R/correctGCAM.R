#' Correct GCAM road transportation data to iso country.
#'
#' @param x a magpie data object
#' @param subtype One of the possible subtypes, see default argument.
#' @return magclass object
#'
#' @examples
#' \dontrun{
#' a <- readSource("GCAM", subtype="histESdemand")
#' }
#' @author Johanna Hoppe, Alois Dirnaichner
#' @seealso [madrat::readSource()]
#' @import data.table
#' @importFrom rmndt magpie2dt
#' @importFrom magclass as.magpie
correctGCAM <- function(x, subtype) {
  value <- period <- region <- subsector <- technology <- NULL

  switch(
    subtype,
    "histESdemand" = {
      dt <- magpie2dt(x)
      # HSR data decreases significantly in 2005 and falls to zero in 2010
      #-> that is not right and needs to be corrected
      #linear interpolation from first value in 1990 to value in 2015
      dt[period %in% c(2005, 2010) & region == "EU-12" & subsector == "HSR", value := NA]
      dt[region == "EU-12" & subsector == "HSR", value := zoo::na.approx(value, x = period),
         by = c("region", "sector", "subsector", "technology")]
      ## Electric trains do not exist in certain countries and need to be listed as zero demand
      miss <- CJ(region = dt$region, period = dt$period, sector = "trn_freight", unit = "million ton-km",
                 variable = "ES", subsector = "Freight Rail", technology = "Electric",
                 unique = TRUE)
      frTrain <- dt[miss, on = c("region", "period", "sector", "subsector", "technology", "variable", "unit")]
      frTrain[is.na(value), value := 0]
      dt <- rbind(dt[!(subsector == "Freight Rail" & technology == "Electric")], frTrain)
      x <- as.magpie(as.data.frame(dt), temporal = "period", spatial = "region")
    }
  )

  return(x)
}
