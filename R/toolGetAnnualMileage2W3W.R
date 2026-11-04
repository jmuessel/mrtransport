#' Get the annual mileage of two- and three-wheelers
#'
#' Annual mileage used to convert the per-vehicle UCD costs of two- and three-wheelers to per vehicle-km
#' (CAPEXother and nonFuelOPEXother).
#'
#' @details Two-wheelers: UCD's annual mileage. IND's Rickshaw (UCD three-wheelers): annual mileage from
#'   toolAdjustAnnualMileage(), since UCD's three-wheeler mileage equals its two-wheeler value.
#'
#' @param filterEntries list of filters for specific branches in the upper decision tree, containing all associated
#'   univocalNames
#' @param lowResYears years of the cost data
#' @return data.table with the columns region, univocalName, technology, period, annualMileage
#' @author Jarusch Muessel
#' @importFrom rmndt approx_dt magpie2dt
toolGetAnnualMileage2W3W <- function(filterEntries, lowResYears) {
  univocalName <- period <- NULL

  amUCD2W <- toolPrepareUCD(readSource("UCD", "annualMileage"), "annualMileage")
  amUCD2W <- amUCD2W[univocalName %in% filterEntries$trn_pass_road_LDV_2W]
  amUCD2W <- amUCD2W[, c("region", "univocalName", "technology", "period", "value")]
  amUCD2W <- approx_dt(amUCD2W, lowResYears, "period", "value", c("region", "univocalName", "technology"),
                       extrapolate = TRUE)
  setnames(amUCD2W, "value", "annualMileage")
  am3W <- magpie2dt(calcOutput(type = "EdgeTransportSAinputs", subtype = "annualMileage",
                               warnNA = FALSE, aggregate = FALSE))
  am3W <- am3W[univocalName %in% filterEntries$trn_pass_road_LDV_3W & period %in% lowResYears,
               c("region", "univocalName", "technology", "period", "value")]
  setnames(am3W, "value", "annualMileage")
  rbind(amUCD2W, am3W, use.names = TRUE)
}
