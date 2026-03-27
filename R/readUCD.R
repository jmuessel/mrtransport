#' Read UCD road transportation data.
#'
#'
#' @param subtype One of the possible subtypes, see default argument.
#' @return magclass object
#'
#' @examples
#' \dontrun{
#' a <- readSource("UCD")
#' }
#' @author Johanna Hoppe, Alois Dirnaichner
#' @seealso [madrat::readSource()]
#' @import data.table
#' @importFrom magclass as.magpie
#'
readUCD <- function(subtype = c(
                                "energyIntensity", "feDemand", "loadFactor", "annualMileage",
                                "nonMotorizedDemand", "speed", "CAPEX", "nonFuelOPEX",
                                "CAPEXandNonFuelOPEX", "OperatingSubsidies")) {
  variable <- unit <- UCD_sector <- NULL

  UCD <- fread("UCD_transportation_database.csv", header = TRUE)
  switch(
    subtype,
    "energyIntensity" = {
      dt <- UCD[variable == "intensity"]
      dt[, variable := "Energy intensity"]
      dt[, unit := "MJ/vehkm"]
    },
    "feDemand" = {
      #fe data is given only for 2005
      dt <- UCD[variable == "energy"]
      dt[, unit := "PJ/yr"]
    },
    "loadFactor" = {
      dt <- UCD[variable == "load factor"]
      dt[, variable := "Load factor"]
      dt[UCD_sector == "Passenger", unit := "p/veh"]
      dt[UCD_sector == "Freight", unit := "t/veh"]
    },
    "annualMileage" = {
      dt <- UCD[variable == "annual travel per vehicle"]
      dt[, variable := "Annual mileage"]
      dt[, unit := "vehkm/veh yr"]
    },
    "nonMotorizedDemand" = {
      dt <- UCD[variable == "service output"]
      dt[, variable := "ES"]
      dt[, unit := "bn pkm/yr"]
    },
    "speed" = {
      dt <- UCD[variable == "speed"]
      dt[, variable := "Speed"]
      dt[unit, "km/h"]
    },
    "CAPEX" = {
      #Either CAPEX are reported in detail or as totals (never in detail + totals)
      dt <- UCD[variable %in% c("CAPEX", "Capital costs (infrastructure)", "Capital costs (other)",
                                "Capital costs (purchase)", "Capital costs (total)")]
      dt[unit == "2005$/vkt", unit := "US$2005/vehkm"]
      dt[unit == "2005$/veh", unit := "US$2005/veh"]
    },
    "nonFuelOPEX" = {
      #Either non fuel OPEX are reported in detail or as totals (never in detail + totals)
      dt <- UCD[variable %in% c("non-fuel OPEX", "Operating costs (maintenance)",
                                "Operating costs (registration and insurance)",
                                "Operating costs (tolls)", "Operating costs (total non-fuel)")]

      dt[unit == "2005$/vkt", unit := "US$2005/vehkm"]
      dt[unit == "2005$/veh/yr", unit := "US$2005/veh yr"]
    },
    "CAPEXandNonFuelOPEX" = {
      dt <- UCD[variable %in% c("CAPEX and non-fuel OPEX")]
      dt[unit == "2005$/vkt", unit := "US$2005/vehkm"]
    },
    "OperatingSubsidies" = {
      dt <- UCD[variable == "Operating subsidy"]
      dt[, unit := "US$2005/vehkm"]
    }
  )

  dt <- data.table::melt(dt, id.vars = c("UCD_region", "UCD_sector", "mode", "size.class", "UCD_technology",
                                         "UCD_fuel", "variable", "unit"), variable.name = "period")
  setnames(dt, "size.class", "size_class")
  return(as.magpie(dt, spatial = "UCD_region", temporal = "period"))

}
