#' Read PSI data.
#'
#'
#' @param subtype One of the possible subtypes, see default argument.
#' @return magpie object
#'
#' @examples
#' \dontrun{
#' a <- readSource("PSI", subtype = "energyIntensity")
#' }
#' @author Johnna Hoppe
#' @seealso [madrat::readSource()]
#' @import data.table
#' @importFrom readxl read_excel
#' @importFrom magclass as.magpie

readPSI <- function(subtype = c("CAPEX", "energyIntensity")) {
  scenario <- value <- variable <- unit <- region <- V1 <- NULL

  switch(
    subtype,
    "CAPEX" = {
      dt <- suppressMessages(data.table(read_excel("Car model result_modified.xlsx",
                                                   sheet = "Vehicle", "A1:X191",
                                                   col_names = TRUE)))
      dt <- dt[scenario %in% c("current", "Baseline"), c("scenario", "technology", "vehicle_type_PSI", "Total1_(Euro)")]
      ## substitute the "scenario" with the year it stands for
      dt[, scenario := ifelse(scenario == "current", 2015, 2040)]
      ## rename columns
      colnames(dt) <- c("period", "technologyPSI", "vehicleTypePSI", "value")
      #automatic column type guessing fails due to additional text information in the 2nd row,
      # set value column as numeric
      dt[, value := as.numeric(value)]
      dt[, variable := "Capital costs (purchase)"][, unit := "2017EUR/veh"][, region := "GLO"]
      # bring to quitte column order
      dt <- dt[, c("region", "technologyPSI", "vehicleTypePSI", "variable", "unit", "period", "value")]
      x <- as.magpie(as.data.frame(dt), spatial = "region", temporal = "period")
    },
    "energyIntensity" = {
      #Load energy intensity for LDV
      dt_LDV <- fread("ttw-efficiencies.csv")
      setnames(dt_LDV, c("size", "powertrain", "kJ per vkm", "year"),
               c("vehicleTypePSI", "technologyPSI", "value", "period"))
      dt_LDV[, variable := "Energy intensity"][, unit := "kJ/vehkm"][, region := "GLO"]
      #Load energy intensity for regional delivery Trucks
      dt_Trucks <- fread("Regional delivery_truck_efficiencies.csv")[, V1 := NULL]
      setnames(dt_Trucks, c("size", "powertrain", "ttw_energy", "year"),
               c("vehicleTypePSI", "technologyPSI", "value", "period"))
      dt_Trucks[, variable := "Energy intensity"][, unit := "kJ/vehkm"][, region := "GLO"]
      dt <- rbind(dt_LDV, dt_Trucks)
      #Bring to quitte column order
      dt <- dt[, c("region", "technologyPSI", "vehicleTypePSI", "variable", "unit", "period", "value")]
      x <- as.magpie(as.data.frame(dt), spatial = "region", temporal = "period")
    }
  )
  return(x)
}
