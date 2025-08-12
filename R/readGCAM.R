#' Read GCAM transportation data.
#'
#'
#' @param subtype One of the possible subtypes, see default argument.
#' @return magpie object
#'
#' @examples
#' \dontrun{
#' a <- readSource("GCAM", subtype = "histEDsemand")
#' }
#' @author Johnna Hoppe, Alois Dirnaichner
#' @seealso [madrat::readSource()]
#' @import data.table
#' @importFrom magclass as.magpie
#' @importFrom magrittr `%>%`
readGCAM <- function(subtype = c("energyIntensity",  "loadFactor", "histESdemand", "speedMotorized",
                                 "speedNonMotorized", "valueOfTimeMultiplier")) {

  market.name <- variable <- unit <- scenario <- . <- supplysector <- value <- NULL

  switch(subtype,
    "energyIntensity" = {
      dt <- fread("L254.StubTranTechCoef.csv", skip = 4)[, market.name := NULL]
      setnames(dt, "year", "period")
      setnames(dt, gsub(".", "_", colnames(dt), fixed = TRUE))
      dt[, variable := "Energy intensity"][, unit := "MJ/vehkm"]
      dt <- dt[, c("region", "supplysector", "tranSubsector", "stub_technology", "minicam_energy_input",
                   "variable", "unit", "period", "coefficient")]
      x <- as.magpie(as.data.frame(dt), temporal = "period", spatial = "region")
    },
    "loadFactor" = {
      dt <- fread("L254.StubTranTechLoadFactor.csv", skip = 4)
      setnames(dt, "year", "period")
      setnames(dt, gsub(".", "_", colnames(dt), fixed = TRUE))
      dt[, variable := "Load factor"][, unit := "(t|p)/veh"]
      dt <- dt[, c("region", "supplysector", "tranSubsector", "stub_technology", "variable", "unit",
                   "period", "loadFactor")]
      x <- as.magpie(as.data.frame(dt), temporal = "period", spatial = "region")
    },
    "histESdemand" = {
      dt <- fread("tech_output.csv", skip = 1, sep = ";", header = TRUE) %>%
        melt(measure.vars = 6:26, variable.name = "period")
      dt[, scenario := NULL]
      dt[, variable := "ES"]
      setnames(dt, "Units", "unit")
      dt <- dt[, c("region", "sector", "subsector", "technology", "variable", "unit", "period", "value")]
      x <- as.magpie(as.data.frame(dt), temporal = "period", spatial = "region")
    },
    "speedMotorized" = {
      dt <- fread("L254.tranSubsectorSpeed.csv", skip = 4)
      setnames(dt, "year", "period")
      # data includes a lot of duplicates
      dt <- dt[!duplicated(dt, by = c("region", "supplysector", "tranSubsector", "period"))]
      dt[, variable := "Speed"][, unit := "km/h"]
      setnames(dt, "speed", "value")
      dt <- dt[, c("region", "supplysector", "tranSubsector", "variable", "unit", "period", "value")]
      x <- as.magpie(as.data.frame(dt), temporal = "period", spatial = "region")
    },
    "speedNonMotorized" = {
      dt <- fread("A54.globaltech_nonmotor.csv", skip = 1, sep = ",", header = TRUE)
      dt[, c("renewable.input", "share.weight", "technology") := NULL]
      dt[, variable := "Speed"][, unit := "km/h"]
      setnames(dt, "speed", "value")
      dt <- dt[, c("supplysector", "tranSubsector", "variable", "unit", "value")]
      x <- as.magpie(as.data.frame(dt), temporal = 0, spatial = 0)
    },
    "valueOfTimeMultiplier" = {
      dt <- fread("A54.tranSubsector_VOTT.csv", skip = 1)[!grepl("#", supplysector)] %>%
        unique() %>%
        setnames(gsub(".", "_", colnames(.), fixed = TRUE))
      dt[, c("speed_source", "fuelprefElasticity", "addTimeValue", "wait_walk_vott",
             "wait_walk_share", "in_vehicle_VOTT") := NULL]
      dt <- melt(dt, id.vars = c("supplysector", "tranSubsector"), na.rm = TRUE)
      dt[, value := as.numeric(value)]
      dt[, variable := "Value of time multiplier"][, unit := "-"]
      dt <- dt[, c("supplysector", "tranSubsector", "variable", "unit", "value")]
      x <- as.magpie(as.data.frame(dt), temporal = 0, spatial = 0)
    },
    "PPPtoMERfactor" = {
      dt <- fread("GCAM_PPP_MER.csv", header = TRUE)
      dt <- dt[, c("PPP_MER", "region")]
      setnames(dt, "PPP_MER", "value")
      dt[, variable := "Factor to move from PPP to MER for the time value multiplier"][, unit := "-"]
      dt <- dt[, c("region", "variable", "unit", "value")]
      x <- as.magpie(as.data.frame(dt), temporal = 0, spatial = "region")
    }
  )

  return(x)
}
