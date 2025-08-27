#' Perform source specific transformations to ensure a compatible structure.
#'
#' Map the source categories to the EDGE-T categories. Apply the full structure of the decision tree.
#'
#' @author Johanna Hoppe
#' @param x the input data read via readSource, a magpie object
#' @param subtype one of the different EDGE-T inputdata subtypes
#' @return a quitte object
#'
#' @importFrom rmndt magpie2dt
#' @import data.table
#' @export

toolPrepareGCAM <- function(x, subtype) {
  region <- period     <-
    technology <- univocalName <- variable <- unit <- esdem <- value <- . <- NULL

  dt <- magpie2dt(x)
  mapfile <- system.file("extdata", "mappingGCAMtoEDGET.csv", package = "mrtransport", mustWork = TRUE)
  mappingGCAMraw <- fread(mapfile)
  
  #make mapping region specific and add additional mapping assumptions:
  #introduce Rickshaws to "IND"
  regions <- unique(dt$region)
  mappingGCAM <- mappingGCAMraw[rep(seq_len(.N), times = length(regions))]
  mappingGCAM[, region := rep(regions, each = nrow(mappingGCAMraw))]
  mappingGCAM[region == "IND" & GCAMsubsector == "Three-Wheeler", univocalName := "Rickshaw"]

  switch(
    subtype,
    "energyIntensity" = {
      weight <- readSource("GCAM", subtype = "histESdemand")
      weight <- magpie2dt(weight)[, c("variable", "unit") := NULL]
      setnames(weight, "value", "esdem")

      setnames(dt, c("tranSubsector", "stub_technology"), c("subsector", "technology"))
      dt <- weight[dt, on = c("region", "period", "subsector", "technology")]
      # some technologies have zero or no demand for certain countries
      # -> set to 1 so that they are equally considered
      dt[is.na(esdem) | esdem == 0, esdem := 1]

      dt <- mappingGCAM[dt, on = c(GCAMsector = "sector", GCAMsubsector = "subsector", GCAMtechnology = "technology", region = "region"), 
                        allow.cartesian = TRUE]
      # GCAM data partly contains aggregated values for different levels of the decision tree
      # -> take only the lowest level
      dt <- dt[!is.na(univocalName)]

      dt <- dt[, .(value = sum(value * esdem) / sum(esdem)),
               by = c("region", "period", "univocalName", "technology", "variable", "unit")]

      # unit conversion from Mbtu/vehkm to MJ/vehkm
      convBTUtoMJ <- 1.055e-3
      dt[, value := value * convBTUtoMJ][, unit := "MJ/vehkm"]
      dt <- dt[, c("region", "univocalName", "technology", "variable", "unit", "period", "value")]
      setkey(dt, region, period, univocalName, technology, variable, unit)
    },
    "loadFactor" = {
      weight <- readSource("GCAM", subtype = "histESdemand")
      weight <- magpie2dt(weight)[, c("variable", "unit") := NULL]
      setnames(weight, "value", "esdem")

      setnames(dt, c("tranSubsector", "stub_technology"), c("subsector", "technology"))
      dt <- weight[dt, on = c("region", "period", "subsector", "technology")]
      # some technologies have zero or no demand for certain countries
      # -> set to 1 so that they are equally considered
      dt[is.na(esdem) | esdem == 0, esdem := 1]

      dt <- mappingGCAM[dt, on = c(GCAMsector = "sector", GCAMsubsector = "subsector", GCAMtechnology = "technology", region = "region"),
                        allow.cartesian = TRUE]
      # GCAM data partly contains aggregated values for different levels of the decision tree
      # -> take only the lowest level
      dt <- dt[!is.na(univocalName)]
      dt <- dt[, .(value = sum(value * esdem) / sum(esdem)),
               by = c("region", "univocalName", "technology", "variable", "unit", "period")]
      dt[, unit := ifelse(grepl("Truck.*|Freight Rail|.*Ship", univocalName), "t/veh", "p/veh")]
      dt <- dt[, c("region", "period", "univocalName", "technology", "variable", "unit", "value")]
      setkey(dt, region, period, univocalName, technology, variable, unit)
    },
    "histESdemand" = {
      # use only historical demand
      dt <- dt[period %in% c(1990, 2005, 2010)]
      # map
      dt <- mappingGCAM[dt, on = c(GCAMsector = "sector", GCAMsubsector = "subsector", GCAMtechnology = "technology", region = "region"),
                        allow.cartesian = TRUE]
      # GCAM data partly contains aggregated values for different levels of the decision tree
      # -> take only the lowest level
      dt <- dt[!is.na(univocalName)]
      # aggregate
      dt <- dt[, .(value = sum(value)),
               by = c("region", "period", "univocalName", "technology", "variable", "unit")]
      dt <- dt[, c("region", "period", "univocalName", "technology", "variable", "unit", "value")]
      setkey(dt,  region, period, univocalName, technology, variable, unit)
    },
    "speedMotorized" = {
      # weights are needed for GCAM vehicle types that are mapped on the same EDGE-T vehicle type
      weight <- readSource("GCAM", subtype = "histESdemand")
      weight <- magpie2dt(weight)
      # Speed is not differentiated between different technologies -> aggregate weights (ES demand)
      # to VehicleType level
      weight <- weight[, .(value = sum(value)),
                       by = c("region", "period", "sector", "subsector")]
      setnames(weight, "value", "esdem")
      setnames(dt, c("supplysector", "tranSubsector"), c("sector", "subsector"))
      dt <- weight[dt, on = c("region", "period", "sector", "subsector")]
      # some technologies have zero or no demand for certain countries
      #-> set to 1 so that they are equally considered
      dt[is.na(esdem) | esdem == 0, esdem := 1]
      #GCAM data for speed of modes is not technology specific
      mappingGCAM <- mappingGCAM[, c("GCAMtechnology", "technology") := NULL]
      mappingGCAM <- unique(mappingGCAM)
      dt <- mappingGCAM[dt, on = c(GCAMsector = "sector", GCAMsubsector = "subsector"), allow.cartesian = TRUE]
      dt <- dt[!is.na(univocalName)]
      dt <- dt[, .(value = sum(value * esdem) / sum(esdem)),
               by = c("region", "univocalName",
                      "variable", "unit", "period")]
      dt <- dt[, c("region", "univocalName", "variable", "unit", "period", "value")]
      setkey(dt,  region, univocalName, variable,
             unit, period)
    },
    "speedNonMotorized" = {
      setnames(dt, c("supplysector", "tranSubsector"), c("sector", "subsector"))
      #GCAM data for speed of modes is not technology specific
      mappingGCAM <- mappingGCAM[, c("GCAMtechnology", "technology") := NULL]
      mappingGCAM <- unique(mappingGCAM)
      dt <- mappingGCAM[dt, on = c(GCAMsector = "sector", GCAMsubsector = "subsector", region = "region"), allow.cartesian = TRUE]
      dt <- dt[, c("region", "univocalName", "variable", "unit", "value")]
      dt <- dt[univocalName %in% c("Cycle", "Walk")]
      setkey(dt,  region, univocalName, variable, unit)
    },
    "valueOfTimeMultiplier" = {
      #weights are needed for GCAM vehicle types that are mapped on the same EDGE-T vehicle type
      weight <- readSource("GCAM", subtype = "histESdemand")
      weight <- magpie2dt(weight)
      #Speed is not differentiated between different technologies -> aggregate weights (ES demand)
      # to VehicleType level
      weight <- weight[, .(value = sum(value)),
                       by = c("region", "period", "sector", "subsector")]
      setnames(weight, "value", "esdem")
      setnames(dt, c("supplysector", "tranSubsector"), c("sector", "subsector"))
      #data has no temporal resolution
      dt[, year := NULL]
      #Choose weight using ES demand for 2010
      weight <- weight[period == "2010"][, period := NULL]
      dt <- weight[dt, on = c("region", "sector", "subsector")]
      # some technologies have zero or no demand for certain countries
      #-> set to 1 so that they are equally considered
      dt[is.na(esdem) | esdem == 0, esdem := 1]
      #GCAM data for speed of modes is not technology specific
      mappingGCAM <- mappingGCAM[, c("GCAMtechnology", "technology") := NULL]
      mappingGCAM <- unique(mappingGCAM)
      dt <- mappingGCAM[dt, on = c(GCAMsector = "sector", GCAMsubsector = "subsector", region = "region"), allow.cartesian = TRUE]
      dt <- dt[!is.na(univocalName)]
      dt <- dt[, .(value = sum(value * esdem) / sum(esdem)),
               by = c("region", "univocalName", "variable", "unit")]
      dt <- dt[, c("region", "univocalName", "variable", "unit", "value")]
      setkey(dt,  region, univocalName, variable, unit)
    }
  )
  if (anyNA(dt) == TRUE) {
    stop("GCAM data contains NAs")
  }
  return(dt)
}
