#' Perform parameter specific adjustments on the input data
#'
#'
#' @author Johanna Hoppe
#' @param dt calculated raw data without adjustments
#' @param completeData All combinations of region, period, univocalName and technology in EDGE-T decision tree
#' @param TRACCScountries countries included in TRACCS database
#' @param filter list of filters for specific branches in the upper decision tree, containing all associated
#' univocalNames
#' @return a quitte object

toolAdjustLoadFactor <- function(dt, completeData, TRACCScountries, filter) {
  value <- region <- univocalName <- univocalName <- check <- unit  <- variable <- NULL

  #1: Correct unrealisitc data
  #a) 3_5t load factor as provided by GCAM is unrealistically high
  dt[!(region %in% TRACCScountries) & univocalName == "Truck (0-3_5t)", value := 0.4]

  #b) 40t dt in TRACCS is too low, using KBA data (2019 dataset)
  # https://www.kba.de/DE/Statistik/Produktkatalog/produkte/Kraftverkehr/vd3_uebersicht.html?nn=3514348
  dt[region == "DEU" & univocalName == "Truck (40t)", value := 11.5]

  #2: Assume missing data
  # Non-motorized modes do not get a loadFactor
  completeData <- completeData[!univocalName %in% c("Cycle", "Walk")]
  dt <- merge.data.table(completeData, dt, all = TRUE)
  dt[, check := NULL]
  #Average first within regions over technologies -> e.g. BEV gets the same value as other technologies
  dt[, value := ifelse(is.na(value), mean(value, na.rm = TRUE), value), by = c("region", "period", "univocalName")]
  #If there are still NAs, average over regions -> e.g. ICE in FRA gets the same value as ICE in DEU
  dt[, value := ifelse(is.na(value), mean(value, na.rm = TRUE), value), by = c("period", "univocalName", "technology")]
  #Add unit and variable for filled values
  dt[is.na(unit), unit := ifelse(univocalName %in% filter$trn_pass, "p/veh", "t/veh")]
  dt[is.na(variable), variable := "Load factor"]

  #Reduce unrealistically high values
  ISOcountriesMap <- system.file("extdata", "regionmappingISOto21to12.csv", package = "mrtransport", mustWork = TRUE)
  ISOcountriesMap <- fread(ISOcountriesMap, skip = 0)

  #dt[region %in% ISOcountriesMap[regionCode21 == "CHA"]$countryCode & univocalName %in% filter$trn_pass_road_LDV_4W, value := mean_value]


  dt[, mean_value := mean(value, na.rm = TRUE), by = c("univocalName", "technology", "period")]
  dt[region %in% c("IND", "AGO") & univocalName == "Large Car and SUV", value := mean_value]
  dt[region %in% c(ISOcountriesMap[regionCode21 == "CAZ"]$countryCode) &
                     univocalName %in% c("Truck (7_5t)"), value := mean_value]

  dt[, mean_value := NULL]

  return(dt)
}
