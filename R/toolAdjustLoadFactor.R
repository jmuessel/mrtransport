#' Perform parameter specific adjustments on the input data
#'
#' Corrects and fills gaps in the raw load factors (TRACCS, GCAM) and sets values for single regions and modes.
#'
#' @details India (IND), values by assumption ID from the EDGETassumptions source ([toolGetAssumption()])
#' - Trucks (step 4, A01): Payloads net of empty running from Indian survey data replace the global values
#' - Passenger three-wheelers ("Rickshaw", step 5, A02): survey occupancy of TERI minus an assumed share of empty
#'   driving
#' - Buses (step 6, A03): passenger-km per bus-km of the State Road Transport Undertakings (MoRTH)
#' - Cars and two-wheelers (step 7, A04, A05): one value for all car classes (GCAM's class values raised the mean
#'   with the SUV share) and one for two-wheelers, Indian survey values
#' - Status, derivation and sources of each value are in the table, references and PDF paths in README.txt of the
#'   source.
#'
#' @author Johanna Hoppe, Jarusch Muessel
#' @param dt calculated raw data without adjustments
#' @param completeData All combinations of region, period, univocalName and technology in EDGE-T decision tree
#' @param TRACCScountries countries included in TRACCS database
#' @param filter list of filters for specific branches in the upper decision tree, containing all associated
#' univocalNames
#' @return a quitte object with the adjusted load factors in p/veh (passenger modes) and t/veh (freight modes)

toolAdjustLoadFactor <- function(dt, completeData, TRACCScountries, filter) {
  period <- value <- region <- univocalName <- univocalName <- check <- unit  <- variable <-
    meanValue <- regionCode21 <- NULL

  #1: Correct unrealisitc data
  #a) 3_5t load factor as provided by GCAM is unrealistically high
  # IND: overwritten in step 4 (all technologies and years).
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

  # data until 2010 has weird spikes for some regions
  # -> remove data between 1991 and 2009 and interpolate afterward to remove the spikes
  xdata <- unique(dt$period)
  dt <- dt[period == 1990 | period > 2010]
  dt <- rmndt::approx_dt(dt, xdata, "period", "value")

  # 3: adjustments for scenarioMIP validation adjust outliers to global mean
  ISOcountriesMap <- system.file("extdata", "regionmappingISOto21to12.csv", package = "mrtransport", mustWork = TRUE)
  ISOcountriesMap <- fread(ISOcountriesMap, skip = 0)
  dt[, meanValue := mean(value, na.rm = TRUE), by = c("univocalName", "technology", "period")]
  dt[region %in% ISOcountriesMap[regionCode21 == "CAZ"]$countryCode & univocalName %in% c("Truck (7_5t)"),
     value := meanValue]
  dt[, meanValue := NULL]

  # Steps 4-7 (India) come after the regional means in step 3, so that they do not change other regions.
  # 4: Trucks in India #plausibilityFix_IND #plausibilityFix_Truck #plausibilityFix_Freight
  # A01: payloads net of empty running from Indian surveys, 2020 values used for all years and technologies
  loadFactorIND <- toolGetAssumption("A01")[filter$trn_freight_road]
  if (anyNA(loadFactorIND)) stop("IND truck load factors (A01) are incomplete")
  dt[region == "IND" & univocalName %in% names(loadFactorIND), value := loadFactorIND[univocalName]]

  # 5: Passenger three-wheelers in India #plausibilityFix_IND #plausibilityFix_Pass
  # A02: survey occupancy x (1 - empty driving share), all technologies and years
  dt[region == "IND" & univocalName == "Rickshaw", value := toolGetAssumption("A02")[["Rickshaw"]]]

  # 6: Buses in India #plausibilityFix_IND #plausibilityFix_Pass
  # A03: passenger-km per bus-km of the State Road Transport Undertakings, all technologies and years
  dt[region == "IND" & univocalName == "Bus", value := toolGetAssumption("A03")[["Bus"]]]

  # 7: Cars and two-wheelers in India #plausibilityFix_IND #plausibilityFix_Pass
  # A04, A05: Indian survey values, one for all car classes and one for two-wheelers, all technologies and years
  dt[region == "IND" & univocalName %in% filter$trn_pass_road_LDV_4W,
     value := toolGetAssumption("A04")[["trn_pass_road_LDV_4W"]]]
  dt[region == "IND" & univocalName %in% filter$trn_pass_road_LDV_2W,
     value := toolGetAssumption("A05")[["trn_pass_road_LDV_2W"]]]

  return(dt)
}
