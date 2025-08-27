#' Perform parameter specific adjustments on the input data
#'
#'
#' @author Johanna Hoppe
#' @param dt calculated raw data without adjustments
#' @param completeData All combinations of region, period, univocalName and technology in EDGE-T decision tree
#' @param filter list of filters for specific branches in the upper decision tree, containing all associated
#' univocalNames
#' @param ariadneAdjustments switch on and off adjustments according to ARIADNE model intercomparison in 2022
#' @return a quitte object

toolAdjustAnnualMileage <- function(dt, completeData, filter, ariadneAdjustments = TRUE) {
  region <- value <- univocalName <- check <- unit <- variable <- annualMileage <- period <- technology <- meanValue <-
    regionCode21 <- . <- NULL

  # 1: Adjustments made by Alois in consequence of the ARIADNE model intercomparison in 2022
  if (ariadneAdjustments) {
    ## according to ViZ data from 2020 there has been a 10% reduction wrt 2010 values
    ## (from 14 kkm to 13.6 kkm per vehicle and year)
    dt[region == "DEU" & univocalName %in% filter$trn_pass_road_LDV_4W, value := value * 0.9]
  }
  # 2: Assume missing data
  # a) Some modes and technologies are missing an annual mileage
  # TRACCS sets annual mileage for not available technologies in certain countries
  # (e.g. NG or BEVs in early years) to zero.
  # This is not helpful for us. We therefore assign a "hypothetical" annual mileage that
  # can be used for fleet calculation once the technologies get into the mix in later years
  dt[value == 0, value := NA]
  # Non-motorized modes do not get an annual mileage (no fleet tracking possible for walking/
  # not planned for non-motorized cycling)
  completeData <- completeData[!univocalName %in% c("Cycle", "Walk")]
  mileageUnit <- unique(dt$unit)
  dt <- merge.data.table(completeData, dt, all = TRUE)
  # For some regions an annual mileage is provided for certain vehicle types, but no demand.
  # These values need to be deleted
  dt <- dt[!is.na(check)]
  # update variable and unit for introduced NAs
  dt[, unit := mileageUnit][, variable := "Annual mileage"][, check := NULL]

  dt[, value := ifelse(is.na(value), mean(value, na.rm = TRUE), value),
     by = c("period", "univocalName", "region")]

  # If there are NAs take mean over regions by technology
  dt[, value := ifelse(is.na(value), mean(value, na.rm = TRUE), value),
     by = c("period", "technology", "univocalName")]
  #         by = .(period, univocalName, region, variable, technology, unit)]
  #missing Rickshaw data in India
  dt[region == "IND" & univocalName == "Rickshaw", value := 10000]
  dt <- dt[period <= 2010, value := value[period == 2010], by = .(region, univocalName, variable, technology)]

  # b) Annual Mileage for Trucks is missing completely - insert assumptions made by Alois in 2022
  # (probably from ARIADNE)
  annualMileageTrucks <- fread(
                               text = "univocalName, annualMileage
              Truck (0-3_5t), 21500
              Truck (7_5t), 34500
              Truck (18t), 53000
              Truck (26t), 74000
              Truck (40t), 136500")
  dt <- merge.data.table(dt, annualMileageTrucks, by = "univocalName", all.x = TRUE, allow.cartesian = TRUE)
  dt[, value := ifelse(!is.na(annualMileage), annualMileage, value)][, annualMileage := NULL]

  # c) We do not have vintage tracking for the rest of the modes -> insert zeros
  # Later on it would be great to top up this data
  missingAnnualMileageData <- fread(
                                    text = "univocalName, annualMileage
              International Aviation, 0
              Domestic Aviation, 0
              Passenger Rail, 0
              HSR, 0
              Domestic Ship, 0
              International Ship, 0
              Freight Rail, 0")
  dt <- merge.data.table(dt, missingAnnualMileageData, by = "univocalName", all.x = TRUE, allow.cartesian = TRUE)
  dt[, value := ifelse(!is.na(annualMileage), annualMileage, value)][, annualMileage := NULL]

  # 3 adjustments for scenarioMIP validation
  # a) adjust outliers to global mean
  ISOcountriesMap <- system.file("extdata", "regionmappingISOto21to12.csv", package = "mrtransport", mustWork = TRUE)
  ISOcountriesMap <- fread(ISOcountriesMap, skip = 0)
  dt[, meanValue := mean(value, na.rm = TRUE), by = c("univocalName", "technology", "period")]
  dt[region %in% ISOcountriesMap[regionCode21 %in% c("NES", "CHA")]$countryCode &
       univocalName %in% filter$trn_pass_road_LDV_4W, value := meanValue]
  dt[region %in% ISOcountriesMap[regionCode21 %in% c("EWN", "ENC", "UKI", "NES")]$countryCode &
       grepl("Bus", univocalName), value := meanValue]

  dt[, meanValue := NULL]

  # b) data until 2010 has weird spikes for some regions -> remove data between 1991 and 2009
  # and interpolate afterward to remove the spikes
  xdata <- unique(dt$period)
  dt <- dt[period == 1990 | period > 2010]
  dt <- rmndt::approx_dt(dt, xdata, "period", "value")

  # c) In the scenarioMIP validation we decided to only use constant annual mileage values
  # until we have better data (this makes 3b obsolete)
  dt <- dt[, value := value[period == 2030], by = setdiff(names(dt), c("value", "period"))]

  return(dt)
}
