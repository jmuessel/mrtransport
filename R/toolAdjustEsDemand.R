#' Perform parameter specific adjustments on the input data
#'
#' Fills gaps in the raw historical energy service demand (GCAM, TRACCS, Eurostat) and corrects implausible values
#' for single regions and modes.
#'
#' @details India (IND), values by assumption ID from the EDGETassumptions source ([toolGetAssumption()])
#' - Truck size split (step 3, A21): Vehicle shares derived from Indian stock and sales data replace previous
#'   assumptions
#' - All other adjustments (step 7): [toolAdjustEsDemandIndia()]
#'
#' @author Johanna Hoppe, Jarusch Muessel
#' @param dt calculated raw data without adjustments
#' @param mapIso2region map iso countries to regions
#' @param completeData All combinations of region, period, univocalName and technology in EDGE-T decision tree
#' @param filter list of filters for specific branches in the upper decision tree, containing all associated
#' univocalNames
#' @param histSourceData the full source data; annual mileage and load factor (2010 Liquids) turn the target
#' truck vehicle shares of step 3 into tkm shares; energy intensity and load factor are used by
#' [toolAdjustEsDemandIndia()]
#' @return a quitte object with the adjusted energy service demand in billion pkm/yr and billion tkm/yr

toolAdjustEsDemand <- function(dt, mapIso2region, completeData, filter, histSourceData) {
  variable <- period  <- unit <- value <-  demldv <- regionCode21 <- technology <- valueNew <- . <-
    regionCode12 <- region <- univocalName <- oldES <- totalES <- targetES <- ESshare <- ESperVeh <-
    loadFactor <- allZero <- annualMileage <- scaling <- NULL

  dt <- merge.data.table(dt, completeData[period <= 2010],
                         by = c("region", "period", "univocalName", "technology"), all = TRUE)
  # completeData does not contain unit so it needs to be added
  dt[univocalName %in% c(filter$trn_pass, "International Aviation"), unit := "billion pkm/yr"]
  dt[univocalName %in% c(filter$trn_freight, "International Ship"), unit := "billion tkm/yr"]
  dt[is.na(value), variable := "ES"]
  dt <- merge.data.table(dt, mapIso2region, by = "region", all.x = TRUE, allow.cartesian = TRUE)




  # 1: Some Truck types, Rail, alternative technologies and active modes are lacking energy service demand data
  # The missing modes get a zero demand for now. After the regional aggregation, the zero demand remains only
  # for alternative technologies
  dt[is.na(value), value := 0]



  # 2: Add some base demand for Cycle & Walk (2%)
  # IND: not applied, as GCAM's walking and cycling are not zero (EDGETassumptions README, section 7)
  dt[, demldv := sum(value), by = c("period", "region")]
  dt[univocalName == "Cycle" & value == 0, value := demldv * 0.01]
  dt[univocalName == "Walk" & value == 0, value := demldv * 0.002]
  dt[univocalName == "Cycle" & value == 0 & regionCode21 %in% c("USA", "AUS", "CAN"), value := demldv * 0.006]
  dt[univocalName == "Cycle" & value == 0 & regionCode21 %in% c("IND", "CHN"), value := demldv * 0.02]
  dt[, demldv := NULL]


  # 3: Correct truck size ES splits for CHN/HKG/MAC, JPN and IND
  # #plausibilityFix_CHN #plausibilityFix_JPN #plausibilityFix_IND #plausibilityFix_Truck #plausibilityFix_Freight
  #
  # Original GCAM input data has implausible truck size distributions:
  #   - CHN/HKG/MAC: 95% of ES in 0-3.5t trucks (should be ~64% per CEIC data)
  #   - JPN: 99.999% in 0-3.5t trucks (heavy-duty trucks essentially missing)
  #   - IND: 55% in 0-3.5t trucks and 0% for Trucks between 7.5t and 40t
  #
  # Real-world target vehicle share (% of vehicle count, not % of tonne-km) used here:
  #   CHN/HKG/MAC: 0-3.5t=76%, 7.5t=13%, 18t=5%, 26t=3%, 40t=3%
  #   JPN:         0-3.5t=88%, 7.5t=9%,  18t=1%, 26t=1%, 40t=1%
  #   IND:         A21 (see below)
  #
  # CHN/HKG/MAC and JPN: these are compromise values: closer to real-world than GCAM defaults, but
  # still producing reasonable 2010/2015 truck numbers given other input data
  # constraints. Should be updated to real-world values once other inputs improve.
  # Source can be found in:
  #   CEIC: https://www.ceicdata.com/en/china/no-of-motor-vehicle/cn-no-of-motor-vehicle-truck-heavy
  #   Data/RegionalData/compiling_CHA_data_heavy_duty_vehicles.xlsx (cells V17:V21, CHA)
  #   Also for Data/RegionalData/compiling_JPN_data_heavy_duty_vehicles.xlsx
  #
  # A21 (IND): vehicle shares from Indian stock and sales data, 2020 values used for 2010

  REGIONS_TO_FIX <- c("CHN", "HKG", "MAC", "JPN", "IND")
  TRUCK_SIZES    <- c("Truck (0-3_5t)", "Truck (7_5t)", "Truck (18t)", "Truck (26t)", "Truck (40t)")
  vehicleSharesIND <- toolGetAssumption("A21")[TRUCK_SIZES]
  if (anyNA(vehicleSharesIND)) stop("IND truck vehicle shares (A21) are incomplete")

  # Step 1: Define target vehicle count shares by size
  # These are shares of *vehicle count*, not ES. ES shares are derived below
  # by weighting by annual tkm per vehicle (mileage × load factor).
  targetVehicleShares <- data.table(
    univocalName = rep(TRUCK_SIZES, length(TRUCK_SIZES)),
    region       = rep(REGIONS_TO_FIX, each = length(TRUCK_SIZES)),
    value        = c(
      rep(c(76, 13, 5, 3, 3), 3),  # CHN, HKG, MAC
      c(88,  9, 1, 1, 1),       # JPN
      unname(vehicleSharesIND)  # IND (A21)
    )
  )

  # Step 2: Extract current ES for trucks in affected regions
  currentES <- dt[region %in% REGIONS_TO_FIX & univocalName %like% "Truck",
                  .(region, univocalName, technology, period, value)]

  # Step 3: Initialize near-zero ES for size classes missing from input data
  # JPN: HD trucks entirely absent → seed with a tiny value so rescaling works.
  # CHN/HKG/MAC: 26t and 40t are zero → seed proportionally from 18t entries
  #   (preserving the technology split, since CNG share matters for CHN).
  # The seed values are negligible relative to total ES (factor 1e-4).
  minTotalES <- min(currentES[, sum(value), by = .(region, period)]$V1)
  seedValue  <- 1e-4 * minTotalES

  currentES[, allZero := all(value == 0), by = .(region, univocalName, period)]
  currentES[allZero == TRUE & technology == "Liquids", value := seedValue]
  currentES[, allZero := NULL]

  # For CHN/HKG/MAC, fill 26t and 40t ES based on 18t ES (preserving technology split)
  currentES[region %in% c("CHN", "HKG", "MAC"),
            value := ifelse(univocalName %in% c("Truck (26t)", "Truck (40t)"),
                            value[univocalName == "Truck (18t)"] * 1e-4,
                            value),
            by = .(period, region, technology)]

  # Step 4: Compute annual tkm per vehicle = mileage × load factor
  # Use 2010 Liquids values as the reference (dominant technology that year).

  annualTkmPerVehicle <- merge(
    histSourceData$annualMileage[
                                 region %in% REGIONS_TO_FIX & univocalName %like% "Truck" & period == 2010 &
                                   technology == "Liquids", .(region, univocalName, annualMileage = value)],
    histSourceData$loadFactor[
                              region %in% REGIONS_TO_FIX & univocalName %like% "Truck" & period == 2010 &
                                technology == "Liquids", .(region, univocalName, loadFactor)],
    by = c("region", "univocalName")
  )[, ESperVeh := annualMileage * loadFactor]

  # Step 5: Convert target vehicle shares → target ES shares
  # ES share ∝ vehicle share × ES per vehicle. Normalise within each region.

  #test whether row dimension matches: REGIONS_TO_FIX x TRUCK_SIZES = 5 x 5 = 25 rows
  if (nrow(targetVehicleShares) != nrow(annualTkmPerVehicle)) {
    stop("Mismatch in number of rows between target vehicle shares and annual tkm per vehicle data.")
  }

  targetESshares <- merge(targetVehicleShares, annualTkmPerVehicle,
                          by = c("region", "univocalName"))

  targetESshares[, ESshare := (value / 100) * ESperVeh]
  targetESshares[, ESshare := ESshare / sum(ESshare), by = region]

  targetESshares <- targetESshares[, .(region, univocalName, ESshare)]

  # Step 6: Derive target ES per size, preserving regional totals

  totalESbyRegion <- currentES[, .(totalES = sum(value)), by = .(region, period)]

  targetESbySize <- targetESshares[
    totalESbyRegion, on = "region", allow.cartesian = TRUE
  ][, targetES := ESshare * totalES]

  # Step 7: Compute per-size scaling factors and rescale

  currentESbySize <- currentES[, .(oldES = sum(value)), by = .(region, univocalName, period)]
  scalingFactors <- currentESbySize[targetESbySize,
    on = c("region", "univocalName", "period"),
    nomatch = NULL
  ][, scaling := targetES / oldES]

  updatedES <- merge(currentES, scalingFactors,
    by = c("region", "univocalName", "period"),
    all.x = TRUE
  )[, value := value * scaling
  ][, c("oldES", "ESshare", "totalES", "targetES", "scaling") := NULL
  ][, ":="(variable = "ES", unit = "billion tkm/yr")]


  # Step 8: Verify regional ES totals are unchanged
  stopifnot(all.equal(
    updatedES[, sum(value), by = .(period, region)],
    currentES[, sum(value), by = .(period, region)]
  ))

  # Step 9: Write updated values back to dt

  setnames(updatedES, "value", "valueNew")
  dt <- merge(dt, updatedES, by = intersect(names(dt), names(updatedES)), all.x = TRUE)
  dt[!is.na(valueNew), value := valueNew][, valueNew := NULL]

  # Also adjust car ES demands upwards to better reflect car stock numbers in 2010 and 2015
  # (~62 mio in 2010, 140 mio in 2015 , eg IEA GEVO and others - see file in the owncloud
  # "Data/RegionalData/compiling_CHA_data_LDV.xlsx", Increasing the ES values means that more
  # cars are on the road, but also that 2010 FE demand BEFORE the IEA calibration is higher
  # - thus preventing the strong upscaling of energy intensities during the IEA FE calibration that was
  # previously the case. The multipliers are staggered (reducing from 2.5 in 2010 to 1.5 in 2005) to represent
  # the fast growth of LDV numbers over these 5 years. The 2.5 multiplier in 2010 is a compromise betwen hitting
  # 2010 and 2015 numbers: 2010 85 mio instead of 62 mio, 2015 125 mio instead of 140 mio
  # plausibilityFix_CHN #plausibilityFix_LDV #plausibilityFix_Pass

  carTypes <- c("Compact Car", "Large Car", "Large Car and SUV", "Midsize Car", "Mini Car", "Subcompact Car", "Van")

  dt[univocalName %in% carTypes & region == "CHN" & period == 2010, value := 2.5 * value]
  dt[univocalName %in% carTypes & region == "CHN" & period == 2009, value := 2.3 * value]
  dt[univocalName %in% carTypes & region == "CHN" & period == 2008, value := 2.1 * value]
  dt[univocalName %in% carTypes & region == "CHN" & period == 2007, value := 1.9 * value]
  dt[univocalName %in% carTypes & region == "CHN" & period == 2006, value := 1.7 * value]
  dt[univocalName %in% carTypes & region == "CHN" & period <= 2005, value := 1.5   * value]

  ############ end of new CHA stuff from Robert (& Jarusch for IND)

  # plausibilityFix_USA #plausibilityFix_Truck #plausibilityFix_Freight
  dt[region %in% c("USA", "PRI", "UMI", "VIR"), value := ifelse(univocalName == "Truck (26t)",
                                                                value[univocalName == "Truck (18t)"] / 3,
                                                                value),
     by = c("period", "region", "technology")]
  dt[region %in% c("USA", "PRI", "UMI", "VIR"), value := ifelse(univocalName == "Truck (40t)",
                                                                value[univocalName == "Truck (18t)"] / 3,
                                                                value),
     by = c("period", "region", "technology")]
  dt[region %in% c("USA", "PRI", "UMI", "VIR") & univocalName == "Truck (18t)", value := value / 3,
     by = c("period", "region", "technology")]


  # plausibilityFix_CHN #plausibilityFix_Rail #plausibilityFix_Pass
  # from https://www.iea.org/reports/tracking-rail-2020-2
  dt[period <= 2010 & regionCode21 == "CHN" & univocalName == "HSR", value := 70000]

  # plausibilityFix_CHN #plausibilityFix_Truck #plausibilityFix_Freight
  # from https://theicct.org/sites/default/files/China_Freight_Assessment_English_20181022.pdf
  # total road freight demand seems to be around 5 billion tkm * 0.8, a factor 3 roughly
  dt[period <= 2010 & regionCode21 == "CHN" & univocalName %in% filter$trn_freight_road, value := value * 3]

  # 4: Demand level corrections, adjusting to ETP demands
  # #plausibilityFix_CHN #plausibilityFix_Bus #plausibilityFix_Pass
  # #plausibilityFix_OAS #plausibilityFix_NEU #plausibilityFix_MEA
  # IND: the former factor 1/2 is replaced by the bus share in toolAdjustEsDemandIndia().
  dt[regionCode21 == "CHA" & univocalName == "Bus", value := value / 2.5]
  dt[regionCode21 == "OAS" & univocalName == "Bus", value := value / 5]
  dt[regionCode12 == "NEU" & univocalName == "Bus", value := value / 2]
  dt[regionCode21 == "MEA" & univocalName == "Bus", value := value / 2]

  # plausibilityFix_DEU #plausibilityFix_Truck #plausibilityFix_Freight
  # 5: Adjust GER Truck size shares according to KBA data (calculated from stocks via AM and LF)
  dt[region == "DEU" & univocalName == "Truck (0-3_5t)", value := value * 2]
  dt[region == "DEU" & univocalName == "Truck (7_5t)", value := value * 0.25]
  dt[region == "DEU" & univocalName == "Truck (18t)", value := value * 0.65]
  dt[region == "DEU" & univocalName == "Truck (40t)", value := value * 1.4]

  # plausibilityFix_DEU #plausibilityFix_Truck #plausibilityFix_Freight
  # 6: Total 2010 Freight demands, from ViZ 2010
  # (the shares are roughly OK)
  dt[region == "DEU" & univocalName %in% filter$trn_freight, value := value * 620 / 587]
  dt[, c("countryName", "regionCode21", "regionCode12", "check") := NULL]

  # 7: Historical activity in India (IND) #plausibilityFix_IND
  # 2010 targets, bus technologies, 1990 road demand; the former factor 1.4 on all IND demand in 2005 and 2010
  # is removed
  dt <- toolAdjustEsDemandIndia(dt, filter, histSourceData)

  return(dt)
}
