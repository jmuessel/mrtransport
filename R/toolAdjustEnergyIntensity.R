#' Perform parameter specific adjustments on the input data
#'
#' Applies trends, corrections and gap fills to the raw energy intensities (TRACCS, GCAM, PSI) and sets values for
#' single regions and modes.
#'
#' @details India (IND), values by assumption ID from the EDGETassumptions source ([toolGetAssumption()])
#' - Trucks (step 2b, A13, A14): Ratios between classes from published Indian fleet values replace PSI's European
#'   ratios
#' - Domestic aviation (step 2c, A15, A16): factor so that the harmonized 2010 fuel use equals the IEA's; after 2010
#'   a further factor falling to 2019, so that 2019 equals the IEA's fuel use over DGCA's activity
#' - Cars, two-, three-wheelers and buses (step 2d, A17, A18): Liquids scaled to Indian survey fuel economy in 2010
#' - Trucks (step 2e, A19): trend after 2020 instead of PSI's European trend
#' - BEV cars and two-wheelers (step 3h, A20, A18): scaled to Indian real-world values in 2025
#' - Status, derivation and sources of each value are in the table, references and PDF paths in README.txt of the
#'   source.
#'
#' @author Johanna Hoppe, Jarusch Muessel
#' @param dt calculated raw data without adjustments
#' @param regionTRACCS iso countries included in TRACCS database
#' @param TrendsEnIntPSI Energy intensity trends from PSI
#' @param filter list of filters for specific branches in the upper decision tree, containing all associated
#' univocalNames
#' @param ariadneAdjustments optional parameter adjustements according to ARIADNE model intercomparison in 2022
#'
#' @return a quitte object with the adjusted raw energy intensities in MJ/vehkm, from 2005 on (earlier years are
#' set to the 2005 values in step 5)
#'
#' @importFrom rmndt magpie2dt
#' @import data.table

toolAdjustEnergyIntensity <- function(dt, regionTRACCS, TrendsEnIntPSI, filter, ariadneAdjustments = TRUE) {
  region <- technology <- period <- value  <- region   <- TRACCS <-
    enIntLargeCarSUV <- univocalName <- meanValue <- regionCode21 <- target <- vkmShare <- psi <- scaling <- raw <-
    group <- . <- NULL

  # To complete dataset and ensure consistency and actuality a number of fixes are applied on the raw source data
  # 1: PSI trends in energyitensity are applied to TRACCS car and trucks data [TRACCS data only available until 2010]
  # Calculate PSI factors for the energy intensity improvement compared to 2010
  TrendsEnIntPSI <- TrendsEnIntPSI[region %in% regionTRACCS & !(region == "ALA") &
                                     technology %in% c("Liquids", "Gases") & period >= 2010]
  TrendsEnIntPSI[, factor := value / value[period == 2010],
                 by = c("region", "univocalName", "technology", "variable", "unit")]
  TrendsEnIntPSI[, value := NULL]
  dt <- merge.data.table(dt, TrendsEnIntPSI, all.x = TRUE,
                         by = c("region", "period", "univocalName", "technology", "variable", "unit"))
  # Multiply constant extrapolated 2010 TRACCS values with calculated factor
  dt[, value := ifelse(!is.na(factor), value * factor, value)][, factor := NULL]

  # 2: Adjustments made by Alois in consequence of the ARIADNE model intercomparison in 2022
  if (ariadneAdjustments) {
    dt[region == "DEU" & univocalName %in% filter$trn_pass_road_LDV_4W & technology == "Liquids",
       value := value * 2.4 / 2.85]
    dt[region == "DEU" & univocalName %in% filter$trn_pass_road_LDV_4W & technology == "BEV",
       value := value * 0.77 / 0.94]
    dt[region == "DEU" & univocalName %in% filter$trn_pass_road_LDV_4W & technology == "FCEV",
       value := value * 1.41 / 1.75]
    dt[region == "DEU" & univocalName %in% filter$trn_pass_road_LDV_4W & technology == "Hybrid electric",
       value := value * 1.86 / 1.3]

    ## we apply all constant from 2020 and adjust for DLR improvements (PSI improvements
    ## are calculated on sales only and are thus too ambitious)
    dt[region == "DEU" & univocalName %in% filter$trn_pass_road_LDV_4W & period >= 2020,
       value := .SD[period == 2020]$value, by = c("univocalName", "technology")]

    improvements <- fread("technology, period, factor
        Liquids, 2030, 0.94
        Liquids, 2045, 0.89
        BEV, 2030, 0.94
        BEV, 2045, 0.86
        FCEV, 2030, 0.83
        FCEV, 2045, 0.75")

    dt[region == "DEU" & univocalName %in% filter$trn_pass_road_LDV_4W,
       factor := improvements[.SD, factor, on = c("technology", "period")]]
    dt[region == "DEU" & univocalName %in% filter$trn_pass_road_LDV_4W & period <= 2020, factor := 1]
    dt[region == "DEU" & univocalName %in% filter$trn_pass_road_LDV_4W,
       factor := zoo::na.approx(factor, x = period, rule = 2), by = c("region", "univocalName", "technology")]
    dt[region == "DEU" & univocalName %in% filter$trn_pass_road_LDV_4W, value := value * factor]
    dt[, factor := NULL]

    ## the intensity deviation is likely coming from a deviation in LF and size shares
    dt[region == "DEU" & univocalName %in% filter$trn_freight_road & technology == "Liquids",
       value := value * 1.4 / 1.6]
    dt[region == "DEU" & univocalName %in% filter$trn_freight_road & technology == "BEV",
       value := value * 2.94 / 2.3]
    dt[region == "DEU" & univocalName %in% filter$trn_freight_road & technology == "FCEV",
       value := value * 4.7 / 4.4]
  }

  # 2b: Ratios between the truck classes in India #plausibilityFix_IND #plausibilityFix_Truck #plausibilityFix_Freight
  # A13, A14: target ratios (A13) at PSI's vkm-weighted (A14) mean in 2010, all years, Liquids and Gases
  trucksIND <- data.table(univocalName = c("Truck (0-3_5t)", "Truck (7_5t)", "Truck (18t)", "Truck (26t)",
                                           "Truck (40t)"))
  trucksIND[, target := toolGetAssumption("A13")[univocalName]]
  trucksIND[, vkmShare := toolGetAssumption("A14")[univocalName]]
  nTrucksIND <- nrow(trucksIND)
  trucksIND <- merge(trucksIND, dt[region == "IND" & period == 2010 & technology == "Liquids",
                                   .(univocalName, psi = value)], by = "univocalName")
  if (nrow(trucksIND) != nTrucksIND || anyNA(trucksIND)) {
    stop("IND truck energy intensities (PSI, Liquids, 2010) are incomplete")
  }
  trucksIND[, scaling := target * sum(vkmShare * psi) / sum(vkmShare * target) / psi]
  scalingIND <- setNames(trucksIND$scaling, trucksIND$univocalName)
  dt[region == "IND" & univocalName %in% names(scalingIND) & technology %in% c("Liquids", "Gases"),
     value := value * scalingIND[univocalName]]

  # 2c: Domestic aviation in India #plausibilityFix_IND #plausibilityFix_Pass
  # A15: factor so that the harmonized 2010 fuel equals the IEA's, all years, Liquids (before 3a: hydrogen follows)
  dt[region == "IND" & univocalName == "Domestic Aviation" & technology == "Liquids",
     value := value * toolGetAssumption("A15")[["Domestic Aviation"]]]
  # A16: further factor, linear between its 2010 and 2019 values, constant afterwards; GCAM's trend continues on top
  aviationIND <- toolGetAssumption("A16", by = "period")
  dt[region == "IND" & univocalName == "Domestic Aviation" & technology == "Liquids" & period > 2010,
     value := value * pmax(aviationIND[["2019"]],
                           aviationIND[["2010"]] - (aviationIND[["2010"]] - aviationIND[["2019"]]) *
                             (period - 2010) / 9)]

  # India, used in 2d and 3h: one factor per group of classes (names: classes, values: groups), so that the
  # vkm-weighted (A18) mean in `year` equals the group's target; ratios between the classes and the trend stay
  scaleGroupsIND <- function(dt, groups, targetID, tech, year) {
    groupsIND <- data.table(univocalName = names(groups), group = unname(groups))
    groupsIND[, target := toolGetAssumption(targetID)[group]]
    groupsIND[, vkmShare := toolGetAssumption("A18")[univocalName]]
    groupsIND <- merge(groupsIND, dt[region == "IND" & period == year & technology == tech,
                                     .(univocalName, raw = value)], by = "univocalName")
    if (nrow(groupsIND) != length(groups) || anyNA(groupsIND)) {
      stop("IND energy intensities (", tech, ", ", year, ") are incomplete")
    }
    groupsIND[, scaling := target * sum(vkmShare) / sum(vkmShare * raw), by = "group"]
    factors <- setNames(groupsIND$scaling, groupsIND$univocalName)
    dt[region == "IND" & univocalName %in% names(factors) & technology == tech,
       value := value * factors[univocalName]]
    dt
  }
  ldvGroupsIND <- c("Mini Car" = "trn_pass_road_LDV_4W", "Subcompact Car" = "trn_pass_road_LDV_4W",
                    "Compact Car" = "trn_pass_road_LDV_4W", "Large Car and SUV" = "trn_pass_road_LDV_4W",
                    "Moped" = "trn_pass_road_LDV_2W", "Motorcycle (50-250cc)" = "trn_pass_road_LDV_2W")

  # 2d: Cars, two-wheelers, three-wheelers and buses in India #plausibilityFix_IND #plausibilityFix_Pass
  # A17, A18: Liquids scaled per group so that the 2010 vkm-weighted (A18) mean equals the survey value (A17)
  dt <- scaleGroupsIND(dt, c(ldvGroupsIND, "Rickshaw" = "Rickshaw", "Bus" = "Bus"), "A17", "Liquids", 2010)

  # 2e: Trend of the truck intensities in India #plausibilityFix_IND #plausibilityFix_Truck #plausibilityFix_Freight
  # A19: from 2020, Liquids and Gases held to the first A19 year, then x A19 per year to the last, constant after
  trendIND <- toolGetAssumption("A19", by = "period")
  trendYearsIND <- as.numeric(strsplit(names(trendIND), "-")[[1]])
  trendTruckIND <- function(period) {
    trendIND[[1]]^(pmin(pmax(period, trendYearsIND[1]), trendYearsIND[2]) - trendYearsIND[1])
  }
  if (nrow(dt[region == "IND" & univocalName %in% names(scalingIND) & technology %in% c("Liquids", "Gases") &
                period == 2020]) != 2 * nTrucksIND) {
    stop("IND truck energy intensities (Liquids, Gases, 2020) are incomplete")
  }
  dt[region == "IND" & univocalName %in% names(scalingIND) & technology %in% c("Liquids", "Gases") & period >= 2020,
     value := value[period == 2020] * trendTruckIND(period), by = c("univocalName", "technology")]

  # 3: Assume missing data
  # a) Hydrogen airplanes are not covered by our datasources
  # based on "A review on potential use of hydrogen in aviation applications", Dincer, 2016:
  # the energy intensity of a hydrogen airplane is around 1MJ/pkm. The range of energy intensity of a
  # fossil-based airplane is here around 3-2 MJ/pkm-> a factor of 0.5 is assumed
  dt <- rbind(dt, dt[univocalName == "Domestic Aviation"][, c("value", "technology") :=
                                                            list(0.5 * value, "Hydrogen")])

  # b) Alternative technologies for Busses are not covered by our sources
  ## Buses are assumed to have the same energy intensity as 18 tons truck
  # IND cross-check: BEV 3.4 MJ/vehkm; CEEW (2025c) Table 2: electric bus 1.0 km/kWh = 3.6 MJ/vehkm (see 3g).
  alternativeBusTech <- dt[univocalName == "Truck (18t)" & technology %in% c("BEV", "FCEV")]
  alternativeBusTech[, univocalName := "Bus"]
  dt <- rbind(dt, alternativeBusTech)

  # c) Some regions are missing electric motorcycles (>250CC)
  BEVmot <- dt[univocalName == "Motorcycle (>250cc)" & technology == "BEV"]
  BEVmotAll <- dt[univocalName == "Motorcycle (>250cc)"]
  BEVmotAll <- unique(BEVmotAll[, c("technology", "value") := NULL])[, technology := "BEV"]
  BEVmotAll <- merge.data.table(BEVmotAll, BEVmot, by = c("region", "period", "univocalName", "technology",
                                                          "variable", "unit"), all = TRUE)
  BEVmotAll[, TRACCS := ifelse(region %in% regionTRACCS, "TRACCS", "nonTRACCS")]
  # NAs (missing data) is filled by averages of other countries
  BEVmotAll <- BEVmotAll[, mean := lapply(.SD, mean, na.rm = TRUE), .SDcols = "value",
                         by = c("period", "univocalName", "technology", "TRACCS")]
  BEVmotAll[is.na(value), value := mean][, c("TRACCS", "mean") := NULL]
  dt <- rbind(dt[!(univocalName == "Motorcycle (>250cc)" & technology == "BEV")], BEVmotAll)

  # d) Energy intensity for large cars is not provided by PSI, hence some nonTRACCS countries are missing
  # alternative technologies for Large cars
  enIntLargeCar <- unique(dt[univocalName == "Large Car and SUV",
                             c("region", "period", "technology", "value")])
  setnames(enIntLargeCar, "value", "enIntLargeCarSUV")
  dt4W <- merge.data.table(dt[univocalName %in% filter$trn_pass_road_LDV_4W], enIntLargeCar,
                           by = c("region", "period", "technology"), all.x = TRUE)
  dt4W[is.na(value) & univocalName == "Large Car", value := enIntLargeCarSUV * 0.9][, enIntLargeCarSUV := NULL]
  dt <- rbind(dt[!univocalName %in% filter$trn_pass_road_LDV_4W], dt4W)

  # e) Some non TRACCS countries ("ARE" "BHR" "IRN" "IRQ" "ISR" "JOR" "KWT" "LBN" "OMN" "PSE" "QAT" "SAU" "SYR" "YEM")
  # are missing data for Freight and Passenger Rail running on liquids. Average of other non TRACCS countries
  # is taken
  missingRailData <- dt[region %in% c("ARE", "BHR", "IRN", "IRQ", "ISR", "JOR", "KWT", "LBN", "OMN", "PSE", "QAT",
                                      "SAU", "SYR", "YEM")                        &
                          univocalName %in% c("Freight Rail", "Passenger Rail") & technology == "Electric"]
  missingRailData[, technology := "Liquids"][, value := NA]
  dt <- rbind(dt, missingRailData)
  dt[, TRACCS := ifelse(region %in% regionTRACCS, "TRACCS", "nonTRACCS")]
  dt[univocalName %in% c("Freight Rail", "Passenger Rail") & technology == "Liquids",
     mean := lapply(.SD, mean, na.rm = TRUE), .SDcols = "value",
     by = c("period", "univocalName", "technology", "TRACCS")]
  dt[univocalName %in% c("Freight Rail", "Passenger Rail") & technology == "Liquids" & is.na(value), value := mean]
  dt[, c("TRACCS", "mean") := NULL]

  # f) IDN (Indonesia) is missing electric Passenger Rail. It is assumed that it is the same as in Malaysia (MYS)
  missingPassRailDataIDN <- dt[region %in% c("MYS") & univocalName == "Passenger Rail" & technology == "Electric"]
  missingPassRailDataIDN[, region := "IDN"]
  dt <- rbind(dt, missingPassRailDataIDN)

  # g) Alternative technologies for Rickshaws are not covered by our sources
  # Rickshaws are assumed to have the same as Motorcycle (50-250cc) BEV
  # IND cross-check: 0.21 MJ/vehkm (2010); CEEW (2025c) Table 2: electric passenger three-wheeler 15 km/kWh =
  # 0.24 MJ/vehkm; 14% difference,kept for now
  # SourceID: S26 (EDGETassumptions README.txt, section 8)
  alternativeRickshawTech <- dt[univocalName == "Motorcycle (50-250cc)" &
                                  technology %in% c("BEV") & region %in% dt[univocalName == "Rickshaw"]$region]
  alternativeRickshawTech[, univocalName := "Rickshaw"]
  dt <- rbind(dt, alternativeRickshawTech)

  # h) BEV cars and two-wheelers in India #plausibilityFix_IND #plausibilityFix_Pass
  # A20, A18: BEV scaled per group so that the 2025 vkm-weighted (A18) mean equals the real-world value (A20)
  # After 3g, so that the rickshaw keeps the motorcycle's BEV value from before this step.
  dt <- scaleGroupsIND(dt, ldvGroupsIND, "A20", "BEV", 2025)

  # 4: adjustments for scenarioMIP validation: adjust outliers to global mean
  ISOcountriesMap <- system.file("extdata", "regionmappingISOto21to12.csv", package = "mrtransport", mustWork = TRUE)
  ISOcountriesMap <- fread(ISOcountriesMap, skip = 0)
  dt[, meanValue := mean(value, na.rm = TRUE), by = c("univocalName", "technology", "period")]
  dt[region %in% ISOcountriesMap[regionCode21 == "LAM"]$countryCode & univocalName %in% c("Compact Car") &
       technology == "Gases", value := meanValue]

  dt[, meanValue := NULL]

  # 5: data until 2005 is fixed to not impact changes in lifetime in the fleet calculation (in edgeT)
  xdata <- unique(dt$period)
  dt <- dt[period >= 2005]
  dt <- rmndt::approx_dt(dt, xdata, "period", "value",   extrapolate = TRUE)

  return(dt)
}
