#' Adjust the raw energy intensities of India
#'
#' Called by [toolAdjustEnergyIntensity()] in two steps, as the general function adds technologies in between:
#' the hydrogen aircraft copies the liquids aircraft after step "Liquids", the BEV rickshaw copies the BEV
#' motorcycle before step "BEV".
#'
#' @details Values by assumption ID from the EDGETassumptions source ([toolGetAssumption()])
#'
#' Step "Liquids" (toolAdjustEnergyIntensity() step 2b, before the missing technologies are added)
#' - Trucks (step a, A13, A14): Ratios between classes from published Indian fleet values replace PSI's European
#'   ratios
#' - Domestic aviation (step b, A15, A16): factor so that the harmonized 2010 fuel use equals the IEA's; after 2010
#'   a further factor falling to 2019, so that 2019 equals the IEA's fuel use over DGCA's activity
#' - Cars, two-, three-wheelers and buses (step c, A17, A18): Liquids scaled to Indian survey fuel economy in 2010
#' - Trucks (step d, A19): trend after 2020 instead of PSI's European trend
#'
#' Step "BEV" (toolAdjustEnergyIntensity() step 3h, after the BEV rickshaw is added)
#' - BEV cars and two-wheelers (A20, A18): scaled to Indian real-world values in 2025
#'
#' Status, derivation and sources of each value are in the table, references and PDF paths in README.txt of the
#' source.
#'
#' @author Jarusch Muessel
#' @param dt raw energy intensities of all regions in MJ/vehkm, see [toolAdjustEnergyIntensity()]
#' @param step "Liquids" or "BEV", see details
#' @return dt with the adjusted values for region IND
#' @import data.table

toolAdjustEnergyIntensityIndia <- function(dt, step = c("Liquids", "BEV")) {
  step <- match.arg(step)
  region <- technology <- period <- value <- univocalName <- target <- vkmShare <- psi <- scaling <- raw <-
    group <- . <- NULL

  trucks <- c("Truck (0-3_5t)", "Truck (7_5t)", "Truck (18t)", "Truck (26t)", "Truck (40t)")

  # One factor per group of classes (names: classes, values: groups), so that the vkm-weighted (A18) mean in `year`
  # equals the group's target; ratios between the classes and the trend stay
  scaleGroups <- function(dt, groups, targetID, tech, year) {
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
  ldvGroups <- c("Mini Car" = "trn_pass_road_LDV_4W", "Subcompact Car" = "trn_pass_road_LDV_4W",
                 "Compact Car" = "trn_pass_road_LDV_4W", "Large Car and SUV" = "trn_pass_road_LDV_4W",
                 "Moped" = "trn_pass_road_LDV_2W", "Motorcycle (50-250cc)" = "trn_pass_road_LDV_2W")

  if (step == "BEV") {
    # BEV cars and two-wheelers #plausibilityFix_IND #plausibilityFix_Pass
    # A20, A18: BEV scaled per group so that the 2025 vkm-weighted (A18) mean equals the real-world value (A20)
    return(scaleGroups(dt, ldvGroups, "A20", "BEV", 2025))
  }

  # a: Ratios between the truck classes #plausibilityFix_IND #plausibilityFix_Truck #plausibilityFix_Freight
  # A13, A14: target ratios (A13) at PSI's vkm-weighted (A14) mean in 2010, all years, Liquids and Gases
  ratios <- data.table(univocalName = trucks)
  ratios[, target := toolGetAssumption("A13")[univocalName]]
  ratios[, vkmShare := toolGetAssumption("A14")[univocalName]]
  ratios <- merge(ratios, dt[region == "IND" & period == 2010 & technology == "Liquids",
                             .(univocalName, psi = value)], by = "univocalName")
  if (nrow(ratios) != length(trucks) || anyNA(ratios)) {
    stop("IND truck energy intensities (PSI, Liquids, 2010) are incomplete")
  }
  ratios[, scaling := target * sum(vkmShare * psi) / sum(vkmShare * target) / psi]
  scalingTrucks <- setNames(ratios$scaling, ratios$univocalName)
  dt[region == "IND" & univocalName %in% trucks & technology %in% c("Liquids", "Gases"),
     value := value * scalingTrucks[univocalName]]

  # b: Domestic aviation #plausibilityFix_IND #plausibilityFix_Pass
  # A15: factor so that the harmonized 2010 fuel equals the IEA's, all years, Liquids (hydrogen follows later)
  dt[region == "IND" & univocalName == "Domestic Aviation" & technology == "Liquids",
     value := value * toolGetAssumption("A15")[["Domestic Aviation"]]]
  # A16: further factor, linear between its 2010 and 2019 values, constant afterwards; GCAM's trend continues on top
  aviation <- toolGetAssumption("A16", by = "period")
  dt[region == "IND" & univocalName == "Domestic Aviation" & technology == "Liquids" & period > 2010,
     value := value * pmax(aviation[["2019"]],
                           aviation[["2010"]] - (aviation[["2010"]] - aviation[["2019"]]) * (period - 2010) / 9)]

  # c: Cars, two-wheelers, three-wheelers and buses #plausibilityFix_IND #plausibilityFix_Pass
  # A17, A18: Liquids scaled per group so that the 2010 vkm-weighted (A18) mean equals the survey value (A17)
  dt <- scaleGroups(dt, c(ldvGroups, "Rickshaw" = "Rickshaw", "Bus" = "Bus"), "A17", "Liquids", 2010)

  # d: Trend of the truck intensities #plausibilityFix_IND #plausibilityFix_Truck #plausibilityFix_Freight
  # A19: from 2020, Liquids and Gases held to the first A19 year, then x A19 per year to the last, constant after
  trend <- toolGetAssumption("A19", by = "period")
  trendYears <- as.numeric(strsplit(names(trend), "-")[[1]])
  trendTruck <- function(period) {
    trend[[1]]^(pmin(pmax(period, trendYears[1]), trendYears[2]) - trendYears[1])
  }
  if (nrow(dt[region == "IND" & univocalName %in% trucks & technology %in% c("Liquids", "Gases") &
                period == 2020]) != 2 * length(trucks)) {
    stop("IND truck energy intensities (Liquids, Gases, 2020) are incomplete")
  }
  dt[region == "IND" & univocalName %in% trucks & technology %in% c("Liquids", "Gases") & period >= 2020,
     value := value[period == 2020] * trendTruck(period), by = c("univocalName", "technology")]

  dt
}
