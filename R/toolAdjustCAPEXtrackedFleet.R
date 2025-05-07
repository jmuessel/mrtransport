#' Perform parameter specific adjustments on the input data
#'
#'
#' @author Johanna Hoppe
#' @param dt calculated raw data without adjustments
#' @param ISOcountries list of iso countries
#' @param yrs temporal resolution of EDGE-T model
#' @param completeData All combinations of region, period, univocalName and technology in EDGE-T decision tree
#' @param GDPpcMER GDP per capita based on market exchange rate
#' @param filter list of filters for specific branches in the upper decision tree,
#'               containing all associated univocalNames
#' @importFrom rmndt magpie2dt
#' @importFrom data.table data.table merge.data.table
#' @return a quitte object

toolAdjustCAPEXtrackedFleet <- function(dt, ISOcountries, yrs, completeData, GDPpcMER, filter) {
  variable <- value <- region <- period <- technology <- univocalName <- univocalName <-
    unit <- check <- gdppc <- . <- NULL

  # 1: LDV 4 Wheeler adjustments
  # 1a: Delete Capital costs other, as it is unclear what it represents
  dt <- dt[!(univocalName %in% filter$trn_pass_road_LDV_4W & variable == "Capital costs (other)")]

  # 1b: Capital costs (purchase) data for LDV 4 Wheelers in EUR from PSI is not region specific
  # and real data is only available for 2015 and 2040 -> rest is interpolated
  LDV4WpurchaseCost <- copy(dt[univocalName %in% filter$trn_pass_road_LDV_4W                               &
                                 variable == "Capital costs (purchase)"                               &
                                 period %in% c(2015, 2040)])
  # First fix applied in toolPSICosts() in EDGE-T old
  LDV4WpurchaseCost[period == 2040 & technology == "BEV", value := 0.9 * value]
  LDV4WpurchaseCost[period == 2040 & technology == "FCEV", value := 0.9 * value]
  # Second fix applied in mergeData() in EDGE-T old
  LDV4WpurchaseCost <- rbind(LDV4WpurchaseCost, copy(LDV4WpurchaseCost[period == 2040][, period := 2100]))
  # in 2100, purchase price for BEVs is 0.8 * purchase price, for Hybrid electric is 0.7, for FCEVs is 0.9
  decr <- data.table(technology = c("BEV", "Hybrid electric", "FCEV", "Liquids", "Gases"),
                     factor = c(0.6, 0.7, 0.9, 1, 1))
  LDV4WpurchaseCost <- merge.data.table(LDV4WpurchaseCost, decr, by = "technology")
  LDV4WpurchaseCost[period == 2100, value := value[technology == "Liquids"]
                    * factor, by = c("region", "univocalName")]
  LDV4WpurchaseCost[, factor := NULL]
  LDV4WpurchaseCost <- approx_dt(LDV4WpurchaseCost, yrs, "period", "value",
                                 c("region", "univocalName", "technology", "variable", "unit"), extrapolate = TRUE)
  dt <- rbind(dt[!(univocalName %in% filter$trn_pass_road_LDV_4W & variable == "Capital costs (purchase)")],
              LDV4WpurchaseCost)

  # 1c: Hybrids get the same Capital costs (infrastructure) as BEVs
  dt[, value := ifelse(technology == "Hybrid electric" & variable == "Capital costs (infrastructure)",
                       value[technology == "BEV" & variable == "Capital costs (infrastructure)"],
                       value), by = c("period", "region", "univocalName")]

  # 2: Alternative trucks and busses
  # Data for alternative trucks and busses is missing
  BEV <- dt[(univocalName %in% filter$trn_freight_road | univocalName == "Bus") & technology == "Liquids"]
  BEV[, technology := "BEV"]
  FCEV <- dt[(univocalName %in% filter$trn_freight_road | univocalName == "Bus") & technology == "Liquids"]
  FCEV[, technology := "FCEV"]
  altCost <- rbind(BEV, FCEV)
  # Operating subsidies are kept as they are in liquids
  altCostCAPEXnonFuelOPEX <- altCost[variable == "CAPEX and non-fuel OPEX"]

  targetYearEarly <- 2035  ## target year for electric trucks and electric and FCEV buses
  targetYearLate <- 2150  ## target year for FCEV trucks

  # cost of electric truck is 60% more than a as conventional truck today
  altCostCAPEXnonFuelOPEX[univocalName %in% filter$trn_freight_road & period <= 2020 & technology == "BEV",
                          value := 1.6 * value]
  # cost of a FCEV truck is 80% more than a as conventional truck today
  altCostCAPEXnonFuelOPEX[univocalName %in% filter$trn_freight_road & period <= 2020 & technology == "FCEV",
                          value :=  1.8 * value]
  # cost of electric and H2 buses is 40% more of a conventional bus today
  altCostCAPEXnonFuelOPEX[univocalName == "Bus" & period <= 2020, value := 1.4 * value]

  altCostCAPEXnonFuelOPEX <- altCostCAPEXnonFuelOPEX[
    period <= 2020 |
      (univocalName == "Bus" & period >= targetYearEarly) |
      (univocalName %in% filter$trn_freight_road & technology == "BEV" & period >= targetYearEarly) |
      (univocalName %in% filter$trn_freight_road & technology == "FCEV" & period >= targetYearLate)
  ]
  # follow linear trends until target years/cost parity with ICE cost -> after the target years we assume no
  # further cost decline. This is somehow odd and should be checked
  altCostCAPEXnonFuelOPEX <- approx_dt(altCostCAPEXnonFuelOPEX, yrs, "period", "value",
                                       c("region", "univocalName", "technology", "variable", "unit"),
                                       extrapolate = TRUE)
  dt <- rbind(altCostCAPEXnonFuelOPEX, altCost[!variable == "CAPEX and non-fuel OPEX"], dt)

  # 3: CAPEX are given combined with non fuel OPEX for trucks and busses: Apply assumptions on CAPEX share
  # 3a: Busses
  # https://mdpi-res.com/d_attachment/wevj/wevj-11-00056/article_deploy/wevj-11-00056.pdf?version=1597829235
  # BEV busses: veh + batt. = 25% of TCO
  dt[univocalName == "Bus" & technology %in% c("BEV", "FCEV"), value := value * 0.25]
  # 3b: diesel busses: 15% of TCO
  dt[univocalName == "Bus" & technology %in% c("Liquids", "Gases"), value := value * 0.15]
  dt[univocalName == "Bus", variable := "Capital costs (total)"]
  # 3c: Trucks
  # https://theicct.org/sites/default/files/publications/TCO-BETs-Europe-white-paper-v4-nov21.pdf
  # p. 11: retail price = 150k for diesel, 500 - 200k for BEV
  # p. 22: TCO 550 for diesel, TCO = 850 - 500k for BEV
  # CAPEX share diesel = 27%, 60-40% for BEV -> 50%
  dt[univocalName %in% filter$trn_freight_road & technology %in% c("Liquids", "Gases"), value := value * 0.3]
  dt[univocalName %in% filter$trn_freight_road & technology %in% c("BEV", "FCEV"), value := value * 0.5]
  dt[univocalName %in% filter$trn_freight_road, variable := "Capital costs (total)"]
  # Sum part of the CAPEX and non Fuel OPEX and the operating subsidies that is now attributed to the CAPEX
  dt <- dt[, .(value = sum(value)), by = c("region", "univocalName", "technology",
                                           "variable", "unit", "period")]
  # Values given in US$/vehkm need to be transferred to US$/veh with the help of annual mileage
  # and annuity factor
  annualMileage <-  magpie2dt(calcOutput(type = "EdgeTransportSAinputs", subtype = "annualMileage",
                                         warnNA = FALSE, aggregate = FALSE))[, c("unit", "variable") := NULL]
  setnames(annualMileage, "value", "annualMileage")
  # magclass converts "." in vehicle types to "_" (e.g. Truck (0-3_5t))
  setkey(annualMileage, region, period, univocalName, technology)

  # UCD applied interest rate of 10% and uniform vehicle lifetime of 15 yrs
  # (https://itspubs.ucdavis.edu/publication_detail.php?id=1884)
  # Calc annuity factor
  discountRate <- 0.1   # discount rate for vehicle purchases
  lifeTime <- 15    # Number of years over which vehicle capital payments are amortized
  annuityFactor <- (discountRate * (1 + discountRate)^lifeTime) / ((1 + discountRate)^lifeTime - 1)

  # Divide by Annual Mileage to get [unit = US$/veh yr]
  dt <- merge.data.table(dt, annualMileage, all.x = TRUE)
  dt[univocalName %in% filter$trn_freight_road | univocalName == "Bus", value := value * annualMileage]
  dt[univocalName %in% filter$trn_freight_road | univocalName == "Bus", unit := gsub("vehkm", "veh yr", unit)]
  # Divide by annuity factor to get CAPEX per veh
  dt[univocalName %in% filter$trn_freight_road | univocalName == "Bus", value := value / annuityFactor]
  dt[, annualMileage := NULL]
  dt[univocalName %in% filter$trn_freight_road | univocalName == "Bus",  unit := gsub("veh yr", "veh", unit)]

  # 4: Missing vehicle types in certain countries
  completeData <- completeData[univocalName %in% filter$trn_freight_road |
                                 univocalName %in% filter$trn_pass_road_LDV_4W |
                                 univocalName == "Bus"]
  dt <- merge.data.table(dt, completeData, all.y = TRUE)

  # 5a: Some Truck types are missing for many countries in the UCD database

  # First approach: If other countries of the same region feature these truck types, assign mean values of them
  # #this is a good example, that our current truck data is somewhat odd, mean Truck 18t
  # are nearly double the price of truck 26t
  # #that is a result of small countries setting the price due to missing data.
  # Here, Puerto Rico (PRI) is setting the price for 18t and
  # #is assigned to the USA region in UCD (therefore prices are much higher)

  # Replacing the missing values with values from other countries in the same region does not work at the moment,
  # 2nd approach: Replace with other vehicle types in the same country

  # Find NAs
  # KOR does not feature truck (0-3_5t) and truck (7_5t) in the UCD database
  # NEU, EUR and REF do not feature truck (18t) in the UCD database
  missing18t <- dt[is.na(value) & univocalName == "Truck (18t)"]
  # OAS, SSA and MEA do not feature truck (26t) in the UCD database
  missing26t <- dt[is.na(value) & univocalName == "Truck (26t)"]
  # OAS, SSA, MEA and CAZ do not feature truck(40t) in the UCD database
  missing40t <- dt[is.na(value) & univocalName == "Truck (40t)"]

  # Get values for other truck types
  truck26t <- dt[!is.na(value) & univocalName == "Truck (26t)"]
  truck26t <- truck26t[, c("region", "technology", "period", "value")]
  setnames(truck26t, "value", "truck26t")

  truck7t <- dt[!is.na(value) & univocalName == "Truck (7_5t)"]
  truck7t <- truck7t[, c("region", "technology", "period", "value")]
  setnames(truck7t, "value", "truck7t")

  truck18t <- dt[!is.na(value) & univocalName == "Truck (18t)"]
  truck18t <- truck18t[, c("region", "technology", "period", "value")]
  setnames(truck18t, "value", "truck18t")

  # Replace NAs step by step with values of other truck types until data is complete
  missing18t <- merge.data.table(missing18t, truck26t, by = c("region", "technology", "period"), all.x = TRUE)
  missing18t <- merge.data.table(missing18t, truck7t, by = c("region", "technology", "period"), all.x = TRUE)
  missing18t[, value := truck26t][, truck26t := NULL]
  missing18t[is.na(value), value := truck7t][, truck7t := NULL]

  missing26t <- merge.data.table(missing26t, truck18t, by = c("region", "technology", "period"), all.x = TRUE)
  missing26t <- merge.data.table(missing26t, truck7t, by = c("region", "technology", "period"), all.x = TRUE)
  missing26t[, value := truck18t][, truck18t := NULL]
  missing26t[is.na(value), value := truck7t][, truck7t := NULL]

  missing40t <- merge.data.table(missing40t, truck26t, by = c("region", "technology", "period"), all.x = TRUE)
  missing40t <- merge.data.table(missing40t, truck18t, by = c("region", "technology", "period"), all.x = TRUE)
  missing40t <- merge.data.table(missing40t, truck7t, by = c("region", "technology", "period"), all.x = TRUE)
  missing40t[, value := truck26t][, truck26t := NULL]
  missing40t[is.na(value), value := truck18t][, truck18t := NULL]
  # JPN gets 7_5t assigned -> this should be fixed
  missing40t[is.na(value), value := truck7t][, truck7t := NULL]

  missingTrucks <- rbind(missing18t, missing26t, missing40t)
  # Korea (KOR) is missing all truck types and gets assigned the values of Taiwan (TWN)
  missingTrucks <- missingTrucks[!region == "KOR"][, variable := "Capital costs (total)"]
  missingTrucks[, unit := unique(dt[!(is.na(value))]$unit)]

  dt <- rbind(dt[!(is.na(value) & univocalName %in% filter$trn_freight_road)], missingTrucks)
  trucksKOR <- dt[region == "TWN" & univocalName %in% filter$trn_freight_road][, region := "KOR"]
  dt <- rbind(dt, trucksKOR)

  # 5b: Some car vehicle classes are missing as well and are replaced by other vehicle classes
  # Find missing values
  missingVan <- dt[is.na(value) & univocalName == "Van"][, c("variable", "unit") := NULL]
  missingMini <- dt[is.na(value) & univocalName == "Mini Car"][, c("variable", "unit") := NULL]
  missingMid <- dt[is.na(value) & univocalName == "Midsize Car"][, c("variable", "unit") := NULL]
  missingSub <- dt[is.na(value) & univocalName == "Subcompact Car"][, c("variable", "unit") := NULL]
  missingCom <- dt[is.na(value) & univocalName == "Compact Car"][, c("variable", "unit") := NULL]
  missingLar <- dt[is.na(value) & univocalName == "Large Car"][, c("variable", "unit") := NULL]

  # Get values of other vehicle types
  SUV <- dt[!is.na(value) & univocalName == "Large Car and SUV"]
  SUV <- SUV[, c("region", "technology", "variable", "period", "value")]
  setnames(SUV, "value", "SUV")

  large <- dt[!is.na(value) & univocalName == "Large Car"]
  large <- large[, c("region", "technology", "variable", "period", "value")]
  setnames(large, "value", "large")

  mid <- dt[!is.na(value) & univocalName == "Midsize Car"]
  mid <- mid[, c("region", "technology", "variable", "period", "value")]
  setnames(mid, "value", "mid")

  sub <- dt[!is.na(value) & univocalName == "Subcompact Car"]
  sub <- sub[, c("region", "technology", "variable", "period", "value")]
  setnames(sub, "value", "sub")

  com <- dt[!is.na(value) & univocalName == "Compact Car"]
  com <- com[, c("region", "technology", "variable", "period", "value")]
  setnames(com, "value", "com")

  mini <- dt[!is.na(value) & univocalName == "Mini Car"]
  mini <- mini[, c("region", "technology", "variable", "period", "value")]
  setnames(mini, "value", "mini")

  # Assign values of other vehicle types (step by step)
  missingVan <- merge.data.table(missingVan, SUV, by = c("region", "technology", "period"),
                                 all.x = TRUE, allow.cartesian = TRUE)
  missingVan[, value := SUV][, SUV := NULL]

  missingMini1 <- merge.data.table(missingMini, sub, by = c("region", "technology", "period"), all.x = TRUE)
  missingMini1[, value := sub][, sub := NULL]
  missingMini2 <- copy(missingMini1[is.na(value)])[, variable := NULL]
  missingMini1 <- missingMini1[!is.na(value)]
  missingMini2 <- merge.data.table(missingMini2, com, by = c("region", "technology", "period"), all.x = TRUE)
  missingMini2[, value := com][, com := NULL]
  missingMini3 <- copy(missingMini2[is.na(value)])[, variable := NULL]
  missingMini2 <- missingMini2[!is.na(value)]
  missingMini3 <- merge.data.table(missingMini3, mid, by = c("region", "technology", "period"), all.x = TRUE)
  missingMini3[, value := mid][, mid := NULL]
  missingMini4 <- copy(missingMini3[is.na(value)])[, variable := NULL]
  missingMini3 <- missingMini3[!is.na(value)]
  missingMini4 <- merge.data.table(missingMini4, SUV, by = c("region", "technology", "period"), all.x = TRUE)
  # this applies only to Saint Pierre and Miquelon (SPM)
  # -> fairly small (SPM has data for midsize cars in the UCD database, but is listed in EDGE-T as a country
  # without midsize cars) -> after a data update, the country specific vehicle map should be checked again
  missingMini4[is.na(value), value := SUV][, SUV := NULL]
  missingMini <- rbind(missingMini1, missingMini2, missingMini3, missingMini4)

  missingMid <- merge.data.table(missingMid, com, by = c("region", "technology", "period"), all.x = TRUE)
  missingMid[, value := com][, com := NULL]

  missingSub1 <- merge.data.table(missingSub, com, by = c("region", "technology", "period"), all.x = TRUE)
  missingSub1[, value := com][, com := NULL]
  missingSub2 <- copy(missingSub1[is.na(value)])[, variable := NULL]
  missingSub1 <- missingSub1[!is.na(value)]
  missingSub2 <- merge.data.table(missingSub2, SUV, by = c("region", "technology", "period"), all.x = TRUE)
  # This again only applies to SPM
  missingSub2[, value := SUV][, SUV := NULL]
  missingSub <- rbind(missingSub1, missingSub2)

  missingCom <- merge.data.table(missingCom, SUV, by = c("region", "technology", "period"), all.x = TRUE)
  # This again only applies to SPM
  missingCom[, value := SUV][, SUV := NULL]

  missingLar <- merge.data.table(missingLar, SUV, by = c("region", "technology", "period"), all.x = TRUE)
  missingLar[, value := SUV][, SUV := NULL]

  missing4W <- rbind(missingVan, missingMini, missingMid, missingSub, missingCom, missingLar)
  missing4W[, unit := unique(dt[!(is.na(value))]$unit)]
  dt <- rbind(dt[!(is.na(value) & univocalName %in% filter$trn_pass_road_LDV_4W)], missing4W)[, check := NULL]

  # 5: Lower the prices for LDW 4 Wheelers depending on the GDP to represent a 2nd hand vehicle market
  convfact <- convertSingle(x = 1, iso3c = "USA",  unit_in = "constant 2005 Int$PPP",
                            unit_out = mrdrivers::toolGetUnitDollar())

  minGDP <- 4000 * convfact     ## minimum GDPcap after which the linear trend starts
  maxGDP <- 30000 * convfact   ## maximum GDPcap marking the level where no factor is implemented
  lowerBound <- 0.3  ## maximum decrease to be applied to the original costs value

  GDPpcMER[, factor := ifelse(gdppc < maxGDP & gdppc >  minGDP,
                              (1 - lowerBound) / (maxGDP - minGDP) * (gdppc - minGDP) + lowerBound, 1)]
  GDPpcMER[, factor := ifelse(gdppc <=   minGDP, lowerBound, factor)]
  GDPpcMER[, factor := ifelse(gdppc >=  maxGDP, 1, factor)]

  dt <- merge(dt, GDPpcMER, by = c("region", "period"))
  dt[univocalName %in% filter$trn_pass_road_LDV_4W & variable == "Capital costs (purchase)", value := value * factor]
  dt[, c("gdppc", "factor") := NULL]

  return(dt)
}
