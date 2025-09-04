#' Perform parameter specific adjustments on the input data
#'
#'
#' @author Johanna Hoppe
#' @param dt calculated raw data without adjustments
#' @param regionTRACCS iso countries included in TRACCS database
#' @param TrendsEnIntPSI Energy intensity trends from PSI
#' @param filter list of filters for specific branches in the upper decision tree, containing all associated
#' univocalNames
#' @param ariadneAdjustments optional parameter adjustements according to ARIADNE model intercomparison in 2022
#'
#' @return a quitte object
#'
#' @importFrom rmndt magpie2dt
#' @import data.table

toolAdjustEnergyIntensity <- function(dt, regionTRACCS, TrendsEnIntPSI, filter, ariadneAdjustments = TRUE) {
  region <- technology <- period <- value  <- region   <- TRACCS <-
    enIntLargeCarSUV <- univocalName <- meanValue <- regionCode21 <- NULL

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

  # 3: Assume missing data
  # a) Hydrogen airplanes are not covered by our datasources
  # based on "A review on potential use of hydrogen in aviation applications", Dincer, 2016:
  # the energy intensity of a hydrogen airplane is around 1MJ/pkm. The range of energy intensity of a
  # fossil-based airplane is here around 3-2 MJ/pkm-> a factor of 0.5 is assumed
  dt <- rbind(dt, dt[univocalName == "Domestic Aviation"][, c("value", "technology") :=
                                                            list(0.5 * value, "Hydrogen")])

  # b) Alternative technologies for Busses are not covered by our sources
  ## Buses are assumed to have the same energy intensity as 18 tons truck
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
  alternativeRickshawTech <- dt[univocalName == "Motorcycle (50-250cc)" &
                                  technology %in% c("BEV") & region %in% dt[univocalName == "Rickshaw"]$region]
  alternativeRickshawTech[, univocalName := "Rickshaw"]
  dt <- rbind(dt, alternativeRickshawTech)

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
