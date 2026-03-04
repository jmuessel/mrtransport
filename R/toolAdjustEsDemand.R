#' Perform parameter specific adjustments on the input data
#'
#'
#' @author Johanna Hoppe
#' @param dt calculated raw data without adjustments
#' @param mapIso2region map iso countries to regions
#' @param completeData All combinations of region, period, univocalName and technology in EDGE-T decision tree
#' @param filter list of filters for specific branches in the upper decision tree, containing all associated
#' @param histSourceData the full source data containing the annual mileage and load factor
#' univocalNames
#' @return a quitte object

toolAdjustEsDemand <- function(dt, mapIso2region, completeData, filter, histSourceData) {
  variable <- period  <- unit <- value <-  demldv <- regionCode21 <-
    regionCode12 <- region <- univocalName <- NULL

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



  #2: Add some base demand for Cycle & Walk (2%)
  dt[, demldv := sum(value), by = c("period", "region")]
  dt[univocalName == "Cycle" & value == 0, value := demldv * 0.01]
  dt[univocalName == "Walk" & value == 0, value := demldv * 0.002]
  dt[univocalName == "Cycle" & value == 0 & regionCode21 %in% c("USA", "AUS", "CAN"), value := demldv * 0.006]
  dt[univocalName == "Cycle" & value == 0 & regionCode21 %in% c("IND", "CHN"), value := demldv * 0.02]
  dt[, demldv := NULL]

  #3: Correct truck size ES splits for CHA and JPN
  #plausibilityFix_CHN #plausibilityFix_JPN #plausibilityFix_Truck #plausibilityFix_Freight

  ## Adjustments on truck size classes in CHN region according to newer data and model results
  ## Treat HKG and MAC like CHN, as we have no data on their specific size split

  ## when combining the input data ES values by size with the input data annualMileage and loadFactor, the following shares come out:
  ## 40t 0.1%, 26t 0.4%, 18t 1.4%, 7.5t 3%, 0-3.5t 95%

  ## The new data is based on downscaled values from CEIC data, https://www.ceicdata.com/en/china/no-of-motor-vehicle/cn-no-of-motor-vehicle-truck-heavy
  ## It is further split to EDGE-T size classes : 40t	5%, 26t	9%, 18t	11%, 7.5t	11%, 0-3.5 t 64%
  ## The original data and the further processing can be found in the transport folder in the owncloud:
  ## "Data/RegionalData/compiling_CHA_data_heavy_duty_vehicles.xlsx", cells V17:V21

  ## However, the real-world split produces too low truck numbers when combined with the rest of the input data (total ES, enIntensity, loadFactor, ...)
  ## Therefore, the following values are used, which are closer to the real-world values than the original data from GCAM, and still produce reasonable 2010/2015 truck numbers
  ## 40t 3%, 26t 3%, 18t 5%, 7.5t 13%, 0-3.5t 76%
  ## When the rest of the input data is improved, this here should be changed to the real-world data as well.

  ## Also the truck size data for JPN is overwritten, as our input data for JPN shows 99.999% of 0-3.5t trucks, while recent real-world data shows relevant numbers of heavy-duty trucks.
  ## "Data/RegionalData/compiling_JPN_data_heavy_duty_vehicles.xlsx"
  ## At the moment, the current vehicle size shares are:  (to be changed in the future once other input data is adjusted)
  ## 40t 3%, 26t 3%, 18t 5%, 7.5t 13%, 0-3.5t 76%

  # First step: define target vehicle shares by size:
  VehSharesTargetSize <- data.table(univocalName = c("Truck (0-3_5t)", "Truck (18t)", "Truck (26t)", "Truck (40t)", "Truck (7_5t)"),
                                    region = c(rep("CHN", 5), rep("HKG", 5), rep("MAC", 5), rep("JPN", 5)),
                                    variable = "Share_in_Vehicles",
                                    unit = "Percent",
                                    value = c( rep(c(76, 5, 3, 3, 13), 3), c(88, 1, 1, 1, 9)))

  # extract existing ES totals for regions and trucks of interest
  histESdemandtoUpdate <- dt[region %in% c("CHN", "HKG", "MAC", "JPN") & univocalName %like% "Truck"]

  histESdemandtoUpdateold <- histESdemandtoUpdate[    # drop variable and unit
    , .(value = value),
    by = .(region, univocalName, technology, period )
  ]

  histESdemandtoUpdateoldTotal <- histESdemandtoUpdateold[
    , .(totalES = sum(value)),
    by = .(region, period)
  ]

  ## for Japan, truck sizes > 3.5 are completely missing from the input data. This makes the later rescaling impossible.
  ## Therefore, I overwrite them with 1e-4 * min(totalES) to have a basis for rescaling, without changing totalES relevantly.
  ## The "min" ensures that really the smallest ES value in any of the observed regions is used, to ensure minimal change of totalES
  MinEsValuePerSize = 1e-4 * min(histESdemandtoUpdateoldTotal$totalES)
  histESdemandtoUpdateold[ region == "JPN" & univocalName %in% c("Truck (18t)", "Truck (40t)", "Truck (7_5t)", "Truck (26t)") & technology == "Liquids",
                           value := MinEsValuePerSize ]

  ## The same problem of zero-value entries holds true for CHA, but for CHA the technology split is more relevant,
  ## as CHA has a relevant CNG truck fleet.
  ## Therefore, the initialization is based on the technology split of 18t-trucks:

  histESdemandtoUpdateold[region %in% c("CHN", "HKG", "MAC"), value := ifelse(univocalName == "Truck (26t)",
                                                         value[univocalName == "Truck (18t)"] * 1e-4,
                                                         value),
     by = c("period", "region", "technology")]
  histESdemandtoUpdateold[region %in% c("CHN", "HKG", "MAC"), value := ifelse(univocalName == "Truck (40t)",
                                                         value[univocalName == "Truck (18t)"] * 1e-4,
                                                         value),
     by = c("period", "region", "technology")]


  ## calculate totals by truck size and overall total truck ES

  histESdemandtoUpdateoldSize <- histESdemandtoUpdateold[
    , .(oldES = sum(value)),
    by = .(region, univocalName, period)
  ]

  histESdemandtoUpdateoldTotal <- histESdemandtoUpdateold[
    , .(totalES = sum(value)),
    by = .(region, period)
  ]

  # calculate new ES splits

  ## First calculate total tkm per vehicle from annual mileage and load factors.
  ## Take the values for "Liquids", as that is the most common technology in 2010

  annualTkmPerVehicle <- histSourceData$annualMileage[region %in% c("CHN", "HKG", "MAC", "JPN") & univocalName %like% "Truck" & period == 2010 & technology == "Liquids",
                                                      .(annualMileage = value), by = .(region, univocalName)]

  dtloadFactor <- histSourceData$loadFactor[region %in% c("CHN", "HKG", "MAC", "JPN") & univocalName %like% "Truck" & period == 2010 & technology == "Liquids",
                                            .(loadFactor = loadFactor), by = .(region, univocalName)]

  annualTkmPerVehicle <- merge(annualTkmPerVehicle, dtloadFactor,
                               by = c("region", "univocalName"))

  annualTkmPerVehicle[, ESperVeh := annualMileage * loadFactor ]


  # calculate old truck shares per vehicle size as coming from the original input data

  VehicleOverview <- merge(annualTkmPerVehicle, histESdemandtoUpdateoldSize [period == 2010],
                           by = c("region", "univocalName"))

  VehicleOverview[, NumOfTrucks := oldES / ESperVeh ]
  VehicleOverview[, TotalNumOfTrucks := sum(NumOfTrucks), , by = region]
  VehicleOverview[, ShareOfTruckSize := NumOfTrucks / TotalNumOfTrucks * 100, by = region]

  ## calculate resulting ES values by size
  ESSharesTargetSize <- merge(VehSharesTargetSize, annualTkmPerVehicle, by = c("region", "univocalName") )

  ESSharesTargetSize[ , ESsharesUnnormalized := value / 100 * ESperVeh ]

  ESSharesTargetSize[ , ESsharesNormalized := ESsharesUnnormalized / sum(ESsharesUnnormalized), by = region ]

  ESSharesTargetSizeBack <- ESSharesTargetSize[ , .(univocalName = univocalName ,
                                                    region = region,
                                                    ESshare = ESsharesNormalized)
  ]

  # calculate target ES per vehicle size with old ES totals
  ## First create new DT with period x univocalname size with the "allow.cartesian = TRUE, as SizeSharesTarget has no period, and histESdemandtoUpdateoldTotal no size

  histESdemandtoUpdatetargetPerSize <- ESSharesTargetSizeBack[
    histESdemandtoUpdateoldTotal,
    on = "region",
    allow.cartesian = TRUE
  ]

  histESdemandtoUpdatetargetPerSize[
    , targetES := ESshare * totalES
  ]

  histESdemandtoUpdatetargetPerSize[ , targetES := ESshare * totalES ]

  ## calculate scaling factors for each size
  ESscaling <- merge(histESdemandtoUpdateoldSize, histESdemandtoUpdatetargetPerSize, by = c("period", "univocalName", "region") )

  ESscaling[ , scaling := targetES / oldES ]

  ## rescale using old ES totals:
  histESdemandtoUpdatenewES <- merge(histESdemandtoUpdateold, ESscaling, by = c("period", "univocalName", "region") )

  histESdemandtoUpdatenewES[, value := value * scaling ]

  ## drop unused columns, add variable and unit
  histESdemandtoUpdatenewES[, c("oldES", "ESshare", "totalES", "targetES", "scaling") := NULL][, ':='(variable = "ES", unit = "billion tkm/yr")]

  ## check that the total new ES and the total old ES are unchanged:

  setkey(histESdemandtoUpdatenewES,region)
  setkey(histESdemandtoUpdateold,region)

  stopifnot(
    all.equal(
      histESdemandtoUpdatenewES[, sum(value), by = .(period,region)],
      histESdemandtoUpdateold[, sum(value), by = .(period,region)]
    )
  )


  ## update the original dt - overwrite values for which there are new values in histESdemandtoUpdatenewES
  setnames(histESdemandtoUpdatenewES, "value", "valueNew")
  dt <- merge(dt, histESdemandtoUpdatenewES, by = intersect(names(dt), names(histESdemandtoUpdatenewES)), all.x = TRUE)
  dt[!is.na(valueNew), value := valueNew][, valueNew := NULL]



  ###### also adjust car ES demands upwards to better reflect car stock numbers in 2010 and 2015
  ## (~62 mio in 2010, 140 mio in 2015 , eg IEA GEVO and others - see file in the owncloud "Data/RegionalData/compiling_CHA_data_LDV.xlsx",
  ## Increasing the ES values means that more cars are on the road, but also that 2010 FE demand BEFORE the IEA calibration is higher - thus preventing
  ## the strong upscaling of energy intensities during the IEA FE calibration that was previously the case.
  ## The multipliers are staggered (reducing from 2.5 in 2010 to 1.5 in 2005) to represent the fast growth of LDV numbers over these 5 years.
  ## The 2.5 multiplier in 2010 is a compromise betwen hitting 2010 and 2015 numbers: 2010 85 mio instead of 62 mio, 2015 125 mio instead of 140 mio
  #plausibilityFix_CHN #plausibilityFix_LDV #plausibilityFix_Pass

  carTypes <- c("Compact Car", "Large Car", "Large Car and SUV", "Midsize Car", "Mini Car", "Subcompact Car","Van")

  dt[univocalName %in% carTypes & region == "CHN" & period == 2010, value := 2.5 * value]
  dt[univocalName %in% carTypes & region == "CHN" & period == 2009, value := 2.3 * value]
  dt[univocalName %in% carTypes & region == "CHN" & period == 2008, value := 2.1 * value]
  dt[univocalName %in% carTypes & region == "CHN" & period == 2007, value := 1.9 * value]
  dt[univocalName %in% carTypes & region == "CHN" & period == 2006, value := 1.7 * value]
  dt[univocalName %in% carTypes & region == "CHN" & period <= 2005, value := 1.5   * value]

  ############ end of new CHA stuff from Robert

  #plausibilityFix_USA #plausibilityFix_Truck #plausibilityFix_Freight
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


  #plausibilityFix_CHN #plausibilityFix_Rail #plausibilityFix_Pass
  #from https://www.iea.org/reports/tracking-rail-2020-2
  dt[period <= 2010 & regionCode21 == "CHN" & univocalName == "HSR", value := 70000]

  #plausibilityFix_CHN #plausibilityFix_Truck #plausibilityFix_Freight
  # from https://theicct.org/sites/default/files/China_Freight_Assessment_English_20181022.pdf
  # total road freight demand seems to be around 5 billion tkm * 0.8, a factor 3 roughly
  dt[period <= 2010 & regionCode21 == "CHN" & univocalName %in% filter$trn_freight_road, value := value * 3]

  #4: Demand level corrections, adjusting to ETP demands
  #plausibilityFix_CHN #plausibilityFix_Bus #plausibilityFix_Pass   #plausibilityFix_IND #plausibilityFix_OAS #plausibilityFix_NEU #plausibilityFix_MEA
  dt[regionCode21 == "CHA" & univocalName == "Bus", value := value / 2.5]
  dt[regionCode21 == "IND" & univocalName == "Bus", value := value / 2]
  dt[regionCode21 == "OAS" & univocalName == "Bus", value := value / 5]
  dt[regionCode12 == "NEU" & univocalName == "Bus", value := value / 2]
  dt[regionCode21 == "MEA" & univocalName == "Bus", value := value / 2]

  #plausibilityFix_DEU #plausibilityFix_Truck #plausibilityFix_Freight
  #5: Adjust GER Truck size shares according to KBA data (calculated from stocks via AM and LF)
  dt[region == "DEU" & univocalName == "Truck (0-3_5t)", value := value * 2]
  dt[region == "DEU" & univocalName == "Truck (7_5t)", value := value * 0.25]
  dt[region == "DEU" & univocalName == "Truck (18t)", value := value * 0.65]
  dt[region == "DEU" & univocalName == "Truck (40t)", value := value * 1.4]

  #plausibilityFix_DEU #plausibilityFix_Truck #plausibilityFix_Freight
  #6: Total 2010 Freight demands, from ViZ 2010
  # (the shares are roughly OK)
  dt[region == "DEU" & univocalName %in% filter$trn_freight, value := value * 620 / 587]
  dt[, c("countryName", "regionCode21", "regionCode12", "check") := NULL]

  return(dt)
}
