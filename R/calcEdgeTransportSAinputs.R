#' Provide EDGE-Transport input parameters
#' @author Johanna Hoppe
#' @param subtype one of the parameters required for EDGE-T SA
#' @param IEAharm switch IEA harmonization of energy intensity on and off
#' @param SSPscen shared socioeconomic pathway
#' @import data.table
#' @importFrom rmndt approx_dt
#' @importFrom magclass time_interpolate

calcEdgeTransportSAinputs <- function(subtype, SSPscen = "SSP2", IEAharm = TRUE) { # nolint: cyclocomp_linter

  temporal <- spatial <- present <- period <- region <- technology <-
    univocalName <- gdppc <- speed <- variable <- value <- multiplier <-
    capex <- untilPrice <- untilPrice2 <- purchasePriceSubsidy <- purchasePriceSubsidy2 <- NULL

  monUnit <- gsub(".*?(\\d{4}).*", "US$\\1", mrdrivers::toolGetUnitDollar())

  lowResYears <- data.table(temporal = "all", period = c(
    1990,
    seq(2005, 2060, by = 5),
    seq(2070, 2110, by = 10),
    2130, 2150
  ))

  highResYears <- data.table(temporal = "all", period = c(
    1990,
    seq(2005, 2100, by = 1),
    2110, 2130, 2150
  ))

  lowResUnivocalNames <- c("Cycle",
                           "Domestic Aviation",
                           "Domestic Ship",
                           "Freight Rail",
                           "HSR",
                           "International Aviation",
                           "International Ship",
                           "Moped",
                           "Motorcycle (50-250cc)",
                           "Motorcycle (>250cc)",
                           "Rickshaw",
                           "Passenger Rail",
                           "Walk")

  highResUnivocalNames <- c("Bus",
                            "Compact Car",
                            "Large Car",
                            "Large Car and SUV",
                            "Midsize Car",
                            "Mini Car",
                            "Subcompact Car",
                            "Truck (0-3_5t)",
                            "Truck (18t)",
                            "Truck (26t)",
                            "Truck (40t)",
                            "Truck (7_5t)",
                            "Van")

  # decisionTree.csv contains all possible branches of the decision tree
  decisionTree <- fread(system.file("extdata/decisionTree.csv", package = "mrtransport", mustWork = TRUE))
  decisionOptions <- decisionTree[, c("univocalName", "technology")]
  decisionOptions[, temporal := "all"][, spatial := "all"]
  # Not all countries feature the same branches of the decision tree - Some vehicleTypes and modes are not
  # available in certain countries
  # Here we create the full structure of the nested decision tree differentiated for all countries to make it testable
  ISOcountriesMap <- system.file("extdata", "regionmappingISOto21to12.csv", package = "mrtransport", mustWork = TRUE)
  ISOcountriesMap <- fread(ISOcountriesMap, skip = 0)
  setnames(ISOcountriesMap, c("countryCode"), c("region"))
  ISOcountries <- ISOcountriesMap[, c("region")][, spatial := "all"]

  completeDataSet <- merge.data.table(decisionOptions, ISOcountries, by = "spatial", allow.cartesian = TRUE)
  completeDataSet[, spatial := NULL]
  mapCountrySpecificVehicleTypes <- fread(system.file("extdata/mapCountrySpecificVehicleTypes.csv",
                                                      package = "mrtransport", mustWork = TRUE))

  completeDataSet <- merge.data.table(completeDataSet, mapCountrySpecificVehicleTypes,
                                      by = c("region", "univocalName"), all = TRUE)
  completeDataSet <- completeDataSet[present == 1][, present := NULL]

  completeDataSet <- merge.data.table(completeDataSet, highResYears, by = "temporal", allow.cartesian = TRUE)

  lowResYears <- lowResYears[, c(period)]
  highResYears <- highResYears[, c(period)]

  completeDataSet <- completeDataSet[(univocalName %in% highResUnivocalNames
                                      & period %in% highResYears)
                                     | (univocalName %in% lowResUnivocalNames
                                        & period %in% lowResYears)]

  completeDataSet[, temporal := NULL]
  completeDataSet[, check := 1]

  setkey(completeDataSet, region, period, univocalName, technology)

  # categories for filtering data
  categories <- c("trn_pass_road_LDV_4W", "trn_pass_road_LDV_3W", "trn_pass_road_LDV_2W", "trn_freight_road",
                  "trn_pass", "trn_freight", "trn_pass_road")

  findEntries <- function(category, dataTable) {
    test <- dataTable[, lapply(.SD, function(x) grepl(category, x))]
    entries <- unique(decisionTree[rowSums(test) > 0]$univocalName)
    return(entries)
  }

  filterEntries <- lapply(setNames(categories, categories), findEntries, dataTable = decisionTree)

  switch(
    subtype,
    "energyIntensity" = {
      unit <- "MJ/vehkm"
      description <- "Energy intensity on technology level. Sources: TRACCS, PSI, UCD, GCAM"
      weight <- calcOutput("GDP",
                           scenario = c("SSPs", "SDPs", "SSP2IndiaDEAs"),
                           average2020 = FALSE,
                           aggregate = FALSE) |>
        time_interpolate(highResYears)
      weight <- weight[, , SSPscen]

      # calc different source data
      enIntGCAM <- toolPrepareGCAM(readSource("GCAM", subtype), subtype)
      enIntUCD <- toolPrepareUCD(readSource("UCD", subtype), subtype)
      enIntTRACCS <- toolPrepareTRACCS(readSource("TRACCS", subtype), subtype)
      countriesTRACCS <- unique(enIntTRACCS$region)
      enIntPSI <- toolPreparePSI(readSource("PSI", subtype))

      # Inter- and extrapolate all data to model input data years
      data <- list(enIntGCAM = enIntGCAM, enIntUCD = enIntUCD, enIntTRACCS = enIntTRACCS, enIntPSI = enIntPSI)

      data <- lapply(data, approx_dt, highResYears, "period", "value",
                     c("region", "univocalName", "technology", "variable", "unit"), extrapolate = TRUE)

      energyIntensityRaw <- toolMergeEnergyIntensity(data, filterEntries, countriesTRACCS)

      # Include data adjustments: fill gaps and correct data if necessary based on projects, other sources and own
      # assumptions
      energyIntensity <- toolAdjustEnergyIntensity(energyIntensityRaw, countriesTRACCS, data$enIntPSI, filterEntries)

      # Harmonize energy intensity data in order to match IEA final energy values
      if (IEAharm == TRUE) {
        energyIntensity <- mrtransport::toolIEAharmonization(enIntensity = energyIntensity)
      }

      # Add energy intensity of zero for active modes
      activeModes <- completeDataSet[univocalName %in% c("Cycle", "Walk")]
      activeModes[, unit := "MJ/vehkm"][, variable := "Energy intensity"][, value := 0][, check := NULL]
      energyIntensity <- rbind(energyIntensity, activeModes)

      energyIntensity <- energyIntensity[, c("region", "period", "univocalName", "technology",
                                             "variable", "unit", "value")]
      energyIntensity <- energyIntensity[(univocalName %in% highResUnivocalNames
                                          & period %in% highResYears)
                                         | (univocalName %in% lowResUnivocalNames
                                            & period %in% lowResYears)]

      setkey(energyIntensity, region, period, univocalName, technology, variable, unit)

      
      # Check whether data is complete
      check <- merge.data.table(completeDataSet, energyIntensity, all = TRUE)
      if (nrow(check[is.na(value)]) > 0) {
        stop("Energy intensity input data is incomplete")
      } else if (nrow(check[is.na(check)]) > 0) {
        stop("Unnecessary data is provided")
      } else if (length(unique(check$unit)) > 1) {
        stop("Something went wrong in generating energy intensity input data.
             Data does not have the same unit.")
      } else if (length(unique(check$variable)) > 1) {
        stop("Something went wrong in generating energy intensity input data.
             Data does not have the same variable type.")
      } else if (anyNA(energyIntensity) == TRUE) {
        stop("Energy intensity data includes NAs")
      }

      quitteobj <- energyIntensity

    },
    "annualMileage" = {
      unit <- "vehkm/veh yr"
      description <- "Annual mileage on technology level. Sources: TRACCS, UCD"
      weight <- calcOutput("GDP",
                           scenario = c("SSPs", "SDPs", "SSP2IndiaDEAs"),
                           average2020 = FALSE,
                           aggregate = FALSE) |>
        time_interpolate(highResYears)
      weight <- weight[, , SSPscen]

      # calc different source data
      AMTRACCS <- toolPrepareTRACCS(readSource("TRACCS", subtype), subtype)
      AMUCD <- toolPrepareUCD(readSource("UCD", subtype), subtype)

      # Inter- and extrapolate all data to model input data years
      data <- list(AMTRACCS = AMTRACCS, AMUCD = AMUCD)
      data <- lapply(data, approx_dt, highResYears, "period", "value",
                     c("region", "univocalName", "technology",
                       "variable", "unit"), extrapolate = TRUE)

      # merge.data.table data
      # TRACCS is used first, than UCD
      # EU data is used from TRACCS, rest is filled with UCD
      annualMileageRaw <- rbind(data$AMTRACCS, data$AMUCD[!(region %in% unique(data$AMTRACCS$region))])

      setkey(annualMileageRaw, region, period, univocalName, technology, variable, unit)

      
      annualMileage <- toolAdjustAnnualMileage(annualMileageRaw, completeDataSet, filterEntries)

      # Add annual mileage of zero for active modes
      activeModes <- completeDataSet[univocalName %in% c("Cycle", "Walk")]
      activeModes[, unit := "vehkm/veh yr"][, variable := "Annual mileage"][, value := 0][, check := NULL]
      annualMileage <- rbind(annualMileage, activeModes)

      annualMileage <- annualMileage[, c("region", "period", "univocalName", "technology",
                                         "variable", "unit", "value")]
      annualMileage <- annualMileage[(univocalName %in% highResUnivocalNames
                                      & period %in% highResYears)
                                     | (univocalName %in% lowResUnivocalNames
                                        & period %in% lowResYears)]
      setkey(annualMileage, region, period, univocalName, technology, variable, unit)

      # Check whether data is complete
      check <- merge.data.table(completeDataSet, annualMileage, all = TRUE)
      if (nrow(check[is.na(value)]) > 0) {
        stop("Annual mileage input data is incomplete")
      } else if (nrow(check[is.na(check)]) > 0) {
        stop("Unnecessary data is provided")
      } else if (length(unique(check$unit)) > 1) {
        stop("Something went wrong in generating annual mileage input data. Data does not have the same unit.")
      } else if (length(unique(check$variable)) > 1) {
        stop("Something went wrong in generating annual mileage input data. Data does not have the same variable type.")
      } else if (anyNA(annualMileage) == TRUE) {
        stop("Annual mileage data includes NAs")
      }

      quitteobj <- annualMileage
    },
    "histESdemand" = {
      unit <- "billion (p|t)km/yr"
      description <- "Energy service demand on technology level. Sources: GCAM, TRACCS, Eurostat"
      weight <- NULL

      # calc different source data
      esDemandGCAM <- toolPrepareGCAM(readSource("GCAM", subtype), subtype)
      esDemandTRACCS <- toolPrepareTRACCS(readSource("TRACCS", subtype), subtype)
      countriesTRACCS <- unique(esDemandTRACCS$region)
      feDemandEurostat <- toolPrepareEurostatEnergyCountryDataSheets(
        readSource("EurostatEnergyCountryDataSheets", "feDemand")
      )
      enIntensity <- magpie2dt(calcOutput(type = "EdgeTransportSAinputs", subtype = "energyIntensity",
                                          IEAharm = FALSE, warnNA = FALSE, aggregate = FALSE))
      loadFactor <- magpie2dt(calcOutput(type = "EdgeTransportSAinputs", subtype = "loadFactor",
                                         warnNA = FALSE, aggregate = FALSE))

      # Inter- and extrapolate all data to model input data years
      data <- list(esDemandGCAM     = esDemandGCAM,
                   esDemandTRACCS   = esDemandTRACCS,
                   feDemandEurostat = feDemandEurostat,
                   enIntensity      = enIntensity,
                   loadFactor       = loadFactor)

      # The historical energy service demand is only used for years <= 2010, future years will be calculated by demand
      # regression in the model
      data <- lapply(data, approx_dt, highResYears[highResYears <= 2010], "period", "value",
                     c("region", "univocalName", "technology",
                       "variable", "unit"), extrapolate = TRUE)

      esDemandRaw <- toolMergeHistESdemand(data, filterEntries, countriesTRACCS)

      esDemand <- toolAdjustEsDemand(esDemandRaw, ISOcountriesMap, completeDataSet, filterEntries)

      # Harmonize energy intensity data in order to match IEA final energy values
      if (IEAharm == TRUE) {
        esDemand <- mrtransport::toolIEAharmonization(esDemand = esDemand)
      }

      esDemand <- esDemand[, c("region", "period", "univocalName", "technology",
                               "variable", "unit", "value")]
      esDemand <- esDemand[(univocalName %in% highResUnivocalNames & period %in% highResYears)
                           | (univocalName %in% lowResUnivocalNames & period %in% lowResYears)]

      setkey(esDemand,  region, period, univocalName, technology,
             variable, unit)

      # Check whether data is complete
      check <- merge.data.table(completeDataSet[period <= 2010], esDemand, all = TRUE)
      if (nrow(check[is.na(value)]) > 0) {
        stop("Historical energy service demand input data is incomplete")
      } else if (nrow(check[is.na(check)]) > 0) {
        stop("Unnecessary data is provided")
      } else if (length(unique(check$unit)) > 2) {
        stop("Something went wrong in generating historical energy service demand input data.
             Data does not have the same unit.")
      } else if (length(unique(check$variable)) > 1) {
        stop("Something went wrong in generating historical energy service demand input data. Data does not have
             the same variable type.")
      } else if (anyNA(esDemand) == TRUE) {
        stop("Historical energy service demand data includes NAs")
      }

      quitteobj <- esDemand
    },
    "loadFactor" = {
      unit <- "(t|p)/veh"
      description <- "Load factor on technology level that states the tons/number of passengers in a vehicle.
                      Sources: TRACCS, GCAM"
      weight <- calcOutput("GDP",
                           scenario = c("SSPs", "SDPs", "SSP2IndiaDEAs"),
                           average2020 = FALSE,
                           aggregate = FALSE) |>
        time_interpolate(highResYears)
      weight <- weight[, , SSPscen]

      # read different source data
      LFTRACCS <- toolPrepareTRACCS(readSource("TRACCS", subtype), subtype)
      LFGCAM <- toolPrepareGCAM(readSource("GCAM", subtype), subtype)
      # Inter- and extrapolate all data to model input data years
      data <- list(LFTRACCS = LFTRACCS, LFGCAM = LFGCAM)
      data <- lapply(data, approx_dt, highResYears, "period", "value",
                     c("region", "univocalName", "technology",
                       "variable", "unit"), extrapolate = TRUE)

      # merge.data.table data
      # Eurostat>TRACCS>GCAM
      #- EU data is used from TRACCS, rest is filled with GCAM
      countriesTRACCS <- unique(data$LFTRACCS$region)
      loadFactorRaw <- rbind(data$LFTRACCS, data$LFGCAM[!(region %in% countriesTRACCS
                                                          & univocalName %in%
                                                            c(filterEntries$trn_pass_road,
                                                              filterEntries$trn_freight_road))])
      loadFactor <- toolAdjustLoadFactor(loadFactorRaw, completeDataSet, countriesTRACCS, filterEntries)

      # Add load factor of zero for active modes
      activeModes <- completeDataSet[univocalName %in% c("Cycle", "Walk")]
      activeModes[, unit := "p/veh"][, variable := "Load factor"][, value := 0][, check := NULL]
      loadFactor <- rbind(loadFactor, activeModes)

      loadFactor <- loadFactor[, c("region", "period", "univocalName", "technology",
                                   "variable", "unit", "value")]
      loadFactor <- loadFactor[(univocalName %in% highResUnivocalNames
                                & period %in% highResYears)
                               | (univocalName %in% lowResUnivocalNames
                                  & period %in% lowResYears)]

      setkey(loadFactor, region, period, univocalName, technology,
             variable, unit)

      # Check whether data is complete
      check <- merge.data.table(completeDataSet, loadFactor, all = TRUE)
      if (nrow(check[is.na(value)]) > 0) {
        stop("Load factor input data is incomplete")
      } else if (nrow(check[is.na(check)]) > 0) {
        stop("Unnecessary data is provided")
      } else if (length(unique(check$unit)) > 2) {
        stop("Something went wrong in generating load factor input data. Data  unit.")
      } else if (length(unique(check$variable)) > 1) {
        stop("Something went wrong in generating load factor input data. Data does not have the same variable type.")
      } else if (anyNA(loadFactor) == TRUE) {
        stop("Load factor data includes NAs")
      }
      quitteobj <- loadFactor
    },
    "CAPEXtrackedFleet" = {
      unit <- paste0(monUnit, "/veh")
      description <- "CAPEX for vehicle types that feature fleet tracking (cars, trucks and busses).
                      Sources: UCD, PSI"
      weight <- calcOutput("GDP",
                           scenario = c("SSPs", "SDPs", "SSP2IndiaDEAs"),
                           average2020 = FALSE,
                           aggregate = FALSE) |>
        time_interpolate(highResYears)
      weight <- weight[, , SSPscen]

      # read PSI CAPEX
      CAPEXPSI <- toolPreparePSI(readSource("PSI", "CAPEX"))
      # read UCD CAPEX given in US$/vehkm and US$/veh
      CAPEXUCD <- toolPrepareUCD(readSource("UCD", "CAPEX"), "CAPEX")
      # For some modes UCD offers only a combined value for CAPEX and non-fuel OPEX given in US$/vehkm
      CAPEXcombinedUCD <- toolPrepareUCD(readSource("UCD", "CAPEXandNonFuelOPEX"), "CAPEXandNonFuelOPEX")
      # Operating subsidies for Freight Rail, Passenger Rail, HSR (+ Bus not used here) are partially attributed to
      # CAPEX otherwise negative values in OPEX
      operatingSubsidyUCD <- toolPrepareUCD(readSource("UCD", "OperatingSubsidies"), "OperatingSubsidies")
      operatingSubsidyUCD <- operatingSubsidyUCD[univocalName == "Bus"]

      # Inter- and extrapolate all data to model input data years
      data <- list(CAPEXPSI = CAPEXPSI,
                   CAPEXUCD = CAPEXUCD,
                   CAPEXcombinedUCD = CAPEXcombinedUCD,
                   operatingSubsidyUCD = operatingSubsidyUCD)
      data <- lapply(data, approx_dt, highResYears, "period", "value",
                     c("region", "univocalName", "technology",
                       "variable", "unit"), extrapolate = TRUE)

      # Use only for cars (trucks and busses are given combined with non fuel OPEX)
      # -> Filter out purchase costs as they are taken from PSI
      CAPEXUCD4W <- data$CAPEXUCD[univocalName %in% filterEntries$trn_pass_road_LDV_4W
                                  & !variable == "Capital costs (purchase)"]

      # Take purchase costs from PSI but keep only the vehicle types present in UCD for non EUR regions
      vehTypeFilter <- unique(CAPEXUCD4W[, c("region", "univocalName")])
      PSIpurchaseCosts <- merge(data$CAPEXPSI, vehTypeFilter, by = c("region", "univocalName"), all.y = TRUE)

      # CAPEX for Busses and Trucks given combined with non-fuel OPEX in the UCD data
      CAPEXcombinedUCD <- data$CAPEXcombinedUCD
      CAPEXcombinedUCD <- CAPEXcombinedUCD[univocalName %in% filterEntries$trn_freight_road | univocalName == "Bus"]

      # merge.data.table data
      # PSI CAPEX for 4 Wheelers feature only purchase costs - take other capital costs from UCD for EUR regions
      CAPEXraw <- rbind(PSIpurchaseCosts, CAPEXUCD4W, CAPEXcombinedUCD, data$operatingSubsidyUCD)

      GDPpcMERmag <- calcOutput("GDPpc",
                                scenario = c("SSPs", "SDPs", "SSP2IndiaDEAs"),
                                aggregate = FALSE,
                                unit = mrdrivers::toolGetUnitDollar()) |>
        time_interpolate(highResYears)
      GDPpcMERmag <- GDPpcMERmag[, , SSPscen]

      GDPpcMER <- magpie2dt(GDPpcMERmag, yearcol = "period", regioncol = "region", valcol = "gdppc")[, variable := NULL]
      CAPEX <- toolAdjustCAPEXtrackedFleet(CAPEXraw, ISOcountriesMap, highResYears,
                                           completeDataSet, GDPpcMER, filterEntries)

      CAPEX <-  CAPEX[, c("region", "period", "univocalName", "technology",
                          "variable", "unit", "value")]

      CAPEX <- CAPEX[(univocalName %in% highResUnivocalNames
                      & period %in% highResYears)
                     | (univocalName %in% lowResUnivocalNames
                        & period %in% lowResYears)]

      setkey(CAPEX, region, period, univocalName, technology,
             variable, unit)

      # CAPEXtrackedFleet data only includes CAPEX data for LDV 4 Wheelers, Trucks and Busses
      completeDataCAPFleet <- completeDataSet[univocalName %in% filterEntries$trn_freight_road
                                              | univocalName %in% filterEntries$trn_pass_road_LDV_4W |
                                                univocalName == "Bus"]

      # Check whether data is complete
      check <- merge.data.table(completeDataCAPFleet, CAPEX, all = TRUE)
      if (nrow(check[is.na(value)]) > 0) {
        stop("CAPEX input data for vehicle types that feature fleet tracking is incomplete")
      } else if (nrow(check[is.na(check)]) > 0) {
        stop("CAPEX input data for vehicle types that feature fleet tracking includes unnecessary data")
      } else if (length(unique(check$unit)) > 1) {
        stop("Something went wrong in generating CAPEX input data for the tracked fleet.
             Data does not have the same unit.")
      } else if (anyNA(CAPEX) == TRUE) {
        stop("CAPEX data for the tracked fleet includes NAs")
      }

      quitteobj <- CAPEX
    },
    "nonFuelOPEXtrackedFleet" = {
      unit <- paste0(monUnit, "/veh yr")
      description <- "Non-fuel OPEX on technology level for vehicle types that feature fleet tracking
                     (cars, trucks, busses). Sources: UCD, PSI"
      weight <- calcOutput("GDP",
                           scenario = c("SSPs", "SDPs", "SSP2IndiaDEAs"),
                           average2020 = FALSE,
                           aggregate = FALSE) |>
        time_interpolate(highResYears)
      weight <- weight[, , SSPscen]

      nonFuelOPEXUCD <- toolPrepareUCD(readSource("UCD", "nonFuelOPEX"), "nonFuelOPEX")
      nonFuelOPEXUCD <- nonFuelOPEXUCD[univocalName %in% filterEntries$trn_pass_road_LDV_4W]
      # For trucks and busses UCD offers only a combined value for CAPEX and non-fuel OPEX given in US$/vehkm
      nonFuelOPEXcombinedUCD <- toolPrepareUCD(readSource("UCD", "CAPEXandNonFuelOPEX"), "CAPEXandNonFuelOPEX")
      nonFuelOPEXcombinedUCD <- nonFuelOPEXcombinedUCD[univocalName %in% filterEntries$trn_freight_road
                                                       | univocalName == "Bus"]
      # Operating subsidies for Freight Rail, Passenger Rail, HSR (+ Bus not used here) are partially
      operatingSubsidyUCD <- toolPrepareUCD(readSource("UCD", "OperatingSubsidies"), "OperatingSubsidies")
      operatingSubsidyUCD <- operatingSubsidyUCD[univocalName == "Bus"]

      # Inter- and extrapolate all data to model input data years
      data <- list(nonFuelOPEXUCD = nonFuelOPEXUCD,
                   nonFuelOPEXcombinedUCD = nonFuelOPEXcombinedUCD,
                   operatingSubsidyUCD = operatingSubsidyUCD)
      data <- lapply(data, approx_dt, highResYears, "period", "value",
                     c("region", "univocalName", "technology",
                       "variable", "unit"), extrapolate = TRUE)

      nonFuelOPEXraw <- rbind(data$nonFuelOPEXUCD, data$nonFuelOPEXcombinedUCD, data$operatingSubsidyUCD)
      nonFuelOPEX <- toolAdjustNonFuelOPEXtrackedFleet(nonFuelOPEXraw, highResYears, completeDataSet, filterEntries)

      # nonFuelOPEXtrackedFleet data only includes data for LDV 4 Wheelers, Trucks and Busses
      completeDataOPFleet <- completeDataSet[univocalName %in% filterEntries$trn_freight_road
                                             | univocalName %in% filterEntries$trn_pass_road_LDV_4W |
                                               univocalName == "Bus"]

      nonFuelOPEX <- nonFuelOPEX[, c("region", "period", "univocalName", "technology",
                                     "variable", "unit", "value")]
      nonFuelOPEX <- nonFuelOPEX[(univocalName %in% highResUnivocalNames
                                  & period %in% highResYears)
                                 | (univocalName %in% lowResUnivocalNames
                                    & period %in% lowResYears)]

      setkey(nonFuelOPEX, region, period, univocalName, technology,
             variable, unit)

      # Check whether data is complete
      check <- merge.data.table(completeDataOPFleet, nonFuelOPEX, all = TRUE)
      if (nrow(check[is.na(value)]) > 0) {
        stop("Non fuel OPEX input data for vehicle types that feature fleet tracking is incomplete")
      } else if (nrow(check[is.na(check)]) > 0) {
        stop("Non fuel OPEX input data for vehicle types that feature fleet tracking includes unnecessary data")
      } else if (length(unique(check$unit)) > 1) {
        stop("Something went wrong in generating non fuel OPEX input data for the tracked fleet.
             Data does not have the same unit.")
      } else if (length(unique(check$variable)) > 1) {
        stop("Something went wrong in generating non fuel OPEX input data for the tracked fleet.
             Data does not have the same variable type.")
      } else if (anyNA(nonFuelOPEX) == TRUE) {
        stop("Non fuel OPEX data for the tracked fleet includes NAs")
      }

      quitteobj <- nonFuelOPEX
    },
    "CAPEXother" = {
      unit <- paste0(monUnit, "/vehkm")
      description <- "CAPEX (purchase costs) for vehicle types that do not feature fleet tracking
                      (all other than cars, trucks and busses). Sources: UCD"
      weight <- calcOutput("GDP",
                           scenario = c("SSPs", "SDPs", "SSP2IndiaDEAs"),
                           average2020 = FALSE,
                           aggregate = FALSE) |>
        time_interpolate(lowResYears)
      weight <- weight[, , SSPscen]

      # read UCD CAPEX given in US$/vehkm and US$/veh
      # non fuel OPEX include Domestic Aviation, International Aviation, Moped, Motorcycle (50-250cc),
      # Motorcycle (>250cc) (+4W not used here)
      CAPEXUCD <- toolPrepareUCD(readSource("UCD", "CAPEX"), "CAPEX")
      CAPEXUCD <- CAPEXUCD[!univocalName %in% filterEntries$trn_pass_road_LDV_4W]
      # For some modes UCD offers only a combined value for CAPEX and non-fuel OPEX given in US$/vehkm
      # combined CAPEX and OPEX include Domestic Ship, Freight Rail, HSR, International Ship, Passenger Rail
      CAPEXcombinedUCD <- toolPrepareUCD(readSource("UCD", "CAPEXandNonFuelOPEX"), "CAPEXandNonFuelOPEX")
      CAPEXcombinedUCD <- CAPEXcombinedUCD[!(univocalName %in% filterEntries$trn_freight_road | univocalName == "Bus")]
      # Operating subsidies for Freight Rail, Passenger Rail, HSR (+ Bus not used here) are partially
      operatingSubsidyUCD <- toolPrepareUCD(readSource("UCD", "OperatingSubsidies"), "OperatingSubsidies")
      operatingSubsidyUCD <- operatingSubsidyUCD[!(univocalName == "Bus")]

      # Inter- and extrapolate all data to model input data years
      data <- list(CAPEXUCD = CAPEXUCD, CAPEXcombinedUCD = CAPEXcombinedUCD, operatingSubsidyUCD = operatingSubsidyUCD)
      data <- lapply(data, approx_dt, lowResYears, "period", "value",
                     c("region", "univocalName", "technology",
                       "variable", "unit"), extrapolate = TRUE)

      # Data for two wheelers is given in US$/veh and needs to be converted to US$/vehkm
      # with the help of annuity and annual mileage

      # UCD applied interest rate of 10% and uniform vehicle lifetime of 15 yrs
      # (https://itspubs.ucdavis.edu/publication_detail.php?id=1884)
      # Calc annuity factor
      discountRate <- 0.1   #discount rate for vehicle purchases
      lifeTime <- 15    #Number of years over which vehicle capital payments are amortized
      annuityFactor <- (discountRate * (1 + discountRate) ^ lifeTime) / ((1 + discountRate) ^ lifeTime - 1)

      AMUCD2W <- toolPrepareUCD(readSource("UCD", "annualMileage"), "annualMileage")
      AMUCD2W <- AMUCD2W[univocalName %in% filterEntries$trn_pass_road_LDV_2W]
      AMUCD2W <- AMUCD2W[, c("region", "univocalName", "technology", "period", "value")]
      AMUCD2W <- approx_dt(AMUCD2W, lowResYears, "period", "value", c("region", "technology", "univocalName"),
                           extrapolate = TRUE)
      setnames(AMUCD2W, "value", "annualMileage")

      CAPEXUCD <- merge.data.table(data$CAPEXUCD, AMUCD2W, by = c("region", "univocalName", "technology", "period"),
                                   all.x = TRUE)
      CAPEXUCD[univocalName %in% filterEntries$trn_pass_road_LDV_2W, value := (value * annuityFactor) / annualMileage]
      CAPEXUCD[univocalName %in% filterEntries$trn_pass_road_LDV_2W,
               unit := paste0(monUnit, "/vehkm")][, annualMileage := NULL]

      # merge.data.table data
      CAPEXraw <- rbind(CAPEXUCD, data$CAPEXcombinedUCD, data$operatingSubsidyUCD)

      GDPpcMERmag <- calcOutput("GDPpc",
                                scenario = c("SSPs", "SDPs", "SSP2IndiaDEAs"),
                                aggregate = FALSE,
                                unit = mrdrivers::toolGetUnitDollar()) |>
        time_interpolate(lowResYears)
      GDPpcMERmag <- GDPpcMERmag[, , SSPscen]

      GDPpcMER <- magpie2dt(GDPpcMERmag, yearcol = "period", regioncol = "region", valcol = "gdppc")[, variable := NULL]

      CAPEX <- toolAdjustCAPEXother(CAPEXraw, ISOcountriesMap, lowResYears, completeDataSet, GDPpcMER, filterEntries)

      CAPEX <- CAPEX[, c("region", "period", "univocalName", "technology", "variable", "unit", "value")]

      CAPEX <- CAPEX[(univocalName %in% highResUnivocalNames
                      & period %in% highResYears)
                     | (univocalName %in% lowResUnivocalNames
                        & period %in% lowResYears)]

      setkey(CAPEX,  region, period, univocalName, technology, variable, unit)

      # CAPEXother only includes data for all other modes than LDV 4 Wheelers, Trucks and Busses
      # (except cycling and walking)
      completeDataCAP <- completeDataSet[!(univocalName %in% c(filterEntries$trn_freight_road, "Cycle", "Walk") |
                                             univocalName %in% filterEntries$trn_pass_road_LDV_4W
                                           | univocalName == "Bus")]

      # Check whether data is complete
      check <- merge.data.table(completeDataCAP, CAPEX, all = TRUE)
      byCols <- names(check)
      byCols <- byCols[!byCols %in% c("value", "variable")]
      check[, sum := sum(value), by = eval(byCols)]
      if (nrow(check[is.na(value)]) > 0) {
        stop("CAPEX input data for vehicle types that do not feature fleet tracking is incomplete")
      } else if (nrow(check[is.na(check)]) > 0) {
        stop("CAPEX input data for vehicle types that do not feature fleet tracking includes unnecessary data")
      } else if (length(unique(check$unit)) > 1) {
        stop("Something went wrong in generating CAPEX input data for vehicle types that do not feature fleet tracking.
             Data does not have the same unit.")
      } else if (length(unique(check$variable)) > 1) {
        stop("Something went wrong in generating CAPEX input data for vehicle types that do not feature fleet tracking.
             Data does not have the same variable type.")
      } else if (anyNA(CAPEX) == TRUE) {
        stop("CAPEX data for vehicle types that do not feature fleet tracking includes NAs")
      } else if (nrow(check[sum < 0]) > 0) {
        stop("Aggregated CAPEX for vehicle types that do not feature fleet tracking includes negative values")
      }

      quitteobj <- CAPEX
    },
    "nonFuelOPEXother" = {
      unit <- paste0(monUnit, "/vehkm")
      description <- "Non fuel OPEX on technology level for vehicle types that do not feature fleet tracking
                     (other than cars, trucks, busses). Sources: UCD, PSI"
      weight <- calcOutput("GDP",
                           scenario = c("SSPs", "SDPs", "SSP2IndiaDEAs"),
                           average2020 = FALSE,
                           aggregate = FALSE) |>
        time_interpolate(lowResYears)
      weight <- weight[, , SSPscen]
      # read UCD non fuel given in US$/vehkm and US$/veh/yr
      # non fuel OPEX include Domestic Aviation, International Aviation, Moped, Motorcycle (50-250cc),
      # Motorcycle (>250cc) (+4W not used here)
      nonFuelOPEXUCD <- toolPrepareUCD(readSource("UCD", "nonFuelOPEX"), "nonFuelOPEX")
      nonFuelOPEXUCD <- nonFuelOPEXUCD[!univocalName %in% filterEntries$trn_pass_road_LDV_4W]
      # For some modes UCD offers only a combined value for CAPEX and non-fuel OPEX given in US$/vehkm
      # combined CAPEX and OPEX include Domestic Ship, Freight Rail, HSR, International Ship, Passenger Rail
      nonFuelOPEXcombinedUCD <- toolPrepareUCD(readSource("UCD", "CAPEXandNonFuelOPEX"), "CAPEXandNonFuelOPEX")
      nonFuelOPEXcombinedUCD <- nonFuelOPEXcombinedUCD[!univocalName %in% c(filterEntries$trn_freight_road,
                                                                            filterEntries$trn_pass_road_LDV_4W,
                                                                            "Bus")]
      # Operating subsidies for Freight Rail, Passenger Rail, HSR (+ Bus not used here)
      operatingSubsidyUCD <- toolPrepareUCD(readSource("UCD", "OperatingSubsidies"), "OperatingSubsidies")
      operatingSubsidyUCD <- operatingSubsidyUCD[!(univocalName %in% filterEntries$trn_freight_road |
                                                     univocalName == "Bus")]

      # Inter- and extrapolate all data to model input data years
      data <- list(nonFuelOPEXUCD = nonFuelOPEXUCD,
                   nonFuelOPEXcombinedUCD = nonFuelOPEXcombinedUCD,
                   operatingSubsidyUCD = operatingSubsidyUCD)
      data <- lapply(data, approx_dt, lowResYears, "period", "value",
                     c("region", "univocalName", "technology", "variable", "unit"), extrapolate = TRUE)

      # Data for two wheelers is given in US$/veh and needs to be converted to US$/vehkm
      # with the help of annual mileage
      AMUCD2W <- toolPrepareUCD(readSource("UCD", "annualMileage"), "annualMileage")
      AMUCD2W <- AMUCD2W[univocalName %in% filterEntries$trn_pass_road_LDV_2W]
      AMUCD2W <- AMUCD2W[, c("region", "univocalName", "technology", "period", "value")]
      AMUCD2W <- approx_dt(AMUCD2W, lowResYears, "period", "value", c("region", "univocalName", "technology"),
                           extrapolate = TRUE)
      setnames(AMUCD2W, "value", "annualMileage")
      nonFuelOPEXUCD <- merge.data.table(data$nonFuelOPEXUCD, AMUCD2W, by = c("region", "period",
                                                                              "univocalName", "technology"),
                                         all.x = TRUE)
      nonFuelOPEXUCD[univocalName %in% filterEntries$trn_pass_road_LDV_2W, value := value / annualMileage]
      nonFuelOPEXUCD[univocalName %in% filterEntries$trn_pass_road_LDV_2W,
                     unit := paste0(monUnit, "/vehkm")][, annualMileage := NULL]

      # merge.data.table data
      nonFuelOPEXraw <- rbind(nonFuelOPEXUCD, data$nonFuelOPEXcombinedUCD, data$operatingSubsidyUCD)

      nonFuelOPEX <- toolAdjustNonFuelOPEXother(nonFuelOPEXraw, ISOcountriesMap,
                                                lowResYears, completeDataSet, filterEntries)

      nonFuelOPEX <- nonFuelOPEX[, c("region", "period", "univocalName", "technology", "variable", "unit", "value")]
      setkey(nonFuelOPEX,  region, period, univocalName, technology, variable, unit)

      nonFuelOPEX <- nonFuelOPEX[(univocalName %in% highResUnivocalNames
                                  & period %in% highResYears)
                                 | (univocalName %in% lowResUnivocalNames
                                    & period %in% lowResYears)]

      # nonFuelOPEXother only includes data for all other modes than LDV 4 Wheelers, Trucks and Busses
      # (except cycling and walking)
      completeDataOP <- completeDataSet[!(univocalName %in% c(filterEntries$trn_freight_road, "Cycle", "Walk") |
                                            univocalName %in% filterEntries$trn_pass_road_LDV_4W
                                          | univocalName == "Bus")]
      # Check whether data is complete
      check <- merge.data.table(completeDataOP, nonFuelOPEX, all = TRUE)
      if (nrow(check[is.na(value)]) > 0) {
        stop("Non fuel OPEX for vehicle types that do not feature fleet tracking is incomplete")
      } else if (nrow(check[is.na(check)]) > 0) {
        stop("Non fuel OPEX for vehicle types that do not feature fleet tracking includes unnecessary data")
      } else if (length(unique(check$unit)) > 1) {
        stop("Something went wrong in generating non fuel OPEX input for vehicle types
             that do not feature fleet tracking. Data does not have the same unit.")
      } else if (length(unique(check$variable)) > 1) {
        stop("Something went wrong in generating non fuel OPEX input for vehicle types
             that do not feature fleet tracking. Data does not have the same variable type.")
      } else if (anyNA(nonFuelOPEX) == TRUE) {
        stop("Non fuel OPEX for vehicle types that do not feature fleet tracking includes NAs")
      } else if (nrow(check[value < 0]) > 0) {
        stop("Aggregated non fuel OPEX for vehicle types that do not feature fleet tracking includes negative values")
      }

      quitteobj <- nonFuelOPEX
    },
    "speedOfModes" = {
      unit <- "km/h"
      description <- "Speed of traveling on vehicle type level (same for all technologies). Changes over time for
                      the motorized modes. Used to calculate the time value costs for passenger transport modes.
                      Sources: GCAM"
      weight <- calcOutput("GDP",
                           scenario = c("SSPs", "SDPs", "SSP2IndiaDEAs"),
                           average2020 = FALSE,
                           aggregate = FALSE) |>
        time_interpolate(highResYears)
      weight <- weight[, , SSPscen]

      # read sources
      speedGCAM <- toolPrepareGCAM(readSource("GCAM", "speedMotorized"), "speedMotorized")
      speedNonMotGCAM <- toolPrepareGCAM(readSource("GCAM", "speedNonMotorized"), "speedNonMotorized")

      # Inter- and extrapolate all data to model input data years
      allYears <- data.table(period = highResYears)
      allYears[, all := "All"]
      speedNonMotGCAM[, all := "All"]
      speedNonMotGCAM <- merge.data.table(speedNonMotGCAM, allYears, by = "all", allow.cartesian = TRUE)
      speedNonMotGCAM[, all := NULL]
      speedGCAM <- approx_dt(speedGCAM, highResYears, "period", "value",
                             c("region", "univocalName", "variable", "unit"), extrapolate = TRUE)
      # merge.data.table data
      # speed of modes is only featured for passenger small-to-medium distance transport
      # (to calculate time value costs).
      speedOfModesRaw <- rbind(speedGCAM[univocalName %in% filterEntries$trn_pass], speedNonMotGCAM)
      speedOfModes <- toolAdjustSpeedOfModes(speedOfModesRaw, completeDataSet, filterEntries)

      speedOfModes <- speedOfModes[, c("region", "period", "univocalName", "technology",
                                       "variable", "unit", "value")]

      speedOfModes <- speedOfModes[(univocalName %in% highResUnivocalNames
                                    & period %in% highResYears)
                                   | (univocalName %in% lowResUnivocalNames
                                      & period %in% lowResYears)]

      setkey(speedOfModes, region, period, univocalName, technology,
             variable, unit)

      # Check whether data is complete
      completeDataSpeed <- completeDataSet[univocalName %in% filterEntries$trn_pass]
      check <- merge.data.table(completeDataSpeed, speedOfModes, all = TRUE)
      if (nrow(check[is.na(value)]) > 0) {
        stop("Speed of modes input data is incomplete")
      } else if (nrow(check[is.na(check)]) > 0) {
        stop("Unnecessary data is provided for speed of modes")
      } else if (length(unique(check$unit)) > 1) {
        stop("Something went wrong in generating speed input data. Data does not have the same unit.")
      } else if (length(unique(check$variable)) > 1) {
        stop("Something went wrong in generating speed input data. Data does not have the same variable type.")
      } else if (anyNA(speedOfModes) == TRUE) {
        stop("Speed of modes data includes NAs")
      }
      quitteobj <- speedOfModes
    },
    "valueOfTimeMultiplier" = {
      unit <- "-"
      description <- "Cost associated with travel expressed as a multiplier of the wage rate.
                      Data is provided for different passenger transport modes and is equal for all regions and years.
                      Used to calculate the time value costs for passenger
                      transport modes. Sources: GCAM"
      weight <- calcOutput("GDP",
                           scenario = c("SSPs", "SDPs", "SSP2IndiaDEAs"),
                           average2020 = FALSE,
                           aggregate = FALSE) |>
        time_interpolate(highResYears)
      weight <- weight[, , SSPscen]

      # read sources
      VOTGCAM <- toolPrepareGCAM(readSource("GCAM", "valueOfTimeMultiplier"), "valueOfTimeMultiplier")

      # Inter- and extrapolate all data to model input data years
      allYears <- data.table(period = highResYears)
      allYears[, all := "All"]
      VOTGCAM[, all := "All"]
      VOTGCAM <- merge.data.table(VOTGCAM, allYears, by = "all", allow.cartesian = TRUE)[, all := NULL]

      VOT <- toolAdjustValueOfTimeMultiplier(VOTGCAM, completeDataSet, filterEntries)

      VOT <- VOT[, c("region", "period", "univocalName", "technology", "variable", "unit", "value")]
      VOT <- VOT[(univocalName %in% highResUnivocalNames
                  & period %in% highResYears)
                 | (univocalName %in% lowResUnivocalNames
                    & period %in% lowResYears)]
      setkey(VOT, region, period, univocalName, technology, variable, unit)

      # Check whether data is complete
      # speed of modes is only featured for passenger transport (to calculate value of time)
      completeDataVOT <- completeDataSet[univocalName %in% filterEntries$trn_pass]
      check <- merge.data.table(completeDataVOT, VOT, all = TRUE)
      if (nrow(check[is.na(value)]) > 0) {
        stop("Value of time multiplier input data is incomplete")
      } else if (nrow(check[is.na(check)]) > 0) {
        stop("Unnecessary data is provided for value of time multiplier")
      } else if (length(unique(check$unit)) > 1) {
        stop("Something went wrong in generating value of time multiplier input data.
             Data does not have the same unit.")
      } else if (length(unique(check$variable)) > 1) {
        stop("Something went wrong in generating value of time multiplier input data.
             Data does not have the same variable type.")
      } else if (anyNA(VOT) == TRUE) {
        stop("Value of time multiplier data includes NAs")
      }
      quitteobj <- VOT
    },
    "timeValueCosts" = {
      unit <- paste0(monUnit, "/pkm")
      description <- "Time value costs for passenger transport modes.
                      Sources: GCAM"
      weight <- calcOutput("GDP",
                           scenario = c("SSPs", "SDPs", "SSP2IndiaDEAs"),
                           average2020 = FALSE,
                           aggregate = FALSE) |>
        time_interpolate(highResYears)
      weight <- weight[, , SSPscen]

      # Speed of modes [km/h]
      speedOfModesMagpieobj <- calcOutput(type = "EdgeTransportSAinputs", aggregate = FALSE, warnNA = FALSE,
                                          subtype = "speedOfModes")
      speedOfModes <- magpie2dt(speedOfModesMagpieobj, valcol = "speed")[, c("variable", "unit") := NULL]
      setkey(speedOfModes, region, period, univocalName, technology)
      # Value of Time multiplier [-]
      valueOfTimeMultiplierMagpieobj <- calcOutput(type = "EdgeTransportSAinputs", aggregate = FALSE,
                                                   warnNA = FALSE, subtype = "valueOfTimeMultiplier")
      valueOfTimeMultiplier <- magpie2dt(valueOfTimeMultiplierMagpieobj,
                                         valcol = "multiplier")[, c("variable", "unit") := NULL]
      setkey(valueOfTimeMultiplier, region, period, univocalName, technology)

      GDPpcMERmag <- calcOutput("GDPpc",
                                scenario = c("SSPs", "SDPs", "SSP2IndiaDEAs"),
                                aggregate = FALSE,
                                unit = mrdrivers::toolGetUnitDollar()) |>
        time_interpolate(highResYears)
      GDPpcMERmag <- GDPpcMERmag[, , SSPscen]

      GDPpcMER <- magpie2dt(GDPpcMERmag, yearcol = "period", regioncol = "region", valcol = "gdppc")[, variable := NULL]

      setkey(GDPpcMER, region, period)

      timeValueCosts <- merge(speedOfModes,  valueOfTimeMultiplier)
      timeValueCosts <- merge(timeValueCosts, GDPpcMER)
      weeksPerYear <- 50
      hoursPerWeek <- 40
      timeValueCosts[, value := gdppc                   ## [US$/person/year]
                     * multiplier                        ## [US$/person/year]
                     / (hoursPerWeek * weeksPerYear) /     ## [US$/h]
                       speed]                            ## [US$/pkm]
      timeValueCosts[, variable := "Time value costs"][, unit := paste0(monUnit, "/pkm")]
      timeValueCosts <- timeValueCosts[, c("region", "univocalName", "technology",
                                           "variable", "unit", "period", "value")]
      timeValueCosts <- timeValueCosts[(univocalName %in% highResUnivocalNames
                                        & period %in% highResYears)
                                       | (univocalName %in% lowResUnivocalNames
                                          & period %in% lowResYears)]
      setkey(timeValueCosts, region, period, univocalName, technology, variable, unit)

      # Check whether data is complete
      # speed of modes is only featured for passenger transport (to calculate value of time)
      completeDataTVC <- completeDataSet[univocalName %in% filterEntries$trn_pass]
      check <- merge.data.table(completeDataTVC, timeValueCosts, all = TRUE)
      if (nrow(check[is.na(value)]) > 0) {
        stop("Time value costs input data is incomplete")
      } else if (nrow(check[is.na(check)]) > 0) {
        stop("Unnecessary data is provided for Time value costs")
      } else if (length(unique(check$unit)) > 1) {
        stop("Something went wrong in generating Time value costs input data.
             Data does not have the same unit.")
      } else if (length(unique(check$variable)) > 1) {
        stop("Something went wrong in generating Time value costs input data.
             Data does not have the same variable type.")
      } else if (anyNA(timeValueCosts) == TRUE) {
        stop("Time value costs data includes NAs")
      }
      quitteobj <- timeValueCosts
    },
    "PurchasePriceSubsidies" = {
      unit <- paste0(monUnit, "/veh")
      description <- "Subsidies for individuals purchasing alternative technology LDVs.
                      If untilPrice is not NA, then the subsidies only apply if the purchase price (CAPEX)
                      is smaller than or equal to the untilPrice."
      weight <- NULL

      data <- toolPreparePurchasePriceSubsidies(readSource("TransportPurchasePriceSubsidies"))
      data <- dcast(data, region + univocalName + technology + unit + period ~ variable, value.var = "value")

      # get historial CAPEX data
      CAPEXtrackedFleetMagpieobj <- calcOutput(type = "EdgeTransportSAinputs", aggregate = FALSE,
                                               warnNA = FALSE, subtype = "CAPEXtrackedFleet")
      CAPEXtrackedFleet <- magpie2dt(CAPEXtrackedFleetMagpieobj)
      CAPEXtrackedFleet <- CAPEXtrackedFleet["variable" == "Capital costs (purchase)", c("unit") := NULL]
      setnames(CAPEXtrackedFleet, "value", "capex")

      # approximate CAPEX to obtain 2021-2023 data
      CAPEXapprox <- approx_dt(CAPEXtrackedFleet, 1990:2150, "period", "capex", idxcols = c("region", "univocalName",
                                                                                            "technology", "variable"))
      CAPEXapprox[, unit := NULL]
      CAPEXapprox <- CAPEXapprox[variable == "Capital costs (purchase)"][, variable := NULL]

      # merge subsidies with CAPEX data to obtain purchase price and calculate subsidies
      data <- merge(data, CAPEXapprox, all.y = TRUE, by = intersect(names(data), names(CAPEXapprox)))
      data$unit <- paste0(monUnit, "/veh")

      # calculate subsidies based on purchase price and untilPrice values
      calculateSubsidies <- function(period, price, subsidy, untilPrice, subsidy2,
                                     untilPrice2) {

        ret <- copy(subsidy2)

        # cases where no subsidy is paid
        # 1. if the subsidy is an na value
        # 2. if the price exceeds the max price for the first subsidy and there is not second subsidy
        # 3. if the price exceeds the max price for the second subsidy
        ret[is.na(subsidy)] <- 0
        ret[!is.na(untilPrice) & price > untilPrice & is.na(subsidy2)] <- 0
        ret[!is.na(untilPrice2) & price > untilPrice2] <- 0

        # case where the first/higher subsidy is paid
        # 1. if there is no price max on the first (and hence only) subsidy
        # 2. if the price is lower than the price max for the first subsidy
        ret <- ifelse(!is.na(subsidy) & is.na(untilPrice), subsidy, ret)
        ret <- ifelse(!is.na(untilPrice) & price <= untilPrice, subsidy, ret)

        return(ret)
      }

      data[, value := calculateSubsidies(period, capex, purchasePriceSubsidy, untilPrice, purchasePriceSubsidy2,
                                         untilPrice2)]

      # drop unnecessary data
      data[, c("untilPrice", "untilPrice2", "purchasePriceSubsidy",
               "purchasePriceSubsidy2", "capex") := NULL]
      data[, value := - value]

      data <- data[(univocalName %in% highResUnivocalNames
                    & period %in% highResYears)
                   | (univocalName %in% lowResUnivocalNames
                      & period %in% lowResYears)]
      # merge with complete data set
      completeDataLDV <- completeDataSet[univocalName %in% filterEntries$trn_pass_road_LDV_4W]
      check1 <- merge.data.table(completeDataLDV, data, all = TRUE)

      if (nrow(check1[is.na(value)]) > 0) {
        stop("Purchase price subsidies input data is incomplete")
      } else if (nrow(check1[is.na(check)]) > 0) {
        stop("Unnecessary data is provided for Purchase price subsidies")
      } else if (length(unique(check1$unit)) > 1) {
        stop("Something went wrong in generating Purchase price subsidies input data.
             Data does not have the same unit.")
      } else if (length(unique(check1$variable)) > 1) {
        stop("Something went wrong in generating Purchase price subsidies input data.
             Data does not have the same variable type.")
      } else if (anyNA(data) == TRUE) {
        stop("Purchase price subsidies data includes NAs")
      }

      quitteobj <- data

    },
    "LDVfleet" = {
      unit <- "Mio veh"
      description <- "historical LDV fleet of Eurostat countries, 4W only"
      weight <- NULL

      data <- readSource("EurostatEnergyCountryDataSheets", "LDVfleet")
      data <- magpie2dt(data)
      quitteobj <- data

    }
  )

  x <- as.magpie(as.data.frame(quitteobj))

  return(list(
    x           = x,
    weight      = weight,
    unit        = unit,
    description = description,
    aggregationFunction = "toolAggregateVehicleTypes"
  ))
}
