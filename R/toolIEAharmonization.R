#' Harmonize the energy intensities to match the IEA energy balances regarding final energy.
#'
#' We provide energy service trajectories. IEA energy balances have to be met and are not
#' consistent with GCAM intensities and energy service trajectories.
#' Therefore we have to adjust energy intensities and set the energy service demand to zero,
#' where the IEA does not report the energy service demand
#'
#' @param ... data to harmonize: Either the energy intensity or the energy service demnand
#' @importFrom rmndt magpie2dt
#' @export

toolIEAharmonization <- function(...) {
  fe <- te <- period <- isBunk <- flow <- . <- feIEA <- region <- univocalName <-
    value <- enService <- harmFactor <- check <- technology  <- groupingColumns <- NULL

  data <- list(...)
  harmonizationYears <- c(1990, 2005, 2010)

  # Load IEA energy balances data for harmonization [unit: EJ]
  IEAbalMag <- calcOutput(type = "IEAOutputTransport", aggregate = FALSE)

  IEAbal <-  magpie2dt(IEAbalMag, datacols = c("se", "fe", "te", "mod", "flow"),
                       regioncol = "region", yearcol = "period")
  IEAbal <- IEAbal[te != "dot"]  #delete fedie.dot #Q: what is fedie.dot?
  setnames(IEAbal, "value", "feIEA")

  # As freight and passenger are not seperated in IEA energy balances harmonize by "short-medium",
  # "MARBUNK" (eq. to shipping international) and "AVBUNK" (eq. to aviation international) and technology (te)
  IEAbal[, isBunk := ifelse(grepl("BUNK", flow), flow, "short-medium")]
  IEAbal[, c("se", "fe", "mod", "flow") := NULL]
  # sum fossil liquids and biofuel to tdlit, and biogas and natural gas to tdgat
  IEAbal[te %in% c("tdfospet", "tdfosdie", "tdbiopet", "tdbiodie"), te := "tdlit"]
  IEAbal[te %in% c("tdfosgat", "tdbiogat"), te := "tdgat"]
  IEAbal <- IEAbal[, .(feIEA = sum(feIEA)), by = .(region, period, te, isBunk)]

  # If energy intensity is transferred -> harmonize energy intensity to IEA balances in 2005
  if (!is.null(data$enIntensity)) {
    enIntensity <- copy(data$enIntensity)
    # Map energy intensity on IEA data
    enIntensity[technology %in% c("BEV", "Electric"), te := "tdelt"]
    enIntensity[technology == "Gases", te := "tdgat"]
    # All others are handled as liquids (including hybrid electric)
    enIntensity[is.na(te), te := "tdlit"]
    enIntensity[, isBunk := ifelse(univocalName == "International Aviation", "AVBUNK", NA)]
    enIntensity[, isBunk := ifelse(univocalName == "International Ship", "MARBUNK", isBunk)]
    enIntensity[, isBunk := ifelse(is.na(isBunk), "short-medium", isBunk)]
    # Energy intensity is given in [MJ/vehkm] and energy service demand in [bn (t|p)km]
    # Read load factor and energy service demand to calculate final energy
    enServiceDemMag <- calcOutput(type = "EdgeTransportSAinputs", aggregate = FALSE, warnNA = FALSE,
                                  subtype = "histESdemand")
    enServiceDem <- magpie2dt(enServiceDemMag)
    enServiceDem <- enServiceDem[!univocalName %in% c("Cycle", "Walk")]
    setnames(enServiceDem, "value", "enService")
    enServiceDem <- enServiceDem[, c("region", "univocalName", "technology", "period", "enService")]
    loadFactorMag <- calcOutput(type = "EdgeTransportSAinputs", aggregate = FALSE, warnNA = FALSE,
                                subtype = "loadFactor")
    loadFactor <- magpie2dt(loadFactorMag)
    loadFactor <- loadFactor[!univocalName %in% c("Cycle", "Walk")]
    setnames(loadFactor, "value", "loadFactor")
    loadFactor <- loadFactor[, c("region", "univocalName", "technology", "period", "loadFactor")]
    enServiceDem <- merge.data.table(enServiceDem, loadFactor,
                                     by = c("region", "univocalName", "technology", "period"))
    # merge load factor and energy service demand with energy intensity
    harmFactor <- merge.data.table(enIntensity[period %in% harmonizationYears],
                                   enServiceDem[period %in% harmonizationYears],
                                   by = c("region", "univocalName", "technology", "period"))
    # Calculate final energy in EJ
    MJtoEJ <- 1e-12
    bn <- 1e9
    harmFactor[, fe := (value / loadFactor) * enService * bn * MJtoEJ]
    harmFactor[, fe := sum(fe), by = c("region", "isBunk", "te", "period")]
    # Merge enery intensity and actual final energy with IEA data
    harmFactor <- merge.data.table(harmFactor, IEAbal[period %in% harmonizationYears],
                                   by = c("region", "isBunk", "te", "period"), all.x = TRUE)
    # For some modes and technologies the IEA fe value is zero.
    # The energy intensity is kept, but the energy service demand is set to zero
    harmFactor[, harmFactor := ifelse(feIEA == 0 | fe == 0, 1, feIEA / fe)]
    harmFactor <- unique(harmFactor[, .(region, period, isBunk, te, harmFactor, feIEA)])

    harmFactor <- approx_dt(harmFactor, unique(data$enIntensity$period), "period", "harmFactor",
                            idxcols = c("region", "isBunk", "te"), extrapolate = TRUE)
    enIntensity <- merge(enIntensity, harmFactor, by = c("region", "period", "isBunk", "te"), all.x = TRUE)
    enIntensity[, value := value * harmFactor]

    # Check whether harmonization worked
    # For some regions the IEA reports final energy gases, where our input data
    # features no energy service demand (e.g. Gases in JPN)
    check <- merge.data.table(enIntensity[period %in% harmonizationYears],
                              enServiceDem[period %in% harmonizationYears],
                              by = c("region", "univocalName", "technology", "period"))
    check[, check := (value / loadFactor) * enService * bn * MJtoEJ]
    check[, check := sum(check), by = c("region", "isBunk", "te", "period")][, diff := abs(check - feIEA)]

    #There are no electric vehicles on the road before 2010. Hence, ES demand is zero and the intensity was set to zero.
    #To avoid having efficiencies near zero in the input data,
    #we reversed the harmonization for such electric road vehicles.
    #We left the 2005 value because this year is relevant for the REMIND calibration
    groupingColumns <- setdiff(names(enIntensity), c("feIEA", "harmFactor", "value"))
    enIntensity[technology %in% c("BEV", "Hybrid electric") & period > 2005,
                value := value / harmFactor, by = groupingColumns]
    if (nrow(check[diff > 1e-2]) > 0) {
      stop("There is a problem regarding the Harmonization of the energy intensity data to match IEA energy balances
         final energy")
    }
    enIntensity <- enIntensity[, c("region", "period", "univocalName", "technology", "variable", "unit", "value")]

    return(enIntensity)

  } else if (!is.null(data$esDemand)) {
    # If energy service demand is transferred -> Set energy service demand to zero, where fe in IEA balances is zero
    # # Apply IEA categories
    esDemand <- data$esDemand
    esDemand[technology %in% c("BEV", "Electric"), te := "tdelt"]
    esDemand[technology == "Gases", te := "tdgat"]
    # All others are handled as liquids (including hybrid electric)
    esDemand[is.na(te), te := "tdlit"]
    esDemand[, isBunk := ifelse(univocalName == "International Aviation", "AVBUNK", NA)]
    esDemand[, isBunk := ifelse(univocalName == "International Ship", "MARBUNK", isBunk)]
    esDemand[, isBunk := ifelse(is.na(isBunk), "short-medium", isBunk)]
    esDemand <- merge.data.table(esDemand, IEAbal, by = c("region", "period", "isBunk", "te"), all.x = TRUE)
    esDemand[feIEA == 0, value := 0][, c("isBunk", "te", "feIEA") := NULL]
    return(esDemand)
  }
}
