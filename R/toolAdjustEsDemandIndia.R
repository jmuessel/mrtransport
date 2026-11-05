#' Adjust the historical energy service demand of India
#'
#' Called by [toolAdjustEsDemand()] after the general corrections (its step 7). The truck size split (its step 3,
#' A21) stays there, as it is computed together with China and Japan.
#'
#' @details Values by assumption ID from the EDGETassumptions source ([toolGetAssumption()])
#' - Former factor 1.4 on all IND demand in 2005 and 2010 and factor 1/2 on buses: removed, replaced by the 2010
#'   targets of step a
#' - Road demand (step a, A22-A24): 2010 bus pkm derived from India's pre-COVID bus use benchmarked against China
#'   at the same income; passenger road = buses + 2-4-wheelers; road freight from the IEA's published 2010 total;
#'   applied to 2005-2010
#' - Domestic shipping and aviation (step a, A25, A26): published 2010 activity (RITES, DGCA)
#' - Bus technologies (step a, A27): share of the 2005-2010 bus pkm on gas (CNG share of the registered buses, TERI)
#' - Road demand 1990 (step b): computed so that the 1990 IEA harmonization factor equals the 2005 one; not an
#'   estimate of the 1990 activity
#' - Check (step c): warns when the aviation intensity factor of [toolAdjustEnergyIntensityIndia()] no longer fits
#' - Status, derivation and sources of each value are in the table, references and PDF paths in README.txt of the
#'   source.
#'
#' @author Jarusch Muessel
#' @param dt energy service demand of all regions after the general corrections, see [toolAdjustEsDemand()]
#' @param filter list of filters for specific branches in the upper decision tree, containing all associated
#' univocalNames
#' @param histSourceData the full source data; its energy intensity and load factor give the raw liquids fuel use
#' for steps b and c
#' @return dt with the adjusted values for region IND
#'
#' @importFrom rmndt magpie2dt
#' @import data.table

toolAdjustEsDemandIndia <- function(dt, filter, histSourceData) {
  period <- value <- technology <- . <- region <- univocalName <- loadFactor <- te <- flow <- fe <-
    enIntensity <- NULL

  # #plausibilityFix_IND #plausibilityFix_Pass #plausibilityFix_Truck #plausibilityFix_Freight

  # a: Road, shipping and aviation demand in 2005-2010
  # A22-A26: 2010 targets for buses, 2-4-wheelers, trucks and domestic shipping, 2005 and 2010 for domestic aviation
  busPkm2010   <- toolGetAssumption("A22")[["Bus"]]                # billion pkm
  ldvPkm2010   <- toolGetAssumption("A23")[["trn_pass_road_LDV"]]  # billion pkm, 2-4-wheelers
  passRoad2010 <- busPkm2010 + ldvPkm2010
  busShare2010 <- busPkm2010 / passRoad2010
  truckTkm2010 <- toolGetAssumption("A24")[["trn_freight_road"]]   # billion tkm
  shipTkm2010  <- toolGetAssumption("A25")[["Domestic Ship"]]      # billion tkm
  airPkm       <- toolGetAssumption("A26", by = "period")          # billion pkm, 2005 and 2010
  bus <- dt[region == "IND" & period == 2010 & univocalName == "Bus", sum(value)]
  ldv <- dt[region == "IND" & period == 2010 & univocalName %in% setdiff(filter$trn_pass_road, "Bus"), sum(value)]
  dt[region == "IND" & univocalName == "Bus",
     value := value * busShare2010 / (1 - busShare2010) * ldv / bus]
  dt[region == "IND" & period >= 2005 & univocalName %in% filter$trn_pass_road,
     value := value * passRoad2010 / sum(value[period == 2010])]
  dt[region == "IND" & period >= 2005 & univocalName %in% filter$trn_freight_road,
     value := value * truckTkm2010 / sum(value[period == 2010])]
  dt[region == "IND" & period == 2010 & univocalName == "Domestic Ship",
     value := value * shipTkm2010 / sum(value)]
  dt[region == "IND" & period %in% c(2005, 2010) & univocalName == "Domestic Aviation",
     value := value * airPkm[as.character(period)] / sum(value), by = "period"]

  # Bus technologies in 2005-2010
  # A27: share of bus pkm on gas (CNG share of the registered buses), the rest on liquids
  cngShareBus <- toolGetAssumption("A27")[["Bus"]]
  dt[region == "IND" & period >= 2005 & univocalName == "Bus",
     value := sum(value) * ifelse(technology == "Gases", cngShareBus,
                                  (1 - cngShareBus) * value / sum(value[technology != "Gases"])),
     by = "period"]

  # b: Road demand in 1990 (passenger road and trucks; rail, aviation and shipping keep GCAM's values)
  # One factor, computed so that the 1990 liquids harmonization factor equals the 2005 one (1.28 on 2026-10-21)
  # With GCAM's 1990 road demand the 1990 factor was 1.41 against 1.13 in 2005, and EDGE-T's 2005 liquids FE was 9 %
  # above the IEA's: its 2005 fleet includes vehicles bought before 2005, with intensities interpolated from 1990.
  # Derived: factor = (IEA 1990 / harm2005 - non-road FE 1990) / road FE 1990, with harm2005 = IEA 2005 / FE 2005
  # FE: final energy of the raw liquids intensities (intensity / LF x demand), as in toolIEAharmonization()
  # IEA: the IEA's liquids without bunkers, as in toolIEAharmonization()
  # TERI (2024), Figures 2.2 and 2.7, prints lower 1990-91 road values (680 billion pkm, 127 billion tkm).
  road <- c(filter$trn_pass_road, filter$trn_freight_road)
  iea <- magpie2dt(calcOutput(type = "IEAOutputTransport", aggregate = FALSE)["IND", , ],
                   datacols = c("se", "fe", "te", "mod", "flow"), regioncol = "region", yearcol = "period")
  iea <- iea[te %in% c("tdfospet", "tdfosdie", "tdbiopet", "tdbiodie") & !grepl("BUNK", flow)]
  feLiquids <- merge(dt[region == "IND" & period %in% c(1990, 2005, 2010) & value > 0 &
                          !technology %in% c("BEV", "Electric", "Gases") &
                          !univocalName %in% c("Cycle", "Walk", "International Aviation", "International Ship"),
                        .(period, univocalName, technology, value)],
                     merge(histSourceData$enIntensity, histSourceData$loadFactor,
                           by = c("region", "period", "univocalName", "technology"))[region == "IND"],
                     by = c("period", "univocalName", "technology"), all.x = TRUE)
  if (anyNA(feLiquids)) stop("IND: energy intensity or load factor missing for a liquids mode with demand")
  feLiquids[, fe := value * enIntensity / loadFactor * 1e-3] # billion (p|t)km x MJ/vehkm / ((p|t)/veh) -> EJ
  harm2005 <- iea[period == 2005, sum(value)] / feLiquids[period == 2005, sum(fe)]
  road1990 <- (iea[period == 1990, sum(value)] / harm2005 -
                 feLiquids[period == 1990 & !univocalName %in% road, sum(fe)]) /
    feLiquids[period == 1990 & univocalName %in% road, sum(fe)]
  dt[region == "IND" & period == 1990 & univocalName %in% road, value := value * road1990]

  # c: Check of the domestic aviation intensity factor (toolAdjustEnergyIntensityIndia(), step b): its harmonized
  # fuel use in 2010 should equal the IEA's domestic aviation (flow DOMESAIR). The factor depends on all other IND
  # liquids inputs through the common harmonization factor, so warn with the multiplier it needs when it is off by
  # over 2 %.
  feAir <- feLiquids[period == 2010 & univocalName == "Domestic Aviation", sum(fe)]
  feOther <- feLiquids[period == 2010 & univocalName != "Domestic Aviation", sum(fe)]
  ieaAir <- iea[period == 2010 & flow == "DOMESAIR", sum(value)]
  airMultiplier <- ieaAir * feOther / (feAir * (iea[period == 2010, sum(value)] - ieaAir))
  if (abs(airMultiplier - 1) > 0.02) {
    warning(sprintf(paste("IND: harmonized domestic aviation fuel in 2010 does not match the IEA's; multiply the",
                          "factor A15 in toolAdjustEnergyIntensityIndia() (step b) by %.3f"), airMultiplier))
  }

  dt
}
