#' Load information from a previous REMIND run
#'
#' @author Falk Benke
#' @param subtype Either "fuelCosts" or "esDemand"
#'
readREMINDinputForTransportStandalone <- function(subtype) {
  gdxPath <- file.path("v1.3", "fulldata.gdx")

  if (subtype == "fuelCosts") {
    out <- gdx2::readGDX(gdxPath, "pm_FEPrice",
      format = "first_found",
      restoreZeros = FALSE
    )[, , "trans.ES", pmatch = TRUE]
  } else if (subtype == "esDemand") {
    out <- gdx2::readGDX(gdxPath, c("vm_cesIO"),
      select = list("_field" = "level"),
      restoreZeros = FALSE
    )
  } else {
    stop("Invalid subtype. Must be either 'fuelCosts' or 'edDemand'")
  }
  return(out)
}
