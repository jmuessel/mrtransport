#' Load information from a previous REMIND run
#'
#' @author Falk Benke
#' @param subtype Either "fuelCosts" or "esDemand"
#'
readREMINDinputForTransportStandalone <- function(subtype) {
  gdxPath <- file.path("v1.2", "fulldata.gdx")

  if (subtype == "fuelCosts") {
    out <- gdx::readGDX(gdxPath, "pm_FEPrice",
      format = "first_found",
      restore_zeros = FALSE
    )[, , "trans.ES", pmatch = TRUE]
  } else if (subtype == "esDemand") {
    out <- gdx::readGDX(gdxPath, c("vm_cesIO"), field = "l", restore_zeros = FALSE)
  } else {
    stop("Invalid subtype. Must be either 'fuelCosts' or 'edDemand'")
  }
  return(out)
}
