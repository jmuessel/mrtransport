#' Read EDGE-T assumptions
#'
#' Read-in the table of IND-specific assumptions used in the toolAdjust* functions as data.table
#'
#' @param subtype version folder of the table, e.g. "v1"
#' @return data.table with one row per value and the columns id, region, variable, univocalName,
#'   technology, period, value, unit, status, derivation, sources
#' @author Jarusch Muessel
#' @seealso [madrat::readSource()]
#' @examples
#' \dontrun{ a <- readSource(type = "EDGETassumptions", subtype = "v1", convert = FALSE)
#' }
readEDGETassumptions <- function(subtype = "v1") {
  status <- NULL

  dt <- fread(file.path(subtype, "assumptionsIND.csv"), colClasses = list(character = "period"))

  cols <- c("id", "region", "variable", "univocalName", "technology", "period", "value", "unit",
            "status", "derivation", "sources")
  if (!identical(names(dt), cols)) stop("readEDGETassumptions: unexpected columns: ",
                                        paste(names(dt), collapse = ", "))
  if (!is.numeric(dt$value) || anyNA(dt$value)) stop("readEDGETassumptions: column value must be numeric")
  if (nrow(dt[!status %in% c("published", "derived", "assumption", "calibrated")]) > 0) {
    stop("readEDGETassumptions: unknown status")
  }
  key <- c("id", "region", "variable", "univocalName", "technology", "period")
  if (anyDuplicated(dt, by = key)) stop("readEDGETassumptions: duplicated keys")

  list(x = dt, class = "data.table")
}
