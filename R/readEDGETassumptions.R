#' Read EDGE-T assumptions
#'
#' Read-in the table of region-specific assumptions used in the toolAdjust* functions as data.table
#'
#' @param subtype region whose table is read, an ISO3 code, e.g. "IND". The source holds one file per
#'   region, assumptions<subtype>.csv; IND is the only one so far.
#' @return data.table with one row per value and the columns id, region, variable, univocalName,
#'   technology, period, value, unit, status, derivation, sources
#' @details The version of the table is fixed in the read path (folder "v1" of the source), as in the other
#'   read functions of this package. A new version means a new folder and a change of this path.
#'   Assumption IDs (A01, ...) are unique within a region's file only; an ID is always read together with
#'   its region. Checked is only what the code depends on: column names, region, a complete numeric value
#'   column and unique keys. The allowed values of status and the form of sources are documented in README.txt
#'   of the source and serve traceability, not the computation.
#' @author Jarusch Muessel
#' @seealso [madrat::readSource()]
#' @examples
#' \dontrun{ a <- readSource(type = "EDGETassumptions", subtype = "IND", convert = FALSE)
#' }
readEDGETassumptions <- function(subtype = "IND") {
  region <- NULL

  file <- file.path("v1", paste0("assumptions", subtype, ".csv"))
  if (!file.exists(file)) stop("readEDGETassumptions: no table for region ", subtype)
  dt <- fread(file, colClasses = list(character = "period"))

  cols <- c("id", "region", "variable", "univocalName", "technology", "period", "value", "unit",
            "status", "derivation", "sources")
  if (!identical(names(dt), cols)) stop("readEDGETassumptions: unexpected columns: ",
                                        paste(names(dt), collapse = ", "))
  if (nrow(dt[region != subtype]) > 0) stop("readEDGETassumptions: column region must equal ", subtype)
  if (!is.numeric(dt$value) || anyNA(dt$value)) stop("readEDGETassumptions: column value must be numeric")
  key <- c("id", "region", "variable", "univocalName", "technology", "period")
  if (anyDuplicated(dt, by = key)) stop("readEDGETassumptions: duplicated keys")

  list(x = dt, class = "data.table")
}
