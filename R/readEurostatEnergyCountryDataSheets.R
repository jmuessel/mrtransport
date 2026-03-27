#' Read Eurostat data.
#'
#'
#' @param subtype One of the possible subtypes, see default argument.
#' @return magclass object
#'
#' @examples
#' \dontrun{
#' a <- readSource("EurostatEnergyCountryDataSheets")
#' }
#' @author Johanna Hoppe
#' @seealso [madrat::readSource()]
#' @import data.table
#' @importFrom magclass as.magpie
#' @importFrom readxl read_excel

readEurostatEnergyCountryDataSheets <- function(subtype = c("feDemand", "LDVfleet")) {
  Eurostatsector <- variable <- region <- NULL

  countries <- c(
    "BE", "BG", "CZ", "DK", "DE", "EE", "IE", "EL", "ES", "FR", "HR", "IT", "CY",
    "LV", "LT", "LU", "HU", "MT", "NL", "AT", "PL", "PT", "RO", "SI", "SK", "FI", "SE", "UK"
  )

  switch(subtype,
    "feDemand" = {
      dt <- do.call("rbind", lapply(
        countries,
        function(x) {
          # here an old EUROSTAT country data sheet is used (the newest does not include UK)
          # when we want to extend the historical ES demand to later years, we have to find an alternative source for UK
          # GCAM offers substantially lower values
          output <- suppressMessages(data.table(read_excel(
            path =
              file.path(
                "Energy statistical country datasheets",
                "energy_statistical_countrydatasheets.xlsx"
              ),
            sheet = x, "C8:X249"
          )))
          colnames(output) <- c("Eurostatsector", as.character(seq(1990, 2010, 1)))
          output <- output[Eurostatsector %in% c(
            "International maritime bunkers",
            "International aviation", "Domestic aviation", "Domestic navigation"
          )]
          output <- melt(output, id.vars = c("Eurostatsector"), variable.name = "period")
          output$region <- x
          output$unit <- "Mtoe"
          return(output)
        }
      ))
      # Bring to quitte column order
      dt[, variable := "Final energy"]
      dt <- dt[, c("region", "Eurostatsector", "variable", "unit", "period", "value")]
      x <- as.magpie(as.data.frame(dt), spatial = "region", temporal = "period")
      return(x)
    },
    "LDVfleetTotal" = {
      dt <- suppressMessages(data.table(read_excel(
        path = file.path(
          "LDV fleet",
          "road_eqs_carpda_spreadsheet.xlsx"
        ),
        sheet = "Sheet 1", "A9:R52"
      )))
      names(dt)[1] <- "region"
      dt <- dt[-1, ]
      dt <- dt[-1, ]
      dt <- dt[, c(
        "region", "2013", "2014", "2015", "2016",
        "2017", "2018", "2019", "2020", "2021"
      )]
      # fill in NAs
      dt[dt == ":"] <- NA

      dt[, "2013" := as.numeric(dt$"2013") / 1000000]
      dt[, "2014" := as.numeric(dt$"2014") / 1000000]
      dt[, "2015" := as.numeric(dt$"2015") / 1000000]
      dt[, "2016" := as.numeric(dt$"2016") / 1000000]
      dt[, "2017" := as.numeric(dt$"2017") / 1000000]
      dt[, "2018" := as.numeric(dt$"2018") / 1000000]
      dt[, "2019" := as.numeric(dt$"2019") / 1000000]
      dt[, "2020" := as.numeric(dt$"2020") / 1000000]
      dt[, "2021" := as.numeric(dt$"2021") / 1000000]

      dt <-
        dt[region != "Kosovo (under United Nations Security Council Resolution 1244/99)"]

      dt <- melt(dt, id.vars = c("region"), variable.name = "period")
      dt$EurostatvehicleType <- "LDV-4Wheeler"
      dt$unit <- "Mio veh"

      dt[, variable := "LDV Fleet"]

      dt[]

      dt <- dt[, c("region", "EurostatvehicleType", "variable", "unit", "period", "value")]
      x <- as.magpie(as.data.frame(dt), spatial = "region", temporal = "period")
      return(x)
    },
    "LDVfleet" = {
      # ATTENTION WHEN USING THIS: ENERGY TYPES ARE NOT DISJOINT
      dt <- do.call("rbind", lapply(
        2:18,
        function(x) {
          output <-
            suppressMessages(data.table(read_excel(
              path = file.path(
                "LDV fleet",
                "road_eqs_carpda_spreadsheet.xlsx"
              ),
              sheet = paste("Sheet", toString(x),
                sep = " "
              ),
              "A9:R52"
            )))

          # load technology name per sheet
          tech <-
            (suppressMessages(read_excel(
              path = file.path(
                "LDV fleet",
                "road_eqs_carpda_spreadsheet.xlsx"
              ),
              sheet = paste("Sheet", toString(x), sep = " "),
              "C7:C7", col_names = FALSE
            )))[[1]]
          names(output)[1] <- "region"
          output <- output[-1, ]
          output <- output[-1, ]
          output <- output[, c(
            "region", "2013", "2014", "2015", "2016",
            "2017", "2018", "2019", "2020", "2021"
          )]
          # fill in NAs
          output[output == ":"] <- NA

          output[, "2013" := as.numeric(output$"2013") / 1000000]
          output[, "2014" := as.numeric(output$"2014") / 1000000]
          output[, "2015" := as.numeric(output$"2015") / 1000000]
          output[, "2016" := as.numeric(output$"2016") / 1000000]
          output[, "2017" := as.numeric(output$"2017") / 1000000]
          output[, "2018" := as.numeric(output$"2018") / 1000000]
          output[, "2019" := as.numeric(output$"2019") / 1000000]
          output[, "2020" := as.numeric(output$"2020") / 1000000]
          output[, "2021" := as.numeric(output$"2021") / 1000000]

          # We drop Cosovo
          output <- output[region != "Kosovo (under United Nations Security Council Resolution 1244/99)"]

          output <- melt(output, id.vars = c("region"), variable.name = "period")
          output$Eurostattechnology <- tech
          output$EurostatvehicleType <- "LDV-4Wheeler"
          output$unit <- "Mio veh"
          return(output)
        }
      ))
      dt[, variable := "LDV Fleet"]

      dt[]

      dt <- dt[, c("region", "EurostatvehicleType", "Eurostattechnology", "variable", "unit", "period", "value")]
      x <- as.magpie(as.data.frame(dt), spatial = "region", temporal = "period")
      return(x)
    },
  )
}
