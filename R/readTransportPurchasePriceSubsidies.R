#' Read transport subsidies data
#'
#' Read-in transport subsidies csv files as magclass object
#'
#' @return magpie object of the transport subsidies for BEV, FCEV and PHEV (euros/car) for private and legal entities
#' @author Caroline Cronjaeger
#' @seealso [madrat::readSource()]
#' @examples
#' \dontrun{ a <- readSource(type="TransportSubsidies")
#' }
readTransportPurchasePriceSubsidies <- function() {
  period <- untilPrice <- untilPrice20 <- untilPrice21 <-
    untilPrice22 <- untilPrice23 <- NULL

  data <- data.table(read_excel(path = file.path("carSubsidies.xlsx"),
                                sheet = "RoundedSubsidiesUSD2005", "A1:K97"))
  data <- melt(data, id.vars = c("region", "subsidiesVehicleType", "subsidiesTechnology", "untilPrice20",
                                 "untilPrice21", "untilPrice22", "untilPrice23"),
               measure.vars = c("2020", "2021", "2022", "2023"),
               variable.name = "period")
  data[period == 2020, untilPrice := untilPrice20]
  data[period == 2021, untilPrice := untilPrice21]
  data[period == 2022, untilPrice := untilPrice22]
  data[period == 2023, untilPrice := untilPrice23]
  data[, c("untilPrice20", "untilPrice21", "untilPrice22", "untilPrice23") := NULL]

  setnames(data, "value", "purchasePriceSubsidy")

  # find duplicates. This happens when a subsidy is granted below a price of A,
  # and a different subsidy is granted for a price greater than A but below B
  dups <- data[duplicated(data, by = c("region", "period", "subsidiesTechnology"))]
  setnames(dups, c("purchasePriceSubsidy", "untilPrice"), c("purchasePriceSubsidy2", "untilPrice2"))

  # this deletes the duplicates, but we have previously saved them in dubs
  unq <- unique(data, by = c("region", "period", "subsidiesTechnology"))

  # add the duplicates back in
  data <- merge(unq, dups, all.x = TRUE)

  # we DONT want this
  data <- melt(data, id.vars = c("region", "subsidiesVehicleType", "subsidiesTechnology", "period"),
               measure.vars = c("untilPrice", "purchasePriceSubsidy", "untilPrice2", "purchasePriceSubsidy2"),
               value.name = "value")
  x <- as.magpie(data, spatial = "region", temporal = "period", datacol = "value")
  return(x)
}
