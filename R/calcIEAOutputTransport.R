#' Calc Input Output
#'
#' Computes IEA-based model data by use of raw IEA "Energy Balances" data
#' and a mapping that corresponds to the structure of "products" and "flows" of IEA.
#'
#' Mapping structure example: IEA product ANTCOAL used for IEA flow TPATFUEL, contributes via REMIND technology
#' coaltr for generating sesofos from pecoal (REMIND names)
#'
#' @return IEA data as MAgPIE object aggregated to country level
#' @author Falk Benke
#'
#' @importFrom rlang .data
#' @importFrom magclass getNames getNames<- setNames dimSums mbind
#'
calcIEAOutputTransport <- function() {

  # read in data and convert from ktoe to EJ
  data <- readSource("IEA", subtype = "EnergyBalances") * 4.1868e-5

  ieamatch <- toolGetMapping(type = "sectoral", name = "structuremappingIO_outputs.csv",
                             where = "mrcommons")

  target <- c("REMINDitems_in", "REMINDitems_out", "REMINDitems_tech", "iea_product", "iea_flows")

  ieamatch <- ieamatch %>%
    dplyr::select(tidyselect::all_of(c("iea_product", "iea_flows", "Weight", target))) %>%
    stats::na.omit() %>%
    # select only fuel types that are represented in EDGE-T
    dplyr::filter(.data$REMINDitems_out %in% c("fedie", "fepet", "fegat", "feelt")) %>%
    tidyr::unite("target", tidyselect::all_of(target), sep = ".", remove = FALSE) %>%
    tidyr::unite("product.flow", c("iea_product", "iea_flows"), sep = ".") %>%
    dplyr::filter(.data$product.flow %in% getNames(data))

  reminditems <- do.call(
    mbind,
    lapply(
      unique(ieamatch$target),
      function(item) {

        productFlow <- ieamatch %>%
          dplyr::filter(.data$target == item) %>%
          dplyr::pull("product.flow")

        weights <- ieamatch %>%
          dplyr::filter(.data$target == item) %>%
          dplyr::pull("Weight") %>%
          as.numeric()

        tmp <- dimSums(
          data[, , productFlow] * setNames(as.magpie(weights), productFlow),
          dim = 3, na.rm = TRUE
        )

        getNames(tmp) <- item

        return(tmp)
      }
    )
  )

  return(list(
    x = reminditems, weight = NULL, unit = "EJ",
    description = "IEA SE Output Data based on 2022 edition of IEA World Energy Balances"
  ))
}
