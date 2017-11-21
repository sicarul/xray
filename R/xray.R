#' \code{xray} package
#'
#' X-Ray - Dataset Analyzer
#'
#'
#' @docType package
#' @name xray
#' @importFrom dplyr %>%
#' @importFrom foreach %do%
NULL

## quiets concerns of R CMD check
utils::globalVariables(c(".", "n", "pNA", "pZero", "pBlank", 'pInf', 'anomalous_percent', 'qDistinct', 'dat', '..density..'))
