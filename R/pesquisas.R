################################################################################
#
#'
#' Utilities to Perform Data Cleaning, Processing, and Analysis on Mozambique
#' Health and Nutrition Surveys
#'
#' In 2019, UNICEF with the support of the European Union launched a 4-year
#' nutrition programme aimed at contributing to recovery efforts for people in
#' need in cyclone-affected districts of Mozambique. Since then, a series of
#' surveys have been conducted by UNICEF and its partners in Mozambique to
#' provide the data needed to monitor progress and assess change in health and
#' nutrition outcomes. This package provides R utilities and tools for
#' performing data cleaning, data processing, and data analysis of these
#' surveys.
#'
#' @docType package
#' @keywords internal
#' @name perquisas
#'
#' @importFrom stringr str_split
#' @importFrom dplyr bind_rows
#'
#'
#
################################################################################
"_PACKAGE"


## quiets concerns of R CMD check on global variables
# if(getRversion() >= "2.15.1") {
#   utils::globalVariables(
#     c(
#       "id", "spid", "district", "ea_code", "geolocation"
#     )
#   )
# }
