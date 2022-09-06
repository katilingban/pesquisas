################################################################################
#
#'
#' Get core variables required for analysis
#'
#' @param raw_data_clean A roughly cleaned/processed raw dataset
#'
#' @return A data.frame to which basic tidying and cleaning has been done in
#'   preparation for further data procesing
#'
#' @examples
#'   \dontrun{
#'     get_core_variables(raw_data_clean)
#'   }
#'
#' @export
#'
#
################################################################################

get_core_variables <- function(raw_data_clean) {
  # raw_data_clean |>
  #   subset(
  #     select = c(
  #       id, spid, district, ea_code, geolocation
  #     )
  #   )
  raw_data_clean[c("id", "spid", "district", "ea_code", "geolocation")]
}


################################################################################
#
#'
#' Recode yes no responses
#'
#' @param x A vector to recode
#' @param na_values A vector of values in x that correspond to an NA. Default
#'   to NULL
#' @param detect Whether to recode to an affirmative response ("yes") or to
#'   a negative response ("no")
#'
#' @return A vector of the same length as `x` of recoded values
#'
#' @examples
#'
#'   recode_yes_no(c(1, 2, 1, 2))
#'   recode_yes_no(c("yes", "no", "yes", "no"))
#'   recode_yes_no(c("Yes", "No", "YES", "NO"))
#'
#' @export
#'
#'
#'
#
################################################################################

recode_yes_no <- function(x, na_values = NULL, detect = c("yes", "no")) {
  ## Which value to detect
  detect <- match.arg(detect)

  ## Recode NAs
  if (!is.null(na_values)) {
    x <- ifelse(
      x %in% na_values, NA, x
    )
  }

  ## Convert x to uppercase to make recoding rules easier
  if (inherits(x, "character")) {
    x <- toupper(x)
  }

  if (detect == "yes") {
    ## Recode x to 1 and 0
    x <- ifelse(
      isTRUE(x) | x == "T" | x == 1 | x == "Y" | x == "YES", 1, 0
    )
  } else {
    ## Recode x to 1 and 0
    x <- ifelse(
      isFALSE(x) | x == "F" | x == 0 | x == "N" | x == "NO", 0, 1
    )
  }

  ## Return
  x
}


################################################################################
#
#'
#' Split select multiple responses into a column for each possible response
#' with values recoded to whether respondent answered this response
#'
#' @param x A vector of select multiple responses produced by the ODK system
#' @param fill A vector of all the possible responses that can be selected in
#'   select multiple
#' @param na_rm Logical. Should NA responses be made into its own colum? Default
#'   to FALSE
#' @param prefix A character value to use as a prefix to the column names
#'
#' @return A data.frame with columns for each type of possible responses
#'   (if fill is specified) or for all responses provided.
#'
#' @examples
#'   split_select_multiple(
#'     x = "1 2 3",
#'     fill = 1:3,
#'     na_rm = FALSE,
#'     prefix = "test"
#'   )
#'   split_select_multiples(
#'     x = c("1 2 3", "1", "1 2", "2"),
#'     fill = 1:3,
#'     na_rm = TRUE,
#'     prefix = "test"
#'   )
#'
#' @export
#' @rdname split_select_multiple
#'
#
################################################################################

split_select_multiple <- function(x, fill, na_rm = FALSE, prefix) {
  if (na_rm) {
    if (is.na(x)) {
      rep(NA_integer_, times = length(fill)) |>
        (\(x) { names(x) <- paste0(prefix, "_", fill); x })()
    } else {
      stringr::str_split(x, pattern = " ") |>
        unlist() |>
        as.integer() |>
        spread_vector_to_columns(fill = fill, prefix = prefix) |>
        colSums(na.rm = TRUE)
    }
  } else {
    stringr::str_split(x, pattern = " ") |>
      unlist() |>
      as.integer() |>
      spread_vector_to_columns(fill = fill, prefix = prefix) |>
      colSums(na.rm = TRUE)
  }
}

################################################################################
#
#'
#' @export
#' @rdname split_select_multiple
#'
#
################################################################################

split_select_multiples <- function(x, fill, na_rm = FALSE, prefix) {
  lapply(
    X = x,
    FUN = split_select_multiple,
    fill = fill,
    na_rm = na_rm,
    prefix = prefix
  ) |>
    dplyr::bind_rows()
}


################################################################################
#
#'
#' Get NA types
#'
#' Determine the NA type needed for recoding a vector of values
#'
#' @param x A vector of values to determine NA type to use
#'
#' @return An NA type specific for the class type of `x`
#'
#' @examples
#'   get_na_type(x = 1:3)
#'   get_na_type(x = c("1", "2", "3"))
#'
#' @export
#'
#
################################################################################

get_na_type <- function(x) {
  if (inherits(x, "character")) {
    na_type <- NA_character_
  }

  if (inherits(x, "integer")) {
    na_type <- NA_integer_
  }

  if (inherits(x, "numeric")) {
    na_type <- NA_real_
  }

  na_type
}


################################################################################
#
#'
#' Convert character vector of categorical responses into unique variables
#'
#' Function transforms a vector of categorical responses into `n` number of
#' new columns/variables equal to the number of unique categorical values.
#'
#' @param x Vector of categorical values
#' @param fill A vector of all the possible responses that can be selected in
#'   select multiple
#' @param na_rm Logical. Should NA responses be made into its own colum? Default
#'   to FALSE
#' @param prefix A character string to prepend to the names of the new columns
#'   to be created
#'
#' @return A data.frame with columns for each type of possible responses
#'   (if fill is specified) or for all responses provided.
#'
#' @examples
#'   spread_vector_to_columns(x = 1:4, prefix = "test")
#'
#' @export
#'
#'
#
################################################################################

spread_vector_to_columns <- function(x, fill = NULL, na_rm = FALSE, prefix) {
  values <- sort(unique(x), na.last = NA)

  if (!is.null(fill)) {
    values <- c(values, fill[!fill %in% values]) |>
      sort(na.last = NA)
  }

  if (na_rm) {
    values <- c(values, NA_integer_)
  }

  values <- values |>
    stringr::str_replace_all(
      pattern = " ", replacement = "_"
    )

  col_names <- paste(prefix, values, sep = "_")

  lapply(
    X = x,
    FUN = function(x, y) ifelse(x == y, 1, 0),
    y = values
  ) |>
    (\(x) do.call(rbind, x))() |>
    data.frame() |>
    (\(x) { names(x) <- col_names; x })()
}

