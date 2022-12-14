% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/misc.R
\name{split_select_multiple}
\alias{split_select_multiple}
\alias{split_select_multiples}
\title{Split select multiple responses into a column for each possible response
with values recoded to whether respondent answered this response}
\usage{
split_select_multiple(x, fill, na_rm = FALSE, prefix)

split_select_multiples(x, fill, na_rm = FALSE, prefix)
}
\arguments{
\item{x}{A vector of select multiple responses produced by the ODK system}

\item{fill}{A vector of all the possible responses that can be selected in
select multiple}

\item{na_rm}{Logical. Should NA responses be made into its own colum? Default
to FALSE}

\item{prefix}{A character value to use as a prefix to the column names}
}
\value{
A data.frame with columns for each type of possible responses
(if fill is specified) or for all responses provided.
}
\description{
Split select multiple responses into a column for each possible response
with values recoded to whether respondent answered this response
}
\examples{
  split_select_multiple(
    x = "1 2 3",
    fill = 1:3,
    na_rm = FALSE,
    prefix = "test"
  )
  split_select_multiples(
    x = c("1 2 3", "1", "1 2", "2"),
    fill = 1:3,
    na_rm = TRUE,
    prefix = "test"
  )

}
