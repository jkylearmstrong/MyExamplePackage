#' Analysis Data
#'
#' An extract of the NHANES data.
#'
#' @format A data frame with 101316 rows and 133 variables:
#' \describe{
#'   \item{SEQN}{respondent identifier}
#'   \item{DIABETES}{Diabetic status of the respondent, 1 for "Yes", 0 for "No", NA otherwise}
#' }
#' @source \url{https://wwwn.cdc.gov/nchs/nhanes/Default.aspx}
"A_DATA_2"

#' Feature Type
#'
#' A list of categorized features
#'
#' @format list of 3
#' \describe{
#'   \item{Not_features}{identifier, targets, ect}
#'   \item{categorical_features}{list of categorical features }
#'   \item{numeric_features}{list of numerical features}
#' }
"FEATURE_TYPE"


