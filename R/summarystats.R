#' Title summary stats
#' @param data input data frame
#' @param by by group or by groups
#' @param var as a analysis variable
#'
#' @returns A dataframe containing the average of the variable grouped by the given grouping variables
#' @export summarystats
#' @importFrom dplyr group_by summarise
#' @importFrom rlang enquo
#' @importFrom magrittr %>%
#' @importFrom testthat
#' @examples summarystats(mtcars, rlang::exprs(cyl), "mpg")
summarystats <- function(data, by, var) {
  # Capture the column names as symbols
  var <- rlang::enquo(var)
  df<-data %>%
    dplyr::group_by(!!!by) %>% #enquote list of quosures
    dplyr::summarise(avg=mean({{var}},na.rm=TRUE),.groups = "drop")
  return(df)
}
