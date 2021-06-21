#' t-test / ks-test wrapper
#'
#' A wrapper function that performs a one-versus the rest t-test & ks-test
#'
#' @param df a tibble - data-frame or connection
#' @param factor a categorical variable in df ; note in SQLlite there is no factor variable but the user thinks of this variable as a factor
#' @param factor_level - sets the **"first"** class in a **one-versus-rest** `t.test` and `ks.test` analysis
#' @param continuous_feature` represented by a character string of the feature name to perform `t.test` and `ks.test` on
#' @param verbose was useful in creating the function to identify variables causing errors, and create output for those types of cases, set to FALSE by default
#' @return A row-tibble with columns: Feature, mean_diff_est, ttest.pvalue, mean_Target, mean_Control, kstest.pvalue, N_Target, N_Control, conf.low, conf.high, statistic, sd_Target, sd_Control, parameter, method, alternative
#' @export
#' @examples
#' \dontrun{
#' wrapper.t_ks_test(df=A_DATA_TBL_2 ,
#'                  factor = DIABETES ,
#'                  factor_level = 1,
#'                  'Age')
#'}
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr select matches filter collect distinct mutate rename
#' @importFrom tibble tibble
#' @importFrom rlang enquo sym !!

wrapper.t_ks_test <- function(df, factor, factor_level,  continuous_feature, verbose = FALSE){

  if(verbose == TRUE){
    cat(paste0('\n','Now on Feature ',continuous_feature,' \n'))
  }

  factor <- enquo(factor)

  data.local <- df %>%
    select(!! factor, matches(continuous_feature)) %>%
    collect()

  X <- data.local %>%
    filter(!! factor == factor_level)

  Y <- data.local %>%
    filter(!(!! factor == factor_level))

  x <- unlist(X[,continuous_feature])
  y <- unlist(Y[,continuous_feature])

  sd_x <- sd(x, na.rm=TRUE)
  sd_y <- sd(y, na.rm=TRUE)

  if(sum(!is.na(x)) < 3 | sum(!is.na(y)) < 3 | is.na(sd_x) | is.na(sd_y) | sd_x == 0 | sd_y == 0){

    bad_return <- tibble(mean_diff_est = mean(x , na.rm=TRUE) - mean(y , na.rm=TRUE),
                         N_Target = sum(!is.na(x)),
                         mean_Target = mean(x , na.rm=TRUE),
                         sd_Target = sd_x,
                         N_Control = sum(!is.na(y)),
                         mean_Control = mean(y , na.rm=TRUE),
                         sd_Control = sd_y,
                         statistic = NA,
                         ttest.pvalue = NA,
                         kstest.pvalue = NA,
                         parameter = NA,
                         conf.low = NA,
                         conf.high = NA,
                         method = "Either Target or Control has fewer than 3 results",
                         alternative = paste0(continuous_feature, " Might be constant"),
                         Feature = continuous_feature) %>%
      select(Feature, mean_diff_est, ttest.pvalue, mean_Target, mean_Control, kstest.pvalue, N_Target, N_Control, conf.low, conf.high, statistic, sd_Target, sd_Control, parameter, method, alternative)
    return(bad_return)

  }

  ttest_return <- t.test(x,y)

  ks.test.pvalue <- ks.test(x,y)$p.value

  result_row <- broom::tidy(ttest_return)

  result_row <- result_row %>%
    mutate(Feature = continuous_feature) %>%
    rename(mean_diff_est = estimate) %>%
    mutate(N_Target = sum(!is.na(x))) %>%
    mutate(N_Control = sum(!is.na(y))) %>%
    rename(mean_Target = estimate1) %>%
    rename(mean_Control = estimate2) %>%
    rename(ttest.pvalue = p.value) %>%
    mutate(sd_Target = sd_x) %>%
    mutate(sd_Control = sd_y) %>%
    mutate(kstest.pvalue = ks.test.pvalue) %>%
    select(Feature, mean_diff_est, ttest.pvalue, mean_Target, mean_Control, kstest.pvalue, N_Target, N_Control, conf.low, conf.high, statistic, sd_Target, sd_Control, parameter, method, alternative)

  if(verbose == TRUE){
    cat(paste0('\n','Finished Feature ',continuous_feature,' \n'))
  }
  return(result_row)
}
