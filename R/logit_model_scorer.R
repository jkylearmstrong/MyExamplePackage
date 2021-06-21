#' logit model scorer
#'
#' A wrapper function that performs scoring on logistic glm model
#'
#' @param my_model a logistic glm model 
#' @param my_data data to be scored 
#' @param target outcome in glm model
#' @param level the level for which the glm should be set
#' @return my_data will be returned along with columns: model, probs, pred 
#' @export
#' @examples
#' \dontrun{
#' logit_model_scorer(logit_glm, A_DATA_2.test, DIABETES_factor, 1)
#'}
#'
#' @importFrom magrittr %>%
#' @import dplyr 
#' @importFrom tibble tibble
#' @importFrom rlang enquo sym !!


logit_model_scorer <- function(my_model, my_data, target , level){ 
  # extracts models name 
  my_model_name <- deparse(substitute(my_model))
  
  enquo_target <- enquo(target)
  
  # store model name into a new column called model
  data.s <- my_data %>%
    mutate(model = my_model_name)
  
  # store the training data someplace
  train_data.s <- my_model$data
  
  # score the training data 
  train_data.s$probs <- predict(my_model, 
                                train_data.s, 
                                'response')
  
  # threshold query
  threshold_value_query <- train_data.s %>% 
    group_by(!!enquo_target) %>%
    summarise(mean_prob = mean(probs, na.rm=TRUE)) %>%
    ungroup() %>%
    filter(!!enquo_target == level)
  # threshold value                      
  threshold_value <- threshold_value_query$mean_prob
  
  # score test data
  data.s$probs <- predict(my_model, 
                          data.s, 
                          'response')
  
  # use threshold to make prediction  
  data.s <- data.s %>%
    mutate(pred = if_else(probs > threshold_value, 1,0)) %>%
    mutate(pred_factor = as.factor(pred))             
  
  # return scored data 
  return(data.s)
}