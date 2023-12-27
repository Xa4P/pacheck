#' Predict using a fitted metamodel
#'
#' @param model model object. Built using a function from the PACHECK package.
#' @param inputs dataframe or vector. When choosing a vector in the case of a three-variable model: the first, second, third, and fourth value represent the input for the first, second, third, and FIRST variable, respectively.
#' @param output character. Choose an output: 'dataframe' or 'vector'.
#'
#' @return ...................................
#' @export
#'
#' @examples
#' #Making 3 predictions for a two-variable metamodel, using a vector as input.
#' data(df_pa)
#' lm_fit = fit_lm_metamodel(df = df_pa,
#'                  y_var = "inc_qaly",
#'                  x_vars = c("p_pfsd", "p_pdd")
#'                  )
#'
#' vec = c(0.1,0.2,0.08,0.15,0.06,0.25)
#'
#' predict_metamodel(model = lm_fit,
#'                  inputs = vec
#'                  )
predict_metamodel = function(model = NULL,
                             inputs = NULL,
                             output = NULL){
  # Flag errors
  ### als inputs NULL is, predictions based on training data geven??????????


  ####################### output: df or vector nog ff doen dat dat kan
  ####################### dan 'flag errors' doen

  # Retrieve model info
  model_fit = model$fit
  model_type = model$model_info$type
  v_names = model$model_info$x_vars

  # Set up

  # Transform input data to dataframe if needed
  if(!(is.data.frame(inputs))){
    arr = array(inputs,dim = c(length(inputs)/length(v_names),length(v_names)))
    newdata = as.data.frame(arr)
    names(newdata) = v_names
  }
  else {
    newdata = inputs
  }


  # Make predictions
  if(model_type == "rf"){
    preds = randomForestSRC::predict.rfsrc(model_fit,newdata = newdata)$predicted
  }
  else if(model_type == "lm"){
    preds = array(stats::predict.lm(model_fit,newdata = newdata))

  }
  return(preds)
}
