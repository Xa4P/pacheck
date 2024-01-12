#' Predict using a fitted metamodel
#'
#' @param model model object. Built using a function from the PACHECK package.
#' @param inputs dataframe or vector. When choosing a vector in the case of a three-variable model: the first, second, third, and fourth value represent the input for the first, second, third, and FIRST variable, respectively. Default gives the predictions based on the training data.
#' @param output character. Choose an output: 'dataframe' or 'vector'.
#'
#' @return returns a vector or a dataframe containing the predictions.
#' @export
#'
#' @examples
#' #Making 3 predictions for a two-variable metamodel, using a vector as input, and yielding a dataframe as output.
#' data(df_pa)
#' lm_fit = fit_lm_metamodel(df = df_pa,
#'                  y_var = "inc_qaly",
#'                  x_vars = c("p_pfsd", "p_pdd")
#'                  )
#'
#' vec = c(0.1,0.2,0.08,0.15,0.06,0.25)
#'
#' predict_metamodel(model = lm_fit,
#'                  inputs = vec,
#'                  output_type = "dataframe"
#'                  )
predict_metamodel = function(model = NULL,
                             inputs = NULL,
                             output_type = "vector"){
  # Flag errors
  if(!(output_type %in% c("dataframe","vector"))){
    stop("Please choose a valid output type: 'dataframe' or 'vector'.")
  }

  # Retrieve model info & flag errors
  model_fit = model$fit

  model_type = model$model_info$type
  if(!(model_type %in% c("rf","lm","lasso"))){
    stop("Please supply a model which is built using the PACHECK package.")
  }
  model_training_data = model$model_info$data
  v_names = model$model_info$x_vars
  y_var = model$model_info$y_var

  if(is.data.frame(inputs)){
    if(ncol(inputs) != length(v_names)){
      stop("Please supply a dataframe with as many columns as there are x-variables.")
    }
  }
  else if(is.vector(inputs)){
    if(length(inputs) %% length(v_names) != 0){
      stop("Please supply a vector whose length is a multiple of the number of x-variables.")
    }
  }

  # Transform input data to dataframe if needed
  if (is.null(inputs)){
    newdata = model_training_data
  }
  else if(is.vector(inputs)){
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
  else if(model_type == "lasso"){
    model_form = model$model_info$form
    newdata = model.matrix(model_form,newdata)[,-1]

    preds = array(glmnet::predict.glmnet(model_fit,newx=newdata))
  }

  # Output type
  if(output_type == "dataframe"){
    newdata['predictions'] = preds
    return(newdata)
  }
  else if(output_type == "vector"){
    return(preds)
  }
}
