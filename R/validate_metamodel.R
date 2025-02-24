#' Validate metamodels
#'
#' @param model model object. Built using a function from the PACHECK package.
#' @param method character, validation method. Choices are: cross-validation ('cross_validation'), train-test split ('train_test_split'), or the user can input a new dataframe which will be used as the test-set ('new_test_set'). No default.
#' @param partition numeric. Value between 0 and 1 to determine the proportion of the observations to use to fit the metamodel. Default is 1 (fitting the metamodel using all observations).
#' @param folds numeric. Number of folds for the cross-validation. Default is 1 (so an error occurs when not specifying this argument when cross-validation is chosen).
#' @param show_intercept logical. Determine whether to show the intercept of the perfect prediction line (x = 0, y = 0). Default is FALSE.
#' @param seed_num numeric. Determine which seed number to use to split the dataframe in fitting and validation sets.
#' @param validate_df dataframe. The dataframe to be used for validating the model. By default the dataframe used when building the model is used.
#'
#' @return .........................
#' @export
#'
#' @examples
#' #Validating meta model with two variables using the probabilistic data, using cross-validation.
#' data(df_pa)
#' lm_fit = fit_lm_metamodel(df = df_pa,
#'                  y_var = "inc_qaly",
#'                  x_vars = c("p_pfsd", "p_pdd")
#'                  )
#'
#' validate_metamodel(model = lm_fit,
#'                  method = "cross_validation",
#'                  folds = 5
#'                  )
validate_metamodel = function(model = NULL,
                              method = NULL,
                              partition = 1,
                              folds = 1,
                              show_intercept = FALSE,
                              seed_num = 1,
                              df_validate = NULL){
  # Flag errors
  if(partition < 0 || partition > 1) {
    stop("Proportion selected for training the metamodel should be between 0 (excluded) and 1 (included).")
  }
  if(partition == 1 && method == "train_test_split") {
    stop("Cannot perform validation because all observations are included in the training set. Lower `partition` below 1.")
  }
  if(!(method %in% c("cross_validation","train_test_split","new_test_set"))) {
    stop("Method must be one of: 'cross_validation', 'train_test_split', 'new_test_set'.")
  }
  if(is.null(method)){
    stop("Please choose a validation method: 'cross_validation' or 'train_test_split'.")
  }
  if(folds < 2 && method == "cross_validation" || folds > nrow(df_pa) && method == "cross_validation"){
    stop("Folds must be bigger than 1 and smaller than or equal to the number of rows of the dataframe.")
  }
  if(is.null(df_validate) && method == "new_test_set" || !(is.data.frame(df_validate)) && method == "new_test_set"){
    stop("Please supply the test set as a dataframe for the argument 'method'.")
  }

  # Retrieve model info
  model_form = model$model_info$form
  df = model$model_info$data
  model_type = model$model_info$type
  if(!(model_type %in% c("rf","lm","lasso"))){
    stop("Please supply a model which is built using the PACHECK package.")
  }
  x_vars = model$model_info$x_vars
  y_var = model$model_info$y_var

  # Set up
  l_out = list(stats_validation = NULL,
               calibration_plot = NULL)
  set.seed(seed_num)

  # Validation
  if (method == "cross_validation"){
    ## Re-sample the data and make folds
    df_validation = df[sample(nrow(df)),]
    folds_ind = cut(seq(1,nrow(df_validation)),breaks=folds,labels=FALSE)

    r_squared_validation = rep(NA,folds)
    mae_validation = rep(NA,folds)
    mre_validation = rep(NA,folds)
    mse_validation = rep(NA,folds)

    for (i in 1:folds){
      test_indices = which(folds_ind==i)
      df_test = df_validation[test_indices,]
      df_train = df_validation[-test_indices,]

      if (model_type == "rf"){
        ## Retrieve model nodesize and mtry
        nodesize_validation = model$fit$nodesize
        mtry_validation = model$fit$mtry

        ## Fit on training data
        rf_fit_validation = rfsrc(model_form,
                                  data = df_train,
                                  splitrule = "mse",
                                  nodesize = nodesize_validation,
                                  mtry = mtry_validation,
                                  forest = TRUE
        )
        ## Test on test data
        preds = predict(rf_fit_validation, newdata = df_test)$predicted
        tests = df_test[,y_var]
      }
      else if (model_type == "lm"){
        ## Fit on training data
        lm_fit <- lm(model_form, data = df_train)

        ## Test on test data
        preds          <- as.numeric(as.character(unlist(predict(lm_fit, newdata = df_test))))
        tests            <- as.numeric(as.character(df_test[, paste(y_var)]))
      }
      else if (model_type == "lasso"){
        x_train = model.matrix(model_form,df_train)
        y_train = df_train[,y_var]

        x_test = model.matrix(model_form,df_test)
        y_test = df_test[,y_var]

        ## Tune lambda and fit the model with best lambda
        cv_out = glmnet::cv.glmnet(x_train,y_train)
        bestlam = cv_out$lambda.min
        lasso_fit = glmnet::glmnet(x_train,y_train,alpha=1,lambda=bestlam)

        preds = predict(lasso_fit,s=bestlam,newx=x_test)
        tests = y_test
      }

      ## Store performance metrics
      r_squared_validation[i] = cor(preds,tests)^2
      mae_validation[i] = mean(abs(preds-tests))
      mre_validation[i] = mean(abs(preds-tests)/abs(tests))
      mse_validation[i] = mean((preds-tests)^2)
    }
    ## Store results
    stats_validation = data.frame(
      Statistic = c("R-squared", "Mean absolute error", "Mean relative error","Mean squared error"),
      Value = round(c(mean(r_squared_validation),mean(mae_validation),mean(mre_validation),mean(mse_validation)),3)
    )
    names(stats_validation)[names(stats_validation) == "Value"] <- "Value (method: cross-validation)"

    l_out[1] = list(stats_validation)
    l_out = l_out[-2]

  }
  else if (method == "train_test_split"){
    ## Partition data and fit to train data
    selection = sample(1:nrow(df), size = round(nrow(df) * partition), replace = FALSE)
    df_train = df[selection, ]
    df_test = df[-selection, ]

    if (model_type == "rf"){
      ## Retrieve model nodesize and mtry
      nodesize_validation = model$fit$nodesize
      mtry_validation = model$fit$mtry

      ## Fit on training data
      rf_fit_validation = rfsrc(model_form,
                                data = df_train,
                                splitrule = "mse",
                                nodesize = nodesize_validation,
                                mtry = mtry_validation,
                                forest = TRUE
      )
      ## Test on test data
      preds = predict(rf_fit_validation, newdata = df_test)$predicted
      tests = df_test[,y_var]
    }
    else if (model_type == "lm"){
      ## Fit on training data
      lm_fit <- lm(model_form, data = df_train)

      ## Test on test data
      preds          <- as.numeric(as.character(unlist(predict(lm_fit, newdata = df_test))))
      tests            <- as.numeric(as.character(df_test[, paste(y_var)]))
    }
    else if (model_type == "lasso"){
      x_train = model.matrix(model_form,df_train)
      y_train = df_train[,y_var]

      x_test = model.matrix(model_form,df_test)
      y_test = df_test[,y_var]

      ## Tune lambda and fit the model with best lambda
      cv_out = glmnet::cv.glmnet(x_train,y_train)
      bestlam = cv_out$lambda.min
      lasso_fit = glmnet::glmnet(x_train,y_train,alpha=1,lambda=bestlam)

      preds = predict(lasso_fit,s=bestlam,newx=x_test)
      tests = y_test
    }

    r_squared_validation = cor(preds,tests)^2
    mae_validation = mean(abs(preds-tests))
    mre_validation = mean(abs(preds-tests)/abs(tests))
    mse_validation = mean((preds-tests)^2)

    ## Calibration plot: predicted versus observed
    df_test$y_pred = preds
    calibration_plot <- ggplot2::ggplot(ggplot2::aes_string(x = "y_pred", y = y_var), data = df_test) +
      ggplot2::geom_point(shape = 1) +
      ggplot2::geom_abline(intercept = 0, slope = 1, colour = "orange") +
      ggplot2::xlab("Predicted values") +
      ggplot2::ylab("Observed values") +
      ggplot2::ggtitle(paste("Calibration plot for",y_var)) +
      ggplot2::theme_bw() +
      ggplot2::theme(plot.title = element_text(hjust = 0.5))

    if(show_intercept == TRUE) {
      calibration_plot <- calibration_plot +
        ggplot2::geom_abline(intercept = 0, slope = 1, colour = "orange")
    }

    stats_validation = data.frame(
      Statistics = c("R-squared","Mean absolute error","Mean relative error","Mean squared error"),
      Value = round(c(r_squared_validation,mae_validation,mre_validation,mse_validation),3)
    )
    names(stats_validation)[names(stats_validation) == "Value"] <- "Value (method: train/test split)"
    l_out[1] = list(stats_validation)
    l_out[2] = list(calibration_plot)
  }
  else if (method == "new_test_set"){
    if(model_type == "rf"){
      ## Retrieve model nodesize and mtry
      #nodesize_validation = model$fit$nodesize
      #mtry_validation = model$fit$mtry

      rf_fit_validation = model$fit

      ## Test on test data
      preds = predict(rf_fit_validation, newdata = df_validate)$predicted
      tests = df_validate[,y_var]
    }
    else if(model_type == "lm"){
      lm_fit_validation = model$fit

      ## Test on test data
      preds          <- as.numeric(as.character(unlist(predict(lm_fit_validation, newdata = df_validate))))
      tests            <- as.numeric(as.character(df_validate[, paste(y_var)]))
    }
    else if(model_type == "lasso"){
      ## Retrieve bestlam
      #bestlam = model$fit$lambda

      lasso_fit_validation = model$fit

      ## Test on test data
      x_test = model.matrix(model_form,df_validate)
      y_test = df_validate[,y_var]

      preds = predict(lasso_fit_validation,newx=x_test)
      tests = y_test
    }

    r_squared_validation = cor(preds,tests)^2
    mae_validation = mean(abs(preds-tests))
    mre_validation = mean(abs(preds-tests)/abs(tests))
    mse_validation = mean((preds-tests)^2)

    ## Calibration plot: predicted versus observed
    df_validate$y_pred = preds
    calibration_plot <- ggplot2::ggplot(ggplot2::aes_string(x = "y_pred", y = y_var), data = df_validate) +
      ggplot2::geom_point(shape = 1) +
      ggplot2::geom_abline(intercept = 0, slope = 1, colour = "orange") +
      ggplot2::xlab("Predicted values") +
      ggplot2::ylab("Observed values") +
      ggplot2::ggtitle(paste("Calibration plot for",y_var)) +
      ggplot2::theme_bw() +
      ggplot2::theme(plot.title = element_text(hjust = 0.5))

    if(show_intercept == TRUE) {
      calibration_plot <- calibration_plot +
        ggplot2::geom_abline(intercept = 0, slope = 1, colour = "orange")
    }

    stats_validation = data.frame(
      Statistics = c("R-squared","Mean absolute error","Mean relative error","Mean squared error"),
      Value = round(c(r_squared_validation,mae_validation,mre_validation,mse_validation),3)
    )
    names(stats_validation)[names(stats_validation) == "Value"] <- "Value (method: new test set)"
    l_out[1] = list(stats_validation)
    l_out[2] = list(calibration_plot)


    }

  return(l_out)

}
