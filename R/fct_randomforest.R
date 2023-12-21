#' Fit random forest metamodel
#'
#' @param df a dataframe.
#' @param y_var character. Name of the output variable in the dataframe. This will be the dependent variable of the metamodel.
#' @param x_vars character or a vector for characters. Name of the input variable(s) in the dataframe. This will be the independent variable of the metamodel.
#' @param standardise logical. Determine whether the parameter of the linear regression should be standardised. Default is FALSE.
#' @param x_poly_2 character. character or a vector for characters. Name of the input variable in the dataframe. These variables will be exponentiated by factor 2.
#' @param x_poly_3 character. character or a vector for characters. Name of the input variable in the dataframe. These variables will be exponentiated by factor 3.
#' @param x_exp character. character or a vector for characters. Name of the input variable in the dataframe. The exponential of these variables will be included in the metamodel.
#' @param x_log character. character or a vector for characters. Name of the input variable in the dataframe. The logarithm of these variables will be included in the metamodel.
#' @param seed_num numeric. Determine which seed number to use to split the dataframe in fitting an validation sets.
#' @param tune logical. Determine whether nodesize and mtry should be tuned. If FALSE, nodesize = 15 (for regression), and mtry = number of x-variables / 3 (for regression). Default is FALSE.
#' @param var_importance logical or character. Determine whether to compute variable importance (TRUE/FALSE), or how to compute variable importance (permute/random/anti). Default is TRUE (= anti).
#' @param pm_plot logical or character. Determine whether to plot the partial ("partial") or marginal ("marginal") effect or both ("both") of an x-variable (which is denoted by pm_vars). Default is FALSE. TRUE corresponds to "both".
#' @param pm_vars character. Name of the input variable(s) for the partial/marginal plot. Default is the first variable from the x_vars.
#' @param validation logical or character. Determine whether to validate the RF model. Choices are "test_train_split" and "cross-validation". TRUE corresponds to "cross-validation", default is FALSE.
#' @param folds numeric. Number of folds for the cross-validation. Default is 5.
#' @param partition numeric. Value between 0 and 1 to determine the proportion of the observations to use to fit the metamodel. Default is 1 (fitting the metamodel using all observations).
#' @param fit_complete_model logical. Determine whether to fit the (final) full model. So the model trained on all available data (as opposed to the model used in validation which is trained on the test data).
#'
#' @import randomForestSRC
#' @import interp
#' @import ggplot2
#'
#' @return A list containing ......
#' @details Standardisation of the parameters is obtained by \deqn{(x - u(x)) / sd(x)}
#' where \eqn{x} is the variable value, \eqn{u(x)} the mean over the variable and \eqn{sd(x)} the standard deviation of \eqn{x}.
#' For more details, see \href{https://doi.org/10.1177/0272989X13492014}{Jalal et al. 2013}.
#'
#' @examples
#' # Fitting random forest meta model with two variables using the probabilistic data
#' data(df_pa)
#' fit = fit_rf_metamodel(df = df_pa,
#'                        y_var = "inc_qaly",
#'                        x_vars = c("p_pfsd","p_pdd"),
#'                        tune = TRUE,
#'                        var_importance = TRUE,
#'                        pm_plot = "both",
#'                        pm_vars = c("p_pfsd","p_pdd")
#'                        )
#' @export
#'
#' @examples
fit_rf_metamodel <- function(df,
                             y_var = NULL,
                             x_vars = NULL,
                             standardise = FALSE,
                             seed_num = 1,
                             x_poly_2 = NULL,
                             x_poly_3 = NULL,
                             x_exp = NULL,
                             x_log = NULL,
                             tune = FALSE,
                             var_importance = TRUE, #or permute/random/ TRUE(=anti)/FALSE
                             pm_plot = FALSE,
                             pm_vars = x_vars[1],
                             validation = FALSE, #= TRUE(=cross_validation)/FALSE/train_test_split
                             folds = 5, #if not a whole number is entered it's rounded DOWN to nearest integer
                             partition = 0.8,
                             fit_complete_model = TRUE
                             ){

  # Flag errors
  if(length(y_var) > 1) {
    stop("Multiple outcomes provided to 'y'.")
  }
  if(is.null(y_var)) {
    stop("Cannot perform random forest regression because there is no value provided for 'y_var'.")
  }
  if(!is.null(x_inter) && length(x_inter) != 2 * round(length(x_inter) / 2)) {
    stop("The number of interaction terms is uneven.")
  }
  if(is.null(x_vars) && is.null(x_poly_2) && is.null(x_poly_3) && is.null(x_exp) && is.null(x_log)) {
    stop("Cannot perform random forest regression because there is no value provided for the predictors.")
  }
  if(length(var_importance)>1 || !all(var_importance %in% c(TRUE,FALSE,"anti","permute","random"))) {
    stop("'var_importance' should be one of: TRUE, FALSE, 'anti','permute','random'.")
  }
  if(length(pm_plot)>1 || !(pm_plot %in% c(TRUE,FALSE,"partial","marginal","both"))) {
    stop("'pm_plot' should be one of: TRUE, FALSE, 'partial','marginal','both'")
  }
  if(!all(pm_vars %in% x_vars)) {
    stop("Cannot produce the partial/marginal plot because at least one of the 'pm_vars' is not in 'x_vars'.")
  }
  if(partition < 0 || partition > 1) {
    stop("Proportion selected for training the metamodel should be between 0 (excluded) and 1 (included).")
  }
  if(!(validation %in% c(TRUE,FALSE,"cross_validation","train_test_split"))) {
    stop("Validation must be one of: TRUE, FALSE, 'cross_validation','train_test_split'.")
  }
  if(folds < 1 || folds > nrow(df_pa)){
    stop("Folds must be bigger than 0 and smaller than or equal to the number of rows of the dataframe.")
  }

  # Remove any possible NA's
  df = na.omit(df)

  # Set up
  set.seed(seed_num)
  l_out = list(stats_validation = NULL,
               calibration_plot = NULL,
               rf_fit = NULL,
               tune_fit = NULL,
               tune_plot = NULL
               )

  # Standardise inputs
  if(standardise == TRUE) {
    if(length(x_vars) > 1){
      df[, x_vars] <- lapply(df[, x_vars], function(i) (i - mean(i)) / sd(i))
    } else {
      df[, x_vars] <- (df[, x_vars] - mean(df[, x_vars])) / sd(df[, x_vars])
    }
  }

  # Transform inputs
  if(!is.null(x_poly_2)) {
    v_poly_2 <- paste("poly(", x_poly_2, ", 2)", collapse = " + ")
  } else {
    v_poly_2 <- NULL
  }
  if(!is.null(x_poly_3)) {
    v_poly_3 <- paste("poly(", x_poly_3, ", 3)", collapse = " + ")
    #x <- x[-which(x %in% v_poly_3)]
  } else {
    v_poly_3 <- NULL
  }
  if(!is.null(x_exp)) {
    v_exp <- paste("exp(", x_exp, ")", collapse = " + ")
    #x <- x[-which(x %in% v_exp)]
  } else {
    v_exp <- NULL
  }
  if(!is.null(x_log)) {
    v_log <- paste("log(", x_log, ")", collapse = " + ")
    #x <- x[-which(x %in% v_log)]
  } else {
    v_log <- NULL
  }

  v_x <- paste(unique(c(x_vars, v_poly_2, v_poly_3, v_exp, v_log)), collapse = " + ")
  form <- as.formula(paste(y_var, "~", v_x))

  # Set default mtry and nodesize
  nodesize = NULL
  mtry = NULL

  # Fit final model if specified
  if (fit_complete_model == TRUE){
    # Tune mtry & nodesize
    if (tune == TRUE){
      rf_tune <- tune(form,
                      data = df,
                      splitrule = "mse")

      nodesize = rf_tune$optimal[[1]]
      mtry = rf_tune$optimal[[2]]

      ## Tune plot
      x <- rf_tune$results[,1]
      y <- rf_tune$results[,2]
      z <- rf_tune$results[,3]

      ### If there is only one value tried for 'mtry' (=y) (so no 2D interpolation, but 1D over 'nodesize' (=x))
      if (length(unique(y))==1){
        xi=seq(min(x),max(x),0.1)
        z_interp = interp1(x=unique(x),y=z,method="linear",xi=xi)

        idx <- which.min(z)
        x0 <- x[idx]
        z0 = z[idx]
        df_interp = data.frame(xi,z_interp)

        tune_plot = ggplot() + geom_line(aes(x=xi,y=z_interp,color=z_interp),data=df_interp,linewidth=1) +
          geom_smooth(aes(color=..y..)) +
          scale_colour_gradient2(low = "blue", mid = "yellow" , high = "red",
                                 midpoint=median(df_interp$z_interp),name="OOB error") +
          guides(fill = guide_colourbar(title="OOB error")) +
          geom_point(aes(x=x,y=z)) +
          geom_point(aes(x=x0,y=z0), colour="black",size=8,pch="x") +
          xlab('Nodesize') +
          ylab("OOB error") +
          ggtitle("Error rate for nodesize") +
          theme_bw() +
          theme(plot.title = element_text(hjust = 0.5))
      }
      else {
        so <- interp(x=x, y=y, z=z, method = "linear",output = "grid")
        idx <- which.min(z)
        x0 <- x[idx]
        y0 <- y[idx]
        so_v = c(so$z)
        xy_grid = expand.grid(so$x,so$y)
        df_interp = data.frame(xy_grid,so_v)
        colnames(df_interp) = c("nodesize","mtry","error")

        tune_plot = ggplot() + geom_raster(aes(nodesize,mtry,fill=error),df_interp, interpolate = TRUE) +
          scale_fill_gradientn(colours=c("yellow","red"),na.value="white") +
          geom_point(aes(x=x0,y=y0), colour="black",size=8,pch="x") +
          geom_point(aes(x=x,y=y)) +
          scale_y_continuous(expand = expansion(mult = .5)) +
          guides(fill = guide_colourbar(title="OOB error")) +
          xlab("Nodesize") +
          ylab("Mtry") +
          ggtitle('Error rate for nodesize and mtry') +
          theme_bw() +
          theme(plot.title = element_text(hjust = 0.5))
      }

      ## return
      l_out[4] = list(rf_tune)
      l_out[5] = list(tune_plot)
    }
    else {
      l_out = l_out[-c(4,5)]
    }


    # Fit random forest model with tuned parameters
    rf_fit = rfsrc(form,
                   data = df,
                   splitrule = "mse",
                   nodesize = nodesize,
                   mtry = mtry,
                   forest = TRUE,
                   importance = var_importance
    )
    l_out[3] = list(rf_fit)

    # Show plots
    ## variable importance plot
    if (var_importance != FALSE){
      plot(rf_fit,verbose=TRUE,plots.one.page=TRUE)
    }
    ## partial and/or marginal plot
    if (pm_plot != FALSE){
      if (pm_plot == "both" || pm_plot == TRUE) {
        plot.variable.rfsrc(rf_fit,xvar.names=pm_vars,partial = TRUE,show.plots=TRUE,sort=TRUE,plots.per.page = 1)
        plot.variable.rfsrc(rf_fit,xvar.names=pm_vars,partial = FALSE,show.plots=TRUE,sort=TRUE,plots.per.page = 1)
      }
      else if (pm_plot == "partial") {
        plot.variable.rfsrc(rf_fit,xvar.names=pm_vars,partial = TRUE,show.plots=TRUE,sort=TRUE,plots.per.page = 1)
      }
      else if (pm_plot == "marginal") {
        plot.variable.rfsrc(rf_fit,xvar.names=pm_vars,partial = FALSE,show.plots=TRUE,sort=TRUE,plots.per.page = 1)
      }
    }
  }
  else {
    l_out = l_out[-3]
  }

  # Validation
  if (validation == TRUE || validation == "cross_validation"){
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

      ## Tune
      if (tune == TRUE){
        rf_tune_validation <- tune(form,
                                   data = df_train,
                                   splitrule = "mse")

        nodesize_validation = rf_tune_validation$optimal[[1]]
        mtry_validation = rf_tune_validation$optimal[[2]]
      }
      else {
        nodesize_validation = NULL
        mtry_validation = NULL
      }

      ## Fit with tuned (if specified) or default parameters
      rf_fit_validation = rfsrc(form,
                                data = df_train,
                                splitrule = "mse",
                                nodesize = nodesize_validation,
                                mtry = mtry_validation,
                                forest = TRUE
      )

      ## Test on test data
      preds = predict(rf_fit_validation, newdata = df_test)$predicted
      tests = df_test[,y_var]

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
    l_out[1] = list(stats_validation)
    l_out = l_out[-2]

  }
  else if (validation == "train_test_split"){
    ## Partition data and fit to train data
    selection = sample(1:nrow(df), size = round(nrow(df) * partition), replace = FALSE)
    df_train = df[selection, ]
    df_test = df[-selection, ]

    ## Tune
    if (tune == TRUE){
      rf_tune_validation <- tune(form,
                                 data = df_train,
                                 splitrule = "mse")

      nodesize_validation = rf_tune_validation$optimal[[1]]
      mtry_validation = rf_tune_validation$optimal[[2]]
    }
    else {
      nodesize_validation = NULL
      mtry_validation = NULL
    }

    ## Fit with tuned (if specified) or default parameters
    rf_fit_validation = rfsrc(form,
                   data = df_train,
                   splitrule = "mse",
                   nodesize = nodesize_validation,
                   mtry = mtry_validation,
                   forest = TRUE
    )

    ## Test on test data
    preds = predict(rf_fit_validation, newdata = df_test)$predicted
    tests = df_test[,y_var]

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
      ggplot2::ggtitle(paste("Calibration plot for","Ozone")) +
      ggplot2::theme_bw() +
      ggplot2::theme(plot.title = element_text(hjust = 0.5))

    stats_validation = data.frame(
      Statistics = c("R-squared","Mean absolute error","Mean relative error","Mean squared error"),
      Value = round(c(r_squared_validation,mae_validation,mre_validation,mse_validation),3)
    )
    l_out[1] = list(stats_validation)
    l_out[2] = list(calibration_plot)
  }
  else {
    l_out = l_out[-c(1,2)]
  }


  # Export
  return(l_out)
}
