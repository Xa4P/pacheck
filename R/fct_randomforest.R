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
#'
#'
#' @import randomForestSRC
#' @import interp
#' @import ggplot2
#'
#' @return A list containing the fit of the model, the tuning results of nodesize and mtry, and a plot illustrating the tuning results.
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
                             x_inter = NULL,
                             tune = FALSE,
                             var_importance = TRUE, #or permute/random/ TRUE(=anti)/FALSE
                             pm_plot = FALSE,
                             pm_vars = x_vars[1]) {
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


  # Set seed
  set.seed(seed_num)

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
  if(!is.null(x_inter)) {
    pairs <- length(x_inter)/2
    pair_seq <- seq(1, pairs, 1)
    pair_seq <- pair_seq - 1
    v_inter <- vapply(pair_seq, function(x) {
      paste0(x_inter[2 * x + 1], ":", x_inter[2 * x + 2])
    }, character(1))
    v_inter <- c(v_inter, unique(x_inter))
  } else {
    v_inter <- NULL
  }

  v_x <- paste(unique(c(x_vars, v_poly_2, v_poly_3, v_exp, v_log, v_inter)), collapse = " + ")
  form <- as.formula(paste(y_var, "~", v_x))

  # Set default mtry and nodesize
  nodesize = NULL
  mtry = NULL

  # Tune mtry & nodesize
  if (tune == TRUE){
    rf_tune <- tune(form,
                    data = df,
                    splitrule = "mse")

    nodesize = rf_tune$optimal[[1]]
    mtry = rf_tune$optimal[[2]]

    ## tune plot
    x <- rf_tune$results[,1]
    y <- rf_tune$results[,2]
    z <- rf_tune$results[,3]
    so <- interp(x=x, y=y, z=z, linear = linear,output = "grid")
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
  else {
    rf_tune = NULL
    tune_plot = NULL
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

  # Export
  l_out = list(rf_fit = rf_fit,
               tune_fit = rf_tune,
               tune_plot = tune_plot
               )

}
