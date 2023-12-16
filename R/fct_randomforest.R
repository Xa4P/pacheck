#' Fit random forest metamodel
#'
#' @param df
#' @param y_var
#' @param x_vars
#' @param standardise
#' @param x_poly_2
#' @param x_poly_3
#' @param x_exp
#' @param x_log
#' @param x_inter
#' @param seed_num
#' @param tune_graph
#' @param VIMP
#' @param pm_plot
#' @param pm_variable
#'
#' @import randomForestSRC
#' @import interp
#'
#' @return
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
                             tune_graph = FALSE,
                             VIMP = FALSE,
                             pm_plot = NULL,
                             pm_variable = NULL) {
  # Flag errors
  if(length(y_var) > 1) {
    stop("Multiple outcomes provided to 'y'.")
  }
  if(is.null(y_var)) {
    stop("Cannot perform linear regression because there is no value provided for 'y_var'.")
  }
  if(!is.null(x_inter) && length(x_inter) != 2 * round(length(x_inter) / 2)) {
    stop("The number of interaction terms is oneven.")
  }
  if(is.null(x_vars) && is.null(x_poly_2) && is.null(x_poly_3) && is.null(x_exp) && is.null(x_log)) {
    stop("Cannot perform linear regression because there is no value provided for the predictors.")
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

  # Tune mtry & nodesize
  rf_tune <- tune(form,
                 data = df,
                 splitrule = "mse",
                 )

  # Fit random forest model with tuned parameters
  rf_fit = rfsrc(form,
                 data = df,
                 splitrule = "mse",
                 nodesize = rf_tune$optimal[[1]],
                 mtry = rf_tune$optimal[[2]],
                 forest = TRUE,
                 importance = "permute"
  )

  # Show plots
  ## tune plot
  if (tune_graph == TRUE){
    plot.tune <- function(o, linear = TRUE){
      x <- o$results[,1]
      y <- o$results[,2]
      z <- o$results[,3]
      so <- interp(x=x, y=y, z=z, linear = linear)
      idx <- which.min(z)
      x0 <- x[idx]
      y0 <- y[idx]
      filled.contour(x = so$x,
                     y = so$y,
                     z = so$z,
                     xlim = range(so$x, finite = TRUE) + c(-2, 2),
                     ylim = range(so$y, finite = TRUE) + c(-2, 2),
                     color.palette =
                       colorRampPalette(c("yellow", "red")),
                     xlab = "nodesize",
                     ylab = "mtry",
                     main = "error rate for nodesize and mtry",
                     key.title = title(main = "OOB error", cex.main = 1),
                     plot.axes = {axis(1);axis(2);points(x0,y0,pch="x",cex=1,font=2);
                       points(x,y,pch=16,cex=.25)})
    }
    plot.tune(rf_tune)
  }
  ## VIMP plot
  else if (VIMP == TRUE){
    plot(rf_fit)
  }
  ## partial and/or marginal plot
  else if (pm_plot == "partial") {
    plot.variable.rfsrc(rf_fit,xvar.names=pm_variable,partial = TRUE)
  }
  else if (pm_plot == "marginal") {
    plot.variable.rfsrc(rf_fit,xvar.names=pm_variable,partial = FALSE)
  }
  else if (pm_plot == "both") {
    plot.variable.rfsrc(rf_fit,xvar.names=pm_variable,partial = TRUE)
    plot.variable.rfsrc(rf_fit,xvar.names=pm_variable,partial = FALSE)
  }

  # Export
  l_out = list(tuned_fit = rf_tune,
               rf_fit = rf_fit,
               )

}
