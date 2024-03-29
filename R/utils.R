table_summary <- function(object, ...) {
  names <- c("ratio"," link", "nobs", "niter", "logLik")
  info <- data.frame("ratio" = object$ratio,
               "cdf" = object$cdf[1],
               "nobs" = object$nobs,
               "niter" = object$iteration,
               "logLik" = object$LogLikelihood,
               row.names = "Model info:"
    )
  return(info)
}

#' Printing a fitted \code{glmcat} model object
#' @description \code{print.summary} method for GLMcat objects.
#' @param x an object of class \code{"glmcat"}.
#' @param digits the number of digits in the printed table.
#' @param ... additional arguments affecting the summary produced.
#' @method print summary.glmcat
#' @export
print.summary.glmcat <-
  function(x, digits = max(3, getOption("digits") - 3),
           ...)
  {
    # x <- glmcat_wine_3
    # cat("formula:", x$formula, fill=TRUE)
    signif.stars <- getOption("show.signif.stars")

    print(x$formula)
    print(x$table_summary)

    printCoefmat(x$coefficients[, , drop=FALSE],
                 digits=digits, signif.stars=signif.stars,
                 has.Pvalue=TRUE, ...)

    if(!is.null(correl <- x$correlation)) {
      cat("\nCorrelation of Coefficients:\n")
      ll <- lower.tri(correl)
      correl[ll] <- format(round(correl[ll], digits))
      correl[!ll] <- ""
      print(correl[-1, -ncol(correl)], quote = FALSE, ...)
    }
    return(invisible(x))
  }
