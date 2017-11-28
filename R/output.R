# x is a matrix containing the data
# method : correlation method. "pearson"" or "spearman"" is supported
# removeTriangle : remove upper or lower triangle# results :  if "html" or "latex"
# the results will be displayed in html or latex format# labels_rows and labels_cols are character vectors for labeling rows and columns
#' Title
#'
#' @param x a matrix containing the data
#' @param method correlation method. "pearson"" or "spearman"" is supported
#' @param removeTriangle remove upper or lower triangle
#' @param result Print result in Console ("none"), generate HTML file ("html"), generate latex file ("latex")
#' @param labels_rows Labels for the rows (i.e., variable names). Length musst be same as number of variables
#' @param labels_cols Labels for columns. Length musst be same as number of variables - 1
#' @param sig.level Significance level (.1 or .05)
#' @param caption Caption for the table
#' @param filename File name to save output to
#'
#' @return Correlation table in console or file
#'
#' @author Dominik Vogel
#'
#'
#' @examples
#' corstars(mtcars, method="pearson", removeTriangle="upper", result="html",
#'    caption = "Correlations", filename = "corr.html",
#'    sig.level = 0.1,
#'    labels_rows = c("(1) mpg", "(2) cyl", "(3) disp", "(4) hp",
#'                    "(5) drat", "(6) wt", "(7) qsec", "(8) vs",
#'                    "(9) am", "(10) gear",
#'                    "(11) carb"),
#'    labels_cols = 1:10)
#'
#' corstars(mtcars, method="pearson", removeTriangle="upper", result="html",
#'    caption = "Correlations", filename = "corr.html",
#'    sig.level = 0.1,
#'    labels_rows = c("(1) mpg", "(2) cyl", "(3) disp", "(4) hp",
#'                    "(5) drat", "(6) wt", "(7) qsec", "(8) vs",
#'                    "(9) am", "(10) gear",
#'                    "(11) carb"),
#'    labels_cols = 1:10)
#'
#' @export
corstars <- function(x, method=c("pearson", "spearman"),
                     removeTriangle=c("upper", "lower"),
                     result=c("none", "html", "latex"),
                     labels_rows, labels_cols = 1:length(labels_rows)-1,
                     sig.level = 0.05,
                     caption = c("Correlation"), filename = ""){
  requireNamespace("Hmisc", quietly = TRUE)
  requireNamespace("xtable", quietly = TRUE)
  stopifnot(length(labels_rows) == ncol(x))
  stopifnot(length(labels_cols) == ncol(x)-1)
  #Compute correlation matrix
  x <- as.matrix(x)
  correlation_matrix<-Hmisc::rcorr(x, type=method[1])
  R <- correlation_matrix$r # Matrix of correlation coeficients
  p <- correlation_matrix$P # Matrix of p-value

  ## Define notions for significance levels; spacing is important.
  ifelse(sig.level == 0.1,
         mystars <- ifelse(p < .01,"**", ifelse(p < .05, "* ", "  ")),
         ifelse(sig.level == 0.05,
                mystars <- ifelse(p < .05, "*", " "),""))

  #mystars <- ifelse(p < .001, "****", ifelse(p < .001, "*** ", ifelse(p < .01, "**  ", ifelse(p < .05, "*   ", "    "))))

  ## trunctuate the correlation matrix to two decimal
  R <- format(round(cbind(rep(-1.11, ncol(x)), R), 2))[,-1]

  ## build a new matrix that includes the correlations with their apropriate stars
  Rnew <- matrix(paste(R, mystars, sep=""), ncol=ncol(x))
  diag(Rnew) <- paste(diag(R), " ", sep="")
  rownames(Rnew) <- colnames(x)
  colnames(Rnew) <- paste(colnames(x), "", sep="")

  ## remove upper triangle of correlation matrix
  if(removeTriangle[1]=="upper"){
    Rnew <- as.matrix(Rnew)
    Rnew[upper.tri(Rnew, diag = TRUE)] <- ""
    Rnew <- as.data.frame(Rnew)
  }

  ## remove lower triangle of correlation matrix
  else if(removeTriangle[1]=="lower"){
    Rnew <- as.matrix(Rnew)
    Rnew[lower.tri(Rnew, diag = TRUE)] <- ""
    Rnew <- as.data.frame(Rnew)
  }

  ## remove last column and return the correlation matrix
  Rnew <- cbind(Rnew[1:length(Rnew)-1])
  rownames(Rnew) <- labels_rows
  colnames(Rnew) <- labels_cols
  if (result[1]=="none") return(Rnew)
  else{
    if(result[1]=="html") print(xtable::xtable(Rnew, caption = caption),
                                type="html",
                                file = filename)
    else print(xtable::xtable(Rnew, caption = caption),
               type="latex")
  }
}




