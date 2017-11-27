# Functions to extract values from models or other packages' reuslts

#' Extract parameters from nlme::lme-models
#'
#' @description Function extracts coefficients and p-values from nlme::lme-models
#'
#' @param model The nlme::lme-model (must be build in advance)
#' @param var Name of Variable for with parameter should be extracted. Put in "".
#' @param parameter "b" = coefficient, "p" = p-value
#'
#' @return "b = ..." or "p = ..."
#'
#' @author Dominik Vogel
#'
#' @examples
#' model <- nlme::lme(mpg ~ cyl, random = ~1|am, data = mtcars)
#' extract_hlm(model, "cyl", "b")
#'
#' @export
extract_hlm <- function(model, var, parameter) {
  #* Establish a new 'ArgCheck' object
  Check <- ArgumentCheck::newArgCheck()

  #* Add an error if parameter is not p or b
  if (parameter != "p" & parameter != "b")
    ArgumentCheck::addError(
      msg = "You can only extract coefficients and p-values. Use 'b' or 'p'",
      argcheck = Check
    )

  #* Add an error if broman is not installed
  if (!requireNamespace("broman", quietly = TRUE))
    ArgumentCheck::addError(
      msg = "package 'broman' is needed for this function. Please install it",
      argcheck = Check
    )

  #* Add an error if ArgumentCheck is not installed
  if (!requireNamespace("ArgumentCheck", quietly = TRUE))
    ArgumentCheck::addError(
      msg = "package 'ArgumentCheck' is needed for this function. Please install it",
      argcheck = Check
    )

  #* Return errors and warnings (if any)
  ArgumentCheck::finishArgCheck(Check)

  sum_model <- summary(model)
  ifelse(parameter == "p",
         ifelse(sum_model[["tTable"]][var, 5] >= 0.001,
                out <- paste0("p = ",
                              broman::myround(sum_model[["tTable"]][var, 5],
                                              digits = 3)),
                out <- "p < 0.001"),
         ifelse(parameter == "b",
                out <- paste0("b = ",
                              broman::myround(sum_model[["coefficients"]][["fixed"]][var],
                                              digits = 2)),
                NULL)
  )
  return(out)
}





#' Extract parameters from stats::lm-models
#'
#' @description Function extracts coefficients and p-values from stats::lm-models
#'
#' @param model The stats::lm-model (must be build in advance)
#' @param var Name of Variable for with parameter should be extracted. Put in "".
#' @param parameter "b" = coefficient, "p" = p-value
#'
#' @return returns "b = ..." or "p = ..."
#'
#' @author Dominik Vogel
#'
#' @examples
#' model <- stats::lm(mpg ~ cyl + am, data = mtcars)
#' extract_lm(model, "cyl", "p")
#'
#' @export
extract_lm <- function(model, var, parameter) {
  #* Establish a new 'ArgCheck' object
  Check <- ArgumentCheck::newArgCheck()

  #* Add an error if parameter is not p or b
  if (parameter != "p" & parameter != "b")
    ArgumentCheck::addError(
      msg = "You can only extract coefficients and p-values. Use 'b' or 'p'",
      argcheck = Check
    )

  #* Add an error if broman is not installed
  if (!requireNamespace("broman", quietly = TRUE))
    ArgumentCheck::addError(
      msg = "package 'broman' is needed for this function. Please install it",
      argcheck = Check
    )

  #* Add an error if ArgumentCheck is not installed
  if (!requireNamespace("ArgumentCheck", quietly = TRUE))
    ArgumentCheck::addError(
      msg = "package 'ArgumentCheck' is needed for this function. Please install it",
      argcheck = Check
    )

  #* Return errors and warnings (if any)
  ArgumentCheck::finishArgCheck(Check)

  sum_model <- summary(model)
  ifelse(parameter == "p",
         ifelse(sum_model[["coefficients"]][var,4] >= 0.001,
                out <- paste0("p = ",
                              broman::myround(sum_model[["coefficients"]][var,4],
                                      digits = 3)),
                out <- "p < 0.001"),
         ifelse(parameter == "b",
                out <- paste0("b = ",
                              broman::myround(sum_model[["coefficients"]][[var,1]],
                                      digits = 2)),
                NULL)
  )
  return(out)
}


#' Extract Cronbach's Alpha
#'
#' @description Function to perform a Cronbach's Alpha analysis and
#' to extract the Alpha value
#'
#' @param vars Variables to include in the analysis
#' @param data Data frame to select variables from
#'
#' @return Alpha value
#'
#' @author Dominik Vogel
#'
#' @examples
#' extract_alpha(vars = c("cyl", "hp", "wt"), data = mtcars)
#'
#' @export
extract_alpha <- function(vars, data) {
  M1 <- dplyr::select(data, vars)
  alpha <- suppressWarnings(psych::alpha(M1, warnings = FALSE))
  alpha <- broman::myround(alpha$total$raw_alpha, digits = 2)
  return(alpha)
}
