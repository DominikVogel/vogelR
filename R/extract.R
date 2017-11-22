# Functions to extract values from models or other packages' reuslts

# nlme::lme
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




# stats::lme
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


