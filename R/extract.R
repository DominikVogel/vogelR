# Functions to extract values from models or other packages' reuslts

# nlme::lme
extract_hlm <- function(model, var, parameter = c("p", "b")) {
  if (!requireNamespace("broman", quietly = TRUE)) {
    stop("Pkg needed for this function to work. Please install it.",
         call. = FALSE)
  }
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
extract_lm <- function(model, var, parameter = c("p", "b")) {
  if (!requireNamespace("broman", quietly = TRUE)) {
    stop("Pkg needed for this function to work. Please install it.",
         call. = FALSE)
  }
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


