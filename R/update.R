update_R <- function() {
  #* Establish a new 'ArgCheck' object
  Check <- ArgumentCheck::newArgCheck()

  #* Add an error if installr is not installed
  if (!requireNamespace("installr", quietly = TRUE))
    ArgumentCheck::addError(
      msg = "package 'installr' is needed for this function. Please install it",
      argcheck = Check
    )

  #* Return errors and warnings (if any)
  ArgumentCheck::finishArgCheck(Check)

  installr::updateR()
}

