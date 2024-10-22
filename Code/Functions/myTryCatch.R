# ---------------------------------------- #
# to catch warning or error from model fit #
# ---------------------------------------- #
myTryCatch <- function(expr) {
  warn <- err <- NULL
   value <- withCallingHandlers(
     tryCatch(expr, 
             error = function(e) {
               err <- conditionMessage(e)
               return(list(value = NULL, 
                           err = err,
                           warn = NULL))
              }
              #, warning = function(w) {
              # warn <- conditionMessage(w)
              # return(list(value = suppressWarnings(expr),
              #             err = NULL,
              #             warn = warn))
              # }
             )
  )
  
  if ("glm" %in% class(value)|"glmerMod" %in% class(value)){
    value <- list(value = value,
                  err = NULL,
                  warn = NULL)
  }
  
  if (is.null(warn)){
    if ("glmerMod" %in% class(value$value)){
      if(!is.null(value$value@optinfo$cov$lme4$message)){
        if(!isSingular(value$value)){
          value$warn <- append(warn, value$value@optinfo$cov$lme4$message) 
        }
      }
      if (length(value$value@optinfo$warnings) > 0){
        value$warn <- append(warn, unlist(value$value@optinfo$warnings)) 
      }
    }
  }
  
  list(value=value$value, 
       warn=value$warn, 
       error=value$err)
}
