maybeTest <- function(z=sample(1:4, 1)) {
  
  if(z == 1) return(1)
  
  if(z == 2) {
    
    warning("z is 2.")
    return(2)
  }
  
  if(z == 3) stop("z is 3!")
  
  if(z == 4) {
    
    warning("z is 4.")
    warning("No, really: z is 4.")
    return(4)
  }
  
  if(z > 4) {
    
    warning("I can't count that high.")
    stop("z is too big!")
  }
  
  stop("Impossible!!")
  
}

maybe <- function(f, returnName="value", warningName="warning", errorName="error")
  function(...) {
  
  returnValue <- NULL 
  warningValue <- NULL
  errorValue <- NULL
  
  returnValue <- tryCatch(
  
    withCallingHandlers(eval.parent(f(...)),
    
    warning=function(w) {
      
      warningValue <<- append(warningValue, w$message)
      invokeRestart("muffleWarning")
    }),
    
    error=function(e) {
      
      errorValue <<- e$message
      return(NULL)
    }
  )
  
  rval <- list()
  class(rval) <- "Maybe"
  
  rval[returnName] <- list(returnValue)
  rval[warningName] <- list(warningValue)
  rval[errorName] <- list(errorValue)
  
  return(rval)
}

list2maybe <- function(x) {
  
  rval <- list()
  
  rval $ value <- as.list(x)
  
  rval $ errorflag <- rep(FALSE, length(x))
  rval $ errormessage <- rep("", length(x))
  
  rval $ warnings <- data.frame(index=integer(), message=character())
  
  class(rval) <- "maybeList"
  
  return(rval)
}

maybe_llply <- function(.data, .fun, .text="", ...) {

  if(!is(.data, "maybeList")) {
    
    .data <- list2maybe(.data)
  }
  
  maybenot <- .data$errorflag

  z <- list()
  z[maybenot] <- llply(.data$errormessage[maybenot], function(e) maybe(stop(e))())
  z[!maybenot] <- llply(.data$value[!maybenot], maybe(.fun), ..., .progress=progress_simr(.text))
  
  .z <<- z  
  
  rval <- list()
  rval $ value <- llply(z, `[[`, "value")
  
  warnings <- llply(z, `[[`, "warning")
  index <- rep(seq_along(warnings), laply(warnings, length))
  message <- unlist(warnings)
  rval $ warnings <- rbind(.data$warnings, data.frame(index, message))
  
  errors <- llply(z, `[[`, "error")
  rval $ errorflag <- !laply(errors, is.null)
  rval $ errormessage <- simplify2array(ifelse(rval $ errorflag, errors, ""))
  
  class(rval) <- "maybeList"
  
  return(rval)
}