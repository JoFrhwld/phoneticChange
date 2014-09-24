#' Create Verboten Vector
#' @export

verboten_vector <- function(x = c("PH76-3-1-",
                                  "PH84-2-5-",
                                  "PH84-2-3-",
                                  "PH92-1-1-",
                                  "PH82-1-16-",
                                  "PH80-2-5-",
                                  "PH80-2-4-",
                                  "PH85-1-5-",
                                  "PH84-1-2-",
                                  "PH84-1-1-",
                                  "PH77-3-4-")){
  return(x)
}

#' Create Verboten String
#' @export

verboten_string <- function(x = c("PH76-3-1-",
                                  "PH84-2-5-",
                                  "PH84-2-3-",
                                  "PH92-1-1-",
                                  "PH82-1-16-",
                                  "PH80-2-5-",
                                  "PH80-2-4-",
                                  "PH85-1-5-",
                                  "PH84-1-2-",
                                  "PH84-1-1-",
                                  "PH77-3-4-")){
  return(paste(x, collapse = "|"))
}