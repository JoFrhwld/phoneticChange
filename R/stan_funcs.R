#' Extract from rstan summary
#' @export
#' @importFrom plyr llply ldply
#' @import stringr

extract_from_summary <- function(summary = NULL, pars){

  input <- as.data.frame(summary)

  pars <- paste(paste("^", pars, sep = ""), collapse = "|")
  summary.df <- input[grepl(pars, row.names(input)),]

  summary.df$full_pars <- row.names(summary.df)
  summary.df$pars <- gsub("\\[.*\\]", "", summary.df$full_pars)

  dim_cols <- ldply(
    llply(
      llply(
        str_split(
          gsub("\\[|\\]","",
               str_extract(summary.df$full_pars, "\\[.*\\]")
          ),
          ","),
        as.numeric),
      rbind),
    as.data.frame)

  summary.df <- cbind(summary.df, dim_cols)

  return(summary.df)

}