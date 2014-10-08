#' Load Demographics
#'
#' @export
#' @import dplyr

load_demographics <- function(path ="/Volumes/jfruehwa/PNC/PNC_corpus_info.txt"){
  demographics <- read.delim(path)
  sub_demo <- demographics %>% filter((!grepl("a|h", ethnicity)),
                                      (!grepl(verboten_string(), speaker))) %>%
    mutate(idstring = gsub("(PH[0-9]+-[0-9]+-[0-9]+-).*",
                           "\\1",
                           speaker))%>%
    select(-speaker)
  return(sub_demo)
}

#' List meas files
#'
#' @export
#' @import dplyr

meas_files <- function(glob = "/Volumes/jfruehwa/PNC/PH*/PH*/*_meas.txt", demo){
  meas_files <- Sys.glob(glob)
  meas_files <- meas_files[grepl(paste0(demo$idstring, collapse = "|"),
                                 meas_files)]
  return(meas_files)
}

#' SQL load
#'
#' @export
#' @import dplyr
#' @import sqldf

sql_load <- function(x, selection = "*", condition, file.format = list(header = TRUE, sep = "\t")){
  fi <- file(x)
  df <- sqldf(paste("select", selection, "from fi", condition, sep = " "),
              file.format = file.format)
  close(fi)
  if(nrow(df)>0){
    df$File <- basename(x)
  }
  return(df)
}

#' Load a vowel
#'
#' @export
#' @importFrom plyr ldply

load_vowels <- function(meas_files, condition = "where plt_vclass in ('ay','ay0')", ...){
     data <- ldply(meas_files,
               sql_load,
               condition = condition,
               ...)
}


#' Fetch and load SubtlexUS
#'
#' @export

subtlex_us <- function(){
  temp <- tempfile()
  download.file("http://crr.ugent.be/papers/SUBTLEX-US_frequency_list_with_PoS_information_final_text_version.zip",
                temp)
  out <- read.delim(unz(temp, "SUBTLEX-US frequency list with PoS information text version.txt"))
  unlink(temp)
  return(out)
}