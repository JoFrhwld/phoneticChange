library(phoneticChange)
library(dplyr)

#' generate and rawest /ay/ data
#' 
#+ meas_list
meas <- meas_files(demo = white)

#+ load_vowels
ays <- load_vowels(meas, condition = "where plt_vclass in ('ay','ay0')")

#+ filtering_etc
ays <- ays %>%
          mutate(idstring = gsub("(PH[0-9]+-[0-9]+-[0-9]+-).*",
                          "\\1",
                          File))%>%
          select(-File)  

nrow(ays)

ays %>%
    select(-t, -beg, -end, -location) %>%
    mutate(rand = rnorm(n()))%>%
    arrange(idstring, rand)%>%
    select(-rand)%>%
    mutate(pre_sp = pre_word == "sp",
           fol_sp = fol_word == "sp",
           pre_misc = grepl("\\{", pre_word),
           fol_misc = grepl("\\{", fol_word))%>%
    select(-pre_word, -fol_word, -pre_word_trans, -fol_word_trans)->ays
    


source("../anon/anon_ays.R")

nrow(ays)

system("afplay /System/Library/Sounds/Glass.aiff")

#save(ays, file="data/ays.rda")
