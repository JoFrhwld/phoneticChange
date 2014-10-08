library(phoneticChange)
library(dplyr)

#' generate and rawest /ay/ data
#'
#+ meas_list
meas <- meas_files(demo = white)

#+ load_vowels
ays <- load_vowels(meas, condition = "where plt_vclass in ('ay','ay0')")
ays_flap <- read.delim(system.file("extdata/word_flap.Table", package="phoneticChange"))

#+ filtering_etc
ays <- ays %>%
          mutate(idstring = gsub("(PH[0-9]+-[0-9]+-[0-9]+-).*",
                          "\\1",
                          File),
                 DOB = year-age)%>%
          select(-File)

ays_flap <- ays_flap %>%
  mutate(idstring = gsub("(PH[0-9]+-[0-9]+-[0-9]+-).*",
                         "\\1",
                         File),
         DOB = year-age)%>%
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

ays_flap %>%
  select(-t, -beg, -end, -location) %>%
  mutate(rand = rnorm(n()))%>%
  arrange(idstring, rand)%>%
  select(-rand)%>%
  mutate(pre_sp = pre_word == "sp",
         fol_sp = fol_word == "sp",
         pre_misc = grepl("\\{", pre_word),
         fol_misc = grepl("\\{", fol_word))%>%
  select(-pre_word, -fol_word, -pre_word_trans, -fol_word_trans,
         -word_fol_seg2, -phrase_fol_seg2)->ays_flap


source("inst/anon/anon_ays.R")

nrow(ays)

system("afplay /System/Library/Sounds/Glass.aiff")

subtlex_data <- subtlex_us()%>%
                  select(Word, SUBTLWF)%>%
                  mutate(word = Word)%>%
                  select(-Word)%>%
                  mutate(word = tolower(word))

ays <- ays %>%
          select(-SUBTLWF)%>%
          mutate(word = tolower(word))%>%
          mutate(word = gsub("in'$", "ing", word))%>%
          left_join(subtlex_data)

ays_flap <- ays_flap %>%
  #select(-SUBTLWF)%>%
  mutate(word = tolower(word))%>%
  mutate(word = gsub("in'$", "ing", word))%>%
  left_join(subtlex_data)

save(ays, file="data/ays.rda")
save(ays_flap, file = "data/ays_flap.rda")
