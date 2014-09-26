library(phoneticChange)
library(dplyr)

#' generate and rawest mid_low_data
#' 
#+ meas_list
meas <- meas_files(demo = white)

#+ load_vowels
mid_low <- load_vowels(meas, condition = "where plt_vclass in ('o','uh')")
system("afplay /System/Library/Sounds/Glass.aiff")

#+ generate_means
mid_low <- mid_low %>%
  mutate(idstring = gsub("(PH[0-9]+-[0-9]+-[0-9]+-).*",
                         "\\1",
                         File))%>%
  select(-File)  

mid_low_means <- mid_low %>%
  group_by(idstring, plt_vclass, word)%>%
  summarise(F1_n = mean(F1_n),
            F2_n = mean(F2_n))%>%
  summarise(F1_n = mean(F1_n),
            F2_n = mean(F2_n))

save(mid_low_means, file = "data/mid_low_means.rda")
