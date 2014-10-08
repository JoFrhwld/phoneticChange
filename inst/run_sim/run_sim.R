library(phoneticChange)
library(plyr)
library(dplyr)
library(reshape2)

#' Calculate means

ays_means <- ays %>%
  filter(context=="internal", stress == 1) %>%
  group_by(idstring, year, age, DOB, sex, years_of_schooling,
           plt_vclass, word) %>%
  summarise(F1_n = mean(F1_n), tokens = n()) %>%
  summarise(F1_n = mean(F1_n),
            tokens = sum(tokens),
            types = n())%>%
  mutate(ratio = tokens/types)


#' Select high and low speakers
low_speakers <- ays_means%>%
  filter(plt_vclass=="ay0",
         tokens > 10,
         ratio < 10)%>%
  ungroup()%>%
  arrange(F1_n)%>%
  tail(6)

high_speakers <- ays_means%>%
  filter(plt_vclass=="ay0",
         tokens > 10,
         ratio < 10)%>%
  ungroup()%>%
  arrange(F1_n)%>%
  head(6)

low_data <- ays %>%
  filter(idstring %in% low_speakers$idstring,
         plt_vclass == "ay0")%>%
  group_by(idstring)%>%
  dplyr::mutate(id = 1:n())%>%
  filter(id <= 60)


high_data <- ays %>%
  filter(idstring %in% high_speakers$idstring,
         plt_vclass == "ay0")%>%
  group_by(idstring)%>%
  dplyr::mutate(id = 1:n())%>%
  filter(id <= 60)


#' Run simulation
simulation <- change_sim(low_data, high_data, nsim = 1000, N = 60, p = seq(0,1, by = 0.1))
save(simulation, file = "data/simulation.rda")




