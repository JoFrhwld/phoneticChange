library(rstan)
library(phoneticChange)
library(dplyr)

ays_faith <- ays_faith %>%
  mutate(F1_80_n = as.numeric(as.character(F1_80_n)),
         context = "faithful")%>%
  select(word, age, sex, year, F1_n, fol_seg, context, idstring, DOB, dur)

ays_flap <- ays_flap %>%
  mutate(F1_80_n = as.numeric(as.character(F1_80_n)),
         context = "flap")%>%
  filter(flap_coding %in% c("app", "approx", "flao", "flap","?"))%>%
  select(word, age, sex, year, F1_n, fol_seg, context, idstring, DOB, dur)


ays_comp <- rbind_list(ays_faith, ays_flap)

mod_data <- ays_comp%>%
  filter(context %in% c("faithful","flap")) %>%
  mutate(DOB0 = DOB-min(DOB)+1,
         speakerN = as.numeric(as.factor(idstring)),
         wordN = as.numeric(as.factor(word)),
         streetid = gsub("(PH[0-9]+-[0-9]+-).*", "\\1", idstring),
         streetN = as.numeric(as.factor(streetid)),
         fol_segN = as.numeric(as.factor(paste(fol_seg,context))),
         log_dur = log2(dur)-mean(log2(dur)))

dob_pred <- mod_data %>%
  group_by(speakerN) %>%
  summarise(DOB0 = max(DOB0)) %>%
  arrange(speakerN)

mod_data_list <- list(
  N = nrow(mod_data),
  y = mod_data$F1_n,
  max_dob = max(mod_data$DOB0),
  dur = mod_data$log_dur,
  max_street = max(mod_data$streetN),
  max_speaker = max(mod_data$speakerN),
  max_word = max(mod_data$wordN),
  max_context = max(mod_data$fol_segN),

  speaker_dob = mod_data$DOB0,


  speaker_id = mod_data$speakerN,
  word_id = mod_data$wordN,
  street_id = mod_data$streetN,
  context_id = mod_data$fol_segN,


  d_indices = c(1,2),
  t_indices = c(3,4),
  flap_indices= c(2,4),
  faith_indices = c(1,3),

  dob_pred = dob_pred$DOB0,
  ss_tot = sum((mod_data$F1_n - mean(mod_data$F1_n))^2)
)

ar1_mod <- stan(file = system.file("stan_modelling/ar1_mod.stan", package = "phoneticChange"),
                      data = mod_data_list,
                      chains = 4,
                      iter = 5000)

ar1_summary <- summary(ar1_mod)$summary
ar1_summary_df <- as.data.frame(ar1_summary)
ar1_summary_df$param <- row.names(ar1_summary_df)

# save(ar1_mod, file ="data/ar1_mod.rda")
# save(ar1_summary_df, file ="data/ar1_summary_df.rda")

