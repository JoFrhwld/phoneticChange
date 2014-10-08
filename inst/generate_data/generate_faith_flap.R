library(phoneticChange)

ayTD <- ays %>%
          filter(context == "internal", fol_seg %in% c("T","D"))




syllabify_no_stress <- function(trans){
  syls <- unlist(syllabify(trans))
  syls <- syls[names(syls) != "stress"]
  return(syls)
}

new_vowel_index <- function(trans, vowel_index){
  while(!grepl("AY", trans[vowel_index])){
    vowel_index <- vowel_index + 1
  }
  return(vowel_index)
}

syllables <- ayTD %>%
  group_by(word_trans, vowel_index) %>%
  summarise(n = n()) %>%
  ungroup()%>%
  group_by(word_trans, vowel_index)%>%
  do(word = syllabify_no_stress(.$word_trans),
     vowel_index2 = new_vowel_index(syllabify_no_stress(.$word_trans),
                                    .$vowel_index))%>%
  group_by(word_trans, vowel_index) %>%
  do(data.frame(word_status = names(.$word[[1]])[.$vowel_index2[[1]]+1]))

ayTD <- ayTD %>% left_join(syllables)



ays_faith <- ayTD %>%
  filter(grepl("AY1 [TD]$", word_trans), fol_sp)

#save(ays_faith, file="data/ays_faith.rda")
