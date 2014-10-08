#' Basic Demographic Info from the PNC
#'
#'
#' @format A data frame with 326 rows
#' \describe{
#'   \item{sex}{speaker sex}
#'   \item{year}{year of interview}
#'   \item{ethnicity}{single character code for reported ethnicity}
#'   \item{schooling}{highest educational attainment}
#'   \item{transcribed}{how many seconds of transcribed speech}
#'   \item{total}{total duration of recording}
#'   \item{nvowels}{number of vowels measured}
#'   \item{idstring}{unique idstring for a speaker}
#' }
#' @source Philadelphia Neighborhood Corpus
"white"


#' Rawest ays data
#'
#'
#' @format A data frame with 84713 rows
#' @source Philadelphia Neighborhood Corpus
"ays"

#' Means for mid and low vowels
#'
#'
#' @format A data frame with 652 rows
#' @source Philadelphia Neighborhood Corpus
"mid_low_means"


#' Simulation
#' A simulation of categorical phonetic change
#' @source see \code{system.file(run_sim/run_sim.R, package = "phoneticChangne")}
"simulation"


#' All Vowel Means
#'
#' Vowel means from the PNC
#' @source Philadelphia Neighborhood Corpus
"all_means"

#' /ay/ before faithful /t/, /d/
#'
#' @source The Philadelphia Neighborhood Corpus
"ays_faith"


#' /ay/ before flapped /t/, /d/
#'
#' @source The Philadelphia Neighborhood Corpus
"ays_flap"


#' full flap tracks
#'
#' @source The Philadelphia Neighborhood Corpus
"flap_tracks"

#' Stan model summary
"ar1_summary_df"

