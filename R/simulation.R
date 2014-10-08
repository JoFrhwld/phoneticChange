#' Sample from two data frames
#' 
#' @export
#' @import dplyr

take_samp <- function(df1, df2, N, p = 1-q, q = 1-p){
    N1 <- round(N * p)
    N2 <- N-N1
    
    df1 %>% 
      ungroup()%>%
      sample_n(N1)->samp1 
  
    df2 %>% 
      ungroup()%>%
      sample_n(N2)->samp2
    
    out <- rbind_list(samp1, samp2)
    return(out)
}


#' Run the simulation
#' 
#'  @export
#'  @import dplyr
#'  @importFrom moments kurtosis

change_sim <- function(df1, df2, nsim = 1000, N = 60,  p = 1-q, q = 1-p){
  sim_setup <- expand.grid(p = rep(p, nsim),
                           N = N)
  sim_setup %>%
      mutate(sim_id = 1:n())%>%
      rowwise()%>%
      do(data.frame(p = .$p,
                    sim_id = .$sim_id,
                    take_samp(df1, df2, .$N, .$p)))%>%
      group_by(p, sim_id)%>%
      summarise(n = n(),
                F1_mean = mean(F1_n),
                F1_sd = sd(F1_n),
                F1_kurtosis = kurtosis(F1_n))->sim
  return(sim)
}
