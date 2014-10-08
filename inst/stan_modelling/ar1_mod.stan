data{
  // N is the number of data points,
  // y is the response variable
  int<lower=0> N;
  real y[N];

  // these max_* variables are used
  // to tell the model how many
  // speaker level intercepts to estimate, etc.
  int<lower=0> max_dob;
  int<lower=0> max_speaker;
  int<lower=0> max_word;
  int<lower=0> max_street;
  int<lower=0> max_context;

  // speaker_dob is the primary explanatory variable
  int<lower=0> speaker_dob[N];


  real dur[N];
  // context_id defines the different contexts
  // for ay. It s just an integer id for each
  // context, since each contexts trajectory is
  // estimated separately
  int<lower=0> context_id[N];

  // d_indices, for example, is
  // two integers which indicate
  // which values of context_id correspond
  // to data from a following d context.
  // primarilly used in the generated quantities block
  int<lower=0> d_indices[2];
  int<lower=0> t_indices[2];
  int<lower=0> flap_indices[2];
  int<lower=0> faith_indices[2];

  // ids for random effects
  int<lower=0> speaker_id[N];
  int<lower=0> word_id[N];
  int<lower=0> street_id[N];

  int<lower=0> dob_pred[max_speaker];
  real ss_tot;
}

parameters{

  // "random effects"
  matrix[max_context, max_speaker] speaker_effects;
  vector[max_word] word_effects;
  matrix[max_context, max_street] street_effects;

  // in this simpler model, there is only one sigma,
  // not one for each speaker.
  // real<lower=0, upper=100> sigma[max_speaker];
  real<lower=0, upper=100> sigma;


  // this simper model doesnt need tau
  // real<lower=0, upper=100> tau;

  //
  real<lower=0, upper=100> speaker_sigma;
  real<lower=0, upper=100> word_sigma;
  real<lower=0, upper=100> street_sigma;

  // Originally, dob_deltas was drawn from a normal(0,1).
  // But, that allowed for too much overfitting.
  // That is, dob_delta capturing variation which would
  // be more appropriately captured by a speaker_effect.
  // So, the scale parameter was added as a way to
  // reduce this effect.
  real<lower=0, upper=100> delta_sigma;

  // This is the primary parameter of interest.
  // For every DOB, it represents the difference from
  // the year before. Its length is max_dob-1 because
  // the delta for the first year is necessarilly 0.
  matrix[max_context, max_dob-1] dob_deltas;

  // After cumulatively summing dob_deltas,
  // there is an additional intercept and
  // context parameters, to shift the mu up or down.
  real intercept;
  real context[max_context-1];
  real dur_slope;


}

transformed parameters{
  // dob_mu is the cumulative sum of the deltas.
  // it represents the expected value of y for a given DOB
  matrix[max_context, max_dob] dob_mu;

  // needed some way to force the first delta to be 0.
  // used real_deltas as container for that.
  // Effectively, its just dob_deltas with a 0 concatennated
  // at the front
  matrix[max_context, max_dob] real_deltas;


  // for each context, the first
  // delta is logically 0.
  for(i in 1:(max_context)){
    real_deltas[i,1] <- 0.0;
  }
  for(i in 1:(max_dob-1)){
    for(j in 1:max_context){

      // after the first delta, the rest of
      // real deltas is identical to dob_deltas.
      real_deltas[j, i+1] <- dob_deltas[j,i];
    }
  }

  // the first ay context is treated as just having an intercept
  dob_mu[1] <- cumulative_sum(real_deltas[1]) + intercept;
  for(i in 2:(max_context)){

   // every subsequent context is the intercept, plus the relevant context effect
    dob_mu[i] <- cumulative_sum(real_deltas[i]) + intercept + context[i-1];
  }

}
model{
  // y_hat and sigma_hat are necessary for
  // utilizing vectorized sampling.
  // it makes the model run faster.
  vector[N] y_hat;
  // vector[N] sigma_hat;

  for(j in 1:max_context){
    // the first dob_delta is relatively unconstrained.
    dob_deltas[j,1] ~ normal(0, 100);
    speaker_effects[j] ~ normal(0, speaker_sigma);
    street_effects[j] ~ normal(0, street_sigma);
    for(i in 2:(max_dob-1)){
      // each subsequent dob_delta is drawn from
      // normal(dob_delta[i-1], delta_sigma)
      dob_deltas[j,i] ~ normal(dob_deltas[j,i-1], delta_sigma);
    }
  }

  intercept ~ normal(0, 100);
  context ~ normal(0, 100);
  dur_slope ~ normal(0, 100);
  word_effects ~ normal(0, word_sigma);

  // only one sigma in this model
  // sigma ~ cauchy(0, tau);

  for(i in 1:N){
    y_hat[i] <-dob_mu[context_id[i],speaker_dob[i]] +
                  (dur[i] * dur_slope) +
                  speaker_effects[context_id[i],speaker_id[i]] +
                  word_effects[word_id[i]] +
                  street_effects[context_id[i],street_id[i]];
    // sigma_hat[i] <- sigma[speaker_id[i]];
  }
  // y ~ normal(y_hat, sigma_hat);
  y ~ normal(y_hat, sigma);


}

generated quantities{
  // diagnostics, etc
  matrix[max_context+1, max_dob] mu_diffs;
  real y_res_diff[N];
  real ss_res;

  matrix[max_context, max_speaker] speaker_est;
  matrix[max_context+1, max_speaker] speaker_diffs;

  real r_squared;

  for(i in 1:N){
    y_res_diff[i] <- pow(y[i] - (dob_mu[context_id[i],speaker_dob[i]] +
                  (dur[i] * dur_slope) +
                  speaker_effects[context_id[i],speaker_id[i]] +
                  word_effects[word_id[i]] +
                  street_effects[context_id[i],street_id[i]]),2);
  }

  ss_res <- sum(y_res_diff);

  r_squared <- 1-(ss_res/ss_tot);

  mu_diffs[1] <- dob_mu[t_indices[1]] - dob_mu[d_indices[1]];
  mu_diffs[2] <- dob_mu[t_indices[2]] - dob_mu[d_indices[2]];
  mu_diffs[3] <- dob_mu[faith_indices[1]] - dob_mu[flap_indices[1]];
  mu_diffs[4] <- dob_mu[faith_indices[2]] - dob_mu[flap_indices[2]];
  mu_diffs[5] <- mu_diffs[1] - mu_diffs[2];

  for(i in 1:max_speaker){
    for(j in 1:max_context){
      speaker_est[j,i] <- dob_mu[j,dob_pred[i]] + speaker_effects[j, i];
    }
  }
  speaker_diffs[1] <- speaker_est[t_indices[1]] - speaker_est[d_indices[1]];
  speaker_diffs[2] <- speaker_est[t_indices[2]] - speaker_est[d_indices[2]];
  speaker_diffs[3] <- speaker_est[faith_indices[1]] - speaker_est[flap_indices[1]];
  speaker_diffs[4] <- speaker_est[faith_indices[2]] - speaker_est[flap_indices[2]];
  speaker_diffs[5] <- speaker_diffs[1] - speaker_diffs[2];

}