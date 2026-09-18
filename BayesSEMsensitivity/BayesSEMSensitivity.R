## LIBRARIES:
library(blavaan)
library(posterior)
library(shinystan)
library(priorsense)
library(rstan)

## DEFINE THE BAYES SEM MODEL:
RI_CLPM_ALSPAC_Z <- '
#   ############################################################
#   # # 1. RANDOM INTERCEPTS (STABLE TRAITS)
#   ############################################################
#   #  General trend over the time
# 
#   RI_BMI =~ 1*bmi_12.5y_z + 1*bmi_14y_z + 1*bmi_16y_z + 1*bmi_18y_z + 1*bmi_24y_z
#   RI_BE  =~ 1*Binge_14 + 1*Binge_16 + 1*Binge_18 + 1*Binge_24
# 
#   ############################################################
#   # 2. WITHIN-PERSON
#   ############################################################
#   #  Create latent factors per each wave that contains only the variation
# 
#   wBMI_12.5 =~ 1*bmi_12.5y_z
#   wBMI_14   =~ 1*bmi_14y_z
#   wBMI_16   =~ 1*bmi_16y_z
#   wBMI_18   =~ 1*bmi_18y_z
#   wBMI_24   =~ 1*bmi_24y_z
# 
#   wBE_14    =~ 1*Binge_14
#   wBE_16    =~ 1*Binge_16
#   wBE_18    =~ 1*Binge_18
#   wBE_24    =~ 1*Binge_24
# 
#   # Force the residual variance of the observed variable to 0
#   # and force the variance in within factors
#   bmi_12.5y_z ~~ 0*bmi_12.5y_z
#   bmi_14y_z   ~~ 0*bmi_14y_z
#   bmi_16y_z   ~~ 0*bmi_16y_z
#   bmi_18y_z   ~~ 0*bmi_18y_z
#   bmi_24y_z   ~~ 0*bmi_24y_z
# 
#   Binge_14    ~~ 0*Binge_14
#   Binge_16    ~~ 0*Binge_16
#   Binge_18    ~~ 0*Binge_18
#   Binge_24    ~~ 0*Binge_24
# 
#   ############################################################
#   # 3. CROSS-LAG AND AUTOREGRESSIVE DYNAMICS
#   ############################################################
# 
#   BMI PREDICTION (Weight-driven effects)
#   wBMI_14 ~ a1*wBMI_12.5
#   wBMI_16 ~ a2*wBMI_14 + d2*wBE_14
#   wBMI_18 ~ a3*wBMI_16 + d3*wBE_16
#   wBMI_24 ~ a4*wBMI_18 + d4*wBE_18
# 
#   Binge Eating PREDICTION (Eating-driven effects)
#   wBE_14 ~ c1*wBMI_12.5
#   wBE_16 ~ b1*wBE_14 + c2*wBMI_14
#   wBE_18 ~ b2*wBE_16 + c3*wBMI_16
#   wBE_24 ~ b3*wBE_18 + c4*wBMI_18
# 
#   ############################################################
#   # 4. VARIANCE AND COVARIANCE
#   ############################################################
# 
#   # STABLE TRAITS
#   RI_BMI ~~ RI_BE
# 
#   # cross-sectional correlation between deviations (residuals)
#   wBMI_14 ~~ wBE_14
#   wBMI_16 ~~ wBE_16
#   wBMI_18 ~~ wBE_18
#   wBMI_24 ~~ wBE_24
# 
#  # Define the correlations between RI and first measurements:
#  RI_BE ~~ prior("beta(3,3)")*wBE_14
#  RI_BMI ~~ prior("beta(3,3)")*wBMI_12.5
# '



## FIT THE MODEL:
fit_RI_CLPM_Z <- bsem(
   RI_CLPM_ALSPAC_Z,
   data = df,
   ordered = c("Binge_14", "Binge_16", "Binge_18", "Binge_24"),
   burnin = 6000,
   sample = 13000,
   n.chains = 4,
   bcontrol = list(
     control = list(
       adapt_delta = 0.99,
       max_treedepth = 15)),
   dp = dpriors(
     beta = "normal(0,0.3)",
     psi  = "gamma(2,1)",
     rho  = "beta(2,2)"
   )
 )
 
 summary(fit_RI_CLPM_Z, standardized = TRUE, fit.measures = TRUE)
 lavaan::fitMeasures(fit_RI_CLPM_Z)
 
 
## STAN-LIKE DATASET CONFIGURATION 
## AND SAVE THE PARAMETER IN WHICH I AM INTERESTED IN:
 stan_data_list <- fit_RI_CLPM_Z@external$mcmcdata
 
 pars_save <- c(
   "B_free",
   "Alpha_free",
   "Psi_sd_free",
   "Psi_r_mat_1",
   "Psi_r_mat_2",
   "Nu_free",
   "Tau_ufree",
   "log_prior",
   "log_lik")
 
 SEM_stan_model <- stan(
   file = "BayesSEM.stan", 
   data = stan_data_list, 
   chains = 4, 
   iter = 16000, 
   warmup = 4000,
   cores = 1,
   pars = pars_save,
   include = TRUE,
   sample_file = "Stan_model/chain")
 
 print(SEM_stan_model, pars = "log_prior")
 
 psd <- priorsense:::create_priorsense_data.stanfit(
   SEM_stan_model,
   log_prior_name = "log_prior"
 )
 
 # If I dind't put the parameter to save I remove the ones that are not useful and contains Nas values
 # any_nan <- sapply(psd$draws, function(z) any(is.nan(z)))
 # bad_vars <- names(any_nan)[any_nan]
 # bad_prefixes <- unique(sub("\\[.*", "", bad_vars))
 # 
 # draws_clean <- posterior::subset_draws(
 #   psd$draws,
 #   variable = bad_prefixes,
 #   exclude = TRUE
 # )
 # 
 # sum(sapply(draws_clean, function(z) any(is.nan(z))))
 # 
 # psd2 <- psd
 # psd2$draws <- draws_clean
 # 
 # ps <- priorsense:::powerscale_sensitivity.priorsense_data(
 #   psd2,
 #   log_prior_name = "log_prior"
 # )
 # 
 
 
 # Check the sensitivity of the parameter I am interested in:
 ps_seq <- powerscale_sequence(
                 SEM_stan_model,
                 component = "prior",
                  variable = c("B_free[2]","B_free[4]","B_free[6]",
                               "B_free[8]", "B_free[10]","B_free[11]",
                               "B_free[14]"),
                 log_prior_name = "log_prior",
                 lower_alpha = 0.7,upper_alpha = 1.5,length = 50)
 
 powerscale_plot_dens(
             ps_seq,
             variable = c(#"B_free[2]","B_free[4]",
                          #"B_free[6]","B_free[8]",
                          #"B_free[10]","B_free[11]",
                          "B_free[14]"#
                          ))

  powerscale_plot_quantities(
            ps_seq,
            variable = c(#"B_free[2]","B_free[4]",
                         #"B_free[6]","B_free[8]", 
                         #"B_free[10]","B_free[11]",
                         "B_free[14]"#
            ),
            quantity = c("mean", "sd", "quantile"))
  
# Try to tweak the priors
# (In this case the normal distribution related to the coefficients)
 stan_data_sens_2 <- stan_data_list
# Now I have N(0, 0.7).
# NOTE: I notice that in the stan model I used the N(0, 0.3^2).
# SO IMPORTANT TO FIX B_SD ACCORDINGLY:
 stan_data_sens_2$b_sd <- rep(sqrt(0.7), 14) 
 SEM_stan_model@stanmodel

 SEM_stan_model_NEW <- sampling(
  SEM_stan_model@stanmodel,
  data = stan_data_sens,
  chains = 4,
  iter = 16000,
  warmup = 4000,
  thin = 1,
  cores=1,
  seed = 1234,
  sample_file = "Stan_sens_03/chain")

cat("Sampling Completed. Cleaning the memory...\n")
gc()
