# Brazil_state
State level hospitalization data from Brazil

Implements STL+PCA method described by Shioda et al. (https://www.biorxiv.org/content/early/2018/04/27/302224).  P This is a fast implementation that uses glmer rather than JAGS. Prediction intervals are  calculated that account for both parameter uncertainty, observation uncertainty, and model uncertainty, following roughly apporach of Lauer et al. PNAS for generating prediction intervals.

TO GET STARTED: Save 'stl_pca_parallel_RUN' to computer and open in r studio. Run this file with sample Brazil data (which is automatically loaded from github). With your own data, change parameters listed under "user defined values" at top of code. Input data structure should have same format as sample data ( strata variable that should be a facto/character, date with a yyyy-mm-01 format, and outcome and predictor variables (integers)

