# Brazil_state
State level hospitalization data from Brazil

TEST version of an AIC-based model averaging alternative to synthetic controls method. Fits a series of negative binomial regressions that control for seasonality and 1 control variable at a time. Also tests seasonality only or seasonality+linear trend only. AIC scores are then extracted and used to calculate model weights. Prediction intervals are then calculated that account for both parameter uncertainty, observation uncertainty, and modle uncertainty, following roughly apporach of Lauer et al. PNAS for generating prediction intervals

TO GET STARTED: Save 'Run glm aic mode ave.R' to computer and open in r studio. Run this file with sample Brazil data (which is automatically loaded from github). With your own data, change parameters listed under "user defined values" at top of code. Input data structure should have same format as sample data ( strata variable that should be a facto/character, date with a yyyy-mm-01 format, and outcome and predictor variables (integers)

