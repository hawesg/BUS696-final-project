################################################################################
#                                                                              #
# Purpose:       OLS Models                                                    #
#                                                                              #
# Author:        Garrett H, Bart C, Ricky L                                    #
# Contact:       hawes102@mail.chapman.edu                                     #
# Client:        Jonathan Hersh                                                #
#                                                                              #
# Code created:  2019-11-22                                                    #
# Last updated:  2019-11-22                                                    #
# Source:        /Users/garretthawes/wine-project                              #
#                                                                              #
# Comment:       Will still need to play around with this, variable selection  #
#                is based on forward/backward step                             #
#                                                                              #
################################################################################

###############################################################################-
#                                                                              #
#  Copy this stuff into the top of any script you are working on for modeling  #
#                                                                              #
###############################################################################-

# rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.

if(!(exists("wine_train")&&exists("wine_train"))) {
  load(here::here("data","output","wine_train.RData"))
  load(here::here("data","output","wine_test.RData")) 
}

###############################################################################-

################################## Libraries ###################################
#                                                                              #
#  Omit this if you want to just load them manually                            #
#                                                                              #
###############################################################################-
#
source("code/libraries.R")


.sig <- function(column, keep_vars) {
  return(fct_relevel(fct_other(column, keep = keep_vars), "Other"))
}
m1 <- lm(log(price) ~ points, data = wine_train)
summary(m1)

# Call:
#   lm(formula = log(price) ~ points, data = wine_train)
#
# Residuals:
#   Min      1Q  Median      3Q     Max
# -1.6916 -0.3708 -0.0491  0.3172  4.8832
#
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)
# (Intercept) -9.071924   0.050889  -178.3   <2e-16 ***
#   points       0.139664   0.000574   243.3   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# Residual standard error: 0.5151 on 91498 degrees of freedom
# Multiple R-squared:  0.3929,	Adjusted R-squared:  0.3929
# F-statistic: 5.921e+04 on 1 and 91498 DF,  p-value: < 2.2e-16
m2 <- lm(log(price) ~ points + country_lump, data = wine_train)
summary(m2)

# Call:
#   lm(formula = log(price) ~ points + country_lump, data = wine_train)
#
# Residuals:
#   Min      1Q  Median      3Q     Max
# -1.6225 -0.3490 -0.0530  0.2973  4.8603
#
# Coefficients:
#   Estimate Std. Error  t value Pr(>|t|)
# (Intercept)             -8.6589680  0.0510134 -169.739  < 2e-16 ***
#   points                   0.1339819  0.0005807  230.707  < 2e-16 ***
#   country_lumpAustralia    0.0467775  0.0138937    3.367 0.000761 ***
#   country_lumpAustria     -0.1513689  0.0126734  -11.944  < 2e-16 ***
#   country_lumpChile       -0.1059646  0.0111777   -9.480  < 2e-16 ***
#   country_lumpFrance       0.1099490  0.0090799   12.109  < 2e-16 ***
#   country_lumpItaly        0.2384090  0.0096484   24.710  < 2e-16 ***
#   country_lumpNew Zealand -0.0104511  0.0162750   -0.642 0.520773
# country_lumpPortugal    -0.2277781  0.0109108  -20.876  < 2e-16 ***
#   country_lumpSpain        0.0081494  0.0102640    0.794 0.427210
# country_lumpUS           0.1685110  0.0087169   19.331  < 2e-16 ***
#   country_lumpOther       -0.0064210  0.0115362   -0.557 0.577803
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# Residual standard error: 0.5005 on 91488 degrees of freedom
# Multiple R-squared:  0.4269,	Adjusted R-squared:  0.4269
# F-statistic:  6196 on 11 and 91488 DF,  p-value: < 2.2e-16

m3 <-
  lm(log(price) ~ points + .sig(country_lump, c("Italy", "France")), data = wine_train)
summary(m3)

# Call:
#   lm(formula = log(price) ~ points + .sig(country_lump, c("Italy",
#                                                           "France")), data = wine_train)
#
# Residuals:
#   Min      1Q  Median      3Q     Max
# -1.6614 -0.3698 -0.0559  0.3123  4.8640
#
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)
# (Intercept)                                    -9.0338181  0.0506152 -178.48   <2e-16 ***
#   points                                          0.1389133  0.0005713  243.16   <2e-16 ***
#   .sig(country_lump, c("Italy", "France"))France  0.0471280  0.0043698   10.79   <2e-16 ***
#   .sig(country_lump, c("Italy", "France"))Italy   0.1749297  0.0054838   31.90   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# Residual standard error: 0.5122 on 91496 degrees of freedom
# Multiple R-squared:  0.3997,	Adjusted R-squared:  0.3997
# F-statistic: 2.031e+04 on 3 and 91496 DF,  p-value: < 2.2e-16

m4 <-
  lm(log(price) ~ points + .sig(country_lump, c("Italy", "France")) + .sig(variety_lump, c("Pinot Noir")),
     data = wine_train)
summary(m4)

# Call:
#   lm(formula = log(price) ~ points + .sig(country_lump, c("Italy",
#                                                           "France")) + .sig(variety_lump, c("Pinot Noir")), data = wine_train)
#
# Residuals:
#   Min      1Q  Median      3Q     Max
# -1.6102 -0.3585 -0.0535  0.2963  4.8896
#
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)
# (Intercept)                                    -8.6949389  0.0500343 -173.78   <2e-16 ***
#   points                                          0.1346265  0.0005657  237.98   <2e-16 ***
#   .sig(country_lump, c("Italy", "France"))France  0.0599375  0.0042962   13.95   <2e-16 ***
#   .sig(country_lump, c("Italy", "France"))Italy   0.2170581  0.0054324   39.96   <2e-16 ***
#   .sig(variety_lump, c("Pinot Noir"))Pinot Noir   0.3220584  0.0055124   58.42   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# Residual standard error: 0.5029 on 91495 degrees of freedom
# Multiple R-squared:  0.4213,	Adjusted R-squared:  0.4213
# F-statistic: 1.665e+04 on 4 and 91495 DF,  p-value: < 2.2e-16

m5 <-
  lm(
    log(price) ~ points + .sig(country_lump, c("Italy", "France")) + .sig(variety_lump, c("Pinot Noir")) + fct_relevel(color_lump, "Other"),
    data = wine_train
  )
summary(m5)

# Call:
#   lm(formula = log(price) ~ points + .sig(country_lump, c("Italy",
#                                                           "France")) + .sig(variety_lump, c("Pinot Noir")) + fct_relevel(color_lump,
#                                                                                                                          "Other"), data = wine_train)
#
# Residuals:
#   Min      1Q  Median      3Q     Max
# -1.6783 -0.3439 -0.0491  0.2964  4.7862
#
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)
# (Intercept)                                    -8.5370390  0.0494359 -172.69   <2e-16 ***
#   points                                          0.1325721  0.0005592  237.07   <2e-16 ***
#   .sig(country_lump, c("Italy", "France"))France  0.0889132  0.0043485   20.45   <2e-16 ***
#   .sig(country_lump, c("Italy", "France"))Italy   0.2051296  0.0053514   38.33   <2e-16 ***
#   .sig(variety_lump, c("Pinot Noir"))Pinot Noir   0.2464528  0.0056151   43.89   <2e-16 ***
#   fct_relevel(color_lump, "Other")Red             0.0972217  0.0069082   14.07   <2e-16 ***
#   fct_relevel(color_lump, "Other")White          -0.1073226  0.0070598  -15.20   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# Residual standard error: 0.495 on 91493 degrees of freedom
# Multiple R-squared:  0.4394,	Adjusted R-squared:  0.4394
# F-statistic: 1.195e+04 on 6 and 91493 DF,  p-value: < 2.2e-16


