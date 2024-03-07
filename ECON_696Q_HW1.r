##################################################
# ECON 696Q Homework 1
# William B. Brasic
# The University of Arizona
# wbrasic@arizona.edu 
# January 2024;     Last revision: 15 February 2024
###################################################


#####################
# Preliminaries
#####################


# Set working directory
setwd("C:/Users/wbras/OneDrive/Desktop/UA/Spring_2024/ECON_696Q/ECON_696Q_HW1")

# Clear environment, console, and plot pane
rm(list = ls())
cat("\014")
graphics.off()

# Load packages
pacman::p_load(dplyr, haven, texreg, data.table, plm, stargazer)

# Read in data (unbalanced panel)
df_up <- data.table(read_dta("ECON_696Q_HW1_Data.dta"))


#####################
# Question 1
#####################


# Number of missing values in each column
colSums(is.na(df_up))

# Create column of indicators for sic 357
df_up <- df_up |> 
  mutate(sic_357 = ifelse(sic3 == 357, 1, 0),
         yr = factor(yr))

# Balanced sub-panel
df_bsp <- df_up |>
  group_by(index) |> 
  filter(n_distinct(yr) == 4) |>
  ungroup() |>
  data.table()

# Firms with at least two periods
df_2p <- df_up |>
  group_by(index) |>
  filter(n_distinct(yr) >= 2) |>
  ungroup() |>
  data.table()

# Summary stats for full dataframe
stargazer(df_up |> select(-c(index, sic3, yr)),
          title = "Summary Statistics for Unbalanced Panel",
          summary.stat = c("n", "mean", "sd", "median", "min", "max"))

# Summary stats for balanced sub-panel
stargazer(df_bsp |> select(-c(index, sic3, yr)), 
          title = "Summary Statistics for Balanced Sub-Panel",
          summary.stat = c("n", "mean", "sd", "median", "min", "max"))

# Summary stats for firms with at least two years
stargazer(df_2p |> select(-c(index, sic3, yr)), 
          title = "Summary Statistics for Firms w/ At Least Two Years",
          summary.stat = c("n", "mean", "sd", "median", "min", "max"))


#####################
# Question 2
#####################


##############
# Part (i)
##############


# Total estimator (same as using lm function)
total <- plm(ldsal ~ lemp + ldnpt + ldrst + yr + sic_357 + yr * sic_357, 
             data = df_bsp,
             model = "pooling",
             index = "index")

# Data with averages over time by group
df_bsp_between <- df_bsp |>
  group_by(index) |>
  mutate(ldsal_bar = mean(ldsal),
         lemp_bar = mean(lemp),
         ldnpt_bar = mean(ldnpt),
         ldrst_bar = mean(ldrst),
         ldrnd_bar = mean(ldrnd),
         ldinv_bar = mean(ldinv)) |>
  distinct(index, .keep_all = TRUE) |>
  select(ldsal_bar, lemp_bar, ldnpt_bar, ldrst_bar, ldrnd_bar, ldinv_bar, sic_357) |>
  ungroup() |>
  data.table()

# Between estimator
between <- plm(ldsal ~ lemp + ldnpt + ldrst + yr + sic_357 + yr * sic_357,
               data = df_bsp,
               model = "between",
               index = "index")

# Between estimator "by hand" (Parameter estimates are the same as above)
between_by_hand <- lm(ldsal_bar ~ lemp_bar + ldnpt_bar + ldrst_bar + sic_357,
                      data = df_bsp_between)

# Data use for within estimator
df_bsp_within <- df_bsp |>
  group_by(index) |>
  mutate(ldsal = mean(ldsal) - ldsal,
         lemp = mean(lemp) - lemp,
         ldnpt = mean(ldnpt) - ldnpt,
         ldrst = mean(ldrst) - ldrst,
         ldrnd = mean(ldrnd) - ldrnd,
         ldinv = mean(ldinv) - ldinv) |>
  ungroup() |>
  data.table()

# Within (fixed effect) estimator "by hand" (signs are flipped; not sure why)
# Dropped sic_357 because it is fixed across time and, thus, cancels out
within_by_hand <- lm(ldsal ~ lemp + ldnpt + ldrst + yr + yr * sic_357,
                      data = df_bsp_within)

# Within (fixed effect) estimator
within <- plm(ldsal ~ lemp + ldnpt + ldrst + yr + sic_357 + yr * sic_357,
              data = df_bsp,
              model = "within",
              index = "index")

# Random effect estimator
random <- plm(ldsal ~ lemp + ldnpt + ldrst + yr + sic_357 + yr * sic_357,
              data = df_bsp,
              model = "random",
              index = "index")

# LaTeX output summary of total, between, within, and random effect estimators
texreg(caption = "Models Using Balanced Sub-Panel",
       caption.above = TRUE,
       list(total, between, within, random),
       custom.model.names = c("Total", "Between", "Within", "Random"),
       custom.coef.names = c("Intercept", "Log Employment", "Log Deflated Capital",
                             "Log Deflated R&D Capital", "1978", "1983", "1988", "SIC_357",
                             "1978 * SIC_357", "1983 * SIC_357", "1988 * SIC_357"),
       custom.note = "Dependent Variable is Log Sales 
       $^{***}p<0.001$; $^{**}p<0.01$; $^{*}p<0.05$",
       digits = 4)


##############
# Part (ii)
##############


# Hausman test (null is random effects while alternative is fixed effects)
phtest(within, random)

  
#####################
# Question 3
#####################


# Create dataframe with variables five year differenced
df_bsp_d1 <- df_bsp |>
  group_by(index) |>
  mutate(d1_ldsal = ldsal - dplyr::lag(ldsal, 1),
         d1_lemp =  lemp  - dplyr::lag(lemp,  1),
         d1_ldnpt = ldnpt - dplyr::lag(ldnpt, 1),
         d1_ldrst = ldrst - dplyr::lag(ldrst, 1),
         d1_ldrnd = ldrnd - dplyr::lag(ldrnd, 1),
         d1_ldinv = ldinv - dplyr::lag(ldinv, 1)) |>
  select(index, sic3, yr, ldsal = d1_ldsal, lemp = d1_lemp, ldnpt = d1_ldnpt, ldrst = d1_ldrst,
         ldinv = d1_ldinv, sic_357) |>
  filter(yr != 73) |>
  ungroup() |>
  data.table()

# Create dataframe with variables ten year differenced
df_bsp_d2 <- df_bsp |>
  group_by(index) |>
  mutate(d2_ldsal = ldsal - dplyr::lag(ldsal, 2),
         d2_lemp =  lemp  - dplyr::lag(lemp,  2),
         d2_ldnpt = ldnpt - dplyr::lag(ldnpt, 2),
         d2_ldrst = ldrst - dplyr::lag(ldrst, 2),
         d2_ldrnd = ldrnd - dplyr::lag(ldrnd, 2),
         d2_ldinv = ldinv - dplyr::lag(ldinv, 2)) |>
  select(index, sic3, yr, ldsal = d2_ldsal, lemp = d2_lemp, ldnpt = d2_ldnpt, ldrst = d2_ldrst,
         ldinv = d2_ldinv, sic_357) |>
  filter(!(yr %in% c(73,78))) |>
  ungroup() |>
  data.table()

# Create dataframe with variables fifteen year differenced
df_bsp_d3 <- df_bsp |>
  group_by(index) |>
  mutate(d3_ldsal = ldsal - dplyr::lag(ldsal, 3),
         d3_lemp  = lemp  - dplyr::lag(lemp,  3),
         d3_ldnpt = ldnpt - dplyr::lag(ldnpt, 3),
         d3_ldrst = ldrst - dplyr::lag(ldrst, 3),
         d3_ldrnd = ldrnd - dplyr::lag(ldrnd, 3),
         d3_ldinv = ldinv - dplyr::lag(ldinv, 3)) |>
  select(index, sic3, yr, ldsal = d3_ldsal, lemp = d3_lemp, ldnpt = d3_ldnpt, ldrst = d3_ldrst,
         ldinv = d3_ldinv, sic_357) |>
  filter(!(yr %in% c(73, 78, 83))) |>
  ungroup() |>
  data.table()

# Regression using five year differences
d1_reg <- plm(ldsal ~ lemp + ldnpt + ldrst + yr + sic_357 + yr * sic_357,
              data = df_bsp_d1,
              model = "pooling",
              index = "index")

# Regression using ten year differences
d2_reg <- plm(ldsal ~ lemp + ldnpt + ldrst + yr + sic_357 + yr * sic_357,
              data = df_bsp_d2,
              model = "pooling",
              index = "index")

# Regression using fifteen year differences
d3_reg <- plm(ldsal ~ lemp + ldnpt + ldrst + sic_357,
              data = df_bsp_d3,
              model = "pooling",
              index = "index")

# LaTeX output of models in one table
texreg(caption = "Differenced Models Using Balanced Sub-Panel",
       caption.above = TRUE,
       list(d1_reg, d2_reg, d3_reg),
       custom.model.names = c("Five Year", "Ten Year", "Fifteen Year"),
       custom.coef.names = c("Intercept", "Log Employment", "Log Deflated Capital",
                             "Log Deflated R&D Capital", "1983", "1988", "SIC_357",
                             "1983 * SIC_357", "1988 * SIC_357"),
       custom.note = "Dependent Variable is Log Sales 
       $^{***}p<0.001$; $^{**}p<0.01$; $^{*}p<0.05$",
       digits = 4)


#####################
# Question 4
#####################


##############
# Part (i)
##############


# Create dataframe with variables five year differenced using unbalanced panel
df_up_d1 <- df_up |>
  group_by(index) |>
  mutate(d1_ldsal = ldsal - dplyr::lag(ldsal, 1),
         d1_lemp = lemp  -  dplyr::lag(lemp,  1),
         d1_ldnpt = ldnpt - dplyr::lag(ldnpt, 1),
         d1_ldrst = ldrst - dplyr::lag(ldrst, 1),
         d1_ldrnd = ldrnd - dplyr::lag(ldrnd, 1),
         d1_ldinv = ldinv - dplyr::lag(ldinv, 1)) |>
  select(index, sic3, yr, ldsal = d1_ldsal, lemp = d1_lemp, ldnpt = d1_ldnpt, 
         ldrst = d1_ldrst, ldinv = d1_ldinv, sic_357) |>
  na.omit() |>
  ungroup() |>
  data.table()

# Total estimator using unbalanced panel
total_up <- plm(ldsal ~ lemp + ldnpt + ldrst + yr + sic_357 + yr * sic_357,
                data = df_up,
                model = "pooling",
                index = "index")

# Regression using five year differences from unbalanced panel
d1_regression_up <- plm(ldsal ~ lemp + ldnpt + ldrst + yr + sic_357 + yr * sic_357,
                        data = df_up_d1,
                        model = "pooling",
                        index = "index")

# Regression using firms only present in at least two periods
two_period_regression <- plm(ldsal ~ lemp + ldnpt + ldrst + yr + sic_357 + yr * sic_357,
                             data = df_2p,
                             model = "pooling",
                             index = "index")

# LaTeX output of models in one table
texreg(caption = "Models Without Using Balanced Sub-Panel",
       caption.above = TRUE,
       list(total_up, two_period_regression, d1_regression_up),
       custom.model.names = c("Total, Unbalanced", 
                              "Total, Two Periods",
                              "Differenced, Five Year"),
       custom.coef.names = c("Intercept", "Log Employment", "Log Deflated Capital",
                             "Log Deflated R&D Capital", "1978", "1983", "1988", "SIC_357",
                            "1978 * SIC_357", "1983 * SIC_357", "1988 * SIC_357"),
       custom.note = "Dependent Variable is Log Sales 
       $^{***}p<0.001$; $^{**}p<0.01$; $^{*}p<0.05$",
       digits = 4)


##############
# Part (ii)
##############


# New dataframe where exists = 1 if yr in the group is not max, i.e.,
# the firm existed in the next time period
# which.max gets the row number in the group that has the maximum yr
# We drop yr 88 b/c we don't know if the firm existed beyond yr 88
df_exists <- df_up |>
  group_by(index) |>
  mutate(exists = ifelse(row_number() != which.max(as.numeric(yr)), 
                         1, 0)) |>
  filter(yr != 88) |>
  ungroup() |>
  data.table()

# Probit regression
exists_probit <- glm(exists ~ ldnpt + ldrst + ldinv,
                     family = binomial(link = "probit"),
                     data = df_exists)

# Inverse mills ratio (sampleSelection package fn invMillsRatio verifies this)
df_exists$imr <- dnorm(predict(exists_probit, 
                               type = "link")) / pnorm(predict(exists_probit, 
                                                               type = "link"))

# Join differenced unbalanced panel data with exists data
df_d1_exists <- left_join(df_up_d1, 
                          df_exists |> select(index, yr, exists, imr), 
                          by = c("index", "yr")) |> na.omit()

# Join data of firms w/ >= two periods with with exists data
df_2p_exists <- left_join(df_2p, 
                          df_exists |> select(index, yr, exists, imr), 
                          by = c("index", "yr")) |> na.omit()

# Five year differenced regression with Heckman correction
d1_exists <- plm(ldsal ~ lemp + ldnpt + ldrst + imr + yr + sic_357 + yr * sic_357,
                 data = df_d1_exists,
                 model = "pooling",
                 index = "index")

# Regression using firms only present in at least two periods
two_period_exists <- plm(ldsal ~ lemp + ldnpt + ldrst + imr + yr + sic_357 + yr * sic_357,
                         data = df_2p_exists,
                         model = "pooling",
                         index = "index")

# LaTeX output of models in one table
texreg(caption = "Probit Model and Models Using IMR",
       caption.above = TRUE,
       list(exists_probit, two_period_exists, d1_exists),
       custom.model.names = c("Probit, Unbalanced", 
                              "Total, Two Periods",
                              "Differenced, Five Year"),
       custom.coef.names = c("Intercept", "Log Deflated Capital",
                             "Log Deflated R&D Capital", "Log Investment",
                             "Log Employment", "IMR", "1983", "SIC_357", 
                             "1983 * SIC_357", "1978", "1978 * SIC_357"),
       custom.note = "$^{***}p<0.001$; $^{**}p<0.01$; $^{*}p<0.05$",
       digits = 4)


#####################
# Question 5
#####################


##############
# Part (i)
##############


# Regression using firms only present in at least two periods
op_1 <- plm(ldsal ~ lemp + ldnpt + I(ldnpt^2) + ldrst + I(ldrst^2) + ldinv + 
              I(ldinv^2) + yr + sic_357 + yr * sic_357,
            data = df_up,
            model = "pooling",
            index = "index")


##############
# Part (ii)
##############


# Create phi_hat
df_up_phi_hat <- df_up |> 
  group_by(index) |>
  mutate(phi_hat = ldsal - (op_1$coefficients["lemp"] * lemp +
                              op_1$coefficients["yr78"] * ifelse(yr == 78, 1, 0) +
                              op_1$coefficients["yr83"] * ifelse(yr == 83, 1, 0) +
                              op_1$coefficients["yr88"] * ifelse(yr == 88, 1, 0) +
                              op_1$coefficients["sic_357"] * sic_357 +
                              op_1$coefficients["yr78:sic_357"] * ifelse(yr == 78, 1, 0) * sic_357 +
                              op_1$coefficients["yr83:sic_357"] * ifelse(yr == 83, 1, 0) * sic_357 +
                              op_1$coefficients["yr88:sic_357"] * ifelse(yr == 88, 1, 0) * sic_357),
         lag_phi_hat = dplyr::lag(phi_hat, 1),
         lag_ldnpt = dplyr::lag(ldnpt, 1),
         lag_ldrst = dplyr::lag(ldrst, 1)) |>
  ungroup() |>
  na.omit() |>
  data.table()

# Estimation using NLLS (seems robust to differing initial values)
nls_1 <- nls(phi_hat ~ beta_0 + beta_2 * ldnpt + beta_3 * ldrst + 
               beta_h1 * (lag_phi_hat - beta_2 * ldnpt - beta_3 * ldrst) + 
               beta_h2 * (lag_phi_hat - beta_2 * ldnpt - beta_3 * ldrst)^2,
             data = df_up_phi_hat,
             start = list(beta_0 = 1, beta_2 = 1, beta_3 = 1,
                          beta_h1 = 1, beta_h2 = 1))


##############
# Part (iii)
##############


# Probability firm exists in t+1
P <- predict(exists_probit, type = "response") |> data.table() |> rename(P = "V1")

# Add P to df_exists and then create phi_hat again
df_exists <- df_exists |> 
  cbind(P) |>
  group_by(index) |>
  mutate(phi_hat = ldsal - (op_1$coefficients["lemp"] * lemp +
                              op_1$coefficients["yr78"] * ifelse(yr == 78, 1, 0) +
                              op_1$coefficients["yr83"] * ifelse(yr == 83, 1, 0) +
                              op_1$coefficients["yr88"] * ifelse(yr == 88, 1, 0) +
                              op_1$coefficients["sic_357"] * sic_357 +
                              op_1$coefficients["yr78:sic_357"] * ifelse(yr == 78, 1, 0) * sic_357 +
                              op_1$coefficients["yr83:sic_357"] * ifelse(yr == 83, 1, 0) * sic_357 +
                              op_1$coefficients["yr88:sic_357"] * ifelse(yr == 88, 1, 0) * sic_357),
         lag_phi_hat = dplyr::lag(phi_hat, 1),
         lag_ldnpt = dplyr::lag(ldnpt, 1),
         lag_ldrst = dplyr::lag(ldrst, 1)) |>
  ungroup() |>
  na.omit() |>
  data.table()
  

# Estimation using NLLS using P and P^2
nls_2 <- nls(phi_hat ~ beta_0 + beta_2 * ldnpt + beta_3 * ldrst + 
               beta_P1 * P + beta_P2 * I(P^2),
             data = df_exists,
             start = list(beta_0 = 1, beta_2 = 1, beta_3 = 1,
                          beta_P1 = 1, beta_P2 = 1))


##############
# Part (iv)
##############


# Estimation using NLLS using second order polynomial in P and h
nls_3 <- nls(phi_hat ~ beta_0 + beta_2 * ldnpt + beta_3 * ldrst + 
               beta_P1 * P + beta_P2 * I(P^2) + 
               beta_h1 * (lag_phi_hat - beta_2 * ldnpt - beta_3 * ldrst) + 
               beta_h2 * (lag_phi_hat - beta_2 * ldnpt - beta_3 * ldrst)^2,
             data = df_exists,
             start = list(beta_0 = 1, beta_2 = 1, beta_3 = 1,
                          beta_P1 = 1, beta_P2 = 1, beta_h1 = 1, beta_h2 = 1))


# LaTeX output for Question 5
texreg(caption = "OP Analog Estimator",
       caption.above = TRUE,
       list(op_1, nls_1, nls_2, nls_3),
       custom.model.names = c("OP, Step One", "OP, Step Two (NLS)", 
                              "OP, Step Two (NLS)", "OP, Step Two (NLS)"),
       custom.note = "Dependent Variable is Log Sales 
       $^{***}p<0.001$; $^{**}p<0.01$; $^{*}p<0.05$",
       digits = 4)










