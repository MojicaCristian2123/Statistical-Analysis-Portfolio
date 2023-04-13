# Load in data
dtiMedData <- read.csv("dtiMedData.csv", header = T)
head(dtiMedData)

# Regress predicted foreclosure rates onto high DTI ratios and the proportion of homeowners with less than 100% AMI.
# This model looks at all counties in the USA.

### DEFINE THE REGIONS OF THE UNITED STATES ###

# a 217 x 7 matrix, so 217 counties in the Northeast
Northeast <- dtiMedData[dtiMedData[,1]=="Connecticut" | dtiMedData[,1]== "Maine" | dtiMedData[,1]== "Massachusetts" | dtiMedData[,1]== "New Hampshire" | dtiMedData[,1]== "Rhode Island"| dtiMedData[,1]== "Vermont" | dtiMedData[,1]=="New Jersey"| dtiMedData[,1]=="New York" | dtiMedData[,1]== "Pennsylvania",]

# a 1052 x 7 matrix, so 1,052 counties in the Midwest
Midwest <- dtiMedData[dtiMedData[,1]=="Illinois"| dtiMedData[,1]=="Indiana"| dtiMedData[,1]=="Michigan"| dtiMedData[,1]=="Ohio"| dtiMedData[,1]=="Wisconsin"| dtiMedData[,1]=="Iowa"| dtiMedData[,1]=="Kansas"| dtiMedData[,1]=="Minnesota"| dtiMedData[,1]=="Missouri"| dtiMedData[,1]=="Nebraska"| dtiMedData[,1]=="North Dakota"| dtiMedData[,1]=="South Dakota",]

# a 1419 x 7 matrix, so 1,419 counties in the South
South <- dtiMedData[dtiMedData[,1]=="Delaware" | dtiMedData[,1]=="Florida"| dtiMedData[,1]=="Georgia"| dtiMedData[,1]=="Maryland"| dtiMedData[,1]=="North Carolina"| dtiMedData[,1]=="South Carolina"| dtiMedData[,1]=="Virginia"| dtiMedData[,1]=="District of Columbia" | dtiMedData[,1]=="West Virginia"| dtiMedData[,1]=="Alabama"| dtiMedData[,1]=="Kentucky"| dtiMedData[,1]=="Mississippi"| dtiMedData[,1]=="Tennessee"| dtiMedData[,1]=="Arkansas"| dtiMedData[,1]=="Louisiana"| dtiMedData[,1]=="Oklahoma"| dtiMedData[,1]=="Texas",]

# a 445 x 7, so 445 counties in the West
West <- dtiMedData[dtiMedData[,1]=="Arizona"| dtiMedData[,1]=="Colorado"| dtiMedData[,1]=="Idaho"| dtiMedData[,1]=="Montana"| dtiMedData[,1]=="Nevada"| dtiMedData[,1]=="New Mexico"| dtiMedData[,1]=="Utah"| dtiMedData[,1]=="Wyoming"| dtiMedData[,1]=="Alaska"| dtiMedData[,1]=="California"| dtiMedData[,1]=="Hawaii"| dtiMedData[,1]=="Oregon"| dtiMedData[,1]=="Washington",]

### END DEFINITION ###




### OVERALL USA ANALYSIS ###
# Start by looking at incomplete cases in the data.
dtiMedData[!complete.cases(dtiMedData),] # 302 incomplete cases


# Impute 'high' missing values with medians.
medHighUSA <- median(dtiMedData$high, na.rm = TRUE) # 1.58
dtiMedData[is.na(dtiMedData$high), "high"] <- medHighUSA
mod_dtiMedData <- dtiMedData[,-c(4)] # make a copy of the data with modifications, drop low column since focus is on high column
head(mod_dtiMedData)

# Impute 'pred_fc_rate' missing values with medians.
medFcUSA <- median(mod_dtiMedData$pred_fc_rate, na.rm = TRUE) # 0.012
dtiMedData[is.na(mod_dtiMedData$pred_fc_rate), "pred_fc_rate"] <- medFcUSA
head(mod_dtiMedData)


# Now run the regression.
# Scale prop as follows (scale(num/den)), not as scale(num)/scale(den).
propUSA <- scale(mod_dtiMedData$Homeowners_less_100_AMI / mod_dtiMedData$total_homeowners)

fcFit <- lm(as.numeric(mod_dtiMedData$pred_fc_rate) ~ propUSA + scale(as.numeric(mod_dtiMedData$high)))
summary(fcFit)
summary(logfcFit)
plot((fcFit))
pairs(mod_dtiMedData[,4:7])

hist(mod_dtiMedData$high, main = "End-2019 high debt-to-income ratios,\nall USA counties", xlab = "Debt-to-income ratio")
hist(propUSA, main = "End-2019 proportion of homeowners earning less than 100% AMI,\nall USA counties", xlab = "proportion of homeowners earning less than 100% AMI")
hist(as.numeric(mod_dtiMedData$pred_fc_rate), main = "End-2019 HAF-predicted foreclosure rates,\nall USA counties", xlab = " HAF-predicted foreclosure rate")
# Y = pred_fc_rate
# Beta0 = intercept
# Beta1 = scaled prop
# Beta2 = high


# Visualize the whole country!
library(usmap) #import the package
library(ggplot2) #use ggplot2 to add layer for visualization


colnames(dtiMedData)[which(names(dtiMedData)=="state_name")] <- "state"
head(dtiMedData)

plot_usmap(regions = "counties", data = mod_dtiMedData, values = "pred_fc_rate", color = "black") + 
  scale_fill_continuous(low = "white", high = "blue", name = "Predicted Foreclosure Rates", label = scales::comma) + 
  labs(title = "United States", subtitle = "Predicted Foreclosure Rates in the U.S.A., EOY 2019") +
  theme(legend.position = "right")

plot_usmap(regions = "counties", data = mod_dtiMedData, values = 'Homeowners_less_100_AMI', color = "black") + 
  scale_fill_continuous(low = "white", high = "green", name = "Number of Homeowners Earning < 100% AMI", label = scales::comma) + 
  labs(title = "United States", subtitle = "Number of Homeowners Earning Below 100% Area Median Income in the U.S.A., EOY 2019") +
  theme(legend.position = "right")

plot_usmap(regions = "counties", data = mod_dtiMedData, values = 'high', color = "black") + 
  scale_fill_continuous(low = "white", high = "red", name = "High Debt-to-Income Ratio", label = scales::comma) + 
  labs(title = "United States", subtitle = "High Debt-to-Income Ratio  in the U.S.A., EOY 2019") +
  theme(legend.position = "right")

# NORTHEAST ANALYSIS
# Start by looking at incomplete cases in the data.
Northeast[!complete.cases(Northeast),] # 9 incomplete cases


# Impute 'high' missing values with medians.
medHighNE <- median(Northeast$high, na.rm = TRUE) # 1.58
Northeast[is.na(Northeast$high), "high"] <- medHighUSA
mod_Northeast <- Northeast[,-c(4)] # make a copy of the data with modifications, drop low column since focus is on high column
head(mod_Northeast)

# Impute 'pred_fc_rate' missing values with medians.
medFcNE <- median(Northeast$pred_fc_rate, na.rm = TRUE) # 0.012
dtiMedData[is.na(Northeast$pred_fc_rate), "pred_fc_rate"] <- medFcNE
head(mod_Northeast)


# Now run the regression.
# Scale prop as follows (scale(num/den)), not as scale(num)/scale(den).
propNE <- scale(as.numeric(mod_Northeast$Homeowners_less_100_AMI) / as.numeric(mod_Northeast$total_homeowners))

fcFitNE <- lm(as.numeric(mod_Northeast$pred_fc_rate) ~ propNE + scale(as.numeric(mod_Northeast$high)))
summary(fcFitNE)



hist(mod_Northeast$high, main = "End-2019 high debt-to-income ratios,\nNortheast USA counties", xlab = "Debt-to-income ratio")
hist(propNE, main = "End-2019 proportion of homeowners earning less than 100% AMI,\nNortheast USA counties", xlab = "proportion of homeowners earning less than 100% AMI")
hist(mod_Northeast$pred_fc_rate, main = "End-2019 HAF-predicted foreclosure rates,\nNortheast USA counties", xlab = " HAF-predicted foreclosure rate")



# MIDWEST ANALYSIS
# Start by looking at incomplete cases in the data.
Midwest[!complete.cases(Midwest),] # 83 incomplete cases


# Impute 'high' missing values with medians.
medHighMW <- median(Midwest$high, na.rm = TRUE) # 1.58
Midwest[is.na(Midwest$high), "high"] <- medHighMW
mod_Midwest <- Midwest[,-c(4)] # make a copy of the data with modifications, drop low column since focus is on high column
head(mod_Midwest)

# Impute 'pred_fc_rate' missing values with medians.
medFcMW <- median(Midwest$pred_fc_rate, na.rm = TRUE)
dtiMedData[is.na(Midwest$pred_fc_rate), "pred_fc_rate"] <- medFcMW
head(mod_Midwest)


# Now run the regression.
# Scale prop as follows (scale(num/den)), not as scale(num)/scale(den).
propMW <- scale(as.numeric(mod_Midwest$Homeowners_less_100_AMI) / as.numeric(mod_Midwest$total_homeowners))

fcFitMW <- lm(as.numeric(mod_Midwest$pred_fc_rate) ~ propMW + scale(as.numeric(mod_Midwest$high)))
summary(fcFitMW)



hist(mod_Midwest$high, main = "End-2019 high debt-to-income ratios,\nMidwest USA counties", xlab = "Debt-to-income ratio")
hist(propMW, main = "End-2019 proportion of homeowners earning less than 100% AMI,\nMidwest USA counties", xlab = "proportion of homeowners earning less than 100% AMI")
hist(mod_Midwest$pred_fc_rate, main = "End-2019 HAF-predicted foreclosure rates,\nMidwest USA counties", xlab = " HAF-predicted foreclosure rate")



# SOUTH ANALYSIS
# Start by looking at incomplete cases in the data.
South[!complete.cases(South),] # 165 incomplete cases


# Impute 'high' missing values with medians.
medHighS <- median(South$high, na.rm = TRUE)
South[is.na(South$high), "high"] <- medHighS
mod_South <- South[,-c(4)] # make a copy of the data with modifications, drop low column since focus is on high column
head(mod_South)

# Impute 'pred_fc_rate' missing values with medians.
medFcS <- median(South$pred_fc_rate, na.rm = TRUE)
dtiMedData[is.na(South$pred_fc_rate), "pred_fc_rate"] <- medFcS
head(mod_South)


# Now run the regression.
# Scale prop as follows (scale(num/den)), not as scale(num)/scale(den).
propS <- scale(as.numeric(South$Homeowners_less_100_AMI) / as.numeric(South$total_homeowners))

fcFitS <- lm(as.numeric(mod_South$pred_fc_rate) ~ propS + scale(as.numeric(mod_South$high)))
summary(fcFitS)


hist(mod_South$high, main = "End-2019 high debt-to-income ratios,\nSouth USA counties", xlab = "Debt-to-income ratio")
hist(propS, main = "End-2019 proportion of homeowners earning less than 100% AMI,\nSouth USA counties", xlab = "proportion of homeowners earning less than 100% AMI")
hist(mod_South$pred_fc_rate, main = "End-2019 HAF-predicted foreclosure rates,\nSouth USA counties", xlab = " HAF-predicted foreclosure rate")




# WEST ANALYSIS
# Start by looking at incomplete cases in the data.
West[!complete.cases(West),] # 45 incomplete cases


# Impute 'high' missing values with medians.
medHighW <- median(West$high, na.rm = TRUE)
West[is.na(West$high), "high"] <- medHighW
mod_West <- West[,-c(4)] # make a copy of the data with modifications, drop low column since focus is on high column
head(mod_West)

# Impute 'pred_fc_rate' missing values with medians.
medFcW <- median(West$pred_fc_rate, na.rm = TRUE)
dtiMedData[is.na(West$pred_fc_rate), "pred_fc_rate"] <- medFcW
head(mod_West)


# Now run the regression.
# Scale prop as follows (scale(num/den)), not as scale(num)/scale(den).
propW <- scale(as.numeric(West$Homeowners_less_100_AMI) / as.numeric(West$total_homeowners))

fcFitW <- lm(as.numeric(mod_West$pred_fc_rate) ~ propW + scale(as.numeric(mod_West$high)))
summary(fcFitW)


hist(mod_West$high, main = "End-2019 high debt-to-income ratios,\nWest USA counties", xlab = "Debt-to-income ratio")
hist(propW, main = "End-2019 proportion of homeowners earning less than 100% AMI,\nWest USA counties", xlab = "proportion of homeowners earning less than 100% AMI")
hist(mod_West$pred_fc_rate, main = "End-2019 HAF-predicted foreclosure rates,\nWest USA counties", xlab = " HAF-predicted foreclosure rate")



# scaled high DTI histograms
hist(scale(as.numeric(mod_dtiMedData$high)), main = "End-2019 scaled high debt-to-income ratios,\nall USA counties", xlab = "scaled DTI ratio")



# transform the foreclosure rates? and dti?
hist(log(as.numeric(mod_West$pred_fc_rate)), main = "End-2019 ln(predicted foreclosure rate),\nWest USA counties", xlab = "Foreclosure rate")
hist(log(as.numeric(mod_dtiMedData$high)), main = "End-2019 ln(high debt-to-income ratios),\nall USA counties", xlab = "Debt-to-income ratio")



# REMEMBER: I AM PREDICTING FORECLOSURE BY MEANS OF DTI RATIO AND PROPORTION OF HOMEOWNERS WITH LESS THAN 100$ AMI

# Idea for priors.
# Let's assume that there is no change, on average, of a dependent variable for each unit increase in
# a predictor. For instance, there is no change in predicted foreclosure rate for each unit change in a county's low DTI ratio,
# or in a county's high DTI ratio, or in a county's proportion of homeowners making < 100% area median income.
# Shouldn't each homeowner be independent of another, barring the effect of the decisions of
# financial institutions such as banks or the Federal Reserve. We'll have a vector of ones for intercept,
# but then the betas for the three predictors will be 0.

# Y = XB + epsilon
# from main data, use intercepts, low, high, (hless100ami / total homeowners), pred fc rate
# (3089 x 1) = (3089 x 5)(5 X 1) + (3089 x 1)

# As for a covariance matrix, this will be a little bit harder.
# I don't want to assume much spread in the predictors; perhaps we should give homeowners the benefit of the doubt
# and assume that they don't want to take on a high debt to income ratio; it doesn't help with their chances
# for qualifying for a loan, especially conventional loans.

# Perhaps an identity matrix multiplied by the sample variance for the entire country?
# If I were observing the USA from outside of the USA, I might naively think that the
# low and high DTI ratios and predicted foreclosure rates were about the same everywhere.
# This is naive, but this research question utilizes weakly informative priors.

# Prior value for intercept
tail(sort(mod_dtiMedData$pred_fc_rate))
#[1] 0.048 0.051 0.051 0.056 0.063 0.079

head(sort(mod_dtiMedData$pred_fc_rate))
#[1] -0.002 -0.002 -0.002 -0.001 -0.001 -0.001

# (.079+(-.002))/2
# [1] 0.0385
# Try 0.0385 for an intercept prior value.

### STAN ###
library(rstanarm)
library(bayesplot)
library(ggplot2)
library(broom)


set.seed(54)

# STAN, all USA #
stan_model_USA <- stan_glm(mod_dtiMedData$pred_fc_rate ~ propUSA + scale(mod_dtiMedData$high), prior = normal(c(0,0), c(1,1)), prior_intercept = normal(0.0385, 0.01),  chains = 4, iter = 10000)
summary(stan_model_USA)
prior_summary(stan_model_USA)
# posterior_vs_prior(stan_model_USA, pars = c("propUSA", "high"), group_by_parameter = T) + theme_bw() + guides(color = F)
pp_check(stan_model_USA, "dens_overlay")



bayesplot::color_scheme_set("blue")
(trace1 <- plot(stan_model_USA, "trace"))

mcmc_dens(stan_model_USA)
summary(stan_model_USA, regex_pars = NULL, probs = NULL, digits = 8)

plot(acf(stan_model_USA[,1]))
launch_shinystan(stan_model_USA) #library(shinystan)

posterior_interval(stan_model_USA, prob = 0.95) # from rstanarm







# STAN, Northeast#
stan_model_NE <- stan_glm(mod_Northeast$pred_fc_rate ~ propNE + scale(mod_Northeast$high), 
                          prior = normal(c(0,0), c(1,1)), prior_intercept = normal(0.022, 0.01),  chains = 4, iter = 10000)
summary(stan_model_NE)
prior_summary(stan_model_NE)


bayesplot::color_scheme_set("blue")
(trace2 <- plot(stan_model_NE, "trace"))

summary(stan_model_NE, regex_pars = NULL, probs = NULL, digits = 8)
posterior_interval(stan_model_NE, prob = 0.95) # from rstanarm

mcmc_dens(stan_model_NE)

#posterior_vs_prior(stan_model_USA, pars = c("propUSA", "high"), group_by_parameter = T) + theme_bw() + guides(color = F)
pp_check(stan_model_NE, "dens_overlay")




# STAN, Midwest#
stan_model_MW <- stan_glm(mod_Midwest$pred_fc_rate ~ propMW + scale(mod_Midwest$high), 
                          prior = normal(c(0,0), c(1,1)), prior_intercept = normal(0.039, 0.01),  chains = 4, iter = 10000)
summary(stan_model_MW)
prior_summary(stan_model_MW)


bayesplot::color_scheme_set("blue")
(trace3 <- plot(stan_model_MW, "trace"))


summary(stan_model_MW, regex_pars = NULL, probs = NULL, digits = 8)
posterior_interval(stan_model_MW, prob = 0.95) # from rstanarm

mcmc_dens(stan_model_MW)

#posterior_vs_prior(stan_model_USA, pars = c("propUSA", "high"), group_by_parameter = T) + theme_bw() + guides(color = F)
pp_check(stan_model_MW, "dens_overlay")




# STAN, South#
stan_model_S <- stan_glm(mod_South$pred_fc_rate ~ propS + scale(mod_South$high), 
                          prior = normal(c(0,0), c(1,1)), prior_intercept = normal(0.0245, 0.01),  chains = 4, iter = 10000)
summary(stan_model_S)
prior_summary(stan_model_S)


bayesplot::color_scheme_set("blue")
(trace4 <- plot(stan_model_S, "trace"))


mcmc_dens(stan_model_S)
#posterior_vs_prior(stan_model_USA, pars = c("propUSA", "high"), group_by_parameter = T) + theme_bw() + guides(color = F)
pp_check(stan_model_MW, "dens_overlay")

summary(stan_model_S, regex_pars = NULL, probs = NULL, digits = 8)
posterior_interval(stan_model_S, prob = 0.95) # from rstanarm




# STAN, West#
stan_model_W <- stan_glm(mod_West$pred_fc_rate ~ propW + scale(mod_West$high), 
                         prior = normal(c(0,0), c(1,1)), prior_intercept = normal(0.027, 0.01),  chains = 4, iter = 10000)
summary(stan_model_W)
prior_summary(stan_model_W)


bayesplot::color_scheme_set("blue")
(trace5 <- plot(stan_model_W, "trace"))


mcmc_dens(stan_model_W)
#posterior_vs_prior(stan_model_USA, pars = c("propUSA", "high"), group_by_parameter = T) + theme_bw() + guides(color = F)
pp_check(stan_model_W, "dens_overlay")

summary(stan_model_W, regex_pars = NULL, probs = NULL, digits = 8)
posterior_interval(stan_model_W, prob = 0.95) # from rstanarm
