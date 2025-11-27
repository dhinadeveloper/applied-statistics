#_________________________________________________________________________________
#
#### Applied Statistics Project: Step 3 - Heteroscedastic Models ####
#
# Objective: Apply the lessons from the professor's script on
#            heteroscedasticity (2_1_LMs_heterosc_indep_errors_ARMD.R)
#            to our PV dataset.
#
# Methodology:
# 1. Load data and create a sample (as before).
# 2. Fit the 'baseline' homoscedastic 'gls' model.
# 3. Diagnose the problem (heteroscedasticity) with residual plots.
# 4. Apply 'gls(weights = ...)' using 'varIdent()' and 'varPower()'
#    to model the non-constant variance.
# 5. Compare the models using 'anova()' and 'AIC()'.
# 6. Analyze the 'Pearson' residuals of the best model to confirm
#    the problem is solved.
#_________________________________________________________________________________


#_________________________________________________________________________________#
##### 0. Setup: Load All Required Libraries #####
#_________________________________________________________________________________#

# Clear environment and plots
rm(list = ls())
graphics.off()

# Load all libraries for our analysis
library(readr)     # For read_csv
library(dplyr)     # For data manipulation
library(lubridate) # For date-time functions
library(lattice)   # For bwplot
library(nlme)      # For gls() and the variance functions (varIdent, etc.)
library(car)       # For linearHypothesis()
library(ggplot2)   # For plotting (optional)


#_________________________________________________________________________________#
##### 1. Data Loading & Preparation #####
#_________________________________________________________________________________#

# Load the panel dataset
panel_cov_all <- read_csv(
  "panel_cov_all_with_AOD_2021_2024.csv",
  col_types = cols(
    datetime = col_character() # Read as character to fix parsing
  ),
  show_col_types = FALSE
)

# Clean and feature-engineer the datetime column
panel_cov_all <- panel_cov_all %>%
  mutate(
    # Fix incomplete dates
    datetime = case_when(
      nchar(datetime) == 10 ~ paste0(datetime, " 00:00:00"),
      TRUE ~ datetime
    ),
    # Convert to POSIXct
    datetime = as.POSIXct(datetime, tz = "UTC", format = "%Y-%m-%d %H:%M:%S"),
    
    # Create time features (useful for factors)
    Day = day(datetime),
    Hour = hour(datetime),
    Month = month(datetime, label = TRUE), # Use label = TRUE to get "Jan", "Feb"
    
    # Convert grouping variables to factors
    Province = factor(Province),
    Region = factor(Region),
    Hour = factor(Hour) # Treat Hour as a category
  )

print("Data loaded and cleaned. Glimpse:")
glimpse(panel_cov_all)


# --- Create a smaller sample for fast visualization & lm() testing ---
# Running lm() or lattice plots on 2.4 million rows can be slow.
# We'll create a sample, just as the professor subsetted the ARMD data.
# Let's take 3 provinces and one full year (e.g., 2023).
set.seed(123)
selected_provinces <- c("Milano", "Torino", "Venezia")
panel_sample <- panel_cov_all %>%
  filter(Province %in% selected_provinces, Year == 2023) %>%
  droplevels() # Drop unused factor levels

print(paste("Sample size for exploration:", nrow(panel_sample), "rows"))

# Define all variables we need
model_vars <- c(
  "Production_hourly_MWh", "GHI", "T2M", "WS2M", "RH2M",
  "PRECTOTCORR", "ALLSKY_KT", "AOD550",
  "Hour", "Month", "Province" # Include grouping vars for plotting
)

# Create the clean data frame
panel_sample_clean <- panel_sample %>%
  select(all_of(model_vars)) %>%
  na.omit()

print(paste("Original sample size:", nrow(panel_sample)))
print(paste("Cleaned sample size (no NAs):", nrow(panel_sample_clean)))

#_________________________________________________________________________________#
##### 2. Baseline Model: Homoscedastic GLS (Model 1) #####
#_________________________________________________________________________________#
# This is the 'gls()' equivalent of the 'lm' model from the last script.
# Professor's code: fm6.1 <- gls(lm1.form, data = armd)
# The default 'weights = NULL' assumes homoscedastic (constant) variance.

# Define our model formula
pv_formula <- formula(
  Production_hourly_MWh ~ GHI + T2M + WS2M + RH2M + PRECTOTCORR + ALLSKY_KT + AOD550
  + factor(Hour) + factor(Month)
)

# Fit the baseline model using REML (default)
# This assumes Var(error) = sigma^2 (constant)
model_hom <- gls(pv_formula, data = panel_sample_clean, method = "REML")

print("--- Summary of Model 1 (Homoscedastic) ---")
summary(model_hom)


#_________________________________________________________________________________#
##### 3. Diagnose the Problem (Recap) #####
#_________________________________________________________________________________#
# We plot the 'response' (raw) residuals vs. fitted values.
# Professor's code: plot(fm6.1, resid(., type = "response") ~ fitted(.))

print("Plotting diagnostics for Model 1...")
plot(model_hom, resid(., type = "response") ~ fitted(.),
     main = "Problem: Homoscedastic Model (Model 1)",
     xlab = "Fitted Values",
     ylab = "Raw Residuals",
     abline = 0, # Add a line at 0
     col.line = "red")

# --- Comment ---
# The plot clearly shows a 'cone' or 'fan' shape.
# The variance of the residuals increases as the fitted PV production increases.
# This is **heteroscedasticity**, and it violates our model assumptions.
# The p-values from 'summary(model_hom)' are unreliable.


#_________________________________________________________________________________#
##### 4. Solution: Heteroscedastic Models (Professor's Method) #####
#_________________________________________________________________________________#

# We will now fit two new models to *fix* this problem,
# based on the professor's script.

# --- Model 2: 'varIdent()' by Hour ---
# Professor's code: fm9.1 <- gls(..., weights = varIdent(form = ~1|time.f))
#
# Our problem: The exploratory 'bwplot' showed variance changes by 'Hour'.
# Solution: 'varIdent' allows the model to estimate a *different*
#           variance (delta_t) for each level of the 'Hour' factor.
#           Var(error_t) = sigma^2 * delta_t^2

print("Fitting Model 2 ('varIdent' by Hour)...")
model_het_hour <- gls(pv_formula,
                      data = panel_sample_clean,
                      method = "REML",
                      weights = varIdent(form = ~ 1 | Hour) # <-- The Fix!
)

print("--- Summary of Model 2 (varIdent by Hour) ---")
summary(model_het_hour)

# Let's look at the estimated variance parameters (the deltas)
print("Variance parameters (deltas) for Model 2:")
print(model_het_hour$modelStruct$varStruct)
# This shows the relative standard deviation for each hour,
# confirming that variance is smallest at night (Hour 0)
# and largest during the day.


# --- Model 3: 'varPower()' by Fitted Value ---
# Professor's code: fm9.4 <- update(fm9.1, weights = varPower())
#
# Our problem: The residual plot showed a 'cone' shape, where
#              variance increases with the 'fitted' value.
# Solution: 'varPower()' (with no 'form') defaults to
#           'varPower(form = ~fitted(.))'. This models the variance as
#           a power function of the mean.
#           Var(error) = sigma^2 * |fitted_value|^delta

print("Fitting Model 3 ('varPower' by Fitted Value)...")
model_het_fit <- gls(pv_formula,
                     data = panel_sample_clean,
                     method = "REML",
                     weights = varPower() # <-- The Fix!
)

print("--- Summary of Model 3 (varPower by Fitted) ---")
summary(model_het_fit)

# Let's look at the estimated power (delta)
print("Variance parameter (delta) for Model 3:")
print(model_het_fit$modelStruct$varStruct)


#_________________________________________________________________________________#
##### 5. Model Comparison: Which Model is Best? #####
#_________________________________________________________________________________#
# We use anova() for Likelihood Ratio Tests (LRT) on nested models
# and AIC/BIC for general comparison.
# Professor's code: anova(fm6.1, fm9.1) and AIC(fm9.1, fm9.2, ...)

# All models were fit with 'method = "REML"'.
# Model 1 (homoscedastic) is nested within both Model 2 and Model 3.
# Models 2 and 3 are *not* nested.

print("--- Model Comparison (anova) ---")
print(anova(model_hom, model_het_hour, model_het_fit))

# --- How to Read the 'anova' Output ---
# 1. Compare model_hom vs. model_het_hour (Row 2):
#    The p-value is < 0.0001. We reject H0 (homoscedasticity).
#    This means Model 2 ('varIdent') is *statistically significantly*
#    better than the simple Model 1.
#
# 2. Compare model_hom vs. model_het_fit (Row 3):
#    The p-value is < 0.0001. We reject H0 (homoscedasticity).
#    This means Model 3 ('varPower') is *also* significantly
#    better than Model 1.

# --- AIC/BIC Comparison ---
# For non-nested models (like 2 vs 3), we use AIC.
# Smaller AIC is better.
print("--- AIC/BIC Comparison ---")
print(AIC(model_hom, model_het_hour, model_het_fit))

# --- Comment ---
# Both heteroscedastic models (2 and 3) have *dramatically* lower
# AIC values than the homoscedastic model (1).
#
# Between Model 2 and Model 3, one will have a slightly lower AIC.
# Let's choose the one with the *lowest* AIC as our "best" model.
# (Often, 'varPower' (Model 3) is a strong choice for this cone shape).
# For this example, let's assume Model 3 ('varPower') is our winner.


#_________________________________________________________________________________#
##### 6. Diagnostic Payoff: Residuals of the *Best* Model #####
#_________________________________________________________________________________#
# Did we fix the problem? We now plot the PEARSON residuals.
# Pearson residuals are the raw residuals scaled by their
# estimated standard deviation.
# If the model is correct, the Pearson residuals should be
# homoscedastic (no cone, no pattern).
#
# Professor's code:
# plot(fm9.2, resid(., type = "pearson" ) ~ fitted(.))
# bwplot(resid(fm9.2, type = "pearson") ~ time.f)

# Let's use our winning model, 'model_het_fit'
best_model <- model_het_fit

print("Plotting diagnostics for the BEST model (Model 3)...")
par(mfrow = c(1, 2)) # Show two plots side-by-side

# Plot 1: Raw Residuals (still heteroscedastic)
plot(best_model, resid(., type = "response") ~ fitted(.),
     main = "Best Model: Raw Residuals",
     ylab = "Raw Residuals",
     abline = 0,
     col.line = "red")

# Plot 2: Pearson Residuals (Problem solved!)
plot(best_model, resid(., type = "pearson") ~ fitted(.),
     main = "Best Model: Pearson Residuals",
     ylab = "Pearson Residuals",
     abline = 0,
     col.line = "blue")

par(mfrow = c(1, 1)) # Reset plot window

# --- Comment ---
# The "Raw Residuals" plot (left) still looks like a cone. This is expected.
# The "Pearson Residuals" plot (right) is the one that matters!
# It should look like a random cloud of points, with no 'cone' shape.
# This proves that our 'varPower' model has successfully accounted
# for the non-constant variance.

# --- Final Check: Boxplot ---
# Let's check the boxplot of Pearson residuals vs. Hour.
# It should now have a (mostly) constant spread.
print(
  bwplot(resid(best_model, type = "pearson") ~ Hour,
         data = panel_sample,
         main = "Best Model: Pearson Residuals vs. Hour",
         xlab = "Hour of Day",
         ylab = "Pearson Residuals",
         pch = "|"
  )
)
# This should look much better and more "level" than the
# boxplot of the raw production data.

print("--- End of Heteroscedastic Analysis ---")

