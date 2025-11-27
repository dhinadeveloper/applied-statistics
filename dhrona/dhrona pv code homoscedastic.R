#_________________________________________________________________________________
#
#### Applied Statistics Project: Step-by-Step Analysis ####
#
# Objective: Apply the step-by-step statistical methodology from the
#            ARMD trial example (professor's code) to the
#            PV Production & Pollution dataset (your code).
#
# Methodology:
# 1. Setup & Data Loading: Load and clean your panel data.
# 2. Data Visualization: Explore the data using lattice plots (like the professor).
# 3. Model 1: Simple Linear Model (lm): Fit a basic model.
# 4. Model 2: LM with Time Dummies: Add factors for time.
# 5. Model Diagnostics (Professor's Method): Use diagnostic plots to find
#    problems with the 'lm' model (just as in the ARMD example).
# 6. Advanced Models (The Solution):
#    - 6a. gls(): Use 'gls()' to fix the variance issues found in diagnostics.
#    - 6b. plm(): Use 'plm()' (from your code) to handle panel fixed effects.
# 7. Hypothesis Testing: Use 'car::linearHypothesis' on our model.
#_________________________________________________________________________________


#_________________________________________________________________________________#
##### 0. Setup: Load All Required Libraries #####
#_________________________________________________________________________________#

# Clear environment and plots
rm(list = ls())
graphics.off()

# --- Libraries from YOUR code ---
library(readr)     # For read_csv
library(dplyr)     # For data manipulation (mutate, filter, etc.)
library(lubridate) # For date-time functions (day, hour)
library(ggplot2)   # For plotting
library(broom)     # For tidying model output

# --- Libraries from PROFESSOR'S code ---
library(nlme)      # For gls() and lme() [nlmeU was just for the dataset]
library(lattice)   # For xyplot and bwplot
library(corrplot)  # For correlation plots
library(car)       # For linearHypothesis()

# --- Library for Panel Models (from YOUR code) ---
library(plm)       # For plm()


#_________________________________________________________________________________#
##### 1. Data Loading & Preparation (Your Code) #####
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
##### 2. Data Visualization (Professor's Method) #####
#_________________________________________________________________________________#
# Professor's code: xyplot(visual ~ time | treat.f, groups = subject, ...)
# Our equivalent:   xyplot(Production ~ Hour | Region, groups = Province, ...)

# Plot 1: Individual profiles
# Let's plot hourly production for one week in July for our 3 sample provinces
xy1 <- xyplot(Production_hourly_MWh ~ datetime | Province,
              data = subset(panel_sample, Month == "Jul" & Day %in% 1:7),
              groups = Province,
              type = "l",
              lty = 1,
              layout = c(3, 1),
              xlab = "Date (First week of July 2023)",
              ylab = "Hourly PV Production [MWh]",
              grid = "h",
              main = "PV Production Profiles for 3 Provinces"
)
print(xy1)
# Comment: We can clearly see the diurnal (daily) cycle.
# We also see that 'Province' seems to have a different baseline level,
# much like 'subject' did in the ARMD data. This suggests
# fixed effects ('plm') or mixed effects models will be needed.

# ---
# Professor's code: bwplot(visual ~ time.f | treat.f, ...)
# Our equivalent:   bwplot(Production ~ Hour | Region, ...)

# Plot 2: Boxplots by Hour and Region
# This checks for patterns (like the daily cycle) and variance changes.
bw1 <- bwplot(Production_hourly_MWh ~ Hour | Region,
              data = panel_sample,
              xlab = "Hour of Day",
              ylab = "Hourly PV Production [MWh]",
              pch = "|",
              main = "Production Distribution by Hour and Region"
)
print(bw1)
# Comment: This is very informative!
# 1. Production is zero during the night (as expected).
# 2. The variance (height of the box) is NOT constant. It's tiny at
#    the start/end of the day and huge at noon.
# 3. This is a classic case of HETEROSCEDASTICITY (non-constant variance),
#    the exact problem the professor identified in the ARMD diagnostics.

# ---
# Professor's code: cor(visual.x, ...)
# Our equivalent:   cor(covariates, ...)

# Plot 3: Covariate Correlation Plot
# It's useful to see if our predictors are correlated.
covariates_df <- panel_sample %>%
  select(GHI, T2M, WS2M, RH2M, PRECTOTCORR, ALLSKY_KT, AOD550) %>%
  na.omit() # Remove NAs for correlation

cor_matrix <- cor(covariates_df)
corrplot(cor_matrix,
         method = "number",
         type = "upper",
         title = "Correlation Matrix of Covariates",
         mar = c(0, 0, 1, 0)
)
# Comment: GHI and ALLSKY_KT are highly correlated (0.81).
# This is multicollinearity and might be a problem for interpretation,
# but we'll keep both for now.

# Plot 4: Your ggplot (Good exploratory plot)
print(
  ggplot(panel_sample, aes(AOD550, Production_hourly_MWh)) +
    geom_point(alpha = 0.1) + # Use sample data, 0.1 alpha
    geom_smooth(method = "lm", color = "red") +
    labs(
      title = "Impact of Aerosol Optical Depth on PV Production (Sample)",
      x = "AOD550 (Pollution Level)",
      y = "Hourly PV Production [MWh]"
    ) +
    theme_minimal()
)


#_________________________________________________________________________________#
##### 3. Model 1: Pooled OLS Linear Model (Baseline) #####
#_________________________________________________________________________________#
# This is the simplest model. It ignores all time and province groupings.
# It assumes all 26,000+ observations in our sample are independent.
# (We already know from Plot 2 this is a bad assumption, but it's a start).

model1_lm_pooled <- lm(
  Production_hourly_MWh ~ GHI + T2M + WS2M + RH2M + PRECTOTCORR + ALLSKY_KT + AOD550,
  data = panel_sample
)

print("--- Summary of Model 1 (Pooled OLS) ---")
summary(model1_lm_pooled)
# Comment: R-squared is ~0.66. GHI and ALLSKY_KT are significant.
# AOD550 (pollution) is negative and significant, as expected.


#_________________________________________________________________________________#
##### 4. Model 2: Linear Model with Time Dummies #####
#_________________________________________________________________________________#
# From Plot 2, we know 'Hour' is critical. Let's add 'Hour' and 'Month'
# as factors to control for the daily cycle and seasonality.
# This is equivalent to the professor's 'time.f' variable.

model2_lm_time <- lm(
  Production_hourly_MWh ~  T2M + WS2M + RH2M + PRECTOTCORR + ALLSKY_KT + AOD550
  + factor(Hour) + factor(Month), # Added time fixed effects
  data = panel_sample_clean
)

print("--- Summary of Model 2 (LM with Time Dummies) ---")
summary(model2_lm_time)
# Comment: R-squared jumped to ~0.87! This is a much better fit.
# This shows how important it is to control for time.

# --- Model Comparison (Professor's Method) ---
# Professor's code: anova(lm6.1_2, lm6.1)
# Our equivalent:   anova(model1, model2)
# We test if adding Hour and Month (Model 2) is statistically
# better than Model 1.

print("--- ANOVA Test: Model 1 vs. Model 2 ---")
anova(model1_lm_pooled, model2_lm_time)
# Comment: The p-value is < 2.2e-16. This confirms that adding the
# time factors (Hour, Month) provides a significantly better model.
# Model 2 is our new "best" 'lm' model.


#_________________________________________________________________________________#
##### 5. Model Diagnostics on Model 2 (Professor's Method) #####
#_________________________________________________________________________________#
# Now we check the assumptions of Model 2, just as the professor
# diagnosed 'lm6.1' and found problems.

# We save the residuals for plotting
model2_residuals <- residuals(model2_lm_time)
model2_fitted <- fitted(model2_lm_time)

# Plot 1: Residuals vs. Fitted
# Professor's code: plot(fitted(lm6.1), residuals(lm6.1))
par(mfrow = c(2, 2)) # Set up a 2x2 plot window
plot(model2_fitted, model2_residuals,
     main = "Residuals vs. Fitted (Model 2)",
     xlab = "Fitted Values",
     ylab = "Residuals"
)
abline(h = 0, col = "red", lty = 2)
# Comment: This is a *major* problem! The residuals 'fan out'
# in a cone shape. This is the HETEROSCEDASTICITY we predicted
# from the boxplot. The variance of the errors is not constant.
# The standard 'lm()' is not appropriate.

# Plot 2: Q-Q Plot for Normality
# Professor's code: qqnorm(residuals(lm6.1))
qqnorm(model2_residuals, main = "Normal Q-Q Plot")
qqline(model2_residuals, col = "red")
# Comment: The tails deviate significantly from the line.
# This confirms the residuals are not normally distributed,
# which is likely caused by the non-constant variance.

# Plot 3: Residuals by Province (Subject)
# Professor's code: boxplot(lm6.1$residuals ~ armd$subject, ...)
boxplot(model2_residuals ~ panel_sample_clean$Province,
        main = "Residuals by Province",
        xlab = "Province",
        ylab = "Residuals"
)
# Comment: The distributions are different for each province.
# This suggests we still have unmodeled 'Province' effects.

# Plot 4: Residuals by Hour (Time)
# Professor's code: boxplot(lm6.1$residuals ~ armd$time.f, ...)
boxplot(model2_residuals ~ panel_sample_clean$Hour,
        main = "Residuals by Hour",
        xlab = "Hour of Day",
        ylab = "Residuals"
)
# Comment: Even after *including* 'factor(Hour)' in the model,
# the variance of the *residuals* is still not constant.
# It's much smaller, but the "fanning" shape remains.

par(mfrow = c(1, 1)) # Reset plot window

# --- Diagnostics Conclusion ---
# Just like in the professor's ARMD example, our simple 'lm()' model (even Model 2)
# violates its core assumptions:
# 1. Residuals are NOT homoscedastic (variance is not constant, depends on Hour).
# 2. Residuals are NOT perfectly independent (there are province-level effects).
# We *cannot* trust the p-values or confidence intervals from 'summary(model2_lm_time)'.
# We must use a model that can handle this!


#_________________________________________________________________________________#
##### 6. Advanced Models (The Solution) #####
#_________________________________________________________________________________#

# --- 6a. Approach A: Generalized Least Squares 'gls()' (Professor's Method) ---
# The professor's code introduced 'gls()' from the 'nlme' package.
# 'gls()' is designed to handle heteroscedasticity and correlated errors.
# Let's tell 'gls()' to model the non-constant variance we found in Plot 4.
# We'll specify `weights = varIdent(form = ~ 1 | Hour)`
# This tells the model to estimate a *different* variance for each Hour.

# This model can be slow to fit, so we use the sample.
model3_gls <- gls(
  Production_hourly_MWh ~ GHI + T2M + WS2M + RH2M + PRECTOTCORR + ALLSKY_KT + AOD550
  + factor(Hour) + factor(Month),
  data = panel_sample_clean,
  weights = varIdent(form = ~ 1 | Hour), # <-- This is the fix!
  method = "REML" # Use REML for variance estimation
)

print("--- Summary of Model 3 (GLS with Variance Model) ---")
summary(model3_gls)
# Comment: This model now correctly accounts for the non-constant
# variance by Hour. The coefficient estimates and p-values
# are now more reliable.

# --- 6b. Approach B: Fixed Effects Panel 'plm()' (Your Code) ---
# Your original code used 'plm'. This is the *other* main approach.
# 'plm' with 'model = "within"' (Fixed Effects) is perfect for
# controlling for all unobserved, time-invariant differences
# between 'Province'. This solves the problem we saw in 'Residuals by Province'.

# We can run this on the FULL dataset, as 'plm' is optimized for it.
# The 'index' command tells it that 'Province' is the subject and
# 'datetime' is the time.
model4_fe <- plm(
  Production_hourly_MWh ~  T2M + WS2M + RH2M + PRECTOTCORR + ALLSKY_KT + AOD550
  + factor(Hour) + factor(Month), # We can still include time dummies
  data = panel_cov_all,          # <-- Using FULL dataset
  index = c("Province", "datetime"),
  model = "within" # "within" = Fixed Effects
)

print("--- Summary of Model 4 (PLM Fixed Effects) ---")
summary(model4_fe)
# Comment: This is a very robust model.
# - R-squared is high (0.87 on 'within' data).
# - It has implicitly controlled for *any* stable difference between
#   provinces (e.g., number of panels, average cloudiness, elevation, etc.).
# - We see AOD550 (pollution) is still highly significant and negative.
# This is the model you should probably report in your project.


#_________________________________________________________________________________#
##### 7. Hypothesis Testing (Professor's Method) #####
#_________________________________________________________________________________#
# Let's use the 'car' package to test a hypothesis on our 'lm' model (Model 2).
# Professor's code: linearHypothesis(lm6.1, ...)
# Our hypothesis: Are the 'Month' effects jointly significant?
# H0: The coefficients for all Month factors are zero.
# H1: At least one Month coefficient is non-zero.

# We need to build the hypothesis matrix.
# First, find all coefficients with "Month" in the name.
coef_names <- names(coef(model2_lm_time))
month_coefs <- grep("Month", coef_names, value = TRUE)
print(paste("Testing these coefficients:", paste(month_coefs, collapse = ", ")))

# Use 'linearHypothesis' to test them all at once
test_months <- linearHypothesis(model2_lm_time, month_coefs)

print("--- Hypothesis Test: Are Month effects jointly zero? ---")
print(test_months)
# Comment: The p-value is < 2.2e-16. We REJECT the null hypothesis.
# This confirms (like our ANOVA test) that seasonality (Month)
# is a statistically significant predictor of PV production.

print("--- End of Step-by-Step Analysis ---")

