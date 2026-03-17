# MAP-ini-sus
############################################################
# README
############################################################

# Project overview:
# This project analyses simulated MAP infection data under
# different testing frequencies and phenotype definitions.
# The aim is to compare estimated breeding values and
# genetic parameter estimates across models.

############################################################
# Suggested running order
############################################################

# Run the scripts in the following order:
# 1. README
# 2. Load packages
# 3. Read in data
# 4. Set parameters
# 5. Set graph layout
# 6. Run the analysis scripts for each testing frequency

############################################################
# Key setting 1: age window
############################################################

# In the binary_wk step, the age window is controlled by:
# time_end = pmin(first(birthweek) + 20, last(test_time), na.rm = TRUE)

# The value "+20" determines the upper limit of the age window.
# Change this value if you want a different age window.
# For example:
# +10  -> age window 1-10
# +20  -> age window 1-20
# +30  -> age window 1-30
# +40  -> age window 1-40
# +52  -> age window 1-52

############################################################
# Key setting 2: phenotype definition
############################################################

# Lifetime phenotype:
# result = as.integer(any(state == 1, na.rm = TRUE))

# This means the animal is coded as positive if it was ever
# infected during the observed period.

# Calf-life phenotype:
# result = as.integer(
#   if (any(age <= 52, na.rm = TRUE)) {
#     any(state == 1 & age <= 52, na.rm = TRUE)
#   } else {
#     any(state == 1, na.rm = TRUE)
#   }
# )

# This means the animal is coded as positive if it was
# infected during calf life, within the chosen age window.

############################################################
# Scripts for different testing frequencies
############################################################

# "weekly"
#   Analysis for weekly testing data

# "monthly"
#   Analysis for monthly testing data

# "seasonal"
#   Analysis for seasonal testing data

# "annually_test_model_7&8"
#   Uses the annual testing dataset for Model 7 and Model 8
#   Suggested to run earlier

############################################################
# True dynamics
############################################################

# "true dynamics" is used to generate the true infection
# dynamics, which are used as the background in the figures
# of the report.

############################################################
# Notes
############################################################

# To change the age window:
# modify the value in
# time_end = pmin(first(birthweek) + XX, last(test_time), na.rm = TRUE)

# To switch phenotype definition:
# replace the "result" line with either the lifetime phenotype
# version or the calf-life phenotype version

# Make sure the correct dataset is used for each testing
# frequency script.

############################################################
# Output
############################################################

# The scripts generate:
# - phenotype datasets under different testing frequencies
# - model results for cow-level and sire-level comparisons
# - figures for the report
# - true dynamics plots for figure backgrounds
############################################################
