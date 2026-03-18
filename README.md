
# MAP-ini-sus

## Overview

This repository contains analyses of simulated *Mycobacterium avium* subsp. *paratuberculosis* (MAP) infection data to evaluate how testing frequency, phenotype definition, and age window affect the estimation of genetic parameters and breeding values for host initial susceptibility.

The project uses simulated longitudinal infection data to compare alternative data collection strategies and phenotype constructions, and to generate figures used in the report.

## Main components

### Testing frequency comparison
Analyses are available for:
- weekly
- monthly
- seasonal
- annual testing schemes

### Phenotype construction
Supports both:
- lifetime infection phenotypes
- calf-life infection phenotypes within a selected age window

### Age-window settings
The upper limit of the age window can be modified in the `binary_wk` step.

### Figure generation
- **R** is used for the main analyses and figure generation
- selected figure processing and assembly are done in **Python**

## Key settings

### Age window
The age window is controlled in the `binary_wk` step:

```r
time_end = pmin(first(birthweek) + 20, last(test_time), na.rm = TRUE)
```

The value +20 determines the upper limit of the age window.
Change this value if you want to use a different age window.

### Phenotype definition
#### Lifetime phenotype

```r
result = as.integer(any(state == 1, na.rm = TRUE))
```

This definition codes an animal as positive if it was infected at any time during the observed period.
#### Calf-life phenotype

```r
result = as.integer(
  if (any(age <= 52, na.rm = TRUE)) {
    any(state == 1 & age <= 52, na.rm = TRUE)
  } else {
    any(state == 1, na.rm = TRUE)
  }
)
```

This definition codes an animal as positive if infection occurred during calf life, within the selected age window.

## Scripts
Main analysis scripts include:

weekly

monthly

seasonal

annually_test_model_7&8

true dynamics

The true dynamics script generates the underlying infection dynamics used as figure backgrounds in the report.

### Python figure scripts
Additional figure-related scripts are located in code/python.

#### confusion plot

Used to generate Figure 3 in the report.
The input data are first summarised in R and then plotted in Python.

#### tu and appendix

Used to combine R-generated figures into the final layout used in the thesis/report.


## Software

This project uses:

R for data processing, phenotype construction, statistical analyses, and most figures

Python for selected figure preparation and figure assembly

## Contact

Yuqi Hu
Wageningen University & Research
yuqi.hu@wur.nl
