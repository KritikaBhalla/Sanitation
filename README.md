# Sanitation
## Project Overview

This project analyzes sanitation outcomes in India using unit-level data from the
76th round of the National Sample Survey (NSS) on Drinking Water, Sanitation, Hygiene,
and Housing Conditions (WASH).

The analysis focuses on two key sanitation indicators:
1. Presence of stagnant water around the household
2. Presence of human faeces around the household

## Data and Methodology

The unit-level NSS data are provided in fixed-width TXT format and are processed
using R. The workflow involves data extraction, cleaning, and variable construction,
followed by the application of survey weights to account for the complex survey design.

The analysis estimates the prevalence of sanitation-related issues across:
- Indian states
- Rural and urban sectors
- Socio-religious groups
- Economic strata

Visualizations are used to present patterns and comparisons clearly.

## Tools and Packages

The analysis is conducted in R, primarily using:
- tidyverse
- vroom
- srvyr
- other supporting packages for data handling and visualization

## Data Availability

The raw NSS unit-level TXT data and official documentation are not included in this
repository due to data access and redistribution restrictions. The scripts are written
to reproduce the analysis given authorized access to the NSS data.
