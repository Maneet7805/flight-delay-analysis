# Flight Delay Analysis

Analysis of U.S. flight data (2015) to identify factors causing flight delays.  
The project includes data cleaning, validation, exploratory analysis, regression modeling, and visualizations to study how departure delays, flight distance, taxi-out times, day of the week, and airline affect arrival delays.

## Technologies
- R Programming
- Libraries: `dplyr`, `ggplot2`, `stringr`, `broom`, `readxl`
- Dataset: US flight data 2015 (`flights.csv`) and airline codes (`iata_airline_codes.csv`)

## Project Objectives & Features
1. **Data Cleaning & Pre-processing**
   - Remove duplicates and handle missing values
   - Filter only completed flights
   - Drop irrelevant columns and correct data types
   - Extract scheduled departure hour for analysis

2. **Data Validation**
   - Check for missing or unrealistic values
   - Validate departure times, distances, taxi-out times, and delays

3. **Exploratory Data Analysis & Visualization**
   - Scatter plots of departure vs arrival delays
   - Effect of flight distance on delay propagation
   - Delay patterns by day of the week
   - Average arrival delay by scheduled departure hour
   - Flight volume and congestion effects
   - Taxi-out time vs arrival delay

4. **Airline-Level Analysis**
   - Merge airline codes with flights
   - Compute descriptive statistics (mean, median delays)
   - Visualizations: bar charts, violin plots of delays by airline
   - Regression analysis to adjust for multiple factors and estimate airline effects

## How to Run
1. Install R and RStudio.
2. Install required packages if not already installed:
```R
install.packages(c("dplyr","ggplot2","stringr","broom","readxl"))
