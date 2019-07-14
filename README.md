# Analytics & Chill - Time series Analysis app
Contain code and documents related to the first project of the Analytics &amp; Chill initiative

## Analytics & Chill
This ongoing project was created by a few students of the Master in Business Analytics and the Master in Statistics at the University of Geneva, we seek to create a forum for the exchange of knowledge and the development of skills in the fields of statistics and data science, all in a fun environment.  We plan to organize our work through discussions and coding sessions to produce projects and develop new skills.


## Project: Time series Analysis app

Development of an R-Shiny application that:
- Retrieves the financial data 
- Automates some basic analysis
- Allows us to the compare forecasting methods by using the price of the asset

## Prerequisites
Install and load listed packages

```
library(shiny)
library(quantmod)
library(ggplot2)
library(shinyTime)
library(tseries)
library(dsa)
library(forecast)
library(ggfortify)
library(smooth)
library(Mcomp)
library(rugarch)
library(tseries)
source("ma_function.R")
source("ra_functions.R")
source("armagarch_functions.R")
```

## Built with
Shiny https://shiny.rstudio.com  
ggplot https://ggplot2.tidyverse.org/index.html  
forecast https://otexts.com/fpp2/  

## Authors

[**Erwan Guyomarch**](https://github.com/misurida) 
[**Victor Mitchell**](https://github.com/V-Mitch)  
[**Thibault Pierotti**](https://github.com/thibpiero)  
**Jean Sutter**  
[**Lionel Voirol**](https://github.com/lionelvoirol)  
