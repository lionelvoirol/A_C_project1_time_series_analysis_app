# Analytics & Chill - Time series Analysis app
Contain code and documents related to the first project of the Analytics &amp; Chill initiative

## Analytics & Chill
Created by a few students of the Master in Business Analytics and the Master in Statistics at the University of Geneva, this group of work seeks to create a forum for the exchange of knowledge and the development of skills in the fields of statistics and data science, all in a fun environment.  We plan to organize our work through discussions and coding sessions to produce projects and develop new skills


## Project 1 - Time series Analysis app
Code a Shiny app that fetch financial data and that enables the comparison of different forecasting method on the given price of a stock/bond/indice

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
source("ma_function.R")
source("ra_functions.R")
```

## Built with
Shiny https://shiny.rstudio.com  
ggplot https://ggplot2.tidyverse.org/index.html  
forecast https://otexts.com/fpp2/  

## Authors
**Erwan Guyomarch**  
**Victor Mitchell**  
**Thibault Pierotti**  
**Jean Sutter**  
**Lionel Voirol**  
