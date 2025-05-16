#==========================================================================================#
# Research Project 1 - Surbhi & Aarthi
#==========================================================================================#

#==========================================================================================#
# Attaching Libraries
#==========================================================================================#

install.packages("foreign")  
install.packages("devtools")
install.packages("tidyverse")
install.packages("readr")
install.packages("readxl")
install.packages("haven")
install.packages("survey")

# Run these every time you re-start R:
library(foreign)
library(devtools)
library(tidyverse)
library(readr)
library(readxl)
library(haven)
library(survey)
library(MEPS)

#==========================================================================================#
# Load MEPS Dataset and save locally as .RData files for faster future loading
#==========================================================================================#

FYCD2022 = read_excel("/Users/surbhi/Desktop/OMS-I/SnAResearch/DataFilesMEPS/h243FYCD.xlsx")
save(FYCD2022, file = "/Users/surbhi/Desktop/OMS-I/SnAResearch/DataFilesMEPS/FYCD2022.Rdata")

#==========================================================================================#
# Using survey library to analyze MEPS data and ensure unbiased estimates
#==========================================================================================#

options(survey.lonely.psu='adjust')
