# /*****************************************************************************
# Program: 				!PHmain.R
# Purpose: 				Main file for the Population and Housing Chapter. 
# 						    The main file will call other do files that will produce the PH indicators and produce tables.
# Data outputs:		Coded variables and table output on screen and in excel tables.  
# Author: 				Shireen Assaf
# Date last modified:		July 25, 2023 by Shireen Assaf to add srvyr library needed for PH.SCHOL file
# ******************************************************************************
rm(list = ls(all = TRUE))

# libraries needed
library(tidyverse)  # most variable creation here uses tidyverse 
library(tidyselect) # used to select variables in FP_EVENTS.R
library(haven)      # used for Haven labeled DHS variables
library(labelled)   # used for Haven labeled variable creation
library(expss)    # for creating tables with Haven labeled data
library(openxlsx)     # for exporting to excel
library(naniar)   # to use replace_with_na function
library(here)       # to get R project path
library(sjlabelled) # to set variables label
library(survey)  # to calculate weighted ratio for GAR
library(srvyr)

#path for R project
here()

# path for this chapter. This is also where the data is stored
chap <- "Chap2"

# select your survey
# PR Files
PRdatafile <- "NGPR7BFL.DTA"

# BR Files

# ****************************

# open datasets
PRdata <-  read_dta(here(chap,PRdatafile))

# ****************************

source(here(paste0(chap,"/PH_HOUS.R")))
# Purpose:	Code housing indicators such as house material, assets, cooking fuel and place, and smoking in the home

# **********************************
# PR file variables (use for indicators where the population is the unit of measurement)

source(here(paste0(chap,"/PH_GINI.R")))
# Purpose:	Code to produce Gini index table. 
# Note: This code will collapse the data and produce the table Table_gini.xls
