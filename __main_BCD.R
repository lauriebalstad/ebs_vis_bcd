#' main file: loads and wrangles data, runs and compares model, 
#' generates sample plots from 00, 01, 02
#' author: LJB
#' date: 11/15/2023
#' need: data folder with EBS survey CSVs (see 00 file)
#' need: files 00, 01, 02, 03, 04 in home directory
#' will generate files into results folder (clean data 00, models/tables 01)
#' will generate figures into plots folder (plots, 02-04)

library(here)

# load data
source(here("~/00_load_BCD_data.R")) 
# ^ this wrangles data

# run models
source(here("~/01_run_BCD_models.R")) 
# ^ this runs all the models & produces some basic data/model plots
# will save model tables in results folder
readRDS("~/results/g2F.rds")
gam.check(g2F) # model checks in console --> DHARMa plot saved in "results" folder

# make figures
source(here("~/02_BCD_fig1.R")) # generates fig 1: data overview
source(here("~/03_BCD_fig2.R")) # generates fig 2: model fit
source(here("~/04_BCD_fig3_fig4.R")) # generates figs 3 and 4: model conditionals
# ^ this generates and saves manuscript figures in plots folder
# note conditions for figures 3 & 4 -- using BCD positive means





