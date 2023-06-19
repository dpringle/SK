
#install.packages('here')
#install.packages('shiny')

library('here')

# by convention give the root folder of the project through an environment variable
project_root_dir <- here::here()
if (project_root_dir == "") {project_root_dir <- "/home/rstudio"} 

######################################################################
###      SETTINGS, R LIBRARIES, USER FUNCTIONS
#######################################################################

library(tidyverse)
library(lubridate)
library(readxl)
library(knitr)
library(pryr)
library(stringi)
library(data.table)

library(shiny)
library(DT)
library(plotly)

# Plotting addon libraries
library(ggrepel)
library(grid)
library(gridExtra)
library(scales)
library(lemon)

library(testcorr)

#######################################################################
###                  PARAMETERS & VARIABLES
#######################################################################

options(datatable.integer64="numeric")

# Otherwise R uses TZ of aws instance!
Sys.setenv(TZ = "NZ")

# milli second resolution for CreateDate
op <- options(digits.secs=3)

#prevent scientific notation
options(scipen = 999)

#######################################################################
###                  DIRECTORY MANAGEMENT
#######################################################################

dir_data <- file.path(project_root_dir, "data")
dir.create(dir_data, recursive=T, showWarnings=F)

#######################################################################
###                  CHARTING
#######################################################################

theme_dan1 <- function () { 
  theme_light() %+replace% 
    theme(axis.text=element_text(size=10), #change font size of axis text
          axis.title=element_text(size=10), #change font size of axis titles
          plot.title=element_text(size=11), #change font size of plot title
          legend.text=element_text(size=10), #change font size of legend text
          strip.text=element_text(size=10), #change font size of facet strip text
          legend.title=element_blank())  #change font size of legend title
}


gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

#######################################################################
###                  DATA
#######################################################################


# babynames <- data.table(read.csv(file.path(dir_data, 'babynames.csv')))
# names(babynames)
# setnames(babynames, old = c("percent"), new = c("prop"))
# babynames[sex=="boy", sex:='M']
# babynames[sex=="girl", sex:='F']
# 
# top_trendy_names <- data.table(read.csv(file.path(dir_data, 'top_trendy_names.csv')))

