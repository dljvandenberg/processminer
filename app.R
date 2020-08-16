## Run Shiny app

library(eventdataR)
library(edeaR)
library(dplyr)

source("R/functions.R")


# Default data
eventlog_default <- sepsis %>%
  filter_trace_frequency(percentage = 0.2) %>%
  filter_activity(c("Return ER"), reverse = T) %>%
  # we fix the datatype of some of the attributes to allow proper rendering of the token color
  # the token size option currently only support numeric attributes
  mutate_at(c("lacticacid", "leucocytes", "crp", "age"), as.numeric) %>%
  mutate_at(c("disfuncorg", "sirscriteria2ormore", "infectionsuspected"), as.logical)

# View
process_viewer()
