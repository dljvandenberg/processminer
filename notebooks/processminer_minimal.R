# Minimal process mining workflow

# Libraries
library(eventdataR)
library(edeaR)
library(processmapR)
library(processanimateR)

# Eventlog data
events <- eventdataR::patients

# Dynamic process flow including throughput times
processanimateR::animate_process(events, type = performance(units = "hours"))

# Timeline overview
processmapR::plotly_dotted_chart(events)
