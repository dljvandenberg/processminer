library(dplyr)
library(tidyr)
library(lubridate)
library(bupaR)



# Add 'time_since_start' column to eventlog
add_time_since_case_start <- function(eventlog, units = "days") {
  # Determine timestamp variable from eventlog
  timestamp_var <- sym(bupaR::timestamp(eventlog))
  
  # Add time_since_start per event, since case start
  eventlog_extended <- eventlog %>% 
    bupaR::group_by_case() %>% 
    mutate(time_since_start = difftime(!!timestamp_var, min(!!timestamp_var, na.rm = TRUE), units = units)) %>% 
    bupaR::ungroup_eventlog()
  
  return(eventlog_extended)
}
