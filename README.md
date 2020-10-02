# ProcessMiner


## Description

ProcessMiner is a simple web-based process mining tool for exploration (and potentially prediction).

It was created by Dennis van den Berg and uses the bupaR process mining library in R, with a UI built in R Shiny.

Its current status is: experimental


## Features

The web interface allows the user to:

- Upload an eventlog and mark relevant fields (case_id, timestamp, activity)
- Alternatively, select example eventlogs
- View the raw data
- View summary statistics (such as number of cases, events, activities, distribution of throughput times)
- Interactively generate animated process flows
- Generate timeline views of events

We will consider adding prediction models for throughput times, success/failure per case, etc in future releases.


## Maintainer

This git repository is maintained by Dennis van den Berg (https://github.com/dljvandenberg).
