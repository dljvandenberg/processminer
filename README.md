# Process Mining Exploration Tool


## Project description

The goal of this project is to build a simple web-based process mining tool for exploration (and potentially prediction).


## Requirements

The web interface should allow the user to:

- Upload an eventlog
- Mark relevant fields (case_id, timestamp, activity, additional features)
- Select desired views
- Visualize: process flows, throughput times, used resources, timelines, etc.
- Download an analysis report

Potentially, we could consider adding prediction models for throughput times, success/failure per case, etc.


## Design

We intend to use the following stack:

- R code
- Dashboard in RShiny
- Containerized using Docker image
- Potentially deployed on Azure



## Maintainer

This git repository is maintained by Dennis van den Berg (https://github.com/dljvandenberg).
