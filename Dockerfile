# Usage
# docker build -t processminer .
# docker run --rm -p 3838:3838 processminer

FROM rocker/shiny-verse:4.0.2

RUN R -e "install.packages('lubridate', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('magrittr', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('glue', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('DT', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('plotly', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('eventdataR', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('edeaR', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('processanimateR', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('shinycssloaders', repos='http://cran.rstudio.com/')"

COPY processminer.Rproj /srv/shiny-server/
COPY app.R /srv/shiny-server/app.R
COPY R /srv/shiny-server/R
COPY data /srv/shiny-server/data
