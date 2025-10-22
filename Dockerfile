FROM rocker/verse
RUN R -e "install.packages(c('patchwork', 'plotly', 'cluster', 'tidyverse', 'ggplot2', 'patchwork', 'htmlwidgets'), repos='https://cloud.r-project.org')"
