#' If running the Shiny app locally you can install all the necessary packages here
#' Watch out for errors in package installation. Most can be avoided using the most up-to-date version of R (4.1.1 as of 2021-08-10).
#' Updating of current R packages on your system is recommended.

install.packages("shiny")
install.packages("DT")
install.packages("sf")
install.packages("leaflet")
install.packages("ggplot2")
install.packages("plotly")
install.packages("plyr")
install.packages("reshape2")
install.packages("tidyr")
install.packages("stringr")
install.packages("htmltools")
install.packages("shinyBS")
install.packages("shinydashboard")
install.packages("rintrojs")
install.packages("sortable")
install.packages("ncdf4")
install.packages("remotes")
install.packages("rvest")
install.packages("xml2")

remotes::install_github('yonicd/slickR') # removed from CRAN - now only on GitHub

# end
