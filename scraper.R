packages <- c("httr2","dplyr","purrr")
installed <- packages %in% rownames(installed.packages())
if(any(!installed)) install.packages(packages[!installed], repos="https://cloud.r-project.org")

library(httr2)
library(dplyr)
library(purrr)

source("R/functions.R")

# Data ophalen
alle_rondes <- collect_played_rounds()

# CSV wegschrijven
dir.create("data", showWarnings = FALSE)
write.csv(alle_rondes, "data/alle_rondes.csv", row.names = FALSE)

cat("scraper finished. Wrote", nrow(alle_rondes), "rows to data/alle_rondes.csv\n")
