library(dplyr)
source("R/functions.R")

# Data ophalen
alle_rondes <- collect_played_rounds()

# CSV wegschrijven
dir.create("data", showWarnings = FALSE)
write.csv(alle_rondes, "data/alle_rondes.csv", row.names = FALSE)

cat("scraper finished. Wrote", nrow(alle_rondes), "rows to data/alle_rondes.csv\n")
