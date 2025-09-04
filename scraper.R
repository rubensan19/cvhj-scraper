packages <- c("httr2","dplyr","purrr")
installed <- packages %in% rownames(installed.packages())
if(any(!installed)) install.packages(packages[!installed], repos="https://cloud.r-project.org")

library(httr2)
library(dplyr)
library(purrr)

source("R/functions.R")

# Data ophalen
alle_rondes <- collect_played_rounds()

#Posities en bedragen berekenen
df_result <- alle_rondes %>%
  group_by(ronde) %>%
  mutate(
    max_points = max(punten_na_aftrek, na.rm = TRUE),
    min_points = min(punten_na_aftrek, na.rm = TRUE),
    second_min_points = sort(unique(punten_na_aftrek))[2],
    n_winnaars = sum(punten_na_aftrek == max_points),
    n_verliezers = sum(punten_na_aftrek == min_points),
    n_1_na_laatst = sum(punten_na_aftrek == second_min_points)
  ) %>%
  mutate(
    ronde_status = case_when(
      punten_na_aftrek == max_points & n_winnaars > 1 ~ "meerdere_winnaars",
      punten_na_aftrek == max_points ~ "rondewinnaar",
      punten_na_aftrek == min_points & n_verliezers > 1 ~ "meerdere_rondeverliezer",
      punten_na_aftrek == min_points ~ "rondeverliezer",
      punten_na_aftrek == second_min_points & n_1_na_laatst > 1 ~ "meerdere_1_na_laatst",
      punten_na_aftrek == second_min_points ~ "1_na_laatst",
      TRUE ~ NA_character_
    ),
    # winst
    winst = case_when(
      punten_na_aftrek == max_points ~ 7.5 / n_winnaars,
      TRUE ~ 0
    ),
    # verlies (NIET gedeeld → meer geld bij meerdere verliezers)
    verlies = case_when(
      punten_na_aftrek == min_points ~ 5,
      punten_na_aftrek == second_min_points ~ 2.5,
      TRUE ~ 0
    )
  ) %>%
  ungroup() %>%
  select(-max_points, -min_points, -second_min_points,
         -n_winnaars, -n_verliezers, -n_1_na_laatst)

# Extra geld per ronde berekenen
extra_geld <- df_result %>%
  group_by(ronde) %>%
  summarise(
    totaal_verlies = sum(verlies),
    totaal_winst = sum(winst),
    verwacht_verlies = 5 + 2.5,
    side_pot_ronde_ronde = totaal_verlies - verwacht_verlies,
    netto_rondestand = totaal_verlies - totaal_winst, # saldo die ronde
    .groups = "drop"
  ) %>%
  mutate(
    side_pot_cumulatief = cumsum(netto_rondestand)
  )

# Terugplakken in de hoofdset
alle_rondes  <- df_result %>%
  left_join(extra_geld %>% select(ronde, side_pot_ronde_ronde, side_pot_cumulatief),
            by = "ronde")

# CSV wegschrijven
dir.create("data", showWarnings = FALSE)
write.csv(alle_rondes, "data/alle_rondes.csv", row.names = FALSE)

cat("scraper finished. Wrote", nrow(alle_rondes), "rows to data/alle_rondes.csv\n")
