library(httr2)
library(dplyr)
library(purrr)

fetch_round <- function(round_nr) {
  url <- paste0(
    "https://www.coachvanhetjaar.nl/api/subleagues/get_subleague_round_ranking/28746/",
    round_nr
  )

  resp <- request(url) |> req_perform() |> resp_body_json()
  if (is.null(resp$data) || length(resp$data) == 0) return(NULL)

  df <- map_dfr(resp$data, ~{
    tibble(
      ranking_na_ronde      = .x[[1]],
      ranking_voor_ronde    = .x[[2]],
      punten_na_aftrek      = .x[[3]],
      punten_zonder_aftrek  = .x[[4]],
      totaal_aantal_punten  = .x[[5]],
      fantasycoach_id       = .x[[6]],
      coach_name            = .x[[7]],
      team_name             = .x[[8]],
      city                  = .x[[9]],
      team_value            = .x[[10]],
      active                = as.logical(.x[[11]]),
      badge_id              = ifelse(is.null(.x[[12]]), NA, .x[[12]]),
      badge_img             = ifelse(is.null(.x[[13]]), NA, .x[[13]])
    )
  })

  df$ronde <- round_nr
  df
}

collect_played_rounds <- function(max_rondes = 34) {
  out <- list()
  prev <- NULL

  for (r in seq_len(max_rondes)) {
    cur <- fetch_round(r)
    if (is.null(cur)) break

    all_zero <- all(cur$punten_na_aftrek == 0, na.rm = TRUE)

    if (!is.null(prev) && all_zero) {
      chk <- cur %>%
        select(fantasycoach_id, punten_zonder_aftrek) %>%
        inner_join(prev %>%
                     select(fantasycoach_id, prev_punten = punten_na_aftrek),
                   by = "fantasycoach_id")

      carried_over <- nrow(chk) > 0 &&
        all(chk$punten_zonder_aftrek == chk$prev_punten, na.rm = TRUE)

      if (carried_over) break
    }

    out[[length(out) + 1]] <- cur
    prev <- cur
  }

  bind_rows(out)
}
