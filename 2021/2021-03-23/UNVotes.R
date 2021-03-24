library(dplyr)
library(readxl)
library(countrycode)
library(tidyr)
library(forcats)
library(lubridate)
library(mice)
library(ggplot2)
library(gganimate)
library(gifski)


vlevels <- c("yes", "abstain", "no")

load("data-raw/UNVotes2021.RData")

un_votes <- completeVotes %>%
        filter(vote <= 3) %>%
        mutate(
                country = countrycode(ccode, "cown", "country.name"),
                country_code = countrycode(ccode, "cown", "iso2c"),
                # Match based on old version of data from unvotes package
                country_code = case_when(
                        country == "Czechoslovakia" ~ "CS",
                        country == "Yugoslavia" ~ "YU",
                        country == "German Democratic Republic" ~ "DD",
                        country == "Yemen People's Republic" ~ "YD",
                        TRUE ~ country_code
                ),
                country = if_else(!is.na(Countryname) & Countryname == "German Federal Republic", "Federal Republic of Germany", country)
        ) %>%
        select(rcid, country, country_code, vote) %>%
        mutate(vote = as.character(vote)) %>%
        mutate(vote = fct_recode(vote, yes = "1", abstain = "2", no = "3"))

descriptions <- completeVotes %>%
        select(session, rcid, abstain, yes, no, importantvote, date, unres, amend, para, short, descr, me, nu, di, hr, co, ec) %>%
        distinct(rcid, .keep_all = TRUE)

un_roll_calls <- descriptions %>%
        select(rcid, session, importantvote:descr) %>%
        mutate(rcid = as.integer(rcid),
               date = as.Date(date)) %>%
        arrange(rcid)

un_roll_call_issues <- descriptions %>%
        select(rcid, me:ec) %>%
        gather(short_name, value, me:ec) %>%
        mutate(rcid = as.integer(rcid),
               value = as.numeric(value)) %>%
        filter(value == 1) %>%
        select(-value) %>%
        mutate(issue = fct_recode(short_name,
                                  "Palestinian conflict" = "me",
                                  "Nuclear weapons and nuclear material" = "nu",
                                  "Colonialism" = "co",
                                  "Human rights" = "hr",
                                  "Economic development" = "ec",
                                  "Arms control and disarmament" = "di"))


votes <- un_votes %>%
        left_join(un_roll_call_issues) %>%
        left_join(un_roll_calls) %>%
        select(rcid, country, country_code, vote, short_name, issue, date) %>%
        filter(!is.na(issue))

issues <- un_roll_call_issues %>%
        left_join(un_roll_calls) %>%
        mutate(date = year(date)) %>%
        group_by(short_name, date) %>%
        summarize(count = n()) %>%
        pivot_wider(id_cols = date, names_from = short_name, values_from = count)

co_me_years <- issues %>%
        filter(co >= 10,
               me >= 10,
               !is.na(co),
               !is.na(me))

co_me <- votes %>%
        filter(short_name %in% c("co", "me")) %>%
        mutate(date = year(date)) %>%
        filter(date %in% co_me_years$date) %>%
        group_by(country_code, short_name, date) %>%
        summarize(yes_percent = sum(vote == "yes")/sum(vote %in% c("yes","no")) * 100) %>%
        pivot_wider(id_cols = c(country_code, date), names_from = short_name, values_from = yes_percent)

co_me$co[is.nan(co_me$co)] <- NA
co_me$me[is.nan(co_me$me)] <- NA

co_me <- co_me %>%
        mice() %>%
        complete() %>%
        as_tibble()

animate(
        ggplot(co_me, aes(x = co, y = me, label = country_code)) + 
        geom_label() +
        transition_states(date,
                          transition_length = 2,
                          state_length = 1) +
                ggtitle('{closest_state}'),
        renderer = gifski_renderer()
)

co_me_us_il <- votes %>%
        filter(short_name %in% c("co", "me")) %>%
        mutate(date = year(date)) %>%
        filter(date %in% co_me_years$date) %>%
        mutate(usil = ifelse(country_code == "US", "US",
                             ifelse(country_code == "IL", "IL", "NO"))) %>%
        group_by(usil, country, date) %>%
        summarize(yes_percent = sum(vote == "yes")/sum(vote %in% c("yes","no")) * 100) %>%
        pivot_wider(id_cols = c(usil, date), names_from = short_name, values_from = yes_percent)

ggplot(filter(co_me_us_il, usil %in% c("US", "IL")), aes(x = date, y = me)) +
        geom_point(data = co_me, color = "grey85") + 
        geom_point() +
        geom_smooth(se = F) +
        facet_wrap(vars(usil))
