require(tidytuesdayR)
require(tidyverse)
require(lubridate)
require(plotly)
require(gghighlight)

tuesdata <- tidytuesdayR::tt_load(2021, week = 12)

find_release <- function(x) {
        len <- dim(x)[1]
        release_month <- x$year_month[1]
        for (i in 2:len) {
                if (interval(x$year_month[i],release_month) %/% months(1) == 1) {
                        release_month <- x$year_month[i]
                }
        }
        release_month
}

games <- tuesdata$games %>%
        group_by(gamename) %>%
        mutate(year_month = mdy(paste(month, "1", year)))

release_date <- games %>%
        group_by(gamename) %>%
        do(data.frame(release_month = find_release(.)))

games <- games %>%
        merge(release_date, all.x = TRUE) %>%
        mutate(months_after_release = interval(release_month, year_month) %/% months(1)))

relevant <- games %>%
        filter(months_after_release >= 11, months_after_release < 103)

first_year <- games %>%
        filter(gamename %in% relevant$gamename,
               months_after_release < 12, months_after_release >= 0) %>%
        arrange(gamename, desc(months_after_release)) %>%
        select(gamename, avg, peak, months_after_release) %>%
        pivot_wider(id_cols = gamename,
                    names_from = months_after_release,
                    values_from = c(avg, peak))

lm0 <- mean(first_year$avg_0)
lm1 <- lm(avg_1 ~ avg_0 + peak_0, first_year)
lm2 <- lm(avg_2 ~ avg_0 + peak_0 + avg_1 + peak_1, first_year)
lm3 <- lm(avg_3 ~ avg_0 + peak_0 + avg_1 + peak_1 + avg_2 + peak_2, first_year)
lm4 <- lm(avg_4 ~ avg_0 + peak_0 + avg_1 + peak_1 + avg_2 + peak_2 +
                  avg_3 + peak_3, first_year)
lm5 <- lm(avg_5 ~ avg_0 + peak_0 + avg_1 + peak_1 + avg_2 + peak_2 +
                  avg_3 + peak_3 + avg_4 + peak_4, first_year)
lm6 <- lm(avg_6 ~ avg_0 + peak_0 + avg_1 + peak_1 + avg_2 + peak_2 +
                  avg_3 + peak_3 + avg_4 + peak_4 + avg_5 + peak_5, first_year)
lm7 <- lm(avg_7 ~ avg_0 + peak_0 + avg_1 + peak_1 + avg_2 + peak_2 +
                  avg_3 + peak_3 + avg_4 + peak_4 + avg_5 + peak_5 +
                  avg_6 + peak_6, first_year)
lm8 <- lm(avg_8 ~ avg_0 + peak_0 + avg_1 + peak_1 + avg_2 + peak_2 +
                  avg_3 + peak_3 + avg_4 + peak_4 + avg_5 + peak_5 +
                  avg_6 + peak_6 + avg_7 + peak_7, first_year)
lm9 <- lm(avg_9 ~ avg_0 + peak_0 + avg_1 + peak_1 + avg_2 + peak_2 +
                  avg_3 + peak_3 + avg_4 + peak_4 + avg_5 + peak_5 +
                  avg_6 + peak_6 + avg_7 + peak_7 + avg_8 + peak_8, first_year)
lm10 <- lm(avg_10 ~ avg_0 + peak_0 + avg_1 + peak_1 + avg_2 + peak_2 +
                  avg_3 + peak_3 + avg_4 + peak_4 + avg_5 + peak_5 +
                  avg_6 + peak_6 + avg_7 + peak_7 + avg_8 + peak_8 +
                   avg_9 + peak_9, first_year)
lm11 <- lm(avg_11 ~ avg_0 + peak_0 + avg_1 + peak_1 + avg_2 + peak_2 +
                   avg_3 + peak_3 + avg_4 + peak_4 + avg_5 + peak_5 +
                   avg_6 + peak_6 + avg_7 + peak_7 + avg_8 + peak_8 +
                   avg_9 + peak_9 + avg_10 + peak_10, first_year)

predictions <- first_year %>%
        mutate(predict_0 = lm0,
               predict_1 = predict(lm1, first_year),
               predict_2 = predict(lm2, first_year),
               predict_3 = predict(lm3, first_year),
               predict_4 = predict(lm4, first_year),
               predict_5 = predict(lm5, first_year),
               predict_6 = predict(lm6, first_year),
               predict_7 = predict(lm7, first_year),
               predict_8 = predict(lm8, first_year),
               predict_9 = predict(lm9, first_year),
               predict_10 = predict(lm10, first_year),
               predict_11 = predict(lm11, first_year),
               error_0 = avg_0 - predict_0,
               error_1 = avg_1 - predict_1,
               error_2 = avg_2 - predict_2,
               error_3 = avg_3 - predict_3,
               error_4 = avg_4 - predict_4,
               error_5 = avg_5 - predict_5,
               error_6 = avg_6 - predict_6,
               error_7 = avg_7 - predict_7,
               error_8 = avg_8 - predict_8,
               error_9 = avg_9 - predict_9,
               error_10 = avg_10 - predict_10,
               error_11 = avg_11 - predict_11) %>%
        pivot_longer(
                cols = -gamename,
                names_to = c("Metric", "Month"),
                names_sep = "_",
                values_to = "Num_Players"
                ) %>%
        pivot_wider(
                id_cols = c(gamename, Month),
                names_from = Metric,
                values_from = Num_Players
        ) %>%
        mutate(Month = as.integer(Month),
               Highlight = ifelse(Month > 0 & abs(error) > 25000, TRUE, FALSE))

plot <- ggplot(predictions, aes(x = Month, y = error, color = gamename)) +
        geom_point() +
        geom_line() +
        gghighlight(Highlight)

plot
ggplotly(plot)
