require(tidytuesdayR)
require(tidyverse)
require(tuber)
require(rvest)
require(httr)
require(emojifont)
require(extrafont)
load.emojifont("EmojiOne.ttf")

tuesdata <- tt_load(2021, week = 10)
youtube <- tuesdata$youtube

raw_data <- read_csv("https://raw.githubusercontent.com/fivethirtyeight/superbowl-ads/main/superbowl-ads.csv")

all_ids <-raw_data$youtube_url %>% 
        str_remove_all("https://www.youtube.com/watch") %>% 
        str_remove("\\?v=") %>% 
        str_subset("NA", negate = TRUE)

all_ids

api_key = "my_api_key"

get_youtube_data <- function(ids_in, query_type = "statistics"){
        
        url_in <- modify_url("https://www.googleapis.com/youtube/v3/videos", 
                             query = list(
                                     "part" = query_type,
                                     "id" = paste(ids_in, collapse=","),
                                     "key" = api_key)
        )
        out_content <- content(GET(url_in), as = "parsed", type = "application/json")
        
        if(query_type == "statistics"){
                out_content$items %>% 
                        enframe() %>% 
                        unnest_wider(value) %>% 
                        unnest_wider(statistics) %>% 
                        janitor::clean_names()
                
        }
}

get_youtube_details <- function(ids_in){
        
        url_in <- modify_url("https://www.googleapis.com/youtube/v3/videos", 
                             query = list(
                                     "part" = "snippet",
                                     "id" = paste(ids_in, collapse=","),
                                     "key" = api_key)
        )
        out_content <- content(GET(url_in), as = "parsed", type = "application/json")
        
        out_content$items %>% 
                enframe() %>% 
                unnest_wider(value) %>% 
                unnest_wider(snippet) %>% 
                janitor::clean_names()
}

all_vid_stats <- list(
        all_ids[1:50],
        all_ids[51:100],
        all_ids[101:150],
        all_ids[151:200],
        all_ids[200:length(all_ids)]
) %>% 
        map_dfr(get_youtube_data)

all_vid_details <- list(
        all_ids[1:50],
        all_ids[51:100],
        all_ids[101:150],
        all_ids[151:200],
        all_ids[200:length(all_ids)]
) %>% 
        map_dfr(get_youtube_details)

clean_details <- all_vid_details %>% 
        hoist(thumbnails, 
              thumbnail = list("standard", "url")) %>% 
        select(id, published_at, title:category_id, -tags, -thumbnails)

combo_vid <- all_vid_stats %>% 
        left_join(clean_details) %>% 
        select(-name)

all_youtube <- raw_data %>% 
        mutate(id = str_remove(youtube_url, "https://www.youtube.com/watch") %>% 
                       str_remove("\\?v=")) %>% 
        left_join(combo_vid) %>% 
        mutate(across(view_count:comment_count, as.integer)) 

write_csv(all_youtube, "R/2021/2021-03-02/youtube.csv")

all_youtube %>% 
        glimpse()

all_youtube <- all_youtube %>%
        mutate(ratio = like_count/dislike_count)

yt_summary <- all_youtube %>%
        group_by(year) %>%
        summarize(across(funny:use_sex, sum)) %>%
        pivot_longer(-year,
                     names_to = "cat",
                     values_to = "num") %>%
        group_by(year) %>%
        arrange(year, desc(num), cat) %>%
        mutate(ranking = row_number())

yt_emoji <- yt_summary %>%
        mutate(cat = 
                       recode(cat, funny = "tears",
                              show_product_quickly = "dash",
                              animals = "cat",
                              danger = "warning",
                              use_sex = "peach",
                              celebrity = "star2",
                              patriotic = "cap")
        ) %>%
        mutate(cat = modify(cat, emo::ji))

yt_emoji$cat[yt_emoji$cat == emo::ji("cap")] <- emo::flag("Scotland")

ggplot(yt_summary, aes(x = year, y = ranking, group = cat)) +
        geom_line(aes(color = cat, alpha = 1), size = 2) +
        geom_point(aes(color = cat, alpha = 1), size = 4) +
        scale_y_reverse(breaks = 1:nrow(yt_summary))

my_theme <- function() {
        
        #Colors
        color.background = "white"
        color.text = "#22211d"
        
        # Begin construction of chart 
        theme_bw(base_size=15) + 
                
                # Format background colors 
                theme(panel.background = element_rect(fill=color.background, 
                                                      color=color.background)) + 
                theme(plot.background = element_rect(fill=color.background, 
                                                     color=color.background)) + 
                theme(panel.border = element_rect(color=color.background)) + 
                theme(strip.background = element_rect(fill=color.background, 
                                                      color=color.background)) + 
                
                # Format the grid 
                theme(panel.grid.major.y = element_blank()) + 
                theme(panel.grid.minor.y = element_blank()) + 
                theme(axis.ticks = element_blank()) + 
                
                # Format the legend 
                theme(legend.position = "none") + 
                
                # Format title and axis labels 
                theme(plot.title = element_text(color=color.text, size=20, face = "bold")) + 
                theme(axis.title.x = element_text(size=14, color="black", face = "bold")) + 
                theme(axis.title.y = element_text(size=14, color="black", face = "bold", 
                                                  vjust=1.25)) + 
                theme(axis.text.x = element_text(size=10, vjust=0.5, hjust=0.5, 
                                                 color = color.text)) + 
                theme(axis.text.y = element_text(size=10, color = color.text)) + 
                theme(strip.text = element_text(face = "bold")) + 
                
                # Plot margins 
                theme(plot.margin = unit(c(0.35, 0.2, 0.3, 0.35), "cm")) } 

gg_color_hue <- function(n) {
        hues = seq(15, 375, length = n + 1)
        hcl(h = hues, l = 65, c = 100)[1:n]
}

cols <- gg_color_hue(7)

ggplot(yt_summary, aes(x = year, y = ranking, group = cat)) +
        geom_line(aes(color = cat, alpha = 1), size = 2) +
        geom_point(aes(color = cat, alpha = 1), size = 4) +
        geom_point(color = "#FFFFFF", size = 1) +
        scale_y_reverse(breaks = 1:nrow(yt_summary)) +
        scale_x_continuous(breaks = 2000:2020) +
        theme(legend.position = "none") +
        geom_text(data = filter(yt_emoji, year == 2000), aes(label = cat, x = 1999.5), 
                  hjust = .5, size = 10) +
        geom_text(data = filter(yt_emoji, year == 2020), aes(label = cat, x = 2020.5), 
                  hjust = .5, size = 10) +
        geom_text(aes(x = 2003, y = .8, label = "Funny", family = "serif", fontface = "italic"), color = cols[4], 
                  size = 6) +
        geom_text(aes(x = 2004, y = 1.8, label = "Shows Product Quickly", family = "serif", fontface = "italic"), color = cols[6], 
                  size = 6) +
        geom_text(aes(x = 2006.5, y = 3.8, label = "Animals", family = "serif", fontface = "italic"), color = cols[1], 
                  size = 6) +
        geom_text(aes(x = 2006.5, y = 2.8, label = "Danger", family = "serif", fontface = "italic"), color = cols[3], 
                  size = 6) +
        geom_text(aes(x = 2018, y = 6.8, label = "Uses Sex", family = "serif", fontface = "italic"), color = cols[7], 
                  size = 6) +
        geom_text(aes(x = 2012, y = 5.8, label = "Celebrity", family = "serif", fontface = "italic"), color = cols[2], 
                  size = 6) +
        geom_text(aes(x = 2004, y = 6.8, label = "Patriotic", family = "serif", fontface = "italic"), color = cols[5], 
                  size = 6) +
        labs(x = "Year", y = "Rank", title = "Super Bowl Commercials",
             subtitle = "Categories ranked by most commercials in each year") +
        my_theme()


