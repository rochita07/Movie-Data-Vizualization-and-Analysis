# Loading necessary libraries and data files ------------------------------
library(rsconnect)
library(shiny) 
library(shinydashboard)
library(shinyjs)
library(htmlwidgets)
library(stringr)
library(countrycode)
library(plotly)
library(shinythemes)
library(DT)
library(rsconnect)
library(ggplot2)
library(dplyr)
library(tidyr)
library(DT)
library(knitr)
library(kableExtra)
library(ggthemes)
library(rsconnect)
library(tidyverse)
library(scales)
library(jsonlite)
library(knitr)
library(kableExtra)
library(ggrepel)
library(gridExtra)
library(lubridate)
library(tidytext)
library(wordcloud)
library(recommenderlab)
library(GGally)
library(stringr)
library(dplyr)
library(plyr)
library(readxl)
library(gridExtra)
library(ggpubr)
library(collapsibleTree)
library(plotly)
library(ggplot2)
df <- read.csv("heatmapData.csv")

# path to html and css file used on home page ------------------------------


htmlFilePath = file.path("index.html")
cssFilePath =  file.path("style.css")


# Preprocessed Code for Visz_1_Director -----------------------

load("final_data.RData")
colnames(final_data)

director_data = final_data %>% filter(tmdb_indicator == 1) %>% select(title, director_1, director_2) %>% pivot_longer(!title, names_to = "director_order", values_to = "director") %>% mutate(director = na_if(director, "")) %>% drop_na(director)

dim(director_data)
colnames(director_data)


director_data = merge(director_data %>% select(title, director),
                      final_data %>% filter(tmdb_indicator == 1) %>% select(title, vote_average, vote_count, popularity, budget, revenue ),
                      by = "title", all = TRUE)
dim(director_data)
colnames(director_data)


genre_data = final_data %>% filter(tmdb_indicator == 1) %>% select(title, starts_with('genre_')) %>% pivot_longer(!title, names_to = "genre_order", values_to = "genre") %>% mutate(genre = na_if(genre, "")) %>% drop_na(genre)

dim(genre_data)
genre_data = merge(genre_data %>% select(title, genre),
                   final_data %>% filter(tmdb_indicator == 1) %>% select(title, vote_average, vote_count, popularity, budget, revenue ),
                   by = "title", all = TRUE)

dim(genre_data)
colnames(genre_data)


director_genre_data = merge(director_data %>% select(title, director), genre_data, by = "title", all = TRUE)
dim(director_genre_data)
colnames(director_genre_data)


director_genre_data = merge(director_data %>% select(title, director), genre_data, by = "title", all = TRUE)
dim(director_genre_data)
colnames(director_genre_data)
head(director_genre_data)

director_data_table <- director_data %>% 
  group_by(director) %>%                            # create a grouping by director
  filter(vote_count>10) %>%                         # filter out directors with few voters
  dplyr::summarise(                                                # get the number of titles
    n = n(),                                        # get the number of titles
    weighted_vote_mean = round(weighted.mean(vote_average, vote_count), 2),
    popularity = round(popularity * 10, 2),
    total_profit = round((sum(revenue) - sum(budget))/(10^9), 3),
    total_revenue = round(sum(revenue)/(10^9), 3),
    total_budget = round(sum(budget)/(10^9), 3))  %>% 
  distinct(director, .keep_all= TRUE)



circular_bar_plot = function(df_dir_genre)
{
  
  df_dir_genre$director = as.factor(df_dir_genre$director)
  df_dir_genre$genre = as.factor(df_dir_genre$genre)
  
  # director_genre_data %>% filter(director %in% plot_dir_df$director) %>% select(director, genre) %>% group_by(director, genre) %>% filter(director == "Steven Spielberg", genre == "Action")
  
  # Set a number of 'empty bar' to add at the end of each group
  
  empty_bar = 3
  to_add = data.frame( matrix(NA, empty_bar*nlevels(df_dir_genre$director), ncol(df_dir_genre)) )
  colnames(to_add) = colnames(df_dir_genre)
  to_add$director = rep(levels(df_dir_genre$director), each = empty_bar)
  df_dir_genre = rbind(df_dir_genre, to_add)
  df_dir_genre = df_dir_genre %>% arrange(director)
  df_dir_genre$id = seq(1, nrow(df_dir_genre))
  
  
  # Get the name and the y position of each label
  label_df_dir_genre = df_dir_genre
  number_of_bar = nrow(label_df_dir_genre)
  angle = 90 - 360 * (label_df_dir_genre$id - 0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
  label_df_dir_genre$hjust <- ifelse( angle < -90, 1, 0)
  label_df_dir_genre$angle <- ifelse(angle < -90, angle + 180, angle)
  
  str(df_dir_genre)
  
  # prepare a data frame for base lines
  base_df_dir_genre = df_dir_genre %>% group_by(director) %>% dplyr::summarise(start = min(id) , end = max(id) - empty_bar) %>% 
    rowwise() %>% 
    mutate(title = mean(c(start, end)))
  base_df_dir_genre
  base_df_dir_genre$id = seq(1, nrow(base_df_dir_genre))
  
  number_of_bar = nrow(base_df_dir_genre)
  angle = 90 - 360 * (base_df_dir_genre$id - 0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
  base_df_dir_genre$hjust <- ifelse( angle < -90, 1, 0)
  base_df_dir_genre$angle <- ifelse(angle < -90, angle + 180, angle)
  base_df_dir_genre
  
  # prepare a data frame for grid (scales)
  grid_df_dir_genre = base_df_dir_genre
  grid_df_dir_genre$end = grid_df_dir_genre$end[ c( nrow(grid_df_dir_genre), 1:nrow(grid_df_dir_genre)-1)] + 1
  grid_df_dir_genre$start = grid_df_dir_genre$start - 1
  grid_df_dir_genre = grid_df_dir_genre[-1,]
  
  colnames(df_dir_genre)
  # Make the plot
  
  p = ggplot(df_dir_genre, aes(x = as.factor(id), y = freq, fill = director)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
    
    geom_bar(aes(x = as.factor(id),  y = freq, fill = director), stat = "identity", alpha = 0.5)  + 
    
    # Add a val=100/75/50/25 lines. I do it at the beginning to make sur barplots are OVER it.
    geom_segment(data = grid_df_dir_genre, aes(x = end, y = 40, xend = start, yend = 40), colour = "grey", alpha = 1, size= 0.3 , inherit.aes = FALSE ) +
    geom_segment(data = grid_df_dir_genre, aes(x = end, y = 30, xend = start, yend = 30), colour = "grey", alpha = 1, size = 0.3 , inherit.aes = FALSE ) +
    geom_segment(data = grid_df_dir_genre, aes(x = end, y = 20, xend = start, yend = 20), colour = "grey", alpha = 1, size = 0.3 , inherit.aes = FALSE ) +
    geom_segment(data = grid_df_dir_genre, aes(x = end, y = 10, xend = start, yend = 10), colour = "grey", alpha = 1, size = 0.3 , inherit.aes = FALSE ) +
    
    
    # Add text showing the value of each 100/75/50/25 lines
    annotate("text", x = rep(max(df_dir_genre$id), 4), y = c(10, 20, 30, 40), label = c("10", "20", "30", "40") , color = "grey", size = 3 , angle = 0, fontface = "bold", hjust = 1) +
    
    geom_bar(aes(x = as.factor(id),  y = freq, fill = director), stat = "identity", alpha = 0.5) +
    ylim(-100,120) +
    theme_minimal() +
    theme(
      legend.position = "none",
      axis.text = element_blank(),
      axis.title = element_blank(),
      panel.grid = element_blank(),
      plot.margin = unit(rep(-1,4), "cm") 
    ) +
    coord_polar() + 
    geom_text(data = label_df_dir_genre, aes(x = id, y = freq + 10, label = genre, hjust = hjust), color = "black", fontface = "bold", alpha = 0.6, size = 2.5, angle = label_df_dir_genre$angle, inherit.aes = FALSE ) +
    
    # Add base line information
    geom_segment(data = base_df_dir_genre, aes(x = start, y = -5, xend = end, yend = -5), colour = "black", alpha = 0.8, size = 0.6 , inherit.aes = FALSE )  +
    
    geom_text(data = base_df_dir_genre, aes(x = title, y = 100, label = director), hjust = base_df_dir_genre$hjust, colour = "black", alpha = 0.8, size = 3, fontface = "bold", inherit.aes = FALSE)
  
  #, angle = base_df_dir_genre$angle
  p
  
  return(p)
}

# Viz_2_recommendation.R Code ---------------

movies = final_data %>% filter(tmdb_indicator == 1)

C <- mean(na.omit(movies$vote_average))
C
m <- quantile(na.omit(movies$vote_count), 0.75)
m
movies$weighted_rating <- (movies$vote_average*movies$vote_count + C*m)/(movies$vote_count + m)
summary(movies$weighted_rating)
similarity_vars <- c("cast_1", "cast_2", "cast_3", "cast_4", "cast_5", "director_1", "director_2", "genre_1", "genre_2", "genre_3", "genre_4", "genre_5")

movies_filter <- movies %>% select(title, !!similarity_vars)
movies_filter <- movies_filter %>% mutate_if(is.factor, as.character)


recommend_similar <- function(movie, sim_obs){
  
  director1 <- movies_filter$director_1[movies_filter$title==movie]
  director2 <- movies_filter$director_2[movies_filter$title==movie]
  
  cast1 <- movies_filter$cast_1[movies_filter$title==movie]
  cast2 <- movies_filter$cast_2[movies_filter$title==movie]
  cast3 <- movies_filter$cast_3[movies_filter$title==movie]
  cast4 <- movies_filter$cast_4[movies_filter$title==movie]
  cast5 <- movies_filter$cast_5[movies_filter$title==movie]
  
  genre1 <- movies_filter$genre_1[movies_filter$title==movie]
  genre2 <- movies_filter$genre_2[movies_filter$title==movie]
  genre3 <- movies_filter$genre_3[movies_filter$title==movie]
  genre4 <- movies_filter$genre_4[movies_filter$title==movie]
  genre5 <- movies_filter$genre_5[movies_filter$title==movie]
  
  rec_df <- movies_filter
  
  rec_df$same_d1 <- NA
  rec_df$same_d2 <- NA
  rec_df$same_c1 <- NA
  rec_df$same_c2 <- NA
  rec_df$same_c3 <- NA
  rec_df$same_c4 <- NA
  rec_df$same_c5 <- NA
  rec_df$same_g1 <- NA
  rec_df$same_g2 <- NA
  rec_df$same_g3 <- NA
  rec_df$same_g4 <- NA
  rec_df$same_g5 <- NA
  
  
  rec_df$same_d1 <- ifelse(rec_df$director_1==director1|rec_df$director_2==director1, 1, 0)
  rec_df$same_d2 <- ifelse(rec_df$director_1==director2|rec_df$director_2==director2, 1, 0)
  
  rec_df$same_c1 <- ifelse(rec_df$cast_1==cast1|rec_df$cast_2==cast1|rec_df$cast_3==cast1|rec_df$cast_4==cast1|rec_df$cast_5==cast1, 1, 0)
  rec_df$same_c2 <- ifelse(rec_df$cast_1==cast2|rec_df$cast_2==cast2|rec_df$cast_3==cast2|rec_df$cast_4==cast2|rec_df$cast_5==cast2, 1, 0)
  rec_df$same_c3 <- ifelse(rec_df$cast_1==cast3|rec_df$cast_2==cast3|rec_df$cast_3==cast3|rec_df$cast_4==cast3|rec_df$cast_5==cast3, 1, 0)
  rec_df$same_c4 <- ifelse(rec_df$cast_1==cast4|rec_df$cast_2==cast4|rec_df$cast_3==cast4|rec_df$cast_4==cast4|rec_df$cast_5==cast4, 1, 0)
  rec_df$same_c5 <- ifelse(rec_df$cast_1==cast5|rec_df$cast_2==cast5|rec_df$cast_3==cast5|rec_df$cast_4==cast5|rec_df$cast_5==cast5, 1, 0)
  
  rec_df$same_g1 <- ifelse(rec_df$genre_1==genre1|rec_df$genre_2==genre1|rec_df$genre_3==genre1|rec_df$genre_4==genre1|rec_df$genre_5==genre1, 1, 0)
  rec_df$same_g2 <- ifelse(rec_df$genre_1==genre2|rec_df$genre_2==genre2|rec_df$genre_3==genre2|rec_df$genre_4==genre2|rec_df$genre_5==genre2, 1, 0)
  rec_df$same_g3 <- ifelse(rec_df$genre_1==genre3|rec_df$genre_2==genre3|rec_df$genre_3==genre3|rec_df$genre_4==genre3|rec_df$genre_5==genre3, 1, 0)
  rec_df$same_g4 <- ifelse(rec_df$genre_1==genre4|rec_df$genre_2==genre4|rec_df$genre_3==genre4|rec_df$genre_4==genre4|rec_df$genre_5==genre4, 1, 0)
  rec_df$same_g5 <- ifelse(rec_df$genre_1==genre5|rec_df$genre_2==genre5|rec_df$genre_3==genre5|rec_df$genre_4==genre5|rec_df$genre_5==genre5, 1, 0)
  
  rec_df <- rec_df %>% mutate_at(vars("same_d1": "same_g5"), list(~replace(., is.na(.), 0)))
  colnames(rec_df)
  rec_df$sim_count <- rowSums(rec_df[, c(14:25)])
  
  rec_df <- left_join(rec_df, movies %>% select(title, weighted_rating), by = "title")
  
  Top_rec <- rec_df %>% arrange(desc(sim_count), desc(weighted_rating)) %>% slice(1:sim_obs) %>% select(title, title, sim_count, weighted_rating, everything()) %>%
    dplyr::mutate(weighted_rating = round(weighted_rating, 2), sim_count = 100*( sim_count/max(sim_count))) %>% 
    dplyr::rename("Movie Title" = "title", "Similarity Score (/100)" = "sim_count", "Weighted Rating (/10)" = "weighted_rating",
                  "Cast 1" = "cast_1", "Cast 2" = "cast_2", "Cast 3" = "cast_3", "Cast 4" = "cast_4", "Cast 5" = "cast_5",
                  "Director 1" = "director_1", "Director 2" = "director_2",
                  "Genre 1" = "genre_1", "Genre 2" = "genre_2", "Genre 3" = "genre_3", "Genre 4" = "genre_4", "Genre 5" = "genre_5") 
  Top_rec[, 1:15]
  
}


# Cast Viz_1_cast.R code ---------------------------------------------------

cast_data = final_data %>% filter(tmdb_indicator == 1) %>% select(title, cast_1, cast_2, cast_3, cast_4, cast_5) %>% pivot_longer(!title, names_to = "cast_order", values_to = "cast") %>% mutate(cast = na_if(cast, "")) %>% drop_na(cast)

cast_data = merge(cast_data %>% select(title, cast),
                  final_data %>% filter(tmdb_indicator == 1) %>% select(title, vote_average, vote_count, popularity, budget, revenue ),
                  by = "title", all = TRUE)

genre_data = final_data %>% filter(tmdb_indicator == 1) %>% select(title, starts_with('genre_')) %>% pivot_longer(!title, names_to = "genre_order", values_to = "genre") %>% mutate(genre = na_if(genre, "")) %>% drop_na(genre)

genre_data = merge(genre_data %>% select(title, genre),
                   final_data %>% filter(tmdb_indicator == 1) %>% select(title, vote_average, vote_count, popularity, budget, revenue ),
                   by = "title", all = TRUE)

cast_genre_data = merge(cast_data %>% select(title, cast), genre_data, by = "title", all = TRUE)

cast_data_table <- cast_data %>% 
  group_by(cast) %>%                            # create a grouping by cast
  filter(vote_count>10) %>%                         # filter out casts with few voters
  dplyr::summarise(                                                # get the number of titles
    n = n(),                                        # get the number of titles
    weighted_vote_mean = round(weighted.mean(vote_average, vote_count), 2),
    popularity = round(popularity * 10, 2),
    total_profit = round((sum(revenue) - sum(budget))/(10^9), 3),
    total_revenue = round(sum(revenue)/(10^9), 3),
    total_budget = round(sum(budget)/(10^9), 3))  %>% 
  distinct(cast, .keep_all= TRUE)


circular_bar_plot_cast = function(df_dir_genre)
{
  
  df_dir_genre$cast = as.factor(df_dir_genre$cast)
  df_dir_genre$genre = as.factor(df_dir_genre$genre)
  
  # cast_genre_data %>% filter(cast %in% plot_dir_df$cast) %>% select(cast, genre) %>% group_by(cast, genre) %>% filter(cast == "Steven Spielberg", genre == "Action")
  
  # Set a number of 'empty bar' to add at the end of each group
  
  empty_bar = 3
  to_add = data.frame( matrix(NA, empty_bar*nlevels(df_dir_genre$cast), ncol(df_dir_genre)) )
  colnames(to_add) = colnames(df_dir_genre)
  to_add$cast = rep(levels(df_dir_genre$cast), each = empty_bar)
  df_dir_genre = rbind(df_dir_genre, to_add)
  df_dir_genre = df_dir_genre %>% arrange(cast)
  df_dir_genre$id = seq(1, nrow(df_dir_genre))
  
  
  # Get the name and the y position of each label
  label_df_dir_genre = df_dir_genre
  number_of_bar = nrow(label_df_dir_genre)
  angle = 90 - 360 * (label_df_dir_genre$id - 0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
  label_df_dir_genre$hjust <- ifelse( angle < -90, 1, 0)
  label_df_dir_genre$angle <- ifelse(angle < -90, angle + 180, angle)
  
  str(df_dir_genre)
  
  # prepare a data frame for base lines
  base_df_dir_genre = df_dir_genre %>% group_by(cast) %>% dplyr::summarise(start = min(id) , end = max(id) - empty_bar) %>% 
    rowwise() %>% 
    mutate(title = mean(c(start, end)))
  base_df_dir_genre
  base_df_dir_genre$id = seq(1, nrow(base_df_dir_genre))
  
  number_of_bar = nrow(base_df_dir_genre)
  angle = 90 - 360 * (base_df_dir_genre$id - 0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
  base_df_dir_genre$hjust <- ifelse( angle < -90, 1, 0)
  base_df_dir_genre$angle <- ifelse(angle < -90, angle + 180, angle)
  base_df_dir_genre
  
  # prepare a data frame for grid (scales)
  grid_df_dir_genre = base_df_dir_genre
  grid_df_dir_genre$end = grid_df_dir_genre$end[ c( nrow(grid_df_dir_genre), 1:nrow(grid_df_dir_genre)-1)] + 1
  grid_df_dir_genre$start = grid_df_dir_genre$start - 1
  grid_df_dir_genre = grid_df_dir_genre[-1,]
  
  colnames(df_dir_genre)
  # Make the plot
  
  p = ggplot(df_dir_genre, aes(x = as.factor(id), y = freq, fill = cast)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
    
    geom_bar(aes(x = as.factor(id),  y = freq, fill = cast), stat = "identity", alpha = 0.5)  + 
    
    # Add a val=100/75/50/25 lines. I do it at the beginning to make sur barplots are OVER it.
    geom_segment(data = grid_df_dir_genre, aes(x = end, y = 40, xend = start, yend = 40), colour = "grey", alpha = 1, size= 0.3 , inherit.aes = FALSE ) +
    geom_segment(data = grid_df_dir_genre, aes(x = end, y = 30, xend = start, yend = 30), colour = "grey", alpha = 1, size = 0.3 , inherit.aes = FALSE ) +
    geom_segment(data = grid_df_dir_genre, aes(x = end, y = 20, xend = start, yend = 20), colour = "grey", alpha = 1, size = 0.3 , inherit.aes = FALSE ) +
    geom_segment(data = grid_df_dir_genre, aes(x = end, y = 10, xend = start, yend = 10), colour = "grey", alpha = 1, size = 0.3 , inherit.aes = FALSE ) +
    
    
    # Add text showing the value of each 100/75/50/25 lines
    annotate("text", x = rep(max(df_dir_genre$id), 4), y = c(10, 20, 30, 40), label = c("10", "20", "30", "40") , color = "grey", size = 3 , angle = 0, fontface = "bold", hjust = 1) +
    
    geom_bar(aes(x = as.factor(id),  y = freq, fill = cast), stat = "identity", alpha = 0.5) +
    ylim(-100,120) +
    theme_minimal() +
    theme(
      legend.position = "none",
      axis.text = element_blank(),
      axis.title = element_blank(),
      panel.grid = element_blank(),
      plot.margin = unit(rep(-1,4), "cm") 
    ) +
    coord_polar() + 
    geom_text(data = label_df_dir_genre, aes(x = id, y = freq + 10, label = genre, hjust = hjust), color = "black", fontface = "bold", alpha = 0.6, size = 2.5, angle = label_df_dir_genre$angle, inherit.aes = FALSE ) +
    
    # Add base line information
    geom_segment(data = base_df_dir_genre, aes(x = start, y = -5, xend = end, yend = -5), colour = "black", alpha = 0.8, size = 0.6 , inherit.aes = FALSE )  +
    
    geom_text(data = base_df_dir_genre, aes(x = title, y = 100, label = cast), hjust = base_df_dir_genre$hjust, colour = "black", alpha = 0.8, size = 3, fontface = "bold", inherit.aes = FALSE)
  
  #, angle = base_df_dir_genre$angle
  p
  
  return(p)
}



#VIZ 5 Continuous Variables Code -------------------------

continuous_variables = final_data %>% filter(tmdb_indicator == 1)%>% filter(vote_count>10) %>%
  select(title, popularity, vote_average, vote_count, runtime, budget, revenue ) %>%
  dplyr::mutate(profit = revenue - budget,
                popularity = round(popularity * 10, 2),
                rating = weighted.mean(vote_average, vote_count)) 



# UI -----------------------------------------------------------------------


header <- dashboardHeader(title = span("Movies Data Visualization and Analysis", 
                                            style = "color: white;"), titleWidth = 380)

sidebar <- dashboardSidebar(
  width = 380,
  sidebarMenu(id = "sidebarid",
              HTML(paste0(
                "<br>",
                "<img src = 'https://www.iamherelearning.com/wp-content/uploads/2020/02/Movie-Icon-1-460x406.png' width=370>",
                "<br><br>"
              )),
              menuItem("Home", tabName = "page1"),
              menuItem("Production Countries Analysis", tabName = "page2"),
              menuItem("Director Analysis", tabName = "page3"),
              menuItem("Movie Recommendation Engine", tabName = "page4"),
              menuItem("Cast Analysis", tabName = "page5"),
              menuItem("Exploration of Continuous Variables", tabName = "page6")
  ),
  # removes navigation show/hide toggle
  tags$script(JS("document.getElementsByClassName('sidebar-toggle')[0].style.visibility = 'hidden';"))
)

body <- dashboardBody(
  tags$style(HTML(".main-sidebar { font-size: 20px; }")),
  tabItems(
    tabItem(tabName = "page1",  includeHTML(htmlFilePath)),
    tabItem(
      tabName = "page2", 
      fluidRow(
        box(
          title = "Heatmap of Frequency of Movies Produced by Country",
          status = "success",
          solidHeader = TRUE,
          plotlyOutput(outputId = "heatmap")
        ),
        box(
          title = "Heatmap Inputs",
          status = "warning",
          solidHeader = TRUE,
          "Pick a year within the provided range using the slidebar. The heatmap will change dynamically.", br(),br(),
          sliderInput(inputId = "timeSlider", label = "Movie Release Year", min = 1933, max = 2021, value = 2010, sep = "")
        )
    )
    ),
    tabItem(
      tabName = "page3", 
            "Page 3 content. This page has sidebar meny items that are used in the plot below.",
            h3("Bar Plot Panel"),
            tags$br(),
            radioButtons(
              "radio_director",
              label = "Select Criteria",
              choices = list(
                "No of total movies" = "n" ,
                "Total Revenue" = "total_revenue",
                "Total Budget" = "total_budget",
                "Total Profit" = "total_profit",
                "Average Movie Rating" = "weighted_vote_mean",
                "Average Movie Popularity Index" = "popularity"
              ),
              selected = "n"
            ),
            tags$hr(),
            
            
            sliderInput(inputId = "top_obs",
                        label = "Choose K",
                        min = 1, 
                        max = 100,
                        value = 10),
            
            checkboxInput(inputId = "top_k_director_show_data",
                          label = "Show Data Table for top K Directors",
                          value = TRUE),
            
            checkboxInput(inputId = "top_k_director_genre_show_data",
                          label = "Show Data Table for top K Directors with Genres",
                          value = TRUE),
            
            h3("Bar plot of top K Directors"),
            plotlyOutput(outputId = "top_k_directors"), # uniPlot
            tags$br(),
            tags$br(),
            
            h3("Data table of top K Directors"),
            dataTableOutput(outputId = "top_k_director_table"),
            tags$br(),
            tags$br(),
            
            h3("Circular Bar Plot of top K Directors with Genre"),
            plotOutput(outputId = "director_genre", width = "100%"),
            tags$br(),
            tags$br(),
            
            h3("Data table of top K Directors with Genres"),
            dataTableOutput(outputId = "top_k_director_genre_table"),
            tags$br(),
            tags$br()
            
            ),
    tabItem(tabName = "page4",
            
            # Copy the line below to make a text input box
            titlePanel("Movie Recommendation Engine"),
            
            # textInput("movie_text", label = h3("Enter Movie Name"),  value = "Titanic"),
            
            selectInput("movie_text", label = h3("Choose The Movie You Like"), selected = "Titanic",
                        choices = as.character(movies_filter$title[1:dim(movies_filter)[1]])),
            
            sliderInput(inputId = "movie_sim_obs",
                        label = "Choose K",
                        min = 1, 
                        max = 100,
                        value = 10),
            
            hr(),
            
            mainPanel(
              h3("Movie Recommendation"),
              tags$br(),
              tableOutput("movie_table"),
              tags$br(),
              tags$br(),
            )
            
            
            ),
    
    tabItem(tabName = "page5",
            
            titlePanel("Movie:cast_Actor"),
            
            sidebarLayout(
              sidebarPanel(
                # sliderInput("bins",
                #             "Number of bins:",
                #             min = 1,
                #             max = 50,
                #             value = 30)
                
                h3("Bar Plot Panel"),
                tags$br(),
                radioButtons(
                  "radio_cast",
                  label = "Select Criteria",
                  choices = list(
                    "No of total movies" = "n" ,
                    "Total Revenue" = "total_revenue",
                    "Total Budget" = "total_budget",
                    "Total Profit" = "total_profit",
                    "Average Movie Rating" = "weighted_vote_mean",
                    "Average Movie Popularity Index" = "popularity"
                  ),
                  selected = "n"
                ),
                tags$hr(),
                
                sliderInput(inputId = "top_obs_cast",
                            label = "Choose K",
                            min = 1, 
                            max = 100,
                            value = 10),
                
                checkboxInput(inputId = "top_k_cast_show_data",
                              label = "Show Data Table for top K casts",
                              value = TRUE),
                
                checkboxInput(inputId = "top_k_cast_genre_show_data",
                              label = "Show Data Table for top K casts with Genres",
                              value = TRUE)
                
              ), # sidebarPanel
              
              # Show a plot of the generated distribution
              mainPanel(
                # plotOutput("distPlot")
                
                h3("Bar plot of top K casts"),
                plotlyOutput(outputId = "top_k_casts"), # uniPlot
                tags$br(),
                tags$br(),
                
                h3("Data table of top K casts"),
                dataTableOutput(outputId = "top_k_cast_table"),
                tags$br(),
                tags$br(),
                
                h3("Circular Bar Plot of top K casts with Genre"),
                plotOutput(outputId = "cast_genre", width = "100%"),
                tags$br(),
                tags$br(),
                
                h3("Data table of top K casts with Genres"),
                dataTableOutput(outputId = "top_k_cast_genre_table"),
                tags$br(),
                tags$br()
                
              ) # main panel:output
              
            )# sidebarLayout
            
            
            
            ),
    tabItem(tabName = "page6",
            
            # Application title
            titlePanel("Exploration of Continuous Variables"),
            
            # --------------------
            # Single Variable : density plot / Violin Plot section
            # --------------------
            
            sidebarLayout(
              sidebarPanel(
                h3("Single Variable Plot"),
                
                radioButtons(
                  "radio_director_continuous",
                  label = "Select Variable",
                  choices = list(
                    "Revenue" = "revenue",
                    "Budget" = "budget",
                    "Profit" = "profit",
                    # "Movie Rating" = "rating",
                    "Popularity Index" = "popularity"
                  ),
                  selected = "revenue"
                ), # radio buttons
                
                tags$hr(),
                
              ), # sidebar Panel
              
              
              mainPanel(
                h3("Distribution of Single Variable"),
                plotlyOutput(outputId = "single_var_plot"),
                tags$br(),
                tags$br(),
                
                h3("Summary Table"),
                verbatimTextOutput(outputId = "summary_table"),
                tags$br(),
                tags$br()
                
              ) # main panel
            ), # side bar layout
            
            
            
            #### Multiple Variable plot ####
            
            sidebarLayout(
              sidebarPanel(
                h3("Multiple variable Scatter Plot"),
                tags$br(),
                
                checkboxGroupInput(
                  "checkGroupbox",
                  label = "Select Variables",
                  choices = list(
                    "Revenue" = "revenue",
                    "Budget" = "budget",
                    "Profit" = "profit",
                    # "Movie Rating" = "rating",
                    "Popularity Index" = "popularity"),
                  
                  selected = list(
                    "Revenue" = "revenue",
                    "Budget" = "budget",
                    "Profit" = "profit",
                    # "Movie Rating" = "rating",
                    "Popularity Index" = "popularity"
                  )
                ), # check box
                tags$hr()
              ), # side bar panel
              
              mainPanel(
                h3("Plot of Multiple Variables"),
                plotOutput(outputId = "multi_var_plot"),
                tags$br(),
                tags$br()
              ) # main panel
            ), # side bar layout
            tags$hr()
            
            
            
            
            
    )
    
  )
)


ui <- dashboardPage(header, sidebar, body, skin = "red", tags$head(
  tags$link(rel = "stylesheet", type = "text/css", href = cssFilePath)
)) 

# server -----------------------------------------------------------------------

server <- function(input, output, session) {
  
  
  
  
  
  
  #### top 'k' casts : data table ####
  
  output$top_k_cast_table <- renderDataTable({
    
    if(input$top_k_cast_show_data)
    {
      
      if(input$radio_cast == "n")
      {
        cast_tab <- cast_data_table %>% filter(n > 0) %>% arrange(desc(n))  %>% head(input$top_obs_cast)  %>% select(cast, n) %>% dplyr::rename("cast" = "cast", "No of total movies" = "n")
      }
      
      else if(input$radio_cast == "total_revenue")
      {
        cast_tab <- cast_data_table %>% filter(total_revenue > 0) %>% arrange(desc(total_revenue))  %>% head(input$top_obs_cast) %>% select(cast, total_revenue) %>% dplyr::rename("cast" = "cast", "Total Revenue (Billion)" = "total_revenue")
      }
      
      else if(input$radio_cast == "total_budget")
      {
        cast_tab <- cast_data_table %>% filter(total_budget > 0) %>% arrange(desc(total_budget))  %>% head(input$top_obs_cast) %>% select(cast, total_budget) %>% dplyr::rename("cast" = "cast", "Total Budget (Billion)" = "total_budget")
      }
      
      else if(input$radio_cast == "total_profit")
      {  
        cast_tab <- cast_data_table %>% filter(total_profit > 0) %>% arrange(desc(total_profit))  %>% head(input$top_obs_cast) %>% select(cast, total_profit) %>% dplyr::rename("cast" = "cast", "Total Profit (Billion)" = "total_profit")
      }
      
      else if (input$radio_cast == "weighted_vote_mean")
      {  
        cast_tab <- cast_data_table %>% filter(weighted_vote_mean > 0) %>% arrange(desc(weighted_vote_mean))  %>% head(input$top_obs_cast) %>% select(cast, weighted_vote_mean) %>% dplyr::rename("cast" = "cast", "Average Movie Rating (/10)" = "weighted_vote_mean")
      }
      
      else if (input$radio_cast == "popularity")
      {
        cast_tab <- cast_data_table %>% filter(popularity > 0) %>% arrange(desc(popularity))  %>% head(input$top_obs_cast) %>% select(cast, popularity) %>% dplyr::rename("cast" = "cast", "Average Movie Popularity Index (/10)" = "popularity")
      }
      
      DT::datatable(cast_tab,
                    options =  list(pageLength = 10),
                    rownames = FALSE)
      
    }
  })
  
  
  #### Bar plot of top 'k' casts ####
  
  output$top_k_casts <- renderPlotly({
    
    if(input$radio_cast == "n")
    {
      
      plot_Dir_no_movies = cast_data_table %>% arrange(desc(n)) %>% head(input$top_obs_cast) %>%
        ggplot(aes(x=reorder(cast, n), 
                   y= n, 
                   text = paste(
                     "cast:", cast,
                     "\nNo of movies:", n),
        )) +
        geom_col(fill="blue") + coord_flip() +
        labs(x="", y="Number of movies") + ggtitle(paste("Top", input$top_obs_cast, "casts based on No of Movies"))
      
      plot_Dir_no_movies = ggplotly(plot_Dir_no_movies, tooltip = "text")
      print(plot_Dir_no_movies)
    }
    
    else if(input$radio_cast == "total_revenue")
    {
      plot_Dir_revenue = cast_data_table  %>% filter(total_revenue >0)  %>% arrange(desc(total_revenue)) %>% head(input$top_obs_cast) %>%
        ggplot(aes(x=reorder(cast, total_revenue),
                   y= total_revenue,
                   text = paste(
                     "cast:", cast,
                     "\nTotal revenue: $", total_revenue, "B"),
        )) +
        geom_col(fill="blue") + coord_flip() +
        labs(x="", y="Total revenue") + ggtitle(paste("Top", input$top_obs_cast, "casts based on Total Revenue"))
      
      plot_Dir_revenue = ggplotly(plot_Dir_revenue, tooltip = "text")
    }
    
    
    else if(input$radio_cast == "total_budget")
    {
      plot_Dir_budget = cast_data_table  %>% filter(total_budget >0)  %>% arrange(desc(total_budget)) %>% head(input$top_obs_cast) %>%
        ggplot(aes(x=reorder(cast, total_budget),
                   y= total_budget,
                   text = paste(
                     "cast:", cast,
                     "\nTotal budget: $", total_budget, "B"),
        )) +
        geom_col(fill="blue") + coord_flip() +
        labs(x="", y="Total budget") + ggtitle(paste("Top", input$top_obs_cast, "casts based on Total Budget"))
      
      plot_Dir_budget = ggplotly(plot_Dir_budget, tooltip = "text")
      print(plot_Dir_budget)
    }  
    
    else if(input$radio_cast == "total_profit")
    {   
      plot_Dir_profit = cast_data_table  %>% filter(total_profit >0)  %>% arrange(desc(total_profit)) %>% head(input$top_obs_cast) %>%
        ggplot(aes(x=reorder(cast, total_profit),
                   y= total_profit,
                   text = paste(
                     "cast:", cast,
                     "\nTotal Profit: $", total_profit, "B"),
        )) +
        geom_col(fill="blue") + coord_flip() +
        labs(x="", y="Total Profit") + ggtitle(paste("Top", input$top_obs_cast, "casts based on Total Profit"))
      
      plot_Dir_profit = ggplotly(plot_Dir_profit, tooltip = "text")
      print(plot_Dir_profit)
    }
    
    else if (input$radio_cast == "weighted_vote_mean")
    {   
      plot_Dir_vote = cast_data_table  %>% filter(weighted_vote_mean >0) %>%  arrange(desc(weighted_vote_mean)) %>% head(input$top_obs_cast) %>%
        ggplot(aes(
          x=reorder(cast, weighted_vote_mean), 
          y= weighted_vote_mean,
          text = paste(
            "cast:", cast,
            "\nAverage Vote:", weighted_vote_mean ,'/10'),
        )) +
        geom_col(fill="blue") + coord_flip() +
        labs(x="", y="Weighted Vote") + ggtitle(paste("Top", input$top_obs_cast, "casts based on Voting Average"))
      plot_Dir_vote = ggplotly(plot_Dir_vote, tooltip = "text")
      print(plot_Dir_vote)
    }
    
    
    else if(input$radio_cast == "popularity")
    {
      
      plot_Dir_popularity = cast_data_table  %>% filter(popularity >0) %>%  arrange(desc(popularity)) %>% head(input$top_obs_cast) %>%
        ggplot(aes(x=reorder(cast, popularity),
                   y= popularity,
                   text = paste(
                     "cast:", cast,
                     "\nPopularity Index:", popularity  ,'/10'),
        )) +
        geom_col(fill="blue") + coord_flip() +
        labs(x="", y="Popularity") + ggtitle(paste("Top", input$top_obs_cast, "casts based on Popularity Index"))
      
      # plot_Dir_popularity
      plot_Dir_popularity = ggplotly(plot_Dir_popularity, tooltip = "text")
      print(plot_Dir_popularity)
    }
    
  })
  
  
  
  
  
  
  output$movie_table <- renderTable({
    recommend_similar(input$movie_text, input$movie_sim_obs )
    
  })
  
  
  #### Bar plot of top 'k' directors ####
  
  output$top_k_directors <- renderPlotly({
    
    if(input$radio_director == "n")
    {
      
      plot_Dir_no_movies = director_data_table %>% arrange(desc(n)) %>% head(input$top_obs) %>%
        ggplot(aes(x=reorder(director, n), 
                   y= n, 
                   text = paste(
                     "Director:", director,
                     "\nNo of movies:", n),
        )) +
        geom_col(fill="blue") + coord_flip() +
        labs(x="", y="Number of movies") + ggtitle(paste("Top", input$top_obs, "Directors based on No of Movies"))
      
      plot_Dir_no_movies = ggplotly(plot_Dir_no_movies, tooltip = "text")
      print(plot_Dir_no_movies)
    }
    
    else if(input$radio_director == "total_revenue")
    {
      plot_Dir_revenue = director_data_table  %>% filter(total_revenue >0)  %>% arrange(desc(total_revenue)) %>% head(input$top_obs) %>%
        ggplot(aes(x=reorder(director, total_revenue),
                   y= total_revenue,
                   text = paste(
                     "Director:", director,
                     "\nTotal revenue: $", total_revenue, "B"),
        )) +
        geom_col(fill="blue") + coord_flip() +
        labs(x="", y="Total revenue") + ggtitle(paste("Top", input$top_obs, "Directors based on Total Revenue"))
      
      plot_Dir_revenue = ggplotly(plot_Dir_revenue, tooltip = "text")
    }
    
    
    else if(input$radio_director == "total_budget")
    {
      plot_Dir_budget = director_data_table  %>% filter(total_budget >0)  %>% arrange(desc(total_budget)) %>% head(input$top_obs) %>%
        ggplot(aes(x=reorder(director, total_budget),
                   y= total_budget,
                   text = paste(
                     "Director:", director,
                     "\nTotal budget: $", total_budget, "B"),
        )) +
        geom_col(fill="blue") + coord_flip() +
        labs(x="", y="Total budget") + ggtitle(paste("Top", input$top_obs, "Directors based on Total Budget"))
      
      plot_Dir_budget = ggplotly(plot_Dir_budget, tooltip = "text")
      print(plot_Dir_budget)
    }  
    
    else if(input$radio_director == "total_profit")
    {   
      plot_Dir_profit = director_data_table  %>% filter(total_profit >0)  %>% arrange(desc(total_profit)) %>% head(input$top_obs) %>%
        ggplot(aes(x=reorder(director, total_profit),
                   y= total_profit,
                   text = paste(
                     "Director:", director,
                     "\nTotal Profit: $", total_profit, "B"),
        )) +
        geom_col(fill="blue") + coord_flip() +
        labs(x="", y="Total Profit") + ggtitle(paste("Top", input$top_obs, "Directors based on Total Profit"))
      
      plot_Dir_profit = ggplotly(plot_Dir_profit, tooltip = "text")
      print(plot_Dir_profit)
    }
    
    else if (input$radio_director == "weighted_vote_mean")
    {   
      plot_Dir_vote = director_data_table  %>% filter(weighted_vote_mean >0) %>%  arrange(desc(weighted_vote_mean)) %>% head(input$top_obs) %>%
        ggplot(aes(
          x=reorder(director, weighted_vote_mean), 
          y= weighted_vote_mean,
          text = paste(
            "Director:", director,
            "\nAverage Vote:", weighted_vote_mean ,'/10'),
        )) +
        geom_col(fill="blue") + coord_flip() +
        labs(x="", y="Weighted Vote") + ggtitle(paste("Top", input$top_obs, "Directors based on Voting Average"))
      plot_Dir_vote = ggplotly(plot_Dir_vote, tooltip = "text")
      print(plot_Dir_vote)
    }
    
    
    else if(input$radio_director == "popularity")
    {
      
      plot_Dir_popularity = director_data_table  %>% filter(popularity >0) %>%  arrange(desc(popularity)) %>% head(input$top_obs) %>%
        ggplot(aes(x=reorder(director, popularity),
                   y= popularity,
                   text = paste(
                     "Director:", director,
                     "\nPopularity Index:", popularity  ,'/10'),
        )) +
        geom_col(fill="blue") + coord_flip() +
        labs(x="", y="Popularity") + ggtitle(paste("Top", input$top_obs, "Directors based on Popularity Index"))
      
      # plot_Dir_popularity
      plot_Dir_popularity = ggplotly(plot_Dir_popularity, tooltip = "text")
      print(plot_Dir_popularity)
    }
    
  })
  
  #### top 'k' directors : data table ####
  
  
  
  
  output$top_k_director_table <- renderDataTable({
    
    if(input$top_k_director_show_data)
    {
      
      if(input$radio_director == "n")
      {
        director_tab <- director_data_table %>% filter(n > 0) %>% arrange(desc(n))  %>% head(input$top_obs)  %>% select(director, n) %>% dplyr::rename("Director" = "director", "No of total movies" = "n")
      }
      
      else if(input$radio_director == "total_revenue")
      {
        director_tab <- director_data_table %>% filter(total_revenue > 0) %>% arrange(desc(total_revenue))  %>% head(input$top_obs) %>% select(director, total_revenue) %>% dplyr::rename("Director" = "director", "Total Revenue (Billion)" = "total_revenue")
      }
      
      else if(input$radio_director == "total_budget")
      {
        director_tab <- director_data_table %>% filter(total_budget > 0) %>% arrange(desc(total_budget))  %>% head(input$top_obs) %>% select(director, total_budget) %>% dplyr::rename("Director" = "director", "Total Budget (Billion)" = "total_budget")
      }
      
      else if(input$radio_director == "total_profit")
      {  
        director_tab <- director_data_table %>% filter(total_profit > 0) %>% arrange(desc(total_profit))  %>% head(input$top_obs) %>% select(director, total_profit) %>% dplyr::rename("Director" = "director", "Total Profit (Billion)" = "total_profit")
      }
      
      else if (input$radio_director == "weighted_vote_mean")
      {  
        director_tab <- director_data_table %>% filter(weighted_vote_mean > 0) %>% arrange(desc(weighted_vote_mean))  %>% head(input$top_obs) %>% select(director, weighted_vote_mean) %>% dplyr::rename("Director" = "director", "Average Movie Rating (/10)" = "weighted_vote_mean")
      }
      
      else if (input$radio_director == "popularity")
      {
        director_tab <- director_data_table %>% filter(popularity > 0) %>% arrange(desc(popularity))  %>% head(input$top_obs) %>% select(director, popularity) %>% dplyr::rename("Director" = "director", "Average Movie Popularity Index (/10)" = "popularity")
      }
      
      DT::datatable(director_tab,
                    options =  list(pageLength = 10),
                    rownames = FALSE)
      
    }
  })
  
  
  #### Circular Bar plot of top 'k' directors  with Genres ####
  
  df_dir_genre_tab <- reactive({
    
    if(input$radio_director == "n")
    {
      plot_dir_df =  director_data_table %>% filter(n > 0) %>% arrange(desc(n)) %>% head(input$top_obs)
      df_dir_genre = director_genre_data %>% filter(director %in% plot_dir_df$director) %>% select(director, genre) %>% group_by(director, genre) %>% count() %>% arrange(director, freq)
    }
    
    else if(input$radio_director == "total_revenue")
    {
      plot_dir_df =  director_data_table %>% filter(total_revenue > 0) %>% arrange(desc(total_revenue)) %>% head(input$top_obs)
      df_dir_genre = director_genre_data %>% filter(director %in% plot_dir_df$director) %>% select(director, genre) %>% group_by(director, genre) %>% count() %>% arrange(director, freq)
    }
    
    else if(input$radio_director == "total_budget")
    {
      plot_dir_df =  director_data_table %>% filter(total_budget > 0) %>% arrange(desc(total_budget)) %>% head(input$top_obs)
      df_dir_genre = director_genre_data %>% filter(director %in% plot_dir_df$director) %>% select(director, genre) %>% group_by(director, genre) %>% count() %>% arrange(director, freq)
    }
    
    else if(input$radio_director == "total_profit")
    {
      plot_dir_df =  director_data_table %>% filter(total_profit > 0) %>% arrange(desc(total_profit)) %>% head(input$top_obs)
      df_dir_genre = director_genre_data %>% filter(director %in% plot_dir_df$director) %>% select(director, genre) %>% group_by(director, genre) %>% count() %>% arrange(director, freq)
    }
    
    if(input$radio_director == "total_revenue")
    {
      plot_dir_df =  director_data_table %>% filter(total_revenue > 0) %>% arrange(desc(total_revenue)) %>% head(input$top_obs)
      df_dir_genre = director_genre_data %>% filter(director %in% plot_dir_df$director) %>% select(director, genre) %>% group_by(director, genre) %>% count() %>% arrange(director, freq)
    }
    
    else if(input$radio_director == "weighted_vote_mean")
    {
      plot_dir_df =  director_data_table %>% filter(weighted_vote_mean > 0) %>% arrange(desc(weighted_vote_mean)) %>% head(input$top_obs)
      df_dir_genre = director_genre_data %>% filter(director %in% plot_dir_df$director) %>% select(director, genre) %>% group_by(director, genre) %>% count() %>% arrange(director, freq)
    }
    
    else if(input$radio_director == "popularity")
    {
      plot_dir_df =  director_data_table %>% filter(popularity > 0) %>% arrange(desc(popularity)) %>% head(input$top_obs)
      df_dir_genre = director_genre_data %>% filter(director %in% plot_dir_df$director) %>% select(director, genre) %>% group_by(director, genre) %>% count() %>% arrange(director, freq)
    }
    
    return(df_dir_genre)
    
  })
  
  #### Circular Bar plot of top 'k' casts  with Genres ####
  
  df_dir_genre_tab_cast <- reactive({
    
    if(input$radio_cast == "n")
    {
      plot_dir_df =  cast_data_table %>% filter(n > 0) %>% arrange(desc(n)) %>% head(input$top_obs)
      df_dir_genre = cast_genre_data %>% filter(cast %in% plot_dir_df$cast) %>% select(cast, genre) %>% group_by(cast, genre) %>% count() %>% arrange(cast, freq)
    }
    
    else if(input$radio_cast == "total_revenue")
    {
      plot_dir_df =  cast_data_table %>% filter(total_revenue > 0) %>% arrange(desc(total_revenue)) %>% head(input$top_obs)
      df_dir_genre = cast_genre_data %>% filter(cast %in% plot_dir_df$cast) %>% select(cast, genre) %>% group_by(cast, genre) %>% count() %>% arrange(cast, freq)
    }
    
    else if(input$radio_cast == "total_budget")
    {
      plot_dir_df =  cast_data_table %>% filter(total_budget > 0) %>% arrange(desc(total_budget)) %>% head(input$top_obs)
      df_dir_genre = cast_genre_data %>% filter(cast %in% plot_dir_df$cast) %>% select(cast, genre) %>% group_by(cast, genre) %>% count() %>% arrange(cast, freq)
    }
    
    else if(input$radio_cast == "total_profit")
    {
      plot_dir_df =  cast_data_table %>% filter(total_profit > 0) %>% arrange(desc(total_profit)) %>% head(input$top_obs)
      df_dir_genre = cast_genre_data %>% filter(cast %in% plot_dir_df$cast) %>% select(cast, genre) %>% group_by(cast, genre) %>% count() %>% arrange(cast, freq)
    }
    
    if(input$radio_cast == "total_revenue")
    {
      plot_dir_df =  cast_data_table %>% filter(total_revenue > 0) %>% arrange(desc(total_revenue)) %>% head(input$top_obs)
      df_dir_genre = cast_genre_data %>% filter(cast %in% plot_dir_df$cast) %>% select(cast, genre) %>% group_by(cast, genre) %>% count() %>% arrange(cast, freq)
    }
    
    else if(input$radio_cast == "weighted_vote_mean")
    {
      plot_dir_df =  cast_data_table %>% filter(weighted_vote_mean > 0) %>% arrange(desc(weighted_vote_mean)) %>% head(input$top_obs)
      df_dir_genre = cast_genre_data %>% filter(cast %in% plot_dir_df$cast) %>% select(cast, genre) %>% group_by(cast, genre) %>% count() %>% arrange(cast, freq)
    }
    
    else if(input$radio_cast == "popularity")
    {
      plot_dir_df =  cast_data_table %>% filter(popularity > 0) %>% arrange(desc(popularity)) %>% head(input$top_obs)
      df_dir_genre = cast_genre_data %>% filter(cast %in% plot_dir_df$cast) %>% select(cast, genre) %>% group_by(cast, genre) %>% count() %>% arrange(cast, freq)
    }
    
    return(df_dir_genre)
    
  })
  
  
  
  
  output$director_genre <- renderPlot({
    
    circular_bar_plot(df_dir_genre_tab())
    
  })
  
  
  output$top_k_director_genre_table <- DT::renderDataTable({
    
    if(input$top_k_director_genre_show_data)
    {
      DT::datatable(df_dir_genre_tab() %>% arrange(director, desc(freq)) %>% head(input$top_obs) %>% dplyr::rename("Director" = "director", "Genre" = "genre", "Counts" = "freq"),
                    options =  list(pageLength = 10),
                    rownames = FALSE)
      
    }
  })
  
  
  
  output$cast_genre <- renderPlot({
    
    circular_bar_plot_cast(df_dir_genre_tab_cast())
    
  })
  
  
  output$top_k_cast_genre_table <- DT::renderDataTable({
    
    if(input$top_k_cast_genre_show_data)
    {
      DT::datatable(df_dir_genre_tab_cast() %>% arrange(cast, desc(freq)) %>% head(input$top_obs_cast) %>% dplyr::rename("cast" = "cast", "Genre" = "genre", "Counts" = "freq"),
                    options =  list(pageLength = 10),
                    rownames = FALSE)
      
    }
  })
  
  
  
  #VIZ 5 Server Code
  
  output$single_var_plot <- renderPlotly({
    
    if(input$radio_director_continuous == "revenue")
    {
      
      pp = ggplot(continuous_variables, aes(x=revenue)) + 
        geom_histogram(aes(y=..density..), colour="black", fill="white")+
        geom_density(alpha = 0.2, fill="#FF6666") + geom_vline(aes(xintercept=mean(revenue)),
                                                               color="blue", linetype="dashed", size= 0.5)
      pp = ggplotly(pp, tooltip = c("revenue"))
      p1 = ggqqplot(continuous_variables, x = "revenue",  ggtheme = theme_pubclean()) 
      p1 = ggplotly(p1)
      subplot(pp, p1, nrows = 1) %>% layout(title = 'Revenue in Billion (US Dollar)',
                                            plot_bgcolor='#e5ecf6') %>%
        layout(showlegend = FALSE,
               annotations = list(
                 list(x = 0.2 , y = 1.0, text = "Density Plot", showarrow = F, xref='paper', yref='paper'),
                 list(x = 0.8 , y = 1.0, text = "QQ Plot", showarrow = F, xref='paper', yref='paper')))
    }
    
    
    else if(input$radio_director_continuous == "budget")
    {
      
      pp = ggplot(continuous_variables, aes(x=budget)) + 
        geom_histogram(aes(y=..density..), colour="black", fill="white")+
        geom_density(alpha = 0.2, fill="#FF6666") + geom_vline(aes(xintercept=mean(budget)),
                                                               color="blue", linetype="dashed", size= 0.5)
      pp = ggplotly(pp, tooltip = c("budget"))
      p1 = ggqqplot(continuous_variables, x = "budget",  ggtheme = theme_pubclean()) 
      p1 = ggplotly(p1)
      subplot(pp, p1, nrows = 1) %>% layout(title = 'Budget in Billion (US Dollar)',
                                            plot_bgcolor='#e5ecf6') %>%
        layout(showlegend = FALSE,
               annotations = list(
                 list(x = 0.2 , y = 1.0, text = "Density Plot", showarrow = F, xref='paper', yref='paper'),
                 list(x = 0.8 , y = 1.0, text = "QQ Plot", showarrow = F, xref='paper', yref='paper')))
    }
    
    
    else if(input$radio_director == "profit")
    {
      
      pp = ggplot(continuous_variables, aes(x=profit)) + 
        geom_histogram(aes(y=..density..), colour="black", fill="white")+
        geom_density(alpha = 0.2, fill="#FF6666") + geom_vline(aes(xintercept=mean(profit)),
                                                               color="blue", linetype="dashed", size= 0.5)
      pp = ggplotly(pp, tooltip = c("profit"))
      p1 = ggqqplot(continuous_variables, x = "profit",  ggtheme = theme_pubclean()) 
      p1 = ggplotly(p1)
      subplot(pp, p1, nrows = 1) %>% layout(title = 'Profit in Billion (US Dollar)',
                                            plot_bgcolor='#e5ecf6') %>%
        layout(showlegend = FALSE,
               annotations = list(
                 list(x = 0.2 , y = 1.0, text = "Density Plot", showarrow = F, xref='paper', yref='paper'),
                 list(x = 0.8 , y = 1.0, text = "QQ Plot", showarrow = F, xref='paper', yref='paper')))
    }
    
    else if(input$radio_director_continuous == "popularity")
    {
      
      pp = ggplot(continuous_variables, aes(x=popularity)) + 
        geom_histogram(aes(y=..density..), colour="black", fill="white")+
        geom_density(alpha = 0.2, fill="#FF6666") + geom_vline(aes(xintercept=mean(popularity)),
                                                               color="blue", linetype="dashed", size= 0.5)
      pp = ggplotly(pp, tooltip = c("popularity"))
      p1 = ggqqplot(continuous_variables, x = "popularity",  ggtheme = theme_pubclean()) 
      p1 = ggplotly(p1)
      subplot(pp, p1, nrows = 1) %>% layout(title = 'Popularity (/10)',
                                            plot_bgcolor='#e5ecf6') %>%
        layout(showlegend = FALSE,
               annotations = list(
                 list(x = 0.2 , y = 1.0, text = "Density Plot", showarrow = F, xref='paper', yref='paper'),
                 list(x = 0.8 , y = 1.0, text = "QQ Plot", showarrow = F, xref='paper', yref='paper')))
    }
    
  })
  
  # Summary Table
  
  output$summary_table <- renderPrint({
    
    if(input$radio_director_continuous == "revenue")
    {
      dt = continuous_variables %>% select(revenue) %>% filter(revenue>0) %>% na.omit() %>% summary() 
      print(dt)
    }
    
    if(input$radio_director_continuous == "budget")
    {
      dt = continuous_variables %>% select(budget) %>% filter(budget>0) %>% na.omit() %>% summary() 
      print(dt)
    }
    
    if(input$radio_director_continuous == "profit")
    {
      dt = continuous_variables %>% select(profit) %>% filter(profit>0) %>% na.omit() %>% summary() 
      print(dt)
    }
    
    if(input$radio_director_continuous == "popularity")
    {
      dt = continuous_variables %>% select(popularity) %>% filter(popularity>0) %>% na.omit() %>% summary() 
      print(dt)
    }
    
  })
  
  df_multi_plot <-  reactive({
    return(continuous_variables %>% dplyr::select(!!!input$checkGroupbox))
  })
  
  # observe(print(head(df_multi_plot())))
  
  output$multi_var_plot <- renderPlot({
    
    ggpairs(df_multi_plot(), title = "Correlogram") 
    
  })
  
  # Summary Table
  
  # output$summary_table <- renderPrint({
  #   
  #   if(input$radio_director == "revenue")
  #   {
  #     dt = continuous_variables %>% select(revenue) %>% filter(revenue>0) %>% na.omit() %>% summary() 
  #     print(dt)
  #   }
  #   
  #   if(input$radio_director == "budget")
  #   {
  #     dt = continuous_variables %>% select(budget) %>% filter(budget>0) %>% na.omit() %>% summary() 
  #     print(dt)
  #   }
  #   
  #   if(input$radio_director == "profit")
  #   {
  #     dt = continuous_variables %>% select(profit) %>% filter(profit>0) %>% na.omit() %>% summary() 
  #     print(dt)
  #   }
  #   
  #   if(input$radio_director == "popularity")
  #   {
  #     dt = continuous_variables %>% select(popularity) %>% filter(popularity>0) %>% na.omit() %>% summary() 
  #     print(dt)
  #   }
  #   
  # })
  
  
  # df_multi_plot <-  reactive({
  #   return(continuous_variables %>% dplyr::select(!!!input$checkGroupbox))
  # })
  # 
  # # observe(print(head(df_multi_plot())))
  # 
  # output$multi_var_plot <- renderPlot({
  #   
  #   ggpairs(df_multi_plot(), title = "Correlogram") 
  #   
  # })
  
  
  
  # Generate Heatmap Visualization 
  inputYear = reactive({input$timeSlider})
  years = df$Year
  yearIndex = reactive({which(inputYear() == years)[[1]]})
  countries = reactive({df$Countries.Code[yearIndex()] %>% str_replace_all("\\[|\\]","") %>% noquote()})
  countries2 = reactive({noquote(gsub(" ","", unlist(strsplit(countries(), ","))))})
  numMoviesProduced = reactive({df$Movies.Produced[yearIndex()] %>% str_replace_all("\\[|\\]","") %>% noquote()})
  numMoviesProduced2 = reactive({noquote(noquote(unlist(strsplit(numMoviesProduced(), ","))))})
  m <- list(
    l = 50,
    r = 50,
    b = 100,
    t = 100,
    pad = 4
  )
  dat = reactive({data.frame(
    countries = countries2(),
    numMovies = numMoviesProduced2()
  )})
  
  
  options = c("Blackbody", "Bluered", "Blues", "Cividis", "Earth", "Electric", 
              "Greens", "Greys", "Hot", "Jet", "Picnic", "Portland", "Rainbow", "RdBu", "Reds", "Viridis", "YlGnBu", "YlOrRd")
  output$heatmap <-renderPlotly({plot_ly(df, type='choropleth', locations=dat()$countries, z=dat()$numMovies, colorscale=options[15],reversescale=FALSE) %>% layout()})
  
  
}



# shiny app --------------------------------------------------------------------

shinyApp(ui, server)
