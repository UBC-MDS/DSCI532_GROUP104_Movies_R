# See Docs here: https://dash-bootstrap-components.opensource.faculty.ai
#import dash_bootstrap_components as dbc
library(dash)
library(dashCoreComponents)
library(dashHtmlComponents)
library(dashTable)
library(tidyverse)
library(plotly)


app <- Dash$new(external_stylesheets = "https://codepen.io/chriddyp/pen/bWLwgP.css")

# Load data 
movies_df <- read_csv('data/clean/movies_clean_df.csv', col_types = cols(X1 = col_skip()))

# Get genre list
#genres = map(unique(movies_df$Major_Genre), as.character)

# Get director list
#directors = map(unique(movies_df$Major_Genre), as.character)

# Add the dropdown for genres
genreDropdown <- dccDropdown(
  id="Major_Genre",
  options = map(
    levels(as.factor(movies_df$Major_Genre)), function(x){
      list(label=x, value=x)
    }),
  value='Comedy',
  multi= FALSE,
  searchable=TRUE
)

directorDropdown <- dccDropdown(
  id="Director",
  options = map(
    levels(as.factor(movies_df$Director)), function(x){
      list(label=x, value=x)
    }),
  value='M. Night Shyamalan',
  multi= TRUE,
  searchable=TRUE
)

#' 
#' Finds the number of movies of the most productive directors in the selected genre.
#' 
#' @param df data frame the data frame to work on
#' @param num int the number of directors to keep in each genre
#' @param genre string the selected genre
#' 
#' @return a data frame only contains movie information from the most productive 
#' directors in the selected genre  
#'

all_genres <- unique(movies_df$Major_Genre)
all_directors <- unique(movies_df$Director)

make_plot <- function(data = movies_df, genres=all_genres){
  data1 <- data %>% 
    filter(Major_Genre %in% genres) %>%
    group_by(Director) %>%
    summarise(Count = n()) %>%
    arrange(desc(Count)) %>%
    head(30)
  
   data_plot <- data1 %>% 
    ggplot(aes(x = reorder(Director, Count), y = Count)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    labs(y = "Number of Movies", x = "Director") +
    ggtitle("Top 30 most productive directors in: ")
    
    ggplotly(data_plot, width = 500, height = 1000)
}
    graph <- dccGraph(
      id = 'gap-graph',
      figure=make_plot() # gets initial data using argument defaults
    )

make_plot2 <- function(data = movies_df, directors=all_directors){
    top_df <- data %>% 
      filter(Director %in% directors)
    
    IMDB <- top_df %>%
      ggplot(aes(x = Year, y = IMDB_Rating, colour = Director)) +
      geom_line() +
      geom_point() +
      labs(y = "IMDB Rating (Scale 1-10)", title = "IMDB Rating by Director") +
      expand_limits(y = 1)
    
    ggplotly(profit, width = 500, height = 1000)
}
    graph2 <- dccGraph(
      id = 'IMDB-graph',
      figure=make_plot2() # gets initial data using argument defaults
    )
    
# Set up the app layout
app$layout(
  htmlDiv(
  list(
    htmlH1("Welcome to the Directors Production Tracker App"),
    htmlH4("Explore different directors based on the number of movies they produce in a genre to find your director for your next movie."),
    htmlH3("**Instructions**"),
    htmlH4("**Hold shift and click on one or more bars** on the bar chart to choose directors to view movie ratings and profits."),
    htmlH4("Hover over a point for more details"),
    #selection components go here
#    htmlLabel("Select a genre"),
    genreDropdown,
    directorDropdown,
#    htmlIframe(height=15, width=10, style=list(borderWidth = 0)), #space
    #end selection components
    graph,
    graph2,
    htmlIframe(height=20, width=10, style=list(borderWidth = 0)) #space
 #   dccMarkdown("[Data Source](https://cran.r-project.org/web/packages/gapminder/README.html)")
  
    )
  )
)


app$callback(
  output=list(id = 'gap-graph', property='figure'),
  params=list(input(id = 'Major_Genre', property='value')),
  function(genre_value){
   #' Takes in the genre and director and calls make_plot to update the plot
    make_plot(movies_df, genre_value)
    
})

app$callback(
  output=list(id = 'IMDB-graph', property='figure'),
  params=list(input(id = 'Major_Genre', property='value'),
              input(id = 'Director', property='value')),
  function(genre_value, director_value){
    #' Takes in the genre and director and calls make_plot to update the plot
    make_plot2(movies_df, director_value)
    
})


app$run_server()