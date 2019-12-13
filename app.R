# See Docs here: https://dash-bootstrap-components.opensource.faculty.ai
#import dash_bootstrap_components as dbc
library(dash)
library(dashCoreComponents)
library(dashHtmlComponents)
library(dplyr)
library(purrr)
library(readr)
library(plotly)


app <- Dash$new(external_stylesheets = "https://codepen.io/chriddyp/pen/bWLwgP.css")

# Load data 
movies_df <- read_csv('https://raw.githubusercontent.com/zouwenjiao/DSCI532_GROUP104_Movies_R/master/data/clean/movies_clean_df.csv', col_types = cols(X1 = col_skip()))

#' 
#' Finds the number of movies of the most productive directors in the selected genre.
#' 
#' @param genre string the selected genre
#' 
#' @return a data frame only contains movie information from the most productive 
#' directors in the selected genre  
#'
get_top_director <- function(genre = 'Comedy'){
  
  movies_df %>% 
    filter(Major_Genre == genre) %>%
    group_by(Director) %>%
    summarise(Count = n()) %>%
    arrange(desc(Count)) %>%
    mutate(Major_Genre = genre) %>%
    head(30) %>%
    mutate(Director = as.factor(Director),
           Director = reorder(Director, Count))
  
}

# Get the list of directors that are most productive in the selected genre
director_list <- get_top_director() %>%
  pull(Director)

# Add the dropdown for genres
genreDropdown <- dccDropdown(
  id="Major_Genre",
  options = map(
    levels(as.factor(movies_df$Major_Genre)), function(x){
      list(label=x, value=x)
    }),
  value = 'Comedy',
  multi = FALSE,
  searchable = TRUE,
  style = list( "margin-left"= "30px",
                "margin-right"= "0px",
                "margin-top"= "0px",
                "margin-bottom"="0px",
                width="180px",
                height="15px",
                padding =  0)
)

# Add the dropdown for directors
directorDropdown <- dccDropdown(
  id = "Director",
  options = map(
    levels(as.factor(director_list)), function(x){
      list(label=x, value=x)
    }),
  value=director_list[1],
  multi= TRUE,
  searchable=TRUE,
  style=list( "margin-left"= "30px",
              "margin-right"= "0px",
              "margin-top"= "0px",
              "margin-bottom"="0px",
              width="650px",
              height="15px",
              padding =  0)
)


#' 
#' Finds the number of movies of the most productive directors in the selected genre.
#' 
#' @param genre string the selected genre
#' @param directors string list the selected directors
#' 
#' @return a data frame only contains movie information from the most productive 
#' directors in the selected genre  
#'
make_plot <- function(genre='Comedy', directors = director_list){
  
  top_director <- get_top_director(genre)
  
  top_df <- movies_df %>% 
    inner_join(top_director, by = c("Major_Genre", "Director")) %>%
    filter(Director %in% directors) %>%
    arrange(Year)
  
  p1 <- top_director %>%
    plot_ly(
      y = ~Director, 
      x = ~Count, 
      height = 500,
      width = 600,
      type = 'bar',
      showlegend = FALSE
    ) %>%
    layout(
      font = list(
        size = 12
      ),
      xaxis = list(
        showline = T, 
        showgrid = F,
        title = '<b>Number of Movies</b>'
      ),
      yaxis = list(
        showline = T,
        showgrid = T,
        title = '<b>Director</b>'
      ),
      yaxis = list(
        categoryorder = "array",
        categoryarray = levels(top_director$Director)
      )
    )
  
  p2 <- top_df %>%
    plot_ly(
      x = ~Year, 
      y = ~IMDB_Rating, 
      text = ~Title,      
      color = ~Director,
      legendgroup = ~Director,
      height = 500,
      width = 600,
      type = 'scatter', 
      mode = 'lines+markers',
      hovertemplate = paste('Title: %{text} <br>',
                            'Year: %{x} <br>',
                            'IMDB Rating: %{y}')  
    ) %>%
    layout(
      
      font = list(
        size = 12
      ),
      xaxis = list(
        showline = T, 
        showgrid = F,
        title = '<b>Year</b>'
      ),
      yaxis = list(
        showline = T,
        showgrid = T,
        title = '<b>IMDB Rating (1-10)</b>'
      )
    )
  
  p3 <- top_df %>%
    plot_ly(
      x = ~Year, 
      y = ~Profit_Million, 
      text = ~Title,      
      color = ~Director,
      legendgroup = ~Director,
      showlegend = FALSE,
      height = 500,
      width = 600,
      type = 'scatter', 
      mode = 'lines+markers',
      hovertemplate = paste('Title: %{text} <br>',
                            'Year: %{x} <br>',
                            'Profit: %{y:.2f} Million USD') 
    ) %>%
    layout(
      font = list(
        size = 12
      ),
      xaxis = list(
        showgrid = F,
        showgrid = F,
        title = '<b>Year</b>'
      ),
      yaxis = list(
        showline = T,
        showgrid = T,
        title = '<b>Profit (M USD)</b>'
      ),
      legend = list( 
        
      )
    )
  
  
  p23 <- subplot(p2, p3, nrows = 2, 
                 titleY = T, titleX = T, 
                 margin = 0.1, heights = c(0.5, 0.5)) %>%
    layout(annotations = list(
      list(
        font = list(size = 16), 
        text = "<b>IMDB Rating by Director</b>",
        x = 0.1, 
        y = 1.0, 
        xref = "paper", 
        yref = "paper", 
        xanchor = "left", 
        yanchor = "bottom", 
        showarrow = FALSE
      ), 
      list(
        font = list(size = 16), 
        text = "<b>Worldwide Profit by Director</b>",
        x = 0.1, 
        y = 0.4, 
        xref = "paper", 
        yref = "paper", 
        xanchor = "left", 
        yanchor = "bottom", 
        showarrow = FALSE
      )
    )
    )
  
  p123 <- subplot(p1, p23, margin = 0.1, titleY = T, titleX = T,nrows = 1) %>%
    layout(
      height = 800,
      width = 1200,
      annotations = list(
        list(
          font = list(size = 16), 
          text = paste("<b>Top", nrow(top_director), "most productive directors in:", genre, "</b>"),
          x = -0.1, 
          y = 1.0, 
          xref = "paper", 
          yref = "paper", 
          xanchor = "left", 
          yanchor = "bottom", 
          showarrow = FALSE
        )
      )
    )
}
graph <- dccGraph(
  id = 'graph',
  figure=make_plot() # gets initial data using argument defaults
)

# color dictionary
colors <- list(
  deep_blue = '#2e4366',
  white = '#ffffff',
  light_blue = '#e6edfa',
  blue = '#023863'
)

# Set up the app layout
app$layout(
  htmlDiv(
    list(
      htmlH1("Welcome to the Directors Production Tracker App",
             style = list(color = colors$deep_blue,
                          textAlign = 'center',
                          'font-family'='Monospace',
                          backgroundColor = colors$light_blue, 
                          "margin-left"= "5px",
                          "margin-right"= "5px",
                          "margin-top"= "5px",
                          "margin-bottom"="0px",
                          'font-size'='40px',
                          padding =  15)),                 
      htmlH4("Explore directors by their production for your next movie!",
             style = list(
               textAlign = 'center',
               backgroundColor = colors$light_blue,
               'font-style'='italic',
               'font-family'='Monospace', 
               "margin-left"= "5px",
               "margin-right"= "5px",
               "margin-top"= "0px", 
               "margin-bottom"="10px",
               'font-size'='18px',
               padding = 10)),
      dccMarkdown("**Instructions**",
                  style = list(
                    #textAlign = 'center',
                    #backgroundColor = colors$light_blue,
                    #'font-style'='italic',
                    'font-family'='George', 
                    "margin-left"= "25px",
                    "margin-top"= "0px", 
                    "margin-bottom"="0px",
                    'font-size'='21px',
                    padding = 0)),
      #dccMarkdown("**Hold shift and click on one or more bars** on the bar chart to choose directors to view movie ratings and profits."),
      
      #selection components go here
      htmlLabel("Step1: Select a genre to see most productive directors",
                style = list(
                  #textAlign = 'center',
                  #backgroundColor = colors$light_blue,
                  #'font-style'='italic',
                  'font-family'='George', 
                  "margin-left"= "30px",
                  "margin-top"= "0px", 
                  "margin-bottom"="10px",
                  'font-size'='18px',
                  padding = 0)),
      genreDropdown,
      htmlLabel("Step2: Select one or more directors to see ratings and profits of their movies",
                style = list(
                  #textAlign = 'center',
                  #backgroundColor = colors$light_blue,
                  #'font-style'='italic',
                  'font-family'='George', 
                  "margin-left"= "30px",
                  "margin-top"= "30px", 
                  "margin-bottom"="10px",
                  'font-size'='18px',
                  padding = 0)),
      directorDropdown,
      dccMarkdown("Tip: Hover over a point for more details         ",
                  style = list(
                    #color = colors$blue,
                    textAlign = 'right',
                    #backgroundColor = colors$light_blue,
                    #'font-style'='italic',
                    'font-family'='Monospace', 
                    "margin-right"= "25px",
                    "margin-top"= "0px", 
                    "margin-bottom"="10px",
                    'font-size'='12px',
                    padding = 0)),
      #    htmlIframe(height=15, width=10, style=list(borderWidth = 0)), #space
      #end selection components
      htmlDiv(
        graph,
        style=list(float='left',
                   "margin-top"= "30px",
                   "margin-bottom"= "30px")
      ),
      
      htmlIframe(height=20, width=10, style=list(borderWidth = 0)), #space

      htmlDiv(
        htmlLabel('This Dash app was made collaboratively by the DSCI 532 class in 2019/20!'),
        style=list(
                    float='left',
                    'margin-left'='30px',
                    'margin-bottom'='30px'
        )
      )
    )
  )
)


app$callback(
  output=list(id = 'graph', property='figure'),
  params=list(input(id = 'Major_Genre', property='value'),
              input(id = 'Director', property='value')),
  function(genre_value, director_value){
    #' Takes in the genre and director and calls make_plot to update the plot
    make_plot(genre_value, director_value)
    
  })

app$callback(
  output=list(id = 'Director', property='options'),
  params=list(input(id = 'Major_Genre', property='value')),
  function(genre_value){
    
    director_list <- get_top_director(genre_value) %>%
      pull(Director)
    
    map(
      levels(as.factor(director_list)), function(x){
        list(label=x, value=x)
      })
  }
)

app$callback(
  output=list(id = 'Director', property='value'),
  params=list(input(id = 'Major_Genre', property='value')),
  function(genre_value){
    
    director_list <- get_top_director(genre_value) %>%
      head(1) %>%
      pull(Director)
  }
)

app$run_server(host = "0.0.0.0", port = Sys.getenv('PORT', 8050))
