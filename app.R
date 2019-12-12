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

top_director <- movies_df 

director_list <- movies_df %>% 
    filter(Major_Genre == 'Comedy') %>%
    group_by(Director) %>%
    summarise(Count = n()) %>%
    arrange(desc(Count)) %>%
    head(30) %>%
    pull(Director)
# Add the dropdown for genres
genreDropdown <- dccDropdown(
  id="Major_Genre",
  options = map(
    levels(as.factor(movies_df$Major_Genre)), function(x){
      list(label=x, value=x)
    }),
  value='Comedy',
  multi= FALSE,
  searchable=TRUE,
  style=list( "margin-left"= "30px",
              "margin-right"= "0px",
              "margin-top"= "0px",
              "margin-bottom"="0px",
              width="120px",
              height="15px",
              padding =  0)
)

directorDropdown <- dccDropdown(
  id="Director",
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
#' 
#' @return a data frame only contains movie information from the most productive 
#' directors in the selected genre  
#'

make_plot <- function(genre='Comedy'){

  top_director <<- movies_df %>% 
    filter(Major_Genre == genre) %>%
    group_by(Director) %>%
    summarise(Count = n()) %>%
    arrange(desc(Count)) %>%
    mutate(Major_Genre = genre) %>%
    head(30) %>%
    mutate(Director = as.factor(Director),
           Director = fct_reorder(Director, Count))

    
    top_director %>%
        plot_ly(
            y = ~Director, 
            x = ~Count, 
            type = 'bar'
        ) %>%
        layout(
            height = 600,
            width = 600,
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
            ),
            title = list(
                text = paste("<b>Top", nrow(top_director), "most productive directors in:", genre, "</b>"),
                font = list(size = 16)
            ) 
        ) 
}
    graph <- dccGraph(
      id = 'gap-graph',
      figure=make_plot() # gets initial data using argument defaults
    )

make_plot2 <- function(directors = director_list){
    top_df <- movies_df %>% 
      inner_join(top_director, by = c("Major_Genre", "Director")) %>%
      filter(Director %in% directors)

    p1 <- top_df %>%
        plot_ly(
            x = ~Year, 
            y = ~IMDB_Rating, 
            color = ~Director,
            legendgroup = ~Director,
            type = 'scatter', 
            mode = 'lines+markers'
        ) %>%
        layout(
            height = 600,
            width = 600,
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

    p2 <- top_df %>%
        plot_ly(
            x = ~Year, 
            y = ~Profit_Million, 
            color = ~Director,
            legendgroup = ~Director,
            showlegend = FALSE,
            type = 'scatter', 
            mode = 'lines+markers'
        ) %>%
        layout(
            height = 600,
            width = 600,
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


    p <- subplot(p1, p2, nrows = 2, 
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
}
    graph2 <- dccGraph(
      id = 'IMDB-graph',
      figure=make_plot2() # gets initial data using argument defaults
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
                "margin-top"= "30px")
    ),
    htmlDiv(
      graph2,
      style=list(float='right')
    ),
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
    make_plot(genre_value)
    
})

app$callback(
  output=list(id = 'Director', property='options'),
  params=list(input(id = 'Major_Genre', property='value')),
  function(genre_value){

    director_list <- movies_df %>% 
      filter(Major_Genre == genre_value) %>%
      group_by(Director) %>%
      summarise(Count = n()) %>%
      arrange(desc(Count)) %>%
      head(30)  %>%
      pull(Director)

    options = map(
      levels(as.factor(director_list)), function(x){
        list(label=x, value=x)
    })
  }
)

app$callback(
  output=list(id = 'Director', property='value'),
  params=list(input(id = 'Major_Genre', property='value')),
  function(genre_value){
    
    movies_df %>% 
      filter(Major_Genre == genre_value) %>%
      group_by(Director) %>%
      summarise(Count = n()) %>%
      arrange(desc(Count)) %>%
      head(1) %>%
      pull(Director)
  }
)

app$callback(
  output=list(id = 'IMDB-graph', property='figure'),
  params=list(
              input(id = 'Director', property='value')),
  function(director_value){
    #' Takes in the genre and director and calls make_plot to update the plot
    make_plot2(director_value)
    
})



app$run_server()