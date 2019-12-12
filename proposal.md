### 1. Motivation and Purpose

Investing in the movie business is like gambling: it may sound glamorous but can be very risky. People sink billions into the movie business each year, fancied by the glam of movies. However, most movies lose money according to a recent post [1]. This year, Disney alone announced a 170 million US dollars loss after *X-Men: Dark Phoenix* [2]. Investors should calm down and study the movie market before diving into the business. Knowing more about the directors' reputation and experience, investors can make better decisions on their investments. To help investors choose the best candidate directors and related studies in the movie industry, we propose creating a data visualization app to organize and visualize the facts of directors. With our app, the investors and researchers can select a movie genre to find out the top 30 film producing directors of that genre in a given time frame, and then review and compare individual director's statistics by filtering.

### 2. Description of the data

The dataset we are visualizing comes from the [Vega dataset on Movies](https://raw.githubusercontent.com/vega/vega-datasets/master/data/movies.json). The data was compiled by Vega to create a repository for example data to be used in their other packages as stated on their [Vega Sources page](https://github.com/vega/vega-datasets/blob/master/sources.md).

This dataset contains 3,200 movies with 16 variables for each movie. Due to the time restriction of this project, we will only be visualizing six variables.

Four of the variables we will be analysing describe the movie attributes including the title of the movie (`Title`), the director of the movie (`Director`), which genre the movie belongs to (`Major_Genre`), and the year when the movie was originally released (`Release_Date`). 

The other two variables describe the outcomes of the movie through ratings (`IMDB_Ratings`) or overall profit (`Worldwide_Profit`). The dataset contains ratings from different sources, but we chose to visualize `IMDB_Rating` as it has 600 more values in the dataset than Rotten Tomatoes and relies on audience reviews. We will create the additional variable `Worldwide_Profit` to summarize the profit for each movie calculated by taking the difference of the variables `Worldwide_Gross` (overall gross income) and `Production_Budget` (amount spent on making the movie) to get the movie's profit. 

The (`Major_Genre`) category has 12 categories such as action, drama, comedy, etc. Each movie is assigned one genre and the genres cannot overlap. For example, comedy and romantic comedy are separate categories and films can only appear in one of the categories. 

### 3. Research questions and Usage Scenarios

Research Questions:

1. Who are the most productive directors for a given genre?
    - This research question will allow users to understand the top directors in their genre of choice. This will narrow down the search for directors since there are thousands of options to explore further analysis. 
2. What are the profits of the movies for a specific director?
    - This research question will allow users to understand the profits of one or more director(s) they might be interested in choosing for their movie and compare with other directors' profits. 
3. What are the ratings of the movies for a specific director?
    - This research question will allow users to understand the IMDB ratings of one or more director(s) the user might be interested in choosing for their movie and compare with other directors' IMDB rating. 

Usage Scenario:

Joyce is a graduate student who majors in Global Communication and she has been working on her paper to analyze the performance of directors in Drama. She wants access to a dataset to explore the performance of movies from different directors and narrow down her research scope to some specific directors. When Joyce logs on to the "Directors Production Tracker app", she will see an overview of the top 30 productive directors in Drama, ordered by the number of movies they have produced. She can choose any director she is interested in and filter out variables for that director's movies. For example, she can choose "Adrian Lyne", set the time to "1980-2000" and choose "Drama". Then, she will see the ratings and profits for each Drama produced by Adrian Lyne from 1980 to 2000. She will find that Adrian Lyne made 3 dramas, *Flashdance*, *Nine ½ Weeks* and *Lolita*, during that period. *Flashdance* received a rating of 5.6 and earned approximately 194 million US dollars worldwide. *Nine ½ Weeks* received a rating of 5.4 and earned a deficit profit, around 11 million US dollars worldwide. *Lolita* received a rating of 6.7 but lost 53 million US dollars worldwide. Now, she gets an overview of the performance of Adrian Lyne's dramas from 1980 to 2000. She can dig into these three dramas further for more information, such as why *Lolita* received the highest rating while losing money, how to keep a balance between ratings and profits when evaluating a movie's performance, etc. She can use the dataset to decide on the director, the genre, and the timespan she wants to focus on. The dataset also inspires her to conduct a follow-up study to add further depth to her research.

### Reference

1. Schuyler Moore (2019) [Most Films Lose Money!](https://www.forbes.com/sites/schuylermoore/2019/01/03/most-films-lose-money/#655084d2739f).

2. Adam White (2019) [Disney announces rare $170 million financial loss after X-Men: Dark Phoenix flop](https://www.independent.co.uk/arts-entertainment/films/news/disney-loss-stock-share-price-x-men-dark-phoenix-box-office-flop-a9046416.html).
 
