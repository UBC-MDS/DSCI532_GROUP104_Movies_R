import pandas as pd
import altair as alt

def make_plot(movie_df, genre='Action'):
    """
    Plots a bar chart of the top 30 most productive directors in the selected genre and 
    two line charts showing the ratings and profits of movies in the selected genre of 
    these 30 directors across the years.

    Parameters
    ----------
    movie_df: pandas.DataFrame
        The data frame to work on
    genre: string
        The selected genre

    Returns
    -------
    altair.vegalite.v3.api.HConcatChart
        a bar chart of the top 30 most productive directors in the selected genre and 
    two line charts showing the ratings and profits of related movies.
    """

    def mds_special():
        """
        Sets the Altair plot theme.
        """

        font = "Arial"
        axisColor = "#000000"
        gridColor = "#DEDDDD"
        return {
            "config": {
                "title": {
                    "fontSize": 24,
                    "font": font,
                    "anchor": "start",  # equivalent of left-aligned.
                    "fontColor": "#000000"
                },
                'view': {
                    "height": 300,
                    "width": 400
                },
                "axisX": {
                    "domain": True,
                    # "domainColor": axisColor,
                    "gridColor": gridColor,
                    "domainWidth": 1,
                    "grid": False,
                    "labelFont": font,
                    "labelFontSize": 12,
                    "labelAngle": 0,
                    "tickColor": axisColor,
                    "tickSize": 5,  # default, including it just to show you can change it
                    "titleFont": font,
                    "titleFontSize": 16,
                    "titlePadding": 10,  # guessing, not specified in styleguide
                    "title": "X Axis Title (units)",
                },
                "axisY": {
                    "domain": False,
                    "grid": True,
                    "gridColor": gridColor,
                    "gridWidth": 1,
                    "labelFont": font,
                    "labelFontSize": 14,
                    "labelAngle": 0,
                    # "ticks": False, # even if you don't have a "domain" you need to turn these off.
                    "titleFont": font,
                    "titleFontSize": 16,
                    "titlePadding": 10,  # guessing, not specified in styleguide
                    "title": "Y Axis Title (units)",
                    # titles are by default vertical left of axis so we need to hack this
                    # "titleAngle": 0, # horizontal
                    # "titleY": -10, # move it up
                    # "titleX": 18, # move it to the right so it aligns with the labels
                },
            }
        }

    # register the custom theme under a chosen name
    alt.themes.register('mds_special', mds_special)
    # enable the newly registered theme
    alt.themes.enable('mds_special')
    # alt.themes.enable('none') # to return to default

    def get_top_df(genre):
        """
        Finds the number of movies of the most productive directors in the selected genre and 
        movie information from the most productive directors in the selected genre.

        Parameters
        ----------
        genre: string
            The selected genre

        Returns
        -------
        (pandas.DataFrame, pandas.DataFrame)
            The data frame only contains movie information from the most productive directors in the selected genre and 
            The data frame contains the number of movies of the most productive directors in the selected genre  
            
        """

        mdfg = movie_df.groupby('Major_Genre')
        top_director = (mdfg.get_group(genre)
                        .groupby('Director')
                        .count()
                        .sort_values(by='Title', ascending=False)
                        .head(30)
                        .reset_index()
                        .iloc[:, :2]
                        .rename(columns={'Title': 'Count'}))
        top_director['Major_Genre'] = genre
        top_df = pd.merge(movie_df,
                          top_director,
                          how="inner",
                          on=['Major_Genre', 'Director'])
        return (top_director, top_df)

    # get information from the 30 most productive directors in the selected genre
    top_director, top_df = get_top_df(genre)
    brush = alt.selection(type='multi', fields=['Director'], init={
                          'Director': top_director.iloc[0, 0]})

    # plot a bar chart of the top 30 most productive directors in the selected genre
    chart_1 = alt.Chart(top_director).mark_bar().encode(
        alt.Y('Director:N',
              title='Director',
              sort=alt.EncodingSortField(field="Count:Q",
                                         order="ascending")),
        alt.X('Count:Q',
              title='Number of movies'),
        opacity=alt.condition(brush, alt.value(0.75), alt.value(0.05)),
    ).properties(
        title='Top ' + str(min(30, top_director.shape[0])) + ' most productive directors in ' + genre,
        width=200,
        height=650
    ).add_selection(brush)

    # plot a line chart of movie ratings in the selected genre of 
    # these 30 directors across the years
    chart_2 = alt.Chart(top_df).mark_line(point=True).encode(
        alt.X("Year:O",
              axis=alt.Axis(title="Year")),
        alt.Y("IMDB_Rating:Q",
              axis=alt.Axis(title="IMDB Rating (1-10)"),
              scale=alt.Scale(zero=False)),
        alt.Color('Director:N'),
        opacity=alt.condition(brush, alt.value(0.75), alt.value(0.01)),
        tooltip=['Title', 'Major_Genre', 'Director', 'IMDB_Rating']
    ).properties(
        title='IMDB Rating',
        width=400,
        height=280
    ).interactive().transform_filter(brush)

    # plot a line chart of movie profits in the selected genre of 
    # these 30 directors across the years
    chart_3 = alt.Chart(top_df).mark_line(point=True).encode(
        alt.X("Year:O",
              axis=alt.Axis(title="Year")),
        alt.Y("Profit_Million:Q",
              axis=alt.Axis(title="Profit (M USD)"),
              scale=alt.Scale(zero=False)),
        alt.Color('Director:N'),
        opacity=alt.condition(brush, alt.value(0.75), alt.value(0.01)),
        tooltip=['Title', 'Major_Genre', 'Director', 'IMDB_Rating']
    ).properties(
        title='Worldwide Profit',
        width=400,
        height=280
    ).interactive().transform_filter(brush)

    return chart_1 | (chart_2 & chart_3)
