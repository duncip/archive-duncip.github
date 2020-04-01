---
title: How negative are the characters in The Office (US)
tags: [sentiment, r, eda]
style: fill
color: info # primary / secondary / success / danger / warning / info / light / dark (choose one only)

description: Sentiment analysis and general EDA of The Office (US) data
---


When I first saw this dataset come up on
[TidyTuesday](https://github.com/rfordatascience/tidytuesday) the main
thing I wanted to do was recreate the graph made by
[pudding.cool](https://pudding.cool/2017/08/the-office/) where they plot
character sentiment by season, prompting me to use Plotly for the first
time. What I like about the chart is how it cleanly shows the
character’s sentiment level throughout all seasons (where they had a
threshold of &gt;30 lines) and also portrays how negative the show is
overall. That said, whilst working on this I became slightly obsessed
and started doing all kinds of different EDA.

**To start my attempt**, we *inner join* the sentiments tibble we got
through tidytext. In this case, I use the Bing Liu lexicon to classify
it as just negative or positive. I then use *spread* to give negative
and positive their own column so I can calculate overall sentiment by
subtracting one from the other.

    sentiments = get_sentiments("bing")

    character_sentiment_season <- transcript_words %>%
      inner_join(sentiments, by = "word") %>% 
      count(season, character, sentiment) %>%
      spread(sentiment, n, fill = 0) %>%
      filter(character %in% main_characters &
               negative+positive > 50) %>%
      mutate(sentimentc = positive - negative)

    character_sentiment_season

    ## # A tibble: 81 x 5
    ##    season character negative positive sentimentc
    ##     <int> <chr>        <dbl>    <dbl>      <dbl>
    ##  1      1 Dwight          55       41        -14
    ##  2      1 Jim             32       53         21
    ##  3      1 Michael        180      175         -5
    ##  4      2 Dwight         174      131        -43
    ##  5      2 Jan             36       38          2
    ##  6      2 Jim            101      186         85
    ##  7      2 Kelly           27       28          1
    ##  8      2 Michael        563      576         13
    ##  9      2 Pam             94       87         -7
    ## 10      3 Andy            80       88          8
    ## # ... with 71 more rows

The below chunk took a bit of fiddling to get right, however, the main
aim was to recreate the plot in a static way before calling *ggplotly*
on it. *avg\_max\_sentiment* here was created to:

1.  avg\_sentiment used for ordering and to colour a character’s average
    sentiment

2.  max\_sentiment used for positioning of *geom\_text*

<!-- -->

    avg_max_sentiment <- character_sentiment_season %>%
      group_by(character) %>%
      summarise(avg_sentiment = mean(sentimentc),
                max_sentiment = max(sentimentc),
                plotly_pos = max(sentimentc) + 3)

    character_sentiment_season_adj <- character_sentiment_season %>%
      inner_join(avg_max_sentiment, by="character")


    # static plot completed
    p <- character_sentiment_season_adj %>%
      mutate(sent_dummy = ifelse(sentimentc < 0, "More Negative", "More Positive")) %>%
      mutate(character = reorder(character, avg_sentiment)) %>%
      ggplot(aes(character, sentimentc, hoverinfo = season)) +
      geom_point(colour = "#666666", size = 4, fill = "#f2f2f2", shape = 21) +
      coord_flip() +
      geom_hline(yintercept = 0, alpha = 0.2) +
      geom_point(aes(character, avg_sentiment, color=avg_sentiment), shape = 21, colour = "#262626", fill = "#ff9933", size = 4) + 
      geom_text(aes(label = character), 
                      size = 3, 
                      data = subset(character_sentiment_season_adj, sentimentc == max_sentiment),
                hjust = -0.5,
                vjust = 0.3) +
      labs(title = "How negative are The Office (US) characters?",
           subtitle = "Sentiment of each character based on Bing sentiment scores") +
      annotate(
        geom = "text",
        x = "Andy",
        y = -70,
        label = "More Negative",
        size = 3,
        colour = "#666666",
        alpha = .4
      ) +
      annotate(
        geom = "text",
        x = "Andy",
        y = 70,
        label = "More Positive",
        size = 3,
        colour = "#2b991f",
        alpha = .4
      ) +
      theme(axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            axis.text.x = element_blank(),
            axis.ticks = element_blank(),
            axis.line = element_blank(),
            panel.background = element_rect(fill = "white"),
            plot.title = element_text(hjust = 0.5, face = "bold"),
            plot.subtitle = element_text(hjust = 0.5),
            plot.caption = element_text(size = 6)
            )

    p

![](https://www.duncanpastoors.com/blog/2020/the_office_files/figure-markdown_strict/char_sentiment_season_graph-1.png)


Overall this comes pretty close to the chart created by pudding.cool -
without the interactivity, of course. The main challenges with creating
it were around getting the grid to disappear (and stay gone), especially
after going through knitr. Additionally, creating the annotations for
‘More Negative’ and ‘More Positive’ could be done better but this is
what I landed on. In terms of making it interactive, I tried creating a
plotly as per the below:

    ggplotly(p,
             tooltip = "season",
             width = 700,
             height = 700) %>%
      style(textposition="right")

<iframe width="1000" height="750" src="https://www.duncanpastoors.com/blog/2020/the_office_files/figure-markdown_strict/animated_char_sentiment.html" scrolling="no"></iframe>

There are plenty of issues here that need solving, however, for the sake
of this exercise I’ve left it where it was (especially as ggplotly
currently does not support hjust/vjust).

With that out of the way, I wanted to look at some other things. As
someone who is not a fan of the series in any way, I was interested in
finding out what seasons were successful and which episodes were fan
favourites.

### EDA

![](https://www.duncanpastoors.com/blog/2020/the_office_files/figure-markdown_strict/top20_episodes_office-1.png)

To me it’s quite interesting that there are high rated episodes in a
variety of seasons. The only two seasons that don’t seem to feature in
the top20 are season 1 and 8 (which judging by the reviews I quickly
read on metacritic, weren’t as well received as a whole anyway). Season
7 is at the top with 4 top rated episodes. Now that we’ve done the best,
it would be interesting to see if Season 1 and 8 appear in the worst…

![](https://www.duncanpastoors.com/blog/2020/the_office_files/figure-markdown_strict/bottom20_episodes_office-1.png)

Season 8 and 9 both have a high share on this list. What stands out to
me as well is that the Pilot is one of the lower rated episodes but
people still kept watching. Even though looking at episodes by
themselves is interesting, they definitely don’t tell the whole story.
Let’s create a line graph so we can get an idea of how each season
performed and what their best / worst episodes were.

![](https://www.duncanpastoors.com/blog/2020/the_office_files/figure-markdown_strict/office_rating_by_episode_annotated-1.png)

There’s a lot going on here but it does immediately give an idea of how
the series did throughout its lifetime. My immediate questions would be
why did people hate *The Banker* and *Get the Girl* so much? Looking it
up, turns out that people hated *The Banker* because it was a clip
episode and *Get the Girl* had Catherine Tate play a character that had
very mixed reviews. As someone who isn’t a fan of the show, I also
didn’t realise Steve Carell left but the *Goodbye, Michael* episode
immediately stands out (especially with a rating of 9.7). Of course, you
can also see the perceived quality of the show drops with Season 8
looking like a complete failure reviews-wise, and Season 9 only pulling
it back with the final few episodes.

As the line chart is pretty convoluted, I thought I’d try doing a
circular bar chart for the first time following the guidelines at
[r-graph-gallery](https://http://www.r-graph-gallery.com/297-circular-barplot-with-groups.html).
In the below chunk, I:

1.  create a dataframe with a filter on the review scores - this will
    create a column where I can easily check the condition

2.  use *case\_when* to pull out title if condition is true, leave blank
    if not

3.  create a few variables, all of the ones here come straight from the
    article above

4.  had to specify a font as default was causing issues

5.  create graph

<!-- -->

    office_bar_chart_data <- data.frame(1:nrow(office_ratings), office_ratings, office_ratings$imdb_rating >= 9 | office_ratings$imdb_rating <= 7.4)
    names(office_bar_chart_data)[1] <- "id"
    names(office_bar_chart_data)[8] <- "condition"


    office_bar_chart_data <- office_bar_chart_data %>%
      mutate(rated_title = case_when(condition == TRUE ~ title,
                                 condition == FALSE ~ ""))


    # label
    label_data <- office_bar_chart_data
    number_of_bars <- nrow(label_data)
    angle <- 90 - 360 * (label_data$id - 0.5) / number_of_bars
    label_data$hjust <- ifelse(angle < -90, 1, 0)
    label_data$angle <- ifelse(angle < -90, angle + 180, angle)

    windowsFonts(Arial=windowsFont("TT Arial"))
    font <- "Arial"

    p <- ggplot(office_bar_chart_data, aes(x = as.factor(id),
                                    y = imdb_rating,
                                    fill = as.factor(season))) +
      geom_bar(stat="identity") +
      coord_polar(start = 0) +
      ylim(-10, 15) +
      scale_fill_brewer(palette = "Set3") +
      labs(title = "The Office (US) episodes with rating",
           subtitle = "Colour represents Season, titles shown for episodes with score equal or greater than 9 OR scores below 7.5") +
      theme_minimal() +
      theme(text = element_text(family = font),
            axis.text = element_blank(),
            axis.title = element_blank(),
            panel.grid = element_blank(),
            plot.margin = unit(c(0, -5, 0, -5), "cm"),
            legend.position = "none",
            plot.title = element_text(face = "bold",
                                      colour = "black", size = 16, hjust = 0.5),
            plot.subtitle = element_text(face = "italic",
                                         colour = "black", size = 11, hjust = 0.5)
      )


    p + geom_text(data=label_data, 
                  aes(x=id, 
                      y=imdb_rating, 
                      label=rated_title, 
                      hjust=hjust), 
                  color="black", 
                  family=font,
                  fontface="bold",
                  alpha=0.6, 
                  size=2.5, 
                  angle = label_data$angle, 
                  inherit.aes = FALSE ) 

![](https://www.duncanpastoors.com/blog/2020/the_office_files/figure-markdown_strict/office_ratings_circular_bar-1.png)

The graph itself would perhaps not really work in the real world as it’s
quite hard to see general trend. That said, it’s quite good tos ee the
top/bottom rated episodes especially with the titles appended to it. The
main struggle with circular bar plot, for me, is getting the angle right
but the article linked was quite helpful. We did episode-specific so I
quickly wanted to a *facet* of seasons too, to show the drops.

![](https://www.duncanpastoors.com/blog/2020/the_office_files/figure-markdown_strict/office_ratings_through_seasons-1.png)

Important to note that Season 1 had fewer episodes which is why the line
is much shorter. Using the average, we can see that season 1 barely
lived up to what was yet to come and the show seems to peak between
season 2 - 5. It now also becomes clearer what effect Steve Carrell
leaving had on the series, with Season 8 not having a single episode
with an average rating or higher, and Season 9 only having 4. Lastly for
this section I wanted to have a quick look at what character has the
highest average rating.

    character_lines_rating %>%
      summarise(
        avg_rating = mean(imdb_rating),
        nb_episodes = n()
      ) %>%
      arrange(desc(avg_rating)) %>%
      ggplot(aes(fct_reorder(character, avg_rating), avg_rating, size = nb_episodes)) +
      geom_point(show.legend = FALSE) +
      coord_flip() +
      labs(x = "Character name",
           y = "Average rating",
           title = "Which character in The Office has the highest average episode rating?",
           subtitle = "Size represents number of episodes featured")

![](https://www.duncanpastoors.com/blog/2020/the_office_files/figure-markdown_strict/char_lines_rating-1.png)

Turns out that Charles, who is only in 7 episodes, has the highest
average rating. After having a look at who he is, it’s a character
played by Idris Elba and was received very well by the audiences.
Michael also has a high average episode rating and it probably helps
that he didn’t appear in the later 2 seasons (except for last episode).

### Text Analysis

So, what are the most commonly used words in The Office?

    transcript_words %>%
      count(word, sort=TRUE) %>%
      head(20) %>%
      ggplot(aes(fct_reorder(word, n), n)) +
      geom_col() +
      coord_flip()

![](https://www.duncanpastoors.com/blog/2020/the_office_files/figure-markdown_strict/office_common_words-1.png)

So apparently characters really like saying each other’s name which
isn’t that surprising in an office environment. To round it off, a quick
wordcloud showing the most used words in the office. I split them by
positive and negative because as the very first chart showed, there’s
definitely more negativity than positivity in the show. That said, the
most commonly used word is, apparently, “love”.

    office_sentiment_count <- transcript_words %>%
      inner_join(sentiments %>%
                   filter(sentiment == "positive"|
                          sentiment == "negative")) %>%
      count(word, sentiment, sort = TRUE)

    office_sentiment_count %>%
      acast(word~sentiment, value.var='n', fill = 0) %>%
      comparison.cloud(colors=c("#F8766D", "#00BFC4"), 
                       max.words=200)

![](https://www.duncanpastoors.com/blog/2020/the_office_files/figure-markdown_strict/office_word_cloud-1.png)
