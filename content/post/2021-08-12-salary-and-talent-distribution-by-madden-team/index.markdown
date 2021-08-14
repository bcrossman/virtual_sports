---
title: Salary and Talent Distribution by Madden Team
author: ''
date: '2021-08-12'
slug: salary-and-talent-distribution-by-madden-team
categories: []
tags: ['data visualization', 'madden', 'nfl']
subtitle: ''
summary: ''
authors: []
lastmod: '2021-08-12T14:16:10-04:00'
featured: no
image:
  caption: ''
  focal_point: ''
  preview_only: no
projects: []
---

<script src="{{< blogdown/postref >}}index_files/htmlwidgets/htmlwidgets.js"></script>
<script src="{{< blogdown/postref >}}index_files/pymjs/pym.v1.js"></script>
<script src="{{< blogdown/postref >}}index_files/widgetframe-binding/widgetframe.js"></script>

## Intro

As much as I use R and Data Analysis in my day job, I find that sports games (like Madden) or fantasy sports (football, baseball, etc.) are the best sandbox to learn and practice data analysis skills. This post will focus on data visualization techniques using Madden data from the Madden 22 EA Sports games release to understand the distribution of talent and skill across teams.

## Package Load

``` r
library(tidyverse)
library(DT)
library(widgetframe)
library(treemapify)
library(testthat)
library(scales)
```

## Data Read

We will read in the raw data with a slight reorganization

``` r
player_data <- 
  read_csv("players.csv") %>% 
  select(sort(current_vars())) %>% 
  select(team, firstName, lastName, position, playerBestOvr,contractSalary, 
         contractBonus, contractLength, capHit, everything())
```

    ## Warning: `current_vars()` was deprecated in dplyr 0.8.4.
    ## Please use `tidyselect::peek_vars()` instead.

Below you’ll see a function I’m creating called “display\_table.” I like this little function as it gives people a downloadable data table. Perhaps it is the financial analyst in me, but I still do a lot of initial data review in excel and so do people I work with, so proving a knit HTML with these data tables is an easy bridge between R and a more excel based audience.

\*Note that for the blog I have to export to a widget but in your code you can just use display\_table(player\_data)

``` r
display_table <- function(x){
  table_output <- 
    DT::datatable(as.data.frame(x),
                  extensions = 'Buttons',
                  
                  options = list(
                    pageLength=5,
                    scrollX = TRUE,
                    dom = 'Bfrtip',
                    buttons = list(
                      list(extend = 'collection',
                           buttons = c('excel', 'csv'),
                           text = 'DOWNLOAD DATA')
                    )
                  )) 
  
  table_output
}

display_table(player_data) %>% widgetframe::frameWidget()
```

<div id="htmlwidget-1" style="width:100%;height:480px;" class="widgetframe html-widget"></div>
<script type="application/json" data-for="htmlwidget-1">{"x":{"url":"index_files/figure-html//widgets/widget_unnamed-chunk-2.html","options":{"xdomain":"*","allowfullscreen":false,"lazyload":false}},"evals":[],"jsHooks":[]}</script>

## Salary Distribution

Let’s start by creating a few cross-walks to help summarize and simplify the data.

The first will be grouping positions to a more simplified level

``` r
group_positions <- 
  list(
    data.frame(group_position = "OL", position = c("RG","LG","C", "LT", "RT")),
    data.frame(group_position = "DL", position = c("LE", "RE", "DT")),
    data.frame(group_position = "LB", position = c("LOLB", "ROLB", "MLB")),
    data.frame(group_position = "DB", position = c("SS", "FS", "CB")),
    data.frame(group_position = "WR", position = c("WR")),
    data.frame(group_position = "TE", position = c("TE")),
    data.frame(group_position = "QB", position = c("QB")),
    data.frame(group_position = "RB", position = c("HB", "FB")),
    data.frame(group_position = "K", position = c("K","P"))
  )

group_positions_df <- bind_rows(group_positions)

##Quick test to make sure you've provided a map for all positions in data

testthat::expect_true(all(unique(player_data$position) %in% group_positions_df$position))

simple_positions <- 
  list(
    data.frame(simple_position = "Offense", group_position = c("OL", "WR", "TE", "RB", "QB") ),
    data.frame(simple_position = "Defense", group_position = c("DL", "LB", "DB")),
    data.frame(simple_position = "Special_Team", group_position = c("K"))
  )

simple_positions_df <- bind_rows(simple_positions)

##Quick test to make sure you've provided a map for all positions in data 

testthat::expect_true(all(unique(group_positions_df$group_position) %in% simple_positions_df$group_position))

player_data_2 <- 
  player_data %>% 
  left_join(group_positions_df, by = "position") %>% 
  left_join(simple_positions_df, by = "group_position")
```

## Cap Hit or Salary + Bonus

We have to make a decision, how do we want to look at the investment a team is making in a player. This gets complex, and over my head, when you take in to account guaranteed and non-guaranteed money and unusually structured contracts. Let’s look at two metrics, the first is Madden’s variable called CapHit vs the sum of contract salary and contract bonus divided by contract length to give a rough estimate of “spent.”

``` r
player_data_2 %>%
  mutate(avg_cost = (contractSalary+contractBonus) / contractLength) %>% 
  mutate(diff = capHit-avg_cost) %>% 
  arrange(diff) %>% 
  top_n(10) %>% 
  select(team:capHit, avg_cost, diff)
```

    ## Selecting by diff

    ## # A tibble: 10 x 11
    ##    team     firstName lastName position playerBestOvr contractSalary contractBonus
    ##    <chr>    <chr>     <chr>    <chr>            <dbl>          <dbl>         <dbl>
    ##  1 Steelers Ezekiel   Elliott  HB                  96       59300000             0
    ##  2 Vikings  Jalen     Ramsey   CB                  99       71000000             0
    ##  3 Saints   Pen       Sewell   LT                  88       23780000             0
    ##  4 Seahawks Travis    Kelce    TE                  95       54000000             0
    ##  5 Bengals  Jordan    Love     QB                  99      117610000     148400000
    ##  6 Ravens   Trevor    Lawrence QB                  98       34780000      24600000
    ##  7 Patriots Justin    Fields   QB                  94       34060000      22600000
    ##  8 Saints   Alvin     Kamara   HB                  99       54800000      22580000
    ##  9 Jaguars  Joey      Bosa     LE                  99       72860000             0
    ## 10 Giants   Patrick   Mahomes  QB                  99      123850000             0
    ## # ... with 4 more variables: contractLength <dbl>, capHit <dbl>,
    ## #   avg_cost <dbl>, diff <dbl>

``` r
player_data_2 %>%
  mutate(avg_cost = (contractSalary+contractBonus) / contractLength) %>% 
  mutate(diff = capHit-avg_cost) %>% 
  arrange(diff) %>% 
  top_n(-10) %>% 
  select(team:capHit, avg_cost, diff)
```

    ## Selecting by diff

    ## # A tibble: 10 x 11
    ##    team      firstName lastName   position playerBestOvr contractSalary contractBonus
    ##    <chr>     <chr>     <chr>      <chr>            <dbl>          <dbl>         <dbl>
    ##  1 Browns    DeForest  Buckner    DT                  96       26460000             0
    ##  2 Browns    David     Bakhtiari  LT                  95       37920000             0
    ##  3 Cardinals Kyler     Murray     QB                  99      110350000     115600000
    ##  4 Lions     Taylor    Decker     LT                  78       18520000       4170000
    ##  5 49ers     Arik      Armstead   LE                  85       35240000      17280000
    ##  6 Texans    Zach      Cunningham MLB                 79       24400000       9330000
    ##  7 Bears     Laremy    Tunsil     LT                  89       50100000             0
    ##  8 Bears     Eddie     Jackson    FS                  87       31150000      13480000
    ##  9 Cowboys   Amari     Cooper     WR                  95       59200000      29520000
    ## 10 Texans    Will      Fuller V   WR                  81       22470000      12920000
    ## # ... with 4 more variables: contractLength <dbl>, capHit <dbl>,
    ## #   avg_cost <dbl>, diff <dbl>

``` r
player_data_2 %>%
  filter(simple_position != "Special_Team") %>% 
  mutate(avg_cost = (contractSalary+contractBonus) / contractLength) %>% 
  group_by(team, simple_position) %>% 
  summarise(avg_cost = sum(avg_cost))%>%
  ungroup() %>% 
  pivot_wider(names_from = simple_position, values_from = avg_cost) %>% 
  mutate(team = fct_reorder(team, Offense)) %>%
  pivot_longer(cols = -team, names_to = "simple_position", values_to = "avg_cost") %>% 
  ggplot(aes(x = team, y = avg_cost, group=simple_position, fill=simple_position))+
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(limits = c(0,NA), labels = unit_format(unit = "MM", scale = 1e-6,big.mark = ",")) +
  coord_flip()
```

    ## `summarise()` has grouped output by 'team'. You can override using the `.groups` argument.

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-5-1.png" width="672" />

We could also look at it on a percentage basis and stack the bar chart

``` r
player_data_2 %>%
  filter(simple_position != "Special_Team") %>% 
  mutate(avg_cost = (contractSalary+contractBonus) / contractLength) %>% 
  group_by(team, simple_position) %>% 
  summarise(avg_cost = sum(avg_cost))%>%
  group_by(team) %>% 
  mutate(avg_cost = avg_cost/sum(avg_cost)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = simple_position, values_from = avg_cost) %>%
  mutate(team = fct_reorder(team, Offense)) %>%
  pivot_longer(cols = -team, names_to = "simple_position", values_to = "avg_cost") %>% 
  ggplot(aes(x = team, y = avg_cost, group=simple_position, fill=simple_position))+
  geom_bar(stat = "identity", position = "stack") +
  scale_y_continuous(limits = c(0,NA), labels = scales::percent) +
  coord_flip()
```

    ## `summarise()` has grouped output by 'team'. You can override using the `.groups` argument.

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-6-1.png" width="672" />
