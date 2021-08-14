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

As much as I use R and Data Analysis in my day job, I find that sports games (like Madden) or fantasy sports (football, baseball, etc.) are the best sandbox to learn and practice data analysis skills. This post will focus on data visualization techniques using Madden data from the Madden 21 EA Sports game to understand the distribution of talent and skill across teams.

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

First decision, how do we want to look at the investment a team is making in a player. This gets complex, and over my head, when you take in to account guaranteed and non-guaranteed money and unusually structured contracts. Let’s look at two metrics, the first is Madden’s variable called CapHit vs the sum of contract salary and contract bonus divided by contract length to give a rough estimate of “spent.”

``` r
player_data_2 %>%
  mutate(avg_cost = (contractSalary+contractBonus) / contractLength) %>% 
  mutate(diff = capHit-avg_cost) %>% 
  arrange(diff) %>% 
  top_n(10) %>% 
    select(team:playerBestOvr, capHit, avg_cost, diff) %>% 

bind_rows(

player_data_2 %>%
  mutate(avg_cost = (contractSalary+contractBonus) / contractLength) %>% 
  mutate(diff = capHit-avg_cost) %>% 
  arrange(diff) %>% 
  top_n(-10) %>% 
  select(team:playerBestOvr, capHit, avg_cost, diff)
) %>% 
  mutate_if(is.numeric, scales::comma) 
```

    ## Selecting by diff
    ## Selecting by diff

    ## # A tibble: 20 x 8
    ##    team      firstName lastName  position playerBestOvr capHit   avg_cost diff  
    ##    <chr>     <chr>     <chr>     <chr>    <chr>         <chr>    <chr>    <chr> 
    ##  1 Rams      Aaron     Donald    RE       99.0          25,000,~ 20,270,~ 4,730~
    ##  2 Raiders   Trent     Brown     RT       87.0          21,250,~ 16,437,~ 4,812~
    ##  3 Cardinals Chandler  Jones     LOLB     94.0          21,330,~ 16,498,~ 4,832~
    ##  4 Broncos   Von       Miller    LOLB     96.0          25,630,~ 20,755,~ 4,875~
    ##  5 Saints    Drew      Brees     QB       86.0          23,650,~ 17,940,~ 5,710~
    ##  6 Colts     Jacoby    Brissett  QB       67.0          20,500,~ 14,515,~ 5,985~
    ##  7 Bengals   William   Jackson ~ CB       84.0          9,950,0~ 3,926,0~ 6,024~
    ##  8 Eagles    Zach      Ertz      TE       82.0          13,240,~ 7,211,6~ 6,028~
    ##  9 Texans    Will      Fuller V  WR       85.0          10,160,~ 4,064,0~ 6,096~
    ## 10 Patriots  Stephon   Gilmore   CB       99.0          25,170,~ 13,800,~ 11,37~
    ## 11 Chiefs    Patrick   Mahomes   QB       99.0          5,350,0~ 32,558,~ -27,2~
    ## 12 Texans    Deshaun   Watson    QB       90.0          9,810,0~ 29,658,~ -19,8~
    ## 13 Rams      Jalen     Ramsey    CB       97.0          6,200,0~ 18,950,~ -12,7~
    ## 14 Cardinals DeAndre   Hopkins   WR       99.0          7,000,0~ 18,884,~ -11,8~
    ## 15 Vikings   Kirk      Cousins   QB       80.0          21,000,~ 32,333,~ -11,3~
    ## 16 Browns    Myles     Garrett   RE       98.0          10,130,~ 21,408,~ -11,2~
    ## 17 Eagles    Darius    Slay Jr   CB       88.0          4,300,0~ 15,137,~ -10,8~
    ## 18 49ers     Arik      Armstead  LE       84.0          6,000,0~ 16,700,~ -10,7~
    ## 19 49ers     Dee       Ford      LE       83.0          6,400,0~ 17,074,~ -10,6~
    ## 20 Cowboys   DeMarcus  Lawrence  LE       87.0          9,900,0~ 20,400,~ -10,5~

So I feel like on some contracts, like Mahomes, the CapHit seems artificially low, likely how Madden dealt with the rookie contract. For other players,like Stephon Gilmore the CapHit seems right but the avg\_cost is thrown off by early contract years. I think for simplicity I’m going to use average cost because the CapHit actually seems a bit more manipulated but this is where, in the real world, you’d spend a lot more time understanding the business need and getting domain experts.

Let’s look at how the average annual cost is being allocated between offense

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

This suggests the Cowboys are most allocated towards offense and the Ravens most towards defense. Let’s look at treemap of how much each of these teams spends by position vs the other teams (averaged)

``` r
  player_data_2 %>%
  mutate(team = if_else(team %in% c("Cowboys", "Ravens"), team, "Avg Team")) %>% 
  mutate(avg_cost = (contractSalary+contractBonus) / contractLength) %>% 
  group_by(team, group_position) %>% 
  summarise(avg_cost = sum(avg_cost))%>%
  group_by(team) %>% 
  arrange(team, desc(avg_cost)) %>%
  ggplot(aes(area = avg_cost, fill = group_position, label = group_position, subgroup = team)) +
  geom_treemap() +
  geom_treemap_text(grow = T, reflow = T, colour = "black") +
                  scale_fill_brewer(palette = "Dark2") +
  theme(legend.position = "bottom") +
  facet_wrap( ~ team) +
  labs(
    title = "Salary by position",
    caption = "Data downloaded by someone with too much time",
    fill = "Team"
  )
```

    ## `summarise()` has grouped output by 'team'. You can override using the `.groups` argument.

    ## Warning in RColorBrewer::brewer.pal(n, pal): n too large, allowed maximum for palette Dark2 is 8
    ## Returning the palette you asked for with that many colors

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-7-1.png" width="672" />

I’m not sure if that format is better or a bar chart. You choose

``` r
 player_data_2 %>%
  mutate(team = if_else(team %in% c("Cowboys", "Ravens"), team, "Avg Team")) %>% 
  mutate(avg_cost = (contractSalary+contractBonus) / contractLength) %>% 
  group_by(team, group_position) %>% 
  summarise(avg_cost = sum(avg_cost))%>%
  group_by(team) %>% 
  mutate(avg_cost = avg_cost/sum(avg_cost)) %>% 
  ungroup() %>% 
  ggplot(aes(x = group_position, y = avg_cost, group=team, fill=team))+
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(limits = c(0,NA), labels = scales::percent)
```

    ## `summarise()` has grouped output by 'team'. You can override using the `.groups` argument.

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-8-1.png" width="672" />

So Ravens are invested in Defensive Backs and Linebackers. Cowboys invested just a bit more in almost all offensive position (OL, QB, RB, WR) than the average team.
