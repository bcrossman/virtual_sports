---
title: Fantasy Football Draft
author: Brent Crossman
date: '2021-08-15'
slug: fantasy-football-draft
categories: []
tags: ['football', 'draft']
subtitle: ''
summary: ''
authors: []
lastmod: '2021-08-15T19:48:05-04:00'
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

I apologize but this is going to be almost a throwaway post. I’m just trying to calculate my rankings for an upcoming fantasy football draft. I suppose I could do some EDA but at this point I just need a rank from which to draft.

## Package Load

``` r
library(tidyverse)
library(DT)
library(widgetframe)
library(testthat)
library(scales)
library(janitor)
library(unglue)

##Functions
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
```

## Data Read

``` r
offense_proj <- read_csv("offense_proj.csv", skip = 1)
```

    ## New names:
    ## * `` -> ...1
    ## * `` -> ...3
    ## * Yds -> Yds...12
    ## * TD -> TD...13
    ## * Yds -> Yds...16
    ## * ...

    ## Rows: 268 Columns: 24

    ## -- Column specification --------------------------------------------------------
    ## Delimiter: ","
    ## chr (24): ...1, Offense, ...3, Forecast, Roster Status, GP*, Bye, Fan Pts ,...

    ## 
    ## i Use `spec()` to retrieve the full column specification for this data.
    ## i Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
offense_proj_slim <- 
  offense_proj %>% 
  janitor::clean_names() %>% 
  rename(fan_pts = fan_pts_u_e002) %>% 
  select(offense, gp, fan_pts) %>% 
  rename(name = offense)

defense_proj <- read_csv("defense_proj.csv", skip = 1)
```

    ## New names:
    ## * `` -> ...1
    ## * `` -> ...3
    ## * TD -> TD...11
    ## * TD -> TD...22

    ## Rows: 214 Columns: 22

    ## -- Column specification --------------------------------------------------------
    ## Delimiter: ","
    ## chr (22): ...1, Defensive Players, ...3, Roster Status, GP*, Bye, Fan Pts ,...

    ## 
    ## i Use `spec()` to retrieve the full column specification for this data.
    ## i Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
defense_proj_slim <- 
  defense_proj %>% 
  janitor::clean_names() %>% 
  select(defensive_players, gp, fan_pts_u_e002) %>% 
  rename(name = defensive_players) %>% 
  rename(fan_pts = fan_pts_u_e002)

proj <- 
  defense_proj_slim %>% 
  bind_rows(offense_proj_slim)
```

My terrible understanding of regex on display in the next section:

``` r
patterns <- c("No new player Notes {player_first_name} {player_last_name} {team} - {position}{extra=:.*}",
              "Player Note {player_first_name} {player_last_name} {team} - {position}{extra=:.*}")

df <- unglue_data(proj$name, patterns)

df <- bind_cols(df, proj)

clean_df <- 
  df %>% 
  separate(team, into = c("suffix", "team"), sep = " ", fill = "left") %>% 
  separate(position, into = c("position", "extra"), sep = " ", fill = "left") %>% 
  select(-extra) %>% 
  unite(col = "clean_name", player_first_name, player_last_name, suffix, 
        na.rm = T,
        sep = " ") %>% 
  filter(!is.na(team)) %>% 
  select(-name) %>% 
  mutate(fan_pts = as.numeric(fan_pts)) %>% 
  arrange(desc(fan_pts)) %>% 
  mutate(original_position = position) %>% 
  mutate(
    position = case_when(
      original_position == "S" ~ "DB",
      original_position == "CB" ~ "DB",
      TRUE ~ original_position
    )
  )
```

    ## Warning: Expected 2 pieces. Additional pieces discarded in 442 rows [1, 2, 3, 4,
    ## 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, ...].

## Replacement Level

We need to figure out what the worst likely remaining player will be by position

``` r
required_depth <- readxl::read_xlsx("starting_position.xlsx")

num_teams <- 10

required_depth$tot_start <- required_depth$tot_start*num_teams

player_data <-
  clean_df %>% 
  left_join(required_depth) %>% 
  arrange(desc(fan_pts)) %>% 
  group_by(position) %>% 
  mutate(rank = rank(-fan_pts, ties.method = "first")) 
```

    ## Joining, by = "position"

``` r
player_data_top_cut <- 
  player_data %>% 
  filter(rank <= tot_start)

player_data_top_remain <- 
  player_data %>% 
  filter(is.na(tot_start) | rank > tot_start) %>% 
  mutate(
    second_position = case_when(
      position == "WR" ~ "Off_Flex",
      position == "RB" ~ "Off_Flex",
      position == "TE" ~ "Off_Flex",
      position == "LB" ~ "Def_Flex",
      position == "DB" ~ "Def_Flex",
      position == "DT" ~ "Def_Flex",
      TRUE ~ "Nothing")
  )

flex_start <- 
  data.frame(second_position = c("Off_Flex", "Def_Flex"), 
             tot_start_2 = c(2, 2)*num_teams)

player_data_addl_draft <-
  player_data_top_remain %>% 
  left_join(flex_start) %>% 
  arrange(desc(fan_pts)) %>% 
  group_by(second_position) %>% 
  mutate(rank = rank(-fan_pts, ties.method = "first")) %>% 
  filter(rank <= tot_start_2)
```

    ## Joining, by = "second_position"

``` r
player_data_total <- 
  player_data_top_cut %>% 
  bind_rows(player_data_addl_draft) %>% 
  select(clean_name:original_position)

replacement_level <- 
  player_data_total %>% 
  group_by(position) %>% 
  arrange(desc(fan_pts)) %>% 
  slice(n()) %>% 
  select(position, fan_pts) %>% 
  rename(rep_fan_pts = fan_pts)

replacement_level
```

    ## # A tibble: 7 x 2
    ## # Groups:   position [7]
    ##   position rep_fan_pts
    ##   <chr>          <dbl>
    ## 1 DB              154.
    ## 2 DT              161.
    ## 3 LB              155.
    ## 4 QB              222.
    ## 5 RB              139.
    ## 6 TE              100.
    ## 7 WR              137.

## Attach Replacement Level and Rank

When choosing a flex, make it an RB or WR because the replacement level is much higher than TE. Looks like Defensive Flex can be either a DB or LB.

``` r
final_rank <- 
  clean_df %>% 
  left_join(replacement_level) %>% 
  mutate(value_ovr_rep = fan_pts - rep_fan_pts) %>% 
  arrange(desc(value_ovr_rep))
```

    ## Joining, by = "position"

``` r
display_table(final_rank) %>% widgetframe::frameWidget()
```

<div id="htmlwidget-1" style="width:100%;height:480px;" class="widgetframe html-widget"></div>
<script type="application/json" data-for="htmlwidget-1">{"x":{"url":"index_files/figure-html//widgets/widget_unnamed-chunk-4.html","options":{"xdomain":"*","allowfullscreen":false,"lazyload":false}},"evals":[],"jsHooks":[]}</script>
