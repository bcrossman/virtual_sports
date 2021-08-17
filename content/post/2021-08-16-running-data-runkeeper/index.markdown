---
title: 'Running Data: Runkeeper'
author: Brent Crossman
date: '2021-08-16'
slug: running-data-runkeeper
categories: []
tags: ['running', 'data visualization']
subtitle: ''
summary: ''
authors: []
lastmod: '2021-08-16T19:33:15-04:00'
featured: no
image:
  caption: ''
  focal_point: ''
  preview_only: no
projects: []
---

My wife and I have been running for a little over a year.  I know the data isn't perfect, mainly because of inconsistencies in whether it includes warmups and cooldowns (I used to include them, now I just capture the actual run) but I still think it will be interesting to track my progress over time. I also realized that I've only been using runkeeper since December, so only about a half year of data so far.

## Package Load


```r
library(tidyverse)
```

## Data Read


```r
run_data <- read_csv("cardioActivities.csv")
```

```
## Rows: 104 Columns: 14
```

```
## -- Column specification --------------------------------------------------------
## Delimiter: ","
## chr  (5): Activity Id, Type, Duration, Average Pace, GPX File
## dbl  (5): Distance (mi), Average Speed (mph), Calories Burned, Climb (ft), A...
## lgl  (3): Route Name, Friend's Tagged, Notes
## dttm (1): Date
```

```
## 
## i Use `spec()` to retrieve the full column specification for this data.
## i Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

```r
glimpse(run_data)
```

```
## Rows: 104
## Columns: 14
## $ `Activity Id`              <chr> "0d7d88b6-e5c8-479f-8494-0c944a5cb415", "36~
## $ Date                       <dttm> 2021-08-15 07:00:44, 2021-08-12 06:39:07, ~
## $ Type                       <chr> "Running", "Running", "Running", "Running",~
## $ `Route Name`               <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,~
## $ `Distance (mi)`            <dbl> 6.22, 3.03, 3.12, 6.22, 0.30, 3.02, 3.12, 6~
## $ Duration                   <chr> "1:02:36", "29:15", "32:23", "1:04:38", "5:~
## $ `Average Pace`             <chr> "10:04", "9:40", "10:23", "10:24", "17:28",~
## $ `Average Speed (mph)`      <dbl> 5.96, 6.21, 5.78, 5.77, 3.44, 6.21, 6.07, 6~
## $ `Calories Burned`          <dbl> 922, 451, 468, 934, 31, 450, 466, 919, 450,~
## $ `Climb (ft)`               <dbl> 208, 144, 154, 358, 8, 144, 155, 195, 142, ~
## $ `Average Heart Rate (bpm)` <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,~
## $ `Friend's Tagged`          <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,~
## $ Notes                      <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,~
## $ `GPX File`                 <chr> "2021-08-15-070044.gpx", "2021-08-12-063907~
```

I want to see how my pace has changed over time among various distance runs. Let's clean up the data a bit to help get that read


```r
clean_data <- 
  run_data %>% 
  filter(`Distance (mi)`>1) %>% 
  mutate(distance = case_when(
    `Distance (mi)`<3.5 ~ "Short",
    `Distance (mi)`<4.5 ~ "Medium",
    TRUE ~ "Long")) %>% 
  separate(`Average Pace`, into = c("min", "sec"), sep = ":", convert = T) %>% 
  mutate(Pace = min+sec/60) %>% 
  filter(Pace<14)

clean_data %>% 
  ggplot(aes(`Distance (mi)`, fill = distance)) +
  geom_density(alpha = .5)
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-2-1.png" width="672" />

```r
clean_data %>% 
  ggplot(aes(x = Date, y = Pace, color=distance)) +
  geom_point()+
  geom_smooth(se =F)+
  ylim(0,NA)
```

```
## `geom_smooth()` using method = 'loess' and formula 'y ~ x'
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-2-2.png" width="672" />

That's some improvement recently

Our short runs, have various hill climbs, let's see if the pace differs in this group by elevation.


```r
clean_data %>% 
  filter(`Distance (mi)`<3.5) %>% 
  ggplot(aes(x = `Climb (ft)`, y = Pace)) +
  geom_point()+
  geom_smooth(se =F)+
  ylim(0,NA)
```

```
## `geom_smooth()` using method = 'loess' and formula 'y ~ x'
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-3-1.png" width="672" />

Kind of a pattern there but not super clean. 

We also run different routes on different days of the week.  Let's see how that splits up


```r
clean_data %>% 
  mutate(day = weekdays(Date)) %>% 
  mutate(day = fct_lump_n(f = day, n = 3)) %>% 
  filter(day!="Other") %>% 
  ggplot(aes(x = Date, y = Pace, color=day)) +
  geom_point()+
  geom_smooth(se =F)+
  ylim(0,NA)
```

```
## `geom_smooth()` using method = 'loess' and formula 'y ~ x'
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-4-1.png" width="672" />

So this is making it a bit more clear, Thursday, our fastest pace is a day when we've historically not run warmups or cooldowns as we do mile segments. So the pace has converged to that recenty because we've stopped recording the warmups and cooldowns on them as well.

Last little thing, let's look at that total distance we ran by month:


```r
clean_data %>% 
  mutate(month = as.factor(update(as.Date(Date), day = 15))) %>% 
  group_by(month) %>% 
  filter(month != "2021-08-15") %>% 
  summarise(Total_Distance = sum(`Distance (mi)`)) %>% 
  ggplot(aes(x= month, y=Total_Distance, fill=month)) + 
  geom_bar(stat="identity") + 
  geom_text(aes(label = Total_Distance), vjust = 1.5, colour = "white")+
  theme(
     axis.text.x=element_text(angle=60,hjust=1),
     legend.position = "none")
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-5-1.png" width="672" />



