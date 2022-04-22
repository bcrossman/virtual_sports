---
title: 'Running Data: Runkeeper'
author: Brent Crossman
date: '2022-04-22'
slug: running-data-runkeeper
categories: []
tags: ['running', 'data visualization']
subtitle: ''
summary: ''
authors: []
lastmod: '2022-04-22T19:33:15-04:00'
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
library(raster)
library(tidyverse)
library(lubridate)
library(plotKML)
```

## Data Read


```r
run_data <- read_csv("cardioActivities.csv")
```

```
## Rows: 196 Columns: 14
## -- Column specification --------------------------------------------------------
## Delimiter: ","
## chr  (4): Activity Id, Type, Average Pace, GPX File
## dbl  (5): Distance (mi), Average Speed (mph), Calories Burned, Climb (ft), A...
## lgl  (3): Route Name, Friend's Tagged, Notes
## dttm (1): Date
## time (1): Duration
## 
## i Use `spec()` to retrieve the full column specification for this data.
## i Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

```r
glimpse(run_data)
```

```
## Rows: 196
## Columns: 14
## $ `Activity Id`              <chr> "0f5e6945-c59c-4c72-8a38-e2ea52ad3665", "7b~
## $ Date                       <dttm> 2022-04-22 07:02:33, 2022-04-20 07:03:19, ~
## $ Type                       <chr> "Running", "Running", "Running", "Running",~
## $ `Route Name`               <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,~
## $ `Distance (mi)`            <dbl> 3.00, 3.12, 3.01, 3.12, 6.22, 3.00, 3.11, 5~
## $ Duration                   <time> 28:11:00, 30:39:00, 28:25:00, 30:29:00, 01~
## $ `Average Pace`             <chr> "9:23", "9:50", "9:27", "9:47", "9:44", "9:~
## $ `Average Speed (mph)`      <dbl> 6.39, 6.10, 6.35, 6.13, 6.16, 6.40, 6.06, 6~
## $ `Calories Burned`          <dbl> 447, 456, 449, 463, 919, 447, 465, 743, 449~
## $ `Climb (ft)`               <dbl> 143, 80, 145, 153, 204, 141, 154, 164, 146,~
## $ `Average Heart Rate (bpm)` <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,~
## $ `Friend's Tagged`          <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,~
## $ Notes                      <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,~
## $ `GPX File`                 <chr> "2022-04-22-070233.gpx", "2022-04-20-070319~
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

## Update

I got thinking that I didn't love the above numbers because they were really thrown off by walking, particularly the warmups and cooldowns.  I have the raw GPX files, so I'm going to try and exclude the "walking" periods and rerun this anaysis. I won't upload that data to github because it shows exactly where I am and at one times for each day and as Tupac says 

"I always got to worry 'bout the pay backs 
Some buck that I roughed up way back
Comin' back after all these years"


```r
file_list <- dir("./gps_data")

gps_df_list <- list()

for(file_name in file_list){
  print(file_name)
  df <- readGPX(paste0("./gps_data/", file_name))$tracks[[1]][[1]]
  
  df$distance <- NA
  
  for(i in 2:nrow(df)){
    df$distance[i] <-pointDistance(p1 = c(df$lon[i],df$lat[i]), 
                              p2 = c(df$lon[i-1],df$lat[i-1])
                              ,
                              lonlat = T)/1609.34  
  }
  
  df <- 
    df %>% 
    # mutate(distance = 69.0975851*sqrt(
    #   ((lon-lag(lon))*cos(lag(lat)))^2+
    #     (lat-lag(lat))^2)) %>% 
    mutate(time_fixed = lubridate::as_datetime(time)) %>% 
    mutate(time_elapsed = 60*as.numeric(time_fixed-lag(time_fixed), 
                                        units="hours")) %>% 
    mutate(pace = (time_elapsed)/distance) %>% 
    select(pace, distance, time_elapsed, ele) %>% 
    mutate(`GPX File` = file_name)
  
  gps_df_list[[file_name]] <- df
}
```

```
## [1] "2020-12-03-063751.gpx"
## [1] "2020-12-08-064053.gpx"
## [1] "2020-12-10-064841.gpx"
## [1] "2020-12-13-073757.gpx"
## [1] "2020-12-15-064238.gpx"
## [1] "2020-12-20-073106.gpx"
## [1] "2020-12-22-062843.gpx"
## [1] "2020-12-24-064415.gpx"
## [1] "2020-12-27-074400.gpx"
## [1] "2020-12-31-072416.gpx"
## [1] "2021-01-03-072309.gpx"
## [1] "2021-01-05-062343.gpx"
## [1] "2021-01-07-064152.gpx"
## [1] "2021-01-12-062513.gpx"
## [1] "2021-01-14-063255.gpx"
## [1] "2021-01-17-071136.gpx"
## [1] "2021-01-19-064949.gpx"
## [1] "2021-01-21-062602.gpx"
## [1] "2021-01-31-065851.gpx"
## [1] "2021-02-01-062942.gpx"
## [1] "2021-02-04-064152.gpx"
## [1] "2021-02-07-065704.gpx"
## [1] "2021-02-09-063328.gpx"
## [1] "2021-02-11-062607.gpx"
## [1] "2021-02-14-071135.gpx"
## [1] "2021-02-17-064527.gpx"
## [1] "2021-02-19-064746.gpx"
## [1] "2021-02-21-070130.gpx"
## [1] "2021-02-23-063250.gpx"
## [1] "2021-02-25-063915.gpx"
## [1] "2021-02-28-065101.gpx"
## [1] "2021-03-03-064655.gpx"
## [1] "2021-03-05-063131.gpx"
## [1] "2021-03-07-065925.gpx"
## [1] "2021-03-09-062915.gpx"
## [1] "2021-03-11-064845.gpx"
## [1] "2021-03-14-075450.gpx"
## [1] "2021-03-16-062943.gpx"
## [1] "2021-03-18-065234.gpx"
## [1] "2021-03-21-070721.gpx"
## [1] "2021-03-23-063755.gpx"
## [1] "2021-03-25-063132.gpx"
## [1] "2021-03-28-072946.gpx"
## [1] "2021-03-30-063442.gpx"
## [1] "2021-04-02-070051.gpx"
## [1] "2021-04-03-085028.gpx"
## [1] "2021-04-04-065822.gpx"
## [1] "2021-04-04-071755.gpx"
## [1] "2021-04-04-072034.gpx"
## [1] "2021-04-06-063035.gpx"
## [1] "2021-04-08-063303.gpx"
## [1] "2021-04-11-070240.gpx"
## [1] "2021-04-13-063150.gpx"
## [1] "2021-04-15-065416.gpx"
## [1] "2021-04-22-070133.gpx"
## [1] "2021-04-25-065115.gpx"
## [1] "2021-04-27-063018.gpx"
## [1] "2021-05-02-071552.gpx"
## [1] "2021-05-04-063039.gpx"
## [1] "2021-05-06-065534.gpx"
## [1] "2021-05-09-070306.gpx"
## [1] "2021-05-11-063541.gpx"
## [1] "2021-05-13-065649.gpx"
## [1] "2021-05-16-071614.gpx"
## [1] "2021-05-18-063906.gpx"
## [1] "2021-05-20-064825.gpx"
## [1] "2021-05-23-074606.gpx"
## [1] "2021-05-25-062750.gpx"
## [1] "2021-05-27-062150.gpx"
## [1] "2021-05-30-074345.gpx"
## [1] "2021-06-01-062405.gpx"
## [1] "2021-06-03-065414.gpx"
## [1] "2021-06-06-063351.gpx"
## [1] "2021-06-08-061949.gpx"
## [1] "2021-06-10-060036.gpx"
## [1] "2021-06-13-070059.gpx"
## [1] "2021-06-15-072122.gpx"
## [1] "2021-06-17-064848.gpx"
## [1] "2021-06-20-070638.gpx"
## [1] "2021-06-22-072604.gpx"
## [1] "2021-06-24-060634.gpx"
## [1] "2021-07-01-071823.gpx"
## [1] "2021-07-04-072622.gpx"
## [1] "2021-07-06-071848.gpx"
## [1] "2021-07-08-074517.gpx"
## [1] "2021-07-11-074435.gpx"
## [1] "2021-07-11-075224.gpx"
## [1] "2021-07-13-063411.gpx"
## [1] "2021-07-15-065355.gpx"
## [1] "2021-07-18-130837.gpx"
## [1] "2021-07-20-063235.gpx"
## [1] "2021-07-22-064215.gpx"
## [1] "2021-07-25-065120.gpx"
## [1] "2021-07-25-065849.gpx"
## [1] "2021-07-27-063541.gpx"
## [1] "2021-07-29-063545.gpx"
## [1] "2021-08-01-064628.gpx"
## [1] "2021-08-03-063452.gpx"
## [1] "2021-08-05-063608.gpx"
## [1] "2021-08-08-063812.gpx"
## [1] "2021-08-08-064346.gpx"
## [1] "2021-08-10-063953.gpx"
## [1] "2021-08-12-063907.gpx"
## [1] "2021-08-15-070044.gpx"
## [1] "2021-08-17-063714.gpx"
## [1] "2021-08-19-070020.gpx"
## [1] "2021-08-22-062629.gpx"
## [1] "2021-08-24-062308.gpx"
## [1] "2021-08-26-064659.gpx"
## [1] "2021-08-29-074711.gpx"
## [1] "2021-08-31-063854.gpx"
## [1] "2021-09-02-062429.gpx"
## [1] "2021-09-05-070426.gpx"
## [1] "2021-09-07-062550.gpx"
## [1] "2021-09-09-063324.gpx"
## [1] "2021-09-12-072326.gpx"
## [1] "2021-09-19-073355.gpx"
## [1] "2021-09-21-063126.gpx"
## [1] "2021-09-23-063729.gpx"
## [1] "2021-09-26-064948.gpx"
## [1] "2021-09-28-063528.gpx"
## [1] "2021-09-30-063614.gpx"
## [1] "2021-10-03-073545.gpx"
## [1] "2021-10-05-063613.gpx"
## [1] "2021-10-07-063505.gpx"
## [1] "2021-10-09-065304.gpx"
## [1] "2021-10-12-063547.gpx"
## [1] "2021-10-14-063726.gpx"
## [1] "2021-10-17-072041.gpx"
## [1] "2021-10-19-063703.gpx"
## [1] "2021-10-21-064052.gpx"
## [1] "2021-10-24-071157.gpx"
## [1] "2021-10-26-063956.gpx"
## [1] "2021-10-28-062030.gpx"
## [1] "2021-10-31-071443.gpx"
## [1] "2021-11-04-063640.gpx"
## [1] "2021-11-07-064059.gpx"
## [1] "2021-11-09-062800.gpx"
## [1] "2021-11-11-062906.gpx"
## [1] "2021-11-14-071626.gpx"
## [1] "2021-11-16-063352.gpx"
## [1] "2021-11-18-064224.gpx"
## [1] "2021-11-21-064308.gpx"
## [1] "2021-11-21-100459.gpx"
## [1] "2021-11-23-064838.gpx"
## [1] "2021-11-25-071123.gpx"
## [1] "2021-11-28-070057.gpx"
## [1] "2021-11-30-063738.gpx"
## [1] "2021-12-02-062740.gpx"
## [1] "2021-12-05-065951.gpx"
## [1] "2021-12-07-064307.gpx"
## [1] "2021-12-09-063735.gpx"
## [1] "2021-12-12-081323.gpx"
## [1] "2021-12-14-064711.gpx"
## [1] "2021-12-16-064716.gpx"
## [1] "2021-12-19-075753.gpx"
## [1] "2021-12-21-064335.gpx"
## [1] "2021-12-23-072530.gpx"
## [1] "2021-12-26-064430.gpx"
## [1] "2021-12-28-071007.gpx"
## [1] "2022-01-01-072207.gpx"
## [1] "2022-01-04-063959.gpx"
## [1] "2022-01-06-064932.gpx"
## [1] "2022-01-08-074652.gpx"
## [1] "2022-01-13-063831.gpx"
## [1] "2022-01-16-112307.gpx"
## [1] "2022-01-18-064428.gpx"
## [1] "2022-01-20-064511.gpx"
## [1] "2022-01-23-071103.gpx"
## [1] "2022-02-03-063855.gpx"
## [1] "2022-02-06-102448.gpx"
## [1] "2022-02-10-064646.gpx"
## [1] "2022-02-13-070608.gpx"
## [1] "2022-02-17-063554.gpx"
## [1] "2022-02-20-072332.gpx"
## [1] "2022-02-20-080021.gpx"
## [1] "2022-02-22-064426.gpx"
## [1] "2022-02-24-061943.gpx"
## [1] "2022-02-27-072703.gpx"
## [1] "2022-03-06-075606.gpx"
## [1] "2022-03-08-064204.gpx"
## [1] "2022-03-10-063645.gpx"
## [1] "2022-03-13-080245.gpx"
## [1] "2022-03-15-064239.gpx"
## [1] "2022-03-17-064141.gpx"
## [1] "2022-03-20-074036.gpx"
## [1] "2022-03-22-065404.gpx"
## [1] "2022-03-31-065004.gpx"
## [1] "2022-04-03-075915.gpx"
## [1] "2022-04-05-063636.gpx"
## [1] "2022-04-07-063903.gpx"
## [1] "2022-04-10-070345.gpx"
## [1] "2022-04-12-064250.gpx"
## [1] "2022-04-14-063436.gpx"
## [1] "2022-04-20-070319.gpx"
## [1] "2022-04-22-070233.gpx"
```

```r
gps_df <- bind_rows(gps_df_list)


only_run <- 
  gps_df %>% 
  group_by(`GPX File`) %>% 
  filter(pace<13) %>% 
  summarise(avg_pace = median(pace, na.rm=T),
            total_distance = sum(distance)) %>% 
  filter(total_distance>2) %>% 
  filter(`GPX File` != "2021-07-04-072622.gpx")
```

Ok, let's re-run the analysis by distance and see what this shows


```r
clean_data %>% 
  left_join(only_run) %>% 
  filter(distance != "Medium") %>% 
  ggplot(aes(x = Date, y = avg_pace, color=distance)) +
  geom_point()+
  geom_smooth(se =T)
```

```
## Joining, by = "GPX File"
## `geom_smooth()` using method = 'loess' and formula 'y ~ x'
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-7-1.png" width="672" />

