Mid Semester Check
================
Benjamin Egan
04/25/2025

As part of a portfolio piece, I looked at the Tidy Tuesday from April
20th, 2025. The Tidy Tuesday for that week explored fatal crashes on
4/20, the “marijuana holiday.” My portfolio piece examined the dataset
and provided evidence that there is actually no link between crashes and
inhibition due to marijuana. I’m going to attempt to build a “check
assignment” for students. This could serve as a portfolio piece, an
extra lab, or an additional assignment you provide students. This could
also serve as a “final project” for students in the undergraduate
version of the course. You could also simply not use any of this (which
would be sad but understandable).

# Tidy Tuesday - 4/20/2025

In January of 2019, Harper and Palayew (2019) published a study looking
at whether a signal could be detected in fatal car crashes in the United
States based on the “4/20” holiday, based on a previous study by Staples
and Redelmeier (2018) that suggested a strong link. Using more robust
methods and a more comprehensive time window, Harper and Palayew could
not find a signal for 4/20, but could for other holidays. The Tidy
Tuesday for the week of April 20th explored this data, examining fatal
crashes from 1992 to 2016.

## Getting started

### Packages

In this lab we will work with the **tidyverse**, **ggplot2**, **corrr**
packages.

``` r
library(tidyverse) 
library(ggplot2)
#library(skimr)
library(corrr)
```

### Loading the data

The data we will be using comes from the Tidy Tuesday Github. You can
download the data using the following:

``` r
tuesdata <- tidytuesdayR::tt_load(2025, week = 16)
```

    ## ---- Compiling #TidyTuesday Information for 2025-04-22 ----
    ## --- There are 2 files available ---
    ## 
    ## 
    ## ── Downloading files ───────────────────────────────────────────────────────────
    ## 
    ##   1 of 2: "daily_accidents.csv"
    ##   2 of 2: "daily_accidents_420.csv"

``` r
daily_accidents <- tuesdata$daily_accidents
daily_accidents_420 <- tuesdata$daily_accidents_420
```

## Exercises

### Part 1: The relationship between dates and car crashes

Before determining any relationship between fatal car crashes in the
United States and the “4/20” holiday, we need to start by examining if
there is any relationship between car crashes and dates.

1.  Start by taking a look through the data. What kinds of data are
    provided? How is the data structured?

1A. Students will answer with something like “there’s two variables, one
with date and the other with number of fatal car crashes.

2.  Separate out `date` into it’s relevant components. You should now
    have separate variables for year, month, and day. <br/> Hint: Try
    creating a data frame that includes date, year, month, day, and the
    fatal crashes.

2A. There should probably be some scaffolding here. I am thinking to
include below:

``` r
# Scaffolding ------------------------------------------------------------------------------------------------------
daily_accidents <- data.frame(
                 # date variable,
                 year = as.numeric(format(daily_accidents$date, format = "%Y")),
                 # repeat for both month and day,
                 # fatal car crashes
                )
```

``` r
# Answer ------------------------------------------------------------------------------------------------------
daily_accidents <- data.frame(date = daily_accidents$date,
                 year = as.numeric(format(daily_accidents$date, format = "%Y")),
                 month = as.numeric(format(daily_accidents$date, format = "%m")),
                 day = as.numeric(format(daily_accidents$date, format = "%d")),
                 daily_accidents$fatalities_count)

names(daily_accidents)[names(daily_accidents) == "daily_accidents.fatalities_count"] <- "fatalities"

head(daily_accidents)
```

    ##         date year month day fatalities
    ## 1 1992-01-01 1992     1   1        144
    ## 2 1992-01-02 1992     1   2        111
    ## 3 1992-01-07 1992     1   7         85
    ## 4 1992-01-12 1992     1  12        127
    ## 5 1992-01-03 1992     1   3        182
    ## 6 1992-01-17 1992     1  17        130

3.  Now that we have separate variables, we can begin to examine trends
    in fatal car crashes. Find the following correlations using `cor()`:

<!-- -->

1.  days of the month and fatal car crashes
2.  months of the year and fatal car crashes
3.  years and fatal car crashes

Hint: Plug ?cor() into your console to get information about how to run
a correlation

3A.

``` r
day_corr <- cor(daily_accidents$day, daily_accidents$fatalities)

month_corr <- cor(daily_accidents$month, daily_accidents$fatalities)

year_corr <- cor(daily_accidents$year, daily_accidents$fatalities)

dates_corr <- tibble(day_corr, month_corr, year_corr)

dates_corr
```

    ## # A tibble: 1 × 3
    ##   day_corr month_corr year_corr
    ##      <dbl>      <dbl>     <dbl>
    ## 1  -0.0388      0.260    -0.273

4.  What does each correlation tell you? How helpful is each correlation
    in determining information about fatal car crashes?

4A. Answers can vary. The idea is that there should be no correlation
for day, a positive correlation for month, and negative for year. Days
shouldn’t matter, months kinda matters, years really matters.

Sometimes statistics don’t tell you everything about the relationships
present in the data. A helpful way to further understand the
relationships is to plot the data and correlations.

6.  Plot the relationship between month and the number of fatalities.
    Does the correlation calculated in question 3 make sense based on
    the data?

6A. I’m expecting plots to look like the first one, but the second plot
will be much easier to read. In this case, students should see that
there is a possible relationship.

``` r
daily_accidents_visual <- daily_accidents %>%
  mutate(month_cat = case_when(
    month == 1 ~ "January",
    month == 2 ~ "February",
    month == 3 ~ "March",
    month == 4 ~ "April",
    month == 5 ~ "May",
    month == 6 ~ "June",
    month == 7 ~ "July",
    month == 8 ~ "August",
    month == 9 ~ "September",
    month == 10 ~ "October",
    month == 11 ~ "November",
    month == 12 ~ "December"
  ))

month_regression <- lm(fatalities ~ month, data = daily_accidents_visual)


month_r_values <- tibble(
  month = 1:12,
  month_cat = factor(month.name, levels = month.name),
  pred = predict(month_regression, newdata = tibble(month = 1:12))
)

daily_accidents_visual %>%
  ggplot(aes(
    x = fct(month_cat),
    y = fatalities
  ))+
  geom_jitter(alpha = .4, width = .25)+
  geom_line(data = month_r_values, 
            aes(x = month_cat, y = pred, group = 1), 
            color = "blue", 
            size = 1) +
  theme_bw()+
  labs(
    x = "Month",
    y = "Number of Fatal car crashes",
    title = "Fatal car crashes by month"
  )
```

    ## Warning: Using `size` aesthetic for lines was deprecated in ggplot2 3.4.0.
    ## ℹ Please use `linewidth` instead.
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
    ## generated.

![](template_files/figure-gfm/month%20corr%20visual-1.png)<!-- -->

``` r
daily_accidents_visual %>%
  ggplot(aes(
    x = month,
    y = fatalities
  ))+
  geom_jitter(alpha = .4, width = .25)+
  geom_line(data = month_r_values, 
            aes(x = month, y = pred, group = 1), 
            color = "blue", 
            size = 1) +
  theme_bw()+
  labs(
    x = "Month",
    y = "Number of Fatal car crashes",
    title = "Fatal car crashes by month"
  )
```

![](template_files/figure-gfm/month%20corr%20visual-2.png)<!-- -->

``` r
monthly_means <- daily_accidents_visual %>%
  group_by(month_cat) %>%
  summarise(mean_fatalities = mean(fatalities, na.rm = TRUE)) %>%
  mutate(month_cat = factor(month_cat, levels = month.name))

daily_accidents_visual %>%
  ggplot(aes(
    x = fct(month_cat),
    y = fatalities
  ))+
  geom_jitter(alpha = .4, width = .25, color = "gray")+
  geom_smooth(aes(x = month), 
              method = "loess", 
              se = FALSE, 
              color = "blue", 
              size = 1, 
              data = daily_accidents_visual) +
  geom_point(data = monthly_means, 
             aes(x = month_cat, y = mean_fatalities), 
             color = "tomato1", 
             size = 2) +
  theme_bw()+
  labs(
    x = "Month",
    y = "Number of Fatalities in the month",
    title = "Fatal car crashes by month with a non-linear line"
  )
```

    ## `geom_smooth()` using formula = 'y ~ x'

![](template_files/figure-gfm/month%20corr%20visual%202-1.png)<!-- -->

5.  Plot the relationship between days and the number of fatalities.
    Does the correlation calculated in question 3 make sense based on
    the data? <br/> Hint: By adding in a `geom_smooth()` line to
    represent the correlation and `geom_point()` to add the mean number
    of crashes per day, you might be better able to see what is
    happening

5A. This plot is slightly over engineered. They should answer that it’s
random and day has no relationship to car crash.

``` r
daily_means <- daily_accidents_visual %>%
  group_by(day) %>%
  summarise(mean_fatalities = mean(fatalities, na.rm = TRUE))

daily_accidents_visual %>%
  ggplot(aes(
    x = day,
    y = fatalities
  ))+
  geom_jitter(alpha = .4, width = .25, color = "gray")+
  geom_point(data = daily_means, aes(x = day, y = mean_fatalities), color = "blue", size = 2) +
  geom_smooth(color = "black", alpha = .6)+
  theme_bw()+
  labs(
    x = "Days",
    y = "Number of Fatalities",
    title = "Fatal car crashes by day"
  )
```

    ## `geom_smooth()` using method = 'gam' and formula = 'y ~ s(x, bs = "cs")'

![](template_files/figure-gfm/day%20corr%20visual-1.png)<!-- -->

1.  Harper S, Palayew A “The annual cannabis holiday and fatal traffic
    crashes.” BMJ Injury Prevention. Published Online First: 29
    January 2019. doi: 10.1136/injuryprev-2018-043068. Manuscript and
    original data/code at <https://osf.io/qnrg6/>

2.  Staples JA, Redelmeier DA. “The April 20 cannabis celebration and
    fatal traffic crashes in the United States.” JAMA Intern Med. 2018
    Feb; 178(4):569–72.
