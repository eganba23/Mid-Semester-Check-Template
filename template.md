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
would be sad but understandable). You’ll have to make aesthetically
pleasing, but this is hopefully a base start.

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

In this lab we will work with the **tidyverse**, **ggplot2**, and
**corrr** packages.

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

Hint: Plug ?cor() into your console to get more information about how to
run a correlation

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

4A. Answers can vary. The idea is that r should be almost zero for day,
an r around .25 correlation for month, and an r around -.25 for year. In
a nutshell: Days doesn’t matter, months kinda matters, years kinda
matters.

Sometimes statistics don’t tell you everything about the relationships
present in the data. A helpful way to further understand the
relationships is to plot the data and correlations.

5.  Plot the relationship between days and the number of fatalities.
    Does the correlation calculated in question 3 make sense based on
    the data? <br/> Stretch: Can you add in a `geom_smooth()` line to
    represent the correlation and use `geom_point()` to add the mean
    number of crashes per day?

5A. This plot is slightly over engineered. They should answer that it’s
random and day has no relationship to car crash. This stretch goal will
also apply to the months and years plot

``` r
# this was done out of order, so I had to move this up. It normally lives under the month visual

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

# daily ------------------------------------------------------------

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

6.  Plot the relationship between month and the number of fatalities.
    Does the correlation calculated in question 3 make sense based on
    the data?

6A. I’m expecting plots to look like the first one, but the second plot
will be much easier to read. In this case, students should see that
there is a possible relationship. They should also see that the data
isn’t linear and hopefully remember that January follows December. If
they remember, then it makes more sense that there is a “high” and
“low”season.

``` r
month_regression <- lm(fatalities ~ month, data = daily_accidents_visual)


month_r_values <- tibble(
  month = 1:12,
  month_cat = factor(month.name, levels = month.name),
  pred = predict(month_regression, newdata = tibble(month = 1:12))
)


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

    ## Warning: Using `size` aesthetic for lines was deprecated in ggplot2 3.4.0.
    ## ℹ Please use `linewidth` instead.
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
    ## generated.

![](template_files/figure-gfm/month%20corr%20visual-1.png)<!-- -->

This one is a stretch, since it changed the x axis to be categorical
data.

``` r
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

![](template_files/figure-gfm/month%20corr%20visual%20stretch%20one-1.png)<!-- -->

Stretch: Can you change create a line using `geom_smooth()` that follows
the non-linear trend of the months data?

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

![](template_files/figure-gfm/month%20corr%20stretch%202-1.png)<!-- -->

7.  Plot the relationship between year and the number of fatalities.
    Does the correlation calculated in question 3 make sense based on
    the data?

7A. Here there’s much more interesting data to examine. If you needed to
cut down this assignment, I would only examine year. Year is the easiest
to see that just the correlation doesn’t provide evidence for
everything. You could also have them google driving in mid 2000s, and
they should see that there were driving programs implementated around
2007/2008.

``` r
year_means <- daily_accidents_visual %>%
  group_by(year) %>%
  summarise(mean_fatalities = mean(fatalities, na.rm = TRUE))



daily_accidents_visual %>%
  ggplot(aes(
    x = year,
    y = fatalities
  ))+
  geom_jitter(alpha = .4, width = .25, color = "gray")+
  geom_smooth(color = "blue", alpha = .6)+
  geom_point(data = year_means, aes(x = year, y = mean_fatalities), color = "coral3", size = 2) +
  theme_bw()+
  annotate("text", 
           x = 1992, y = 60, 
           label = "1992", 
           vjust = 2, 
           color = "black")+
   annotate("text", 
           x = 2016, y = 60, 
           label = "2016", 
           vjust = 2, 
           color = "black")+
  labs(
    x = "Year",
    y = "Number of Fatalities",
    title = "Fatal car crashes by year"
  )
```

    ## `geom_smooth()` using method = 'gam' and formula = 'y ~ s(x, bs = "cs")'

![](template_files/figure-gfm/corr%20year-1.png)<!-- -->

``` r
min(daily_accidents_visual$year)
```

    ## [1] 1992

``` r
max(daily_accidents_visual$year)
```

    ## [1] 2016

### Part 2: Lets get more specific?

8.  What was the worst date for fatal car crashes?

8A. November 3rd, 2007. There were 299 fatal car crashes.

``` r
daily_accidents %>%
  arrange(desc(fatalities)) %>%
head()
```

    ##         date year month day fatalities
    ## 1 2007-11-03 2007    11   3        299
    ## 2 2001-02-22 2001     2  22        285
    ## 3 1998-09-04 1998     9   4        273
    ## 4 1996-08-24 1996     8  24        270
    ## 5 1993-06-19 1993     6  19        266
    ## 6 2000-09-30 2000     9  30        258

9.  What was the worst day of the month for fatal car crashes?

9A. A stretch can be asking which day of the month has the highest
average number of fatal car crashes

``` r
daily_accidents %>%
  distinct(day, fatalities) %>%
    group_by(day) %>%
  summarize(total_fatal = sum(fatalities)) %>%
    arrange(desc(total_fatal)) %>%
head()
```

    ## # A tibble: 6 × 2
    ##     day total_fatal
    ##   <dbl>       <dbl>
    ## 1     3       19244
    ## 2    16       18691
    ## 3    24       18461
    ## 4    11       18375
    ## 5    10       18296
    ## 6    22       18288

``` r
daily_accidents %>%
  distinct(day, fatalities) %>%
    group_by(day) %>%
  summarize(avg_fatal = mean(fatalities)) %>%
    arrange(desc(avg_fatal)) %>%
head()
```

    ## # A tibble: 6 × 2
    ##     day avg_fatal
    ##   <dbl>     <dbl>
    ## 1     3      155.
    ## 2     1      154.
    ## 3     4      153.
    ## 4    13      153.
    ## 5    30      153.
    ## 6    20      153.

### Part 3: Is driving on April 20th truly more dangerous?

10. Using a t-test, determine if there is a significant difference in
    car crashes for April 20th. To answer this question, examine
    `daily_accidents_420`. What does that third variable represent?
    Start by getting rid of any missing data and graph the results you
    find.

10A. Students should find evidence that 420 actually has lower fatal car
crashes, providing evidence to the contrary. The graph shows this as
well.

``` r
daily_accidents_420 <- daily_accidents_420 %>%
  filter(!e420 == "NA")

t.test(fatalities_count ~ e420, data = daily_accidents_420)
```

    ## 
    ##  Welch Two Sample t-test
    ## 
    ## data:  fatalities_count by e420
    ## t = 29.369, df = 24.632, p-value < 2.2e-16
    ## alternative hypothesis: true difference in means between group FALSE and group TRUE is not equal to 0
    ## 95 percent confidence interval:
    ##  83.98584 96.66370
    ## sample estimates:
    ## mean in group FALSE  mean in group TRUE 
    ##            144.9248             54.6000

``` r
daily_accidents_420 %>%
  ggplot(aes(
    x = e420,
    y = fatalities_count
  ))+
  geom_jitter(width = .2)+
  labs(
    x = "Is it 4/20?",
    y = "Fatal car crashes"
  )
```

![](template_files/figure-gfm/420%20fatalities-1.png)<!-- -->

As part of the article, Harper and Palayew (2019) could not find any
signals that 4/20 is more dangerous than the average day. They did find
other holidays, such as July 4th, were more dangerous.

11. Repeat the process but for America’s Independence day, July 4th.
    This time, there is no variable that designates if a date is July
    4th. Using the modified `daily_accidents` you created, create
    variable `July4` that represents all dates that are July 4th. Then,
    use a t-test to determine if there is a significant difference in
    car crashes for July 4th and graph the results.

11A. I wrote a function to figure this out. Overengineered? Yes. Is it
cool? Also yes. The function will be the next question’s stretch goal,
but I didn’t want to include the code twice. Students should find that
July 4th has higher car crashes than normal.

``` r
day_oty <- function(name, specific_month, specific_day) {
  daily_accidents %>%
    mutate({{name}} := if_else(
        day == specific_day & month == specific_month, "Yes", "No"
      ))
}
```

``` r
july4 <- day_oty(july4, 7, 4)

july4 %>%
  ggplot(aes(
    x = july4,
    y = fatalities
  ))+
  geom_jitter(width = .2)+
  labs(
    x = "Is it July 4th?",
    y = "Fatal car crashes"
  )
```

![](template_files/figure-gfm/4th%20of%20july%20fatalities-1.png)<!-- -->

``` r
t.test(fatalities ~ july4, data = july4)
```

    ## 
    ##  Welch Two Sample t-test
    ## 
    ## data:  fatalities by july4
    ## t = -6.3811, df = 24.249, p-value = 1.282e-06
    ## alternative hypothesis: true difference in means between group No and group Yes is not equal to 0
    ## 95 percent confidence interval:
    ##  -41.03296 -20.98483
    ## sample estimates:
    ##  mean in group No mean in group Yes 
    ##          144.9911          176.0000

12. What about other holidays? See if Christmas (December 25th) and New
    Years Eve (December 31st) differ in car crashes from the average
    day. <br/> Stretch: Can you create a function that can do this?

12A. The Christmas one is representative of what students would most
likely create. Christmas has lower fatalities and New Years is not
significantly different. The answer for the stretch is above in the July
4th data.

``` r
Christmas <- daily_accidents %>%
  mutate(Christmas = if_else(
    day == 25 & month == 12, "Christmas", "Not Christmas"
  ))

Christmas %>%
  ggplot(aes(
    x = Christmas,
    y = fatalities
  ))+
  geom_jitter(width = .2)+
  labs(
    x = "Is it Christmas?",
    y = "Fatal car crashes"
  )
```

![](template_files/figure-gfm/christmas%20fatalities-1.png)<!-- -->

``` r
t.test(fatalities ~ Christmas, data = Christmas)
```

    ## 
    ##  Welch Two Sample t-test
    ## 
    ## data:  fatalities by Christmas
    ## t = -6.8592, df = 24.25, p-value = 4.058e-07
    ## alternative hypothesis: true difference in means between group Christmas and group Not Christmas is not equal to 0
    ## 95 percent confidence interval:
    ##  -43.24549 -23.24854
    ## sample estimates:
    ##     mean in group Christmas mean in group Not Christmas 
    ##                     111.920                     145.167

``` r
NYE <- day_oty(NYE, 12, 31)

NYE %>%
  ggplot(aes(
    x = NYE,
    y = fatalities
  ))+
  geom_jitter(width = .2)+
  labs(
    x = "Is it New years eve?",
    y = "Fatal car crashes"
  )
```

![](template_files/figure-gfm/new%20years%20fatalities-1.png)<!-- -->

``` r
t.test(fatalities ~ NYE, data = NYE)
```

    ## 
    ##  Welch Two Sample t-test
    ## 
    ## data:  fatalities by NYE
    ## t = 1.2682, df = 24.219, p-value = 0.2168
    ## alternative hypothesis: true difference in means between group No and group Yes is not equal to 0
    ## 95 percent confidence interval:
    ##  -4.119777 17.267764
    ## sample estimates:
    ##  mean in group No mean in group Yes 
    ##           145.094           138.520

Optional Stretch Question:

13. How about holidays that change dates, such as Thanksgiving?
    Thanksgiving always falls on the 4th Thursday of November. For this,
    you will need to alter your function to count the number of
    Thursdays in November.

13A. This accomplishses it, and can be altered to check for other
holidays like Black Friday. Assuming everything is correct, they’ll find
that thanksgiving is no different than the average day.

``` r
daily_accidents <- daily_accidents %>%
    mutate(date = make_date(year, month, day))

check_thanksgiving <- function(df) {
  
  thanksgiving_flags <- df %>%
    filter(month(date) == 11) %>%
    arrange(date) %>%
    group_by(year(date)) %>%
    mutate(
      weekday = wday(date, label = TRUE),
      thursday_count = cumsum(weekday == "Thu"),
      is_thanksgiving = if_else(weekday == "Thu" & thursday_count == 4, "Yes", "No")
    ) %>%
    ungroup() %>%
    select(date, is_thanksgiving)

  # Merge flags back into the main data
  df %>%
    left_join(thanksgiving_flags, by = "date") %>%
    mutate(is_thanksgiving = replace_na(is_thanksgiving, "No"))
  
}


thanksgiving_accidents <- check_thanksgiving(daily_accidents)

head(thanksgiving_accidents)
```

    ##         date year month day fatalities is_thanksgiving
    ## 1 1992-01-01 1992     1   1        144              No
    ## 2 1992-01-02 1992     1   2        111              No
    ## 3 1992-01-07 1992     1   7         85              No
    ## 4 1992-01-12 1992     1  12        127              No
    ## 5 1992-01-03 1992     1   3        182              No
    ## 6 1992-01-17 1992     1  17        130              No

``` r
thanksgiving_accidents %>%
  ggplot(aes(
    x = is_thanksgiving,
    y = fatalities
  ))+
  geom_jitter(width = .2)+
  labs(
    x = "Is it Thanksgiving?",
    y = "Fatal car crashes"
  )
```

![](template_files/figure-gfm/thanksgiving%20difference-1.png)<!-- -->

``` r
t.test(fatalities ~ is_thanksgiving, data = thanksgiving_accidents)
```

    ## 
    ##  Welch Two Sample t-test
    ## 
    ## data:  fatalities by is_thanksgiving
    ## t = 0.94434, df = 24.223, p-value = 0.3543
    ## alternative hypothesis: true difference in means between group No and group Yes is not equal to 0
    ## 95 percent confidence interval:
    ##  -5.743886 15.442430
    ## sample estimates:
    ##  mean in group No mean in group Yes 
    ##          145.0893          140.2400

\[1\]. Harper S, Palayew A “The annual cannabis holiday and fatal
traffic crashes.” BMJ Injury Prevention. Published Online First: 29
January 2019. doi: 10.1136/injuryprev-2018-043068. Manuscript and
original data/code at <https://osf.io/qnrg6/>

\[2\]. Staples JA, Redelmeier DA. “The April 20 cannabis celebration and
fatal traffic crashes in the United States.” JAMA Intern Med. 2018 Feb;
178(4):569–72.
