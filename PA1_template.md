# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

The following code chunk reads the data stored inside the *zip* file, formatted 
as a typical CSV. The file itself should be packed together with this document, 
so all we have to do is unpack them and read.


```r
raw_data <- read.csv(unz("activity.zip", "activity.csv"))
str(raw_data)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

From this summary one can immediately notice that the column *steps* has 
missing data, which needs to be handled later. Also, the date has been handled
as *factors*, which can be fixed using the following code:


```r
raw_data$date <- as.Date(raw_data$date, format="%Y-%m-%d")
str(raw_data)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```


We'll be using the following libraries throughout out analysis. 


```r
suppressWarnings(library(ggplot2))
```

## What is mean total number of steps taken per day?

The raw data has the number of steps taken at regular intervals of 5 minutes.
Our first analysis is to estimate the mean total number of steps taken per day.
With such goal in mind, the first step is to prepare the data by summing the 
number of steps on each day and create a histogram on how frequently the sample
individual takes a certain number of steps in a single day.


```r
steps_per_day <- aggregate(steps ~ date, raw_data, sum)

binwidth <- 1000
ggplot(steps_per_day, aes(steps)) + 
    geom_histogram(binwidth = binwidth) + 
    xlab("Steps") + ylab("Frequency")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

Then we extract the mean and the median of steps taken per day, ignoring the
missing values in the process.


```r
mean(steps_per_day$steps, na.rm = T)
```

```
## [1] 10766.19
```

```r
median(steps_per_day$steps, na.rm = T)
```

```
## [1] 10765
```

## What is the average daily activity pattern?

Another way to analyze the raw data is to check the activity pattern by 
averaging the number of steps taken on each 5-minute interval across all 
days.


```r
steps_per_interval <- aggregate(steps ~ interval, raw_data, mean)
ggplot(aes(x=interval, y=steps), data = steps_per_interval) +
    geom_line(stat = 'identity', position = position_dodge(width=1)) + 
    xlab("Interval") + ylab("Number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

Then, by using this code:


```r
max_steps_index <- which.max(steps_per_interval$steps)
max_steps_interval <- steps_per_interval$interval[max_steps_index]
max_steps_value <- steps_per_interval$steps[max_steps_index]
```

We can see that, on average, the highest number of steps taken is
206.1698113 on the 835th interval.

**Bonus:** Another way to view the same data is to plot the whole data along 
the mean:


```r
ggplot(aes(x=interval, y=steps), data = raw_data) + 
    geom_point(color="black", alpha = 0.1, na.rm = T) + 
    stat_summary(aes(y = steps), fun.y=mean, colour="red", geom="line", na.rm = T)
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

## Imputing missing values

As noted before, there are some intervals whose number of steps is missing. 
Using the following code:


```r
sum(is.na(raw_data$steps))
```

```
## [1] 2304
```

We can see that there are 2304 missing values out of 
17568 rows, which makes up for 
13.1147541% of the whole dataset, 
which can make the data biased to the moments the measurements were being taken,
particularly if the probability of having missing data is uneven across 
the intervals.

In order to minimize the effects of those missing values, we can replace them 
with the mean of the corresponding interval.


```r
steps_per_interval_arr <- tapply(raw_data$steps, raw_data$interval, mean, na.rm = T)

## Get the row indices whose steps is NA
indx <- which(is.na(raw_data$steps), arr.ind=TRUE)

## Replace those rows' values with the mean of their corresponding interval
filtered_data             <- raw_data
intervals_for_nas         <- as.character(filtered_data$interval[indx])
filtered_data$steps[indx] <- steps_per_interval_arr[intervals_for_nas]
```

Replicating the code from the first analysis, we compare both histograms and
see if it any different from the original one. The dark red histogram is the 
original one while the light red is the addition caused by filling the
missing values.


```r
steps_per_day_new    <- aggregate(steps ~ date, filtered_data, sum)
steps_per_day_new$na <- F

steps_per_day_old    <- steps_per_day
steps_per_day_old$na <- T

steps_per_day_comparison <- rbind(steps_per_day_new, steps_per_day_old)

ggplot(steps_per_day_comparison, aes(steps)) + 
    geom_histogram(data=steps_per_day_new, fill = "red", alpha = 0.5, binwidth = binwidth) + 
    geom_histogram(data=steps_per_day_old, fill = "black", alpha = 0.5, binwidth = binwidth) + 
    xlab("Steps") + ylab("Frequency")
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

It is possible to notice that the new distribution is mostly the same with one
major difference: the frequency around the mean has greatly increased. Under the 
hypothesis that the the probability of having missing data has no correlation 
to the intervals, this is the expected behavior, as replacing them with the mean 
number of steps of the interval should increase the frequency around the mean,
and less so everywhere else.

In terms of numeric values, changes on the mean and median have been minimal 
as well:


```r
mean(steps_per_day_new$steps, na.rm = T)
```

```
## [1] 10766.19
```

```r
median(steps_per_day_new$steps, na.rm = T)
```

```
## [1] 10766.19
```

## Are there differences in activity patterns between weekdays and weekends?

Finally, we check is there is a difference between the individual's behavior on 
weekdays and weekends. In order to do that, first we create new columns holding
information about the type of weekday.


```r
filtered_data$weekday    <- weekdays(filtered_data$date)
filtered_data$is.weekend <- as.factor(c("weekday", "weekend"))

weekend_indices <- filtered_data$weekday == "Sunday" | filtered_data$weekday == "Saturday"
weekday_indices <- filtered_data$weekday != "Sunday" & filtered_data$weekday != "Saturday"

filtered_data$is.weekend[weekday_indices] <- factor("weekday")
filtered_data$is.weekend[weekend_indices] <- factor("weekend")

str(raw_data)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

Now we calculate the mean number of steps based on both *interval* and whether
the date is a weekend or not and plot a chart to compare them.


```r
steps_per_interval <- aggregate(steps ~ interval + is.weekend, filtered_data, mean)
ggplot(aes(x=interval, y=steps, color=is.weekend), data = steps_per_interval) +
    geom_line(stat = 'identity', position = position_dodge(width=1)) + 
    xlab("Interval") + ylab("Number of steps") + theme(legend.title=element_blank())
```

![](PA1_template_files/figure-html/unnamed-chunk-14-1.png)<!-- -->

There are some obvious changes in behavior: starting from the earlier 
intervals, it seems that the sample individual wakes up much later on weekends,
but the amount of steps taken is more evenly distributed while on weekdays, he/she
walks much more soon after waking up and by the end of the day - probably the 
result from commuting from/to work or school. Also, it seems that the hour 
he/she goes to sleep does not differ greatly.

Finally, on average, the said individual seems to walk less on weekdays, which can be
a hint about his/her occupation. It is unlikely to be anything that requires a
lot of physical work or moving around, though further inferences without more 
information may be speculative at best.
