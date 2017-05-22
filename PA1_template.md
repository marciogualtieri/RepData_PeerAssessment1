Installing the Required Packages
--------------------------------

You might need to install the following packages if you don't already have them:

``` r
install.packages("ktable")
install.packages("dplyr")
install.packages("Matrix")
install.packages("mice")
install.packages("ggplot2")
```

Just uncomment the packages you need and run this chunk before you run the remaining ones in this notebook.

Importing the Required Packages
-------------------------------

Once the libraries are installed, they need to be loaded as follows:

``` r
suppressMessages(library(dplyr))   # Data frame manipulation
suppressMessages(library(scales))  # Color helpers
suppressMessages(library(mice))    # Data imputation
suppressMessages(library(ggplot2)) # Plotting
```

Loading and preprocessing the data
----------------------------------

### Downloading the Data-set Files

``` r
download_zipped_data <- function(url, destination) {
  temp_file <- tempfile()
  download.file(url, temp_file)
  unzip(zipfile = temp_file, exdir = destination)
  unlink(temp_file)
}

download_zipped_data("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip",
                     "./data")
```

Here's a list of files in the zip file:

``` r
list.files(path = "./data", recursive = TRUE)
```

    ## [1] "activity.csv"

### Loading the Data-set

``` r
activity <- read.csv("./data/activity.csv", stringsAsFactors = FALSE)
str(activity)
```

    ## 'data.frame':    17568 obs. of  3 variables:
    ##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ date    : chr  "2012-10-01" "2012-10-01" "2012-10-01" "2012-10-01" ...
    ##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...

### Sampling the Data-set

``` r
sample_data_frame <- function(data, size) {
  sample_index <- sample(1:nrow(data), size)
  return(data[sample_index, ])
}

sample_data_frame(activity, 6)
```

|       |  steps| date       |  interval|
|-------|------:|:-----------|---------:|
| 2     |     NA| 2012-10-01 |         5|
| 16859 |      0| 2012-11-28 |      1250|
| 7381  |    110| 2012-10-26 |      1500|
| 16345 |     26| 2012-11-26 |      1800|
| 2052  |     NA| 2012-10-08 |       255|
| 12661 |      0| 2012-11-13 |      2300|

What is mean total number of steps taken per day?
-------------------------------------------------

### Total Number of Steps per Day

1.  Calculate the total number of steps taken per day

``` r
total_steps_per_day <- na.omit(activity) %>% group_by(date) %>% summarize(total_steps = sum(steps))
sample_data_frame(total_steps_per_day, 6)
```

| date       |  total\_steps|
|:-----------|-------------:|
| 2012-11-16 |          5441|
| 2012-10-17 |         13452|
| 2012-11-08 |          3219|
| 2012-11-05 |         10439|
| 2012-10-22 |         13460|
| 2012-10-27 |         10119|

### Histogram of the Total Steps per Day

1.  If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day.

``` r
total_steps_per_day_hist <- function(data) {
    par(lwd = 3)
    hist(data$total_steps,
         main = "Total Number of Steps Taken Each Day",
         xlab ="Total Steps",
         ylim = c(0, 35),
         font.lab = 2,
         col = alpha("burlywood1", 0.3),
         border = "burlywood3")
    box(lwd = 1, lty = "solid")
}

total_steps_per_day_hist(total_steps_per_day)
```

![](PA1_template_files/figure-markdown_github/unnamed-chunk-8-1.png)

### Mean and Median of the Total Steps per Day

1.  Calculate and report the mean and median of the total number of steps taken per day

Applying the mean and median functions over the total number of steps per day:

``` r
mean_total_steps_per_day <- mean(total_steps_per_day$total_steps)
median_total_steps_per_day <- median(total_steps_per_day$total_steps)

mean_total_steps_per_day
```

    ## [1] 10766.19

``` r
median_total_steps_per_day
```

    ## [1] 10765

The mean of the total number of steps each day is 10766.19 and the median is 10765.

What is the average daily activity pattern?
-------------------------------------------

### Time-series of the Average Steps

1.  Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis).

First we need to compute the average number of steps per interval:

``` r
average_steps_per_interval <- na.omit(activity) %>% group_by(interval) %>% summarize(average_steps = mean(steps))
sample_data_frame(average_steps_per_interval, 6)
```

|  interval|  average\_steps|
|---------:|---------------:|
|      1510|      35.4905660|
|       935|      45.2264151|
|      1405|      51.9622642|
|       530|       2.0943396|
|       110|       0.1509434|
|       635|      39.3396226|

Here's the time series for this data:

``` r
with(average_steps_per_interval,
     plot(interval, average_steps, type = "l",
     main = "Time-series of the Average Number of Steps Taken",
     xlab ="5-minute Interval Index",
     ylab ="Average Steps Taken",
     font.lab = 2,
     col = "burlywood2",
     lwd = 3)
)

with(average_steps_per_interval,
     polygon(interval, average_steps, col = alpha("burlywood1", 0.3), border = NA)
)
```

![](PA1_template_files/figure-markdown_github/unnamed-chunk-11-1.png)

### Interval with Maximum Average Number of Steps

1.  Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

``` r
maximum_number_steps <- slice(average_steps_per_interval, which.max(average_steps))
maximum_number_steps
```

|  interval|  average\_steps|
|---------:|---------------:|
|       835|        206.1698|

The interval with index 835 contains the maximum average number of steps (206.1698).

Imputing missing values
-----------------------

### Missing Values Report

1.  Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

``` r
na_summary <- function(data)
    data.frame(na_count = colSums(is.na(data)), na_fraction = colMeans(is.na(data)))

na_summary(activity)
```

|          |  na\_count|  na\_fraction|
|----------|----------:|-------------:|
| steps    |       2304|     0.1311475|
| date     |          0|     0.0000000|
| interval |          0|     0.0000000|

About 13% of the rows in the data-set have missing values. That's 2304 rows.

### Imputation Method

1.  Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

I'm going to use the [mice]() package to do imputation. The package offers the following imputation methods:

``` r
suppressWarnings(methods(mice))
```

    ##  [1] mice.impute.2l.norm      mice.impute.2lonly.mean 
    ##  [3] mice.impute.2lonly.norm  mice.impute.2lonly.pmm  
    ##  [5] mice.impute.2l.pan       mice.impute.cart        
    ##  [7] mice.impute.fastpmm      mice.impute.lda         
    ##  [9] mice.impute.logreg       mice.impute.logreg.boot 
    ## [11] mice.impute.mean         mice.impute.midastouch  
    ## [13] mice.impute.norm         mice.impute.norm.boot   
    ## [15] mice.impute.norm.nob     mice.impute.norm.predict
    ## [17] mice.impute.passive      mice.impute.pmm         
    ## [19] mice.impute.polr         mice.impute.polyreg     
    ## [21] mice.impute.quadratic    mice.impute.rf          
    ## [23] mice.impute.ri           mice.impute.sample      
    ## [25] mice.mids                mice.theme              
    ## see '?methods' for accessing help and source code

But given that the specification asked for a "non-sophisticated method", we are going to use "mean" for imputation.

``` r
imputation_model_fit <- mice(data = activity, method = "mean", printFlag = FALSE)
```

### New Imputed Data-set

1.  Create a new dataset that is equal to the original dataset but with the missing data filled in.

``` r
imputed_activity <- complete(imputation_model_fit)
na_summary(imputed_activity)
```

|          |  na\_count|  na\_fraction|
|----------|----------:|-------------:|
| steps    |          0|             0|
| date     |          0|             0|
| interval |          0|             0|

As expected, no `NA`'s to be found in the data-set.

### Histogram of the Total Steps per Day After Imputation

1.  Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

Let's compute the total steps per day for the imputed data:

``` r
imputed_total_steps_per_day <- imputed_activity %>% group_by(date) %>% summarize(total_steps = sum(steps))
sample_data_frame(imputed_total_steps_per_day, 6)
```

| date       |  total\_steps|
|:-----------|-------------:|
| 2012-11-30 |      10766.19|
| 2012-11-19 |       8841.00|
| 2012-10-29 |       5018.00|
| 2012-10-14 |      15098.00|
| 2012-11-16 |       5441.00|
| 2012-11-13 |       7336.00|

We can now compare the histograms for before and after the imputation:

``` r
all_total_steps_per_day <-rbind(mutate(total_steps_per_day, Imputed = "No"), mutate(imputed_total_steps_per_day, Imputed = "Yes"))

ggplot(data = all_total_steps_per_day, aes(x = total_steps, fill = Imputed)) +
    geom_histogram(alpha = 0.3, binwidth = 5000, boundary = -5000, position = "identity", colour = "burlywood3", size = 1) +
    ggtitle("Total Number of Steps Taken Each Day") +
    xlab("Total Steps") +
    ylab("Frequency") +
    ylim(c(0, 40)) +
    scale_x_continuous(breaks = seq(from = 0, to = 25000, by = 5000)) +
    scale_fill_manual(values = c("burlywood3", "burlywood1")) +
    theme(plot.title = element_text(size = 16, face = "bold",
                                    hjust = 0.5, margin = margin(b = 30, unit = "pt"))) +
    theme(axis.title.x = element_text(size = 12, face="bold")) +
    theme(axis.title.y = element_text(size = 12, face="bold")) +
    theme(panel.background = element_blank(), axis.line = element_line(colour = "black")) +
    theme(panel.border = element_rect(colour = "black", fill = NA, size = 0.5))
```

![](PA1_template_files/figure-markdown_github/unnamed-chunk-18-1.png)

As expected, we see an increase on the frequency for the middle bin, which makes sense, given that we have imputed the missing data with the mean.

``` r
mean_imputed_total_steps_per_day <- mean(imputed_total_steps_per_day$total_steps)
median_imputed_total_steps_per_day <- median(imputed_total_steps_per_day$total_steps)

mean_imputed_total_steps_per_day
```

    ## [1] 10766.19

``` r
median_imputed_total_steps_per_day
```

    ## [1] 10766.19

The mean of the total number of steps each day after imputation is 10766.19 and the median is 10766.19.

The mean remained the same and the median is equal to the mean after the imputation.

Are there differences in activity patterns between weekdays and weekends?
-------------------------------------------------------------------------

### Type of Day ("weekday" or "weekend")

1.  Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

``` r
to_weekday <- function(day)
    ifelse(weekdays(day) %in% c("Saturday", "Sunday"), "weekend", "weekday")

imputed_activity <- mutate(imputed_activity, day_type = to_weekday(as.Date(date)))
sample_data_frame(imputed_activity, 6)
```

|       |     steps| date       |  interval| day\_type |
|-------|---------:|:-----------|---------:|:----------|
| 15377 |    0.0000| 2012-11-23 |       920| weekday   |
| 11249 |   37.3826| 2012-11-09 |       120| weekday   |
| 15191 |    0.0000| 2012-11-22 |      1750| weekday   |
| 10814 |  147.0000| 2012-11-07 |      1305| weekday   |
| 11045 |    0.0000| 2012-11-08 |       820| weekday   |
| 13775 |   55.0000| 2012-11-17 |      1950| weekend   |

### Time-series of the Average Steps per Type of Day

1.  Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

As earlier, first we need to compute the average number of steps per interval:

``` r
imputed_average_steps_per_interval <- imputed_activity %>% group_by(interval, day_type) %>% summarize(average_steps = mean(steps))
sample_data_frame(imputed_average_steps_per_interval, 6)
```

|  interval| day\_type |  average\_steps|
|---------:|:----------|---------------:|
|       220| weekend   |        4.672825|
|      1715| weekday   |       44.895458|
|       405| weekday   |        6.095458|
|       805| weekend   |       53.797825|
|      1830| weekend   |       68.360325|
|       805| weekday   |       67.851013|

Here's the time series for this data broken by weekday:

``` r
ggplot(imputed_average_steps_per_interval, aes(x = interval, y = average_steps)) +
    geom_area(colour = "burlywood2", fill = "burlywood1", size = 1, alpha = 0.3) +
    ggtitle("Time-series of the Average Number of Steps Taken") +
    xlab("5-minute Interval Index") +
    ylab("Average Steps Taken") +
    facet_wrap(~ day_type, ncol = 1) +
    theme(plot.title = element_text(size = 16, face = "bold",
                                    hjust = 0.5, margin = margin(b = 30, unit = "pt"))) +
    theme(axis.title.x = element_text(size = 12, face="bold")) +
    theme(axis.title.y = element_text(size = 12, face="bold")) +
    theme(panel.background = element_blank(), axis.line = element_line(colour = "black")) +
    theme(panel.border = element_rect(colour = "black", fill = NA, size = 0.5)) +
    theme(strip.background = element_rect(fill = alpha("burlywood3", 0.3), color = "black", size = 0.5))
```

![](PA1_template_files/figure-markdown_github/unnamed-chunk-22-1.png)

The activity patterns indeed look different: On weekdays the activity seems more concentrated at the beginning of the day, while during weekends seems more distributed along the day.
