	###////////// PROJECT 1 - knitr \\\\\\\\\\\\###


# Downloading of documents
install.packages("knitr")
```{r}
url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"

download.file(url, file.path(getwd(), "repdata_data_activity.zip"))

unzip(zipfile = "repdata_data_activity.zip")

```

# Load the data (i.e.read.csv())
```{r}
actAll <- data.table::as.data.table(read.csv("activity.csv", header = TRUE, sep = ","))

```


####////////  What is mean total number of steps taken per day?

# 1. Calculate the total number of steps taken per day
```{r}
Total_Steps_perDay <- actAll[, lapply(.SD, sum, na.rm = TRUE), .SDcols = c("steps"), by = date]
```

# 2. Make a histogram of the total number of steps taken each day
```{r}
barplot(Total_Steps_perDay[, steps], axisnames = TRUE, xlab = "day", ylab = "Steps", main = "Total Steps per day")
```

# 3. Calculate and report the mean and median of the total number of steps taken per day
```{r}
Total_Steps_perDay[, .(MEAN_STEPS = mean(steps, na.rm= TRUE), MEDIAN_STEPS = median(steps, na.rm= TRUE))]
```



####////////  What is the average daily activity pattern?

# 1. Make a time series plot (i.e.type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
```{r}
Cincominutal <- actAll[, c(lapply(.SD, mean, na.rm = TRUE)), .SDcols = c("steps"), by = .(interval)] 
plot(Cincominutal[, interval], Cincominutal[, steps], type = "l", col = "orange", xlab = "day", ylab = "Steps", main = "Total Steps per day")
```

# 2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
```{r}
Cincominutal[steps == max(steps)]
```


####////////  Imputing missing values

# 1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
```{r}
actAll[,.(NUMBER_NAs = sum(is.na(steps)))]
```

# 2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. 
# For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
```{r}
actAll_NO_NA <- actAll
actAll_NO_NA[is.na(steps), "steps"] <- actAll_NO_NA[, c(lapply(.SD, median, na.rm = TRUE)), .SDcols = c("steps")]
```

# 3. Create a new dataset that is equal to the original dataset but with the missing data filled in.
```{r}
sumaNA <- actAll_NO_NA[, sum(is.na(steps))]
sumaNA 
actAll_NO_NA
```

# 4.  Make a histogram of the total number of steps taken each day and Calculate and report 
# the mean and median total number of steps taken per day. 
```{r}
actAll_NO_NA_perDay <- actAll_NO_NA[, lapply(.SD, sum, na.rm = TRUE), .SDcols = c("steps"), by = date]

barplot(actAll_NO_NA_perDay[, steps], col = "orange", axisnames = TRUE, xlab = "day", ylab = "Steps", main = "Total Steps per day - No NAs")

actAll_NO_NA_perDay[, .(MEAN_STEPS_noNAs = mean(steps), MEDIAN_STEPS_noNAs = median(steps))]
```

# Do these values differ from the estimates from the first part of the assignment? 
```{r}
Total_Steps_perDayType <- Total_Steps_perDay
Total_Steps_perDayType <- Total_Steps_perDayType[,.(date = Total_Steps_perDay[, date], steps = Total_Steps_perDay[, steps], Type = "Nas")]
Total_Steps__noNAs_perDayType <- actAll_NO_NA_perDay 
Total_Steps__noNAs_perDayType<-Total_Steps__noNAs_perDayType[,.(date = actAll_NO_NA_perDay[, date], steps = actAll_NO_NA_perDay[, steps], Type = "NO_Nas")]

mix_actAll <- rbind(Total_Steps_perDayType, Total_Steps__noNAs_perDayType)
```
#[,.(date = Total_Steps_perDay[, date], TotalSteps = Total_Steps_perDay[, steps], Steps_no_NAs = actAll_NO_NA_perDay[, steps])]
```{r}
ggplot2::ggplot(mix_actAll, ggplot2::aes(factor(date), steps, fill = Type )) + 
	ggplot2::geom_bar(stat="identity") + 
	ggplot2::facet_grid(.~Type,scales = "free",space="free") + 
	ggplot2::labs(x="days", y=expression("Total number of steps")) + 
	ggplot2::labs(title=expression("Steps per day considening invalid data and just valid data"))
```


# What is the impact of imputing missing data on the estimates of the total daily 
#number of steps?



# Are there differences in activity patterns between weekdays and weekends?
# For this part the weekdays() function may be of some help here. Use the dataset 
# with the filled-in missing values for this part.



# 1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” 
# indicating whether a given date is a weekday or weekend day.

```{r}
allAct <- data.table::as.data.table(read.csv("activity.csv", header = TRUE, sep = ","))

allAct[, date := as.POSIXct(date, format = "%Y-%m-%d")]
allAct[, Day := weekdays(x = date)]
allAct[grepl(pattern = "lunes|martes|miércoles|jueves|viernes", x = Day), "Type"] <- "weekday"
allAct[grepl(pattern = "sábado|domingo", x = Day), "Type"] <- "weekend"
allAct[, Type := as.factor(Type)]
```

# Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval
# (x-axis) and the average number of steps taken, averaged across all weekday days or 
# weekend days (y-axis). See the README file in the GitHub repository to see an example 
# of what this plot should look like using simulated data.

```{r}
allAct[is.na(steps), "steps"] <- actAll[, c(lapply(.SD, median, na.rm = TRUE)), .SDcols = c("steps")]
IntervalAll <- allAct[, c(lapply(.SD, mean, na.rm = TRUE)), .SDcols = c("steps"), by = .(interval, Type)] 

ggplot2::ggplot(IntervalAll , ggplot2::aes(x = interval , y = steps, fill= Type, color = Type)) + 
ggplot2::geom_line() + 
ggplot2::labs(title = "Avg. Daily Steps by Weektype", x = "Interval", y = "No. of Steps") + 
ggplot2::facet_wrap(~"weekday  or weekend" , ncol = 1, nrow=2)
```
