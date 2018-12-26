Coursera assignment, week2, Reproducible Research
-------------------------------------------------

Loading and preprocessing the data

Load libraries

    library(ggplot2)
    library(dplyr)
    library(readr)
    library(stringr)
    library(markdown)
    library(knitr)

Import the data

    act<-read.csv("activity.csv")

Change date's class to Date and inspect the table

    actv<-transform(act,date=as.Date(date))
    str(actv)

    ## 'data.frame':    17568 obs. of  3 variables:
    ##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
    ##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...

### Question 1: What is the mean total number of steps taken per day?

Ignoring NAs, as required, calculate the total number of daily steps and
inspect the new table

    actv_d<-summarize(group_by(actv,date),totsteps=sum(steps))
    str(actv_d)

    ## Classes 'tbl_df', 'tbl' and 'data.frame':    61 obs. of  2 variables:
    ##  $ date    : Date, format: "2012-10-01" "2012-10-02" ...
    ##  $ totsteps: int  NA 126 11352 12116 13294 15420 11015 NA 12811 9900 ...

Plot the histogram for daily steps

    g<-ggplot(actv_d,aes(totsteps))
    g+geom_histogram()  

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

    ## Warning: Removed 8 rows containing non-finite values (stat_bin).

![](PA1_template_files/figure-markdown_strict/histogram%20daily%20steps-1.png)

Calculate mean and median of daily steps and inspect table

    actv_dm<-summarize(actv_d,mean=mean(totsteps,na.rm=T),median=median(totsteps,na.rm=T))

Print the mean

    print(formatC(as.numeric(actv_dm[,1]),format="f",digits=1,big.mark=","))

    ## [1] "10,766.2"

Print the median

    print(formatC(as.numeric(actv_dm[,2]),format="f",digits=1,big.mark=","))

    ## [1] "10,765.0"

### Question 2: What is the average daily activity pattern?

Calculate the average number of steps taken at each 5 minute interval
across all days, and inspect the table

    actv_i<-summarize(group_by(actv,interval),avesteps=mean(steps,na.rm=T))
    str(actv_i)

    ## Classes 'tbl_df', 'tbl' and 'data.frame':    288 obs. of  2 variables:
    ##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
    ##  $ avesteps: num  1.717 0.3396 0.1321 0.1509 0.0755 ...

Plot the average daily activity, taking the opportunity to show on the
plot which 5 mins interval is the most active on average (interval=835)
and the average number of steps (avgsteps=206.2) as lines cutting both
axes

    h<-ggplot(actv_i,aes(interval,avesteps))+geom_line()+geom_vline(xintercept=filter(actv_i,avesteps==max(avesteps))$interval,linetype="dashed",color="violet")+scale_x_continuous(expand = c(0, 0),breaks= c(seq(min(actv_i$interval),max(actv_i$interval),by=500), filter(actv_i,avesteps==max(avesteps))$interval))+geom_hline(yintercept=max(actv_i$avesteps),linetype="dashed",color="violet")+scale_y_continuous(labels=scales::comma_format(accuracy=0.1), expand = c(0, 0),breaks= c(seq(min(actv_i$avesteps),max(actv_i$avesteps),by=25),max(actv_i$avesteps)))+theme_bw()+theme(axis.line = element_line(colour = "blue"), panel.border = element_blank())
    print(h)

![](PA1_template_files/figure-markdown_strict/plot%20avg%20daily%20actv-1.png)

### Question 3: Imputing missing values

Compute the number of rows with NAs, and inspect

    inc<-actv[!complete.cases(actv),]
    dim(inc)

    ## [1] 2304    3

There is a total of 2,304 rows with NAs

Fill the missing values in the data set

The strategy to fill the NAs will be to replace NAs with the mean for
that 5 minutes interval already calculated and stored in actv\_i. Create
a new dataset that is equal to the original data set but with the
missing data filled in with a loop. I prefer to create a new variable
"stf"" with all old existing and newly filled values, instead of
rewriting the old variable "steps""

    for(j in 1:nrow(actv)){
    actv$stf[j]<-ifelse(is.na(actv$steps[j])==TRUE,filter(actv_i,interval==actv$interval[j])$avesteps,actv$steps[j])
    }

Check that the new actv table has indeed no NAs in the new variable stf.

    length(actv$stf[is.na(actv$stf)])

    ## [1] 0

Make a histogram of the total number of steps taken each day and
Calculate and report the mean and median total number of steps taken per
day. Do these values differ from the estimates from the first part of
the assignment? What is the impact of imputing missing data on the
estimates of the total daily number of steps? Calculate the new total
number of steps, and inspect the new table

    actv_d<-summarize(group_by(actv,date),totsteps=sum(stf))
    str(actv_d)

    ## Classes 'tbl_df', 'tbl' and 'data.frame':    61 obs. of  2 variables:
    ##  $ date    : Date, format: "2012-10-01" "2012-10-02" ...
    ##  $ totsteps: num  10766 126 11352 12116 13294 ...

Histogram of new calculated steps variable

    g<-ggplot(actv_d,aes(totsteps))
    g+geom_histogram()

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](PA1_template_files/figure-markdown_strict/new%20hist-1.png)

Calculate again mean and median of total daily steps. Obviously, no need
to pass na.rm=T this time

    actv_dm<-summarize(actv_d,mean=mean(totsteps),median=median(totsteps))

Print the new mean

    print(formatC(as.numeric(actv_dm[,1]),format="f",digits=1,big.mark=","))

    ## [1] "10,766.2"

Print the new median

    print(formatC(as.numeric(actv_dm[,2]),format="f",digits=1,big.mark=","))

    ## [1] "10,766.2"

There is not a big impact as far as the shape of the histogram is
concerned. However, the frequency for the median value is higher, as
seen from the different y scales of the two histograms. From the
calculated values we see that mean and median are now equal. It must be
a consequence of the filling strategy followed.

### Question 4: Are there differences in activity patterns between weekdays and weekends?

The new factor to discriminate weekends and weekdays will be called
"day". Since my computer is set for **Spanish language** the selection
will be based on the Spanish names of the weekend days. This time I do
not need to use a loop. Important to remember that the old variable
"steps"" still contains NAs, and the new variable **"stf"** is the
result of the replacement strategy

    wend<-c("sábado","domingo")
    actv<-mutate(actv,day=as.factor(ifelse(weekdays(actv$date)%in%wend==TRUE,"weekend","weekday")))
    str(actv)

    ## 'data.frame':    17568 obs. of  5 variables:
    ##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
    ##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
    ##  $ stf     : num  1.717 0.3396 0.1321 0.1509 0.0755 ...
    ##  $ day     : Factor w/ 2 levels "weekday","weekend": 1 1 1 1 1 1 1 1 1 1 ...

Make a panel plot containing a time series plot of the 5-minute interval
(x-axis) and the average number of steps taken, averaged across all
weekday days or weekend days (y-axis).

Calculate the average number of steps by interval across all days and
split by the category weekend/weekday. Inspect the new table

    actv_w<-summarize(group_by(actv,day,interval),avesteps=mean(stf))
    str(actv_w)

    ## Classes 'grouped_df', 'tbl_df', 'tbl' and 'data.frame':  576 obs. of  3 variables:
    ##  $ day     : Factor w/ 2 levels "weekday","weekend": 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
    ##  $ avesteps: num  2.251 0.445 0.173 0.198 0.099 ...
    ##  - attr(*, "vars")= chr "day"
    ##  - attr(*, "drop")= logi TRUE

Plot the time series

    l<-ggplot(actv_w,aes(interval,avesteps))+geom_line()+facet_wrap(day~.,strip.position="top",ncol=1)+labs(y="Number of steps",x="Interval")
    print(l)

![](PA1_template_files/figure-markdown_strict/plot%20time%20series-1.png)
