# 

data=filter(january2018, ARR_DELAY>0 & DEP_DELAY>0)
ggplot(data, aes(DAY_OF_WEEK)) + geom_bar()
data=filter(august2018, ARR_DELAY>0 & DEP_DELAY>0)
ggplot(data, aes(DAY_OF_WEEK)) + geom_bar()

#T test for weekends and weekdays
weekends=filter(january2018, DAY_OF_WEEK>5)
weekdays=filter(january2018, DAY_OF_WEEK<6)
t.test(weekends$ARR_DELAY,weekdays$ARR_DELAY)
weekends=filter(august2018, DAY_OF_WEEK>5)
weekdays=filter(august2018, DAY_OF_WEEK<6)
t.test(weekends$ARR_DELAY,weekdays$ARR_DELAY)

# Yaya Liu
###January Departure Delay Percentage
#denoting day of week using names instead of numbers
january2018$DAY_OF_WEEK <- factor(january2018$DAY_OF_WEEK,levels = c(1,2,3,4,5,6,7),labels = c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))
view(january2018)
#leave out the flights that departed early
delay_Jan = filter(january2018, DEP_DELAY>0)
view(delay_Jan) 
#calculate the percentage of delayed flights in every day of the week through the whole month
mon_delay1=count(filter(delay_Jan,DAY_OF_WEEK=="Monday"))
mon_all1=count(filter(january2018,DAY_OF_WEEK=="Monday"))
mon_percent1=mon_delay1/mon_all1
tues_delay1=count(filter(delay_Jan,DAY_OF_WEEK=="Tuesday"))
tues_all1=count(filter(january2018,DAY_OF_WEEK=="Tuesday"))
tues_percent1=tues_delay1/tues_all1
wed_delay1=count(filter(delay_Jan,DAY_OF_WEEK=="Wednesday"))
wed_all1=count(filter(january2018,DAY_OF_WEEK=="Wednesday"))
wed_percent1=wed_delay1/wed_all1
thur_delay1=count(filter(delay_Jan,DAY_OF_WEEK=="Thursday"))
thur_all1=count(filter(january2018,DAY_OF_WEEK=="Thursday"))
thur_percent1=thur_delay1/thur_all1
fri_delay1=count(filter(delay_Jan,DAY_OF_WEEK=="Friday"))
fri_all1=count(filter(january2018,DAY_OF_WEEK=="Friday"))
fri_percent1=fri_delay1/fri_all1
sat_delay1=count(filter(delay_Jan,DAY_OF_WEEK=="Saturday"))
sat_all1=count(filter(january2018,DAY_OF_WEEK=="Saturday"))
sat_percent1=sat_delay1/sat_all1
sun_delay1=count(filter(delay_Jan,DAY_OF_WEEK=="Sunday"))
sun_all1=count(filter(january2018,DAY_OF_WEEK=="Sunday"))
sun_percent1=sun_delay1/sun_all1
#make a new data frame using the calculated percentage of delayed flights
january_dep_delay= data.frame(day_of_week= c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"), pct_delay= c(0.4914, 0.4322, 0.3706, 0.3554, 0.4699, 0.3806, 0.3676))
view(january_dep_delay)
#make a histogram of the percentage of delayed flights of every day of the week in January
day.of.week <- factor(january_dep_delay$day_of_week, levels=c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturaday", "Sunday"))
ggplot(january_dep_delay, aes(day.of.week, pct_delay)) +  
  geom_bar(stat = 'identity', fill="light pink", color = "black") +
  ggtitle("January Departure Delay Percentage") +
  theme(plot.title = element_text(hjust = 0.5)) 

###August Departure Delay Percentage
#denoting day of week using names instead of numbers
august2018$DAY_OF_WEEK <- factor(august2018$DAY_OF_WEEK,levels = c(1,2,3,4,5,6,7),labels = c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))
view(august2018)
#leave out the flights that departured early
delay_Aug = filter(august2018, DEP_DELAY>0)
view(delay_Aug) 
#calculate the percentage of delayed flights in every day of the week through the whole month
mon_delay8=count(filter(delay_Aug,DAY_OF_WEEK=="Monday"))
mon_all8=count(filter(august2018,DAY_OF_WEEK=="Monday"))
mon_percent8=mon_delay8/mon_all8
tues_delay8=count(filter(delay_Aug,DAY_OF_WEEK=="Tuesday"))
tues_all8=count(filter(august2018,DAY_OF_WEEK=="Tuesday"))
tues_percent8=tues_delay8/tues_all8
wed_delay8=count(filter(delay_Aug,DAY_OF_WEEK=="Wednesday"))
wed_all8=count(filter(august2018,DAY_OF_WEEK=="Wednesday"))
wed_percent8=wed_delay8/wed_all8
thur_delay8=count(filter(delay_Aug,DAY_OF_WEEK=="Thursday"))
thur_all8=count(filter(august2018,DAY_OF_WEEK=="Thursday"))
thur_percent8=thur_delay8/thur_all8
fri_delay8=count(filter(delay_Aug,DAY_OF_WEEK=="Friday"))
fri_all8=count(filter(august2018,DAY_OF_WEEK=="Friday"))
fri_percent8=fri_delay8/fri_all8
sat_delay8=count(filter(delay_Aug,DAY_OF_WEEK=="Saturday"))
sat_all8=count(filter(august2018,DAY_OF_WEEK=="Saturday"))
sat_percent8=sat_delay8/sat_all8
sun_delay8=count(filter(delay_Aug,DAY_OF_WEEK=="Sunday"))
sun_all8=count(filter(august2018,DAY_OF_WEEK=="Sunday"))
sun_percent8=sun_delay8/sun_all8
#make a new data frame using the calculated percentage of delayed flights
august_dep_delay= data.frame(day_of_week= c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturaday", "Sunday"), pct_delay= c(0.3496, 0.3381, 0.3779, 0.3877, 0.3646, 0.3389, 0.3010))
view(august_dep_delay)
#make a histogram of the percentage of delayed flights of every day of the week in August
day.of.week = factor(august_dep_delay$day_of_week, levels=c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturaday", "Sunday"))
ggplot(august_dep_delay, aes(day.of.week, pct_delay)) +  
  geom_bar(stat = 'identity', fill="light blue", color = "black") +
  scale_y_continuous(limits = c(0.0,0.5)) +
  ggtitle("August Departure Delay Percentage") +
  theme(plot.title = element_text(hjust = 0.5)) 

### Regression lines and Residual Plots
## create a function to compute sd 
sd_fpp <-function(x){sd(x)*sqrt((length(x)-1)/(length(x)))}
## compute the correlation of departure delay and arrival delay in January
dep_delay1=delay_Jan$DEP_DELAY
arr_delay1=delay_Jan$ARR_DELAY
r_delay1=cor(dep_delay1, arr_delay1)
# make a regression line of arrival delay and departure delay in January
(reg_slope=r_delay1*sd_fpp(dep_delay1)/sd_fpp(arr_delay1))
(reg_intercept=mean(dep_delay1)-reg_slope*mean(arr_delay1))
delay_lm=lm(ARR_DELAY ~ DEP_DELAY, data=delay_Jan)
ggplot(delay_Jan,aes(x=DEP_DELAY,y = ARR_DELAY))+
  geom_point(color="orange",alpha=0.3)+
  geom_smooth(method = lm,se=FALSE) +
  ggtitle("January Delay Regression Line") +
  theme(plot.title = element_text(hjust = 0.5)) 
# make a residual plot for the regression line
delay_resid=resid(delay_lm)
plot(delay_Jan$DEP_DELAY, delay_resid, ylab="RESIDUALS", xlab="DEP_DELAY", main="JanuaryDELAY Residual Plot")
abline(0, 0)
## compute the correlation of departure delay and arrival delay in August
dep_delay8=august2018$DEP_DELAY
arr_delay8=august2018$ARR_DELAY
r_delay8=cor(dep_delay8, arr_delay8)
# make a regression line of arrival delay and departure delay in August
(reg_slope=r_delay8*sd_fpp(dep_delay8)/sd_fpp(arr_delay8))
(reg_intercept=mean(dep_delay8)-reg_slope*mean(arr_delay8))
delay_lm=lm(ARR_DELAY ~ DEP_DELAY, data=delay_Aug)
ggplot(delay_Aug,aes(x=DEP_DELAY,y = ARR_DELAY))+
  geom_point(color="orange",alpha=0.3)+
  geom_smooth(method = lm,se=FALSE) +
  ggtitle("August Delay Regression Line") +
  theme(plot.title = element_text(hjust = 0.5)) 
# make a residual plot for the regression line
delay_resid=resid(delay_lm)
plot(delay_Aug$DEP_DELAY, delay_resid, ylab="RESIDUALS", xlab="DEP_DELAY", main="AugustDELAY Residual Plot")
abline(0, 0)
# count the flights in both months that delayed to take off but arrived early
delay_dep_early_arr_JAN=count(filter(january2018, DEP_DELAY>0, ARR_DELAY<0))
delay_dep_early_arr_JAN
delay_dep_early_arr_Aug=count(filter(august2018, DEP_DELAY>0, ARR_DELAY<0))
delay_dep_early_arr_Aug
count(delay_Jan)
count(delay_Aug)

# Jiehui Li
library(dplyr)
view(august2018)
real_arrival_delay_Aug = filter(august2018, ARR_DELAY > 0)
boxplot(ARR_DELAY~DAY_OF_WEEK, data=real_arrival_delay_Aug, main=toupper("relation between day of week and the arrival delay in August"), font.main=3, cex.main=1.2, xlab="the day of week", ylab="the minutes delay", font.lab=3, col="darkgreen")

Line plot:
  ###mean for arrival delay in Aug
  view(august2018)
library(dplyr)
real_arrival_delay_Aug = filter(august2018, ARR_DELAY > 0)
view(real_arrival_delay_Aug)
week_group_august = group_by(real_arrival_delay_Aug, DAY_OF_WEEK)
arrival_mean_august = summarize(week_group_august, arrival_mean = mean(ARR_DELAY))
View(arrival_mean_august)

###median for arrival delay in Aug
arrival_median_Aug = summarize(week_group_august, arrival_median = median(ARR_DELAY))
View(arrival_median_Aug)

###mean for departure delay in Aug
real_departure_delay_Aug = filter(august2018, DEP_DELAY > 0)
view(real_departure_delay_Aug)
week_group_august2 = group_by(real_departure_delay_Aug, DAY_OF_WEEK)
departure_mean_august = summarize(week_group_august2, departure_mean = mean(DEP_DELAY))
View(departure_mean_august)

###median for departure delay in Aug
departure_median_Aug = summarize(week_group_august2, departure_median = median(DEP_DELAY))
view(departure_median_Aug)

library(ggplot2)
x <- arrival_mean_august$DAY_OF_WEEK
y1<- arrival_mean_august$arrival_mean
y2 <- arrival_median_Aug$arrival_median
y3 <- departure_mean_august$departure_mean
y4 <- departure_median_Aug$departure_median
df <- data.frame(x,y1,y2,y3,y4)
ggplot(df,aes(x),group=1) +
  geom_line(aes(y=y1,color= "red"), size=2) +
  geom_line(aes(y=y2,color= "orange"),size=2) +
  geom_line(aes(y=y3,color= "green"),size=2) +
  geom_line(aes(y=y4,color= "blue"),size=2) +
  geom_point(aes(y=y1), size=2) +
  geom_point(aes(y=y2),size=2) +
  geom_point(aes(y=y3),size=2) +
  geom_point(aes(y=y4),size=2) +
  geom_text(aes(y=y1),label=round(y1),position = position_jitter(width=0,height = 5)) +
  geom_text(aes(y=y2),label=round(y2),position = position_jitter(width=0,height = 5)) +
  geom_text(aes(y=y3),label=round(y3),position = position_jitter(width=0,height = -5)) +
  geom_text(aes(y=y4),label=round(y4),position = position_jitter(width=0,height = -5)) +
  ggtitle("Relation Between Day of Week and Delay in August") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(y="arrival delay (mins)", x = "day of week", size= 3) +
  scale_colour_manual("Delay",labels = c("mean of arrival delay","median of arrival delay","mean of departure delay","median of departure delay"),
                      values=c("red","orange","green","blue"))


###mean for arrival delay in Aug
view(january2018)
library(dplyr)
real_arrival_delay_Aug = filter(january2018, ARR_DELAY > 0)
view(real_arrival_delay_Aug)
week_group_august = group_by(real_arrival_delay_Aug, DAY_OF_WEEK)
arrival_mean_august = summarize(week_group_august, arrival_mean = mean(ARR_DELAY))
view(arrival_mean_august)

###median for arrival delay in Aug
arrival_median_Aug = summarize(week_group_august, arrival_median = median(ARR_DELAY))
view(arrival_median_Aug)

###mean for departure delay in Aug
real_departure_delay_Aug = filter(january2018, DEP_DELAY > 0)
view(real_departure_delay_Aug)
week_group_august2 = group_by(real_departure_delay_Aug, DAY_OF_WEEK)
departure_mean_august = summarize(week_group_august2, departure_mean = mean(DEP_DELAY))
view(departure_mean_august)

###median for departure delay in Aug
departure_median_Aug = summarize(week_group_august2, departure_median = median(DEP_DELAY))
view(departure_median_Aug)

library(ggplot2)
x <- arrival_mean_august$DAY_OF_WEEK
y1<- arrival_mean_august$arrival_mean
y2 <- arrival_median_Aug$arrival_median
y3 <- departure_mean_august$departure_mean
y4 <- departure_median_Aug$departure_median
df <- data.frame(x,y1,y2,y3,y4)
ggplot(df,aes(x),group=1) +
  geom_line(aes(y=y1,color= "orange"), size=2) +
  geom_line(aes(y=y2,color= "red"),size=2) +
  geom_line(aes(y=y3,color= "blue"),size=2) +
  geom_line(aes(y=y4,color= "green"),size=2) +
  geom_point(aes(y=y1), size=2) +
  geom_point(aes(y=y2),size=2) +
  geom_point(aes(y=y3),size=2) +
  geom_point(aes(y=y4),size=2) +
  geom_text(aes(y=y1),label=round(y1),position = position_jitter(width=0,height = 5)) +
  geom_text(aes(y=y2),label=round(y2),position = position_jitter(width=0,height = 5)) +
  geom_text(aes(y=y3),label=round(y3),position = position_jitter(width=0,height = -5)) +
  geom_text(aes(y=y4),label=round(y4),position = position_jitter(width=0,height = -5)) +
  ggtitle("Relation Between Day of Week and Delay in January") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(y="arrival delay (mins)", x = "day of week", size= 3) +
  scale_colour_manual("Delay",labels = c("mean of arrival delay","median of arrival delay","mean of departure delay","median of departure delay"),
                      values=c("red","orange","green","blue"))


# Regression: Henry and Edward

library(dplyr)
library(ggplot2)
library(reshape)

# 1.Read data
df_jan <- read.csv("january2018.csv")
df_aug <- read.csv("august2018.csv")

df <- rbind(df_aug, df_jan)

# 2. Get stats
df_means <- df %>%
  select(OP_UNIQUE_CARRIER, DEP_DELAY, ARR_DELAY) %>%
  group_by(OP_UNIQUE_CARRIER) %>%
  summarise_all(funs(mean))

df_medians <- df %>%
  select(OP_UNIQUE_CARRIER, DEP_DELAY, ARR_DELAY) %>%
  group_by(OP_UNIQUE_CARRIER) %>%
  summarise_all(funs(median))

colnames(df_means) <- c('Carrier', 'Departure delay mean', 'Arrival delay mean')
colnames(df_medians) <- c('Carrier', 'Departure delay median', 'Arrival delay median')

df_stats <- merge(df_means, df_medians, by = "Carrier", all = TRUE)

# 3. Plot
df_plot <- melt(df_stats)


p <- ggplot(data=df_plot, aes(x=Carrier, y=value, fill=variable)) +
  geom_bar(stat="identity", position=position_dodge()) +
  ggtitle("Statistics of flights by different carriers") +
  xlab("Carrier") + ylab("Minutes")

ggsave('plot.png', p, device = 'png', width = 9, height = 6, units = c("in"), dpi = 300)








