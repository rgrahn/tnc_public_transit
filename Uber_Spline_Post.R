{library(AER)
  library(tidyverse)
  library(knitr)
  library(quantreg)
  library(mgcv)
  library(arm)
  library(car)
  library(HLMdiag)
  library(lmtest)
  library(plm)
  library(zoo)
  library(cvTools)
  library(DataCombine)
  library(scales)
  require(lubridate)
  library(reshape2)
  library(DT)}


{setwd("C:/Users/rickr/Desktop/Research_CMU/Uber_Surge/Data_Code")
df <- read.csv("lyft_cost_pit_sep16_mar17.csv")
tran.poi <- read.csv("buff_tran_2000ft.csv")}


d2 <- as.POSIXct(df$request_date, format='%Y-%m-%d %H:%M:%S', tz="GMT")
df$datetime <- format(d2,tz="America/New_York")

#uber
df1 <- data.frame(utc_datetime = df$request_date, est_datetime=df$datetime, 
                  surge = df$surge_multiplier, place_label = df$place_label, 
                  display_name = df$display_name)
#lyft
df1 <- data.frame(utc_datetime = df$request_date, est_datetime=df$datetime, 
                  surge = df$primetime_percentage, place_label = df$place_label, 
                  display_name = df$display_name)

df1$surge = as.character(df1$surge)
df1$surge = substr(df1$surge,1,nchar(df1$surge)-1)
df1$surge = as.numeric(df1$surge)

allegheny <- data.frame(poi = unique(tran.poi$Name))

#uber
df2 <- df1 %>%
       filter(place_label %in% allegheny$poi, display_name == "uberX")
#lyft
df2 <- df1 %>%
  filter(place_label %in% allegheny$poi, display_name == "Lyft")

df2$est_datetime <- as.POSIXct(df2$est_datetime, "EST")
a <- df2[order(df2$place_label,df2$est_datetime),]
a$round_date <- round_date(a$est_datetime, unit = "10 minutes")
poi <- unique(df2$place_label)

j <- 0

for (i in poi){
    sub.df <- subset(a, place_label == i)
    j <- j + 1
    print(i)
    zero_time <- as.POSIXct(sub.df$est_datetime[1], "EST")
    sub.df$difftime <- as.numeric(difftime(sub.df$est_datetime, zero_time, units = "mins"))
    sub.df$difftime2 <- as.numeric(difftime(sub.df$round_date, zero_time, units = "mins"))
    newsurge <- splinefun(sub.df$difftime, sub.df$surge, method = "monoH.FC")
    sub.df$surge_int <- newsurge(sub.df$difftime2)
    if(j == 1){
      complete.df <- sub.df
    }else{
      complete.df <- rbind(complete.df, sub.df)
    }
}



complete.df1 <- complete.df %>%
                group_by(place_label, round_date) %>%
                summarize(surge_int = mean(surge_int))

filt_date1 <- with(complete.df1, complete.df1[date(round_date) > as.Date('2016-11-05') & date(round_date) <= as.Date('2017-03-18'), ])
complete.df2 <- data.frame(filt_date1)
filt_date1 <- with(filt_date1, filt_date1[hour(round_date) >= 5 & hour(round_date) < 24, ])
complete.df2 <- data.frame(filt_date1)


date_range1 <- data.frame(DateTime = seq(as.POSIXct("2016-09-08", tz = "EST"),
                                as.POSIXct("2017-03-31", tz = "EST"), 
                                by=(10*60)))

filt_date <- with(date_range1, date_range1[date(DateTime) > as.Date('2016-11-05') & date(DateTime) <= as.Date('2017-03-18'), ])
date_range <- data.frame(filt_date)
filt_date <- with(date_range, date_range[hour(filt_date) >= 5 & hour(filt_date) < 24, ])
date_range <- data.frame(filt_date)

k <- 0
for (ii in poi){
  sub.df2 <- subset(complete.df2, place_label == ii)
  k <- k + 1
  join.df <- left_join(date_range, sub.df2, by = c("filt_date" = "round_date"))
  join.df <- mutate(join.df, place_label = zoo::na.locf(place_label), surge_int = zoo::na.approx(surge_int, maxgap = 1))
  if(k == 1){
    df <- join.df
  }else{
    df <- rbind(df, join.df)
  }
}

#lyft
complete.df <- mutate(df, surge_int = ifelse(surge_int <= 0, 0, surge_int))
#uber
complete.df <- mutate(df, surge_int = ifelse(surge_int <= 1, 1, surge_int))
colnames(complete.df)<- c("round_date","place_label","surge_multiplier")

#write.csv(complete.df, file = "Lyft_surge_spline.csv", row.names = TRUE)










