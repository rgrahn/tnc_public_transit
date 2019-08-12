#------------------------Load Packages-----------------------------------------
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
  library(DT)
  library(data.table)}

#-------------------------------------------------------------------------------
#Load Data
{setwd("C:/Users/rickr/Desktop/Research/Uber_Surge/Data_Code")
  raw.transit <- read.csv("transit_counts_1500ft_after.csv")
  raw.surge <- read.csv("Uber_surge_spline.csv")
  transit.poi.map <- read.csv("buff_tran_1500ft.csv")}

#----------------------------------------------------------------------------
#Delete some columns that are not needed and create data frame
{transit.df <- data.frame(DOW = raw.transit$DOW, VEHNOA = raw.transit$VEHNOA,
                          QSTOPA = raw.transit$QSTOPA, ON = raw.transit$ON,
                          LOAD = raw.transit$LOAD, LAT = raw.transit$stop_lat_x,
                          LONG = raw.transit$stop_lon_x, DATE = raw.transit$date,
                          TIME = raw.transit$time, DATETIME = raw.transit$datetime,
                          DOW_TEXT = raw.transit$day_of_week, ONES = raw.transit$ones,
                          ROUTE = raw.transit$ROUTE, STOP = raw.transit$ANAME)


#change data types
transit.df <- transform(transit.df, VEHNOA = as.character(VEHNOA))
transit.df$MODEL = str_sub(transit.df$VEHNOA, 1, 2)
transit.df <- transform(transit.df, MODEL = as.numeric(MODEL))
#add in bus capacities for the different bus models. Data obtained from the Port Authority
bus.capacity <- data.frame(MODEL = c(30,31,50,51,52,53,54,55,56,57,58,59,32,33,39,19,17,60,61,62),
                           CAPACITY = c(63,60,37,37,37,37,37,40,40,40,40,42,56,42,60,57,42,42,42,42))

transit.df1 <- left_join(transit.df, bus.capacity, by = c("MODEL"))

#Sum all transit riders for all buses at each specific 
#location and at each specific time. Duplicate values
#exist for VEHNOA so need to aggregate here first.
transit <- transit.df1 %>%
  group_by(TIME, QSTOPA, VEHNOA, DATETIME) %>%
  summarise(ONES = mean(ONES), RIDERSHIP = sum(ON), CAP = mean(CAPACITY))

transit <- filter(transit, CAP >= RIDERSHIP)

#inner join transit stop information with the appropriate poi for uber surge data
transit.new <- left_join(transit, transit.poi.map, by = c("QSTOPA" = "stop_id"))}

#==========================================================================================

#delete some columns
{transit.new$geometry = NULL
transit.new$index_right = NULL
transit.new$X = NULL}

#count number of buses
{bus_count <- transit.new %>%
    group_by(Name, DATETIME) %>%
    summarize(BUS_COUNT = n_distinct(VEHNOA))
  
  bus_count$DATETIME <- strptime(as.character(bus_count$DATETIME), format = "%Y-%m-%d %H:%M:%S")
  bus_count$DATETIME <- as.POSIXct(bus_count$DATETIME)
  
  #aggregate all bus stops by surge point of interest at a 10 min level
  df.agg <- transit.new %>% 
    group_by(Name, DATETIME) %>%
    summarize(RIDERSHIP = sum(RIDERSHIP), total.stops = sum(ONES))
  
  df.agg$DATETIME <- strptime(as.character(df.agg$DATETIME), format = "%Y-%m-%d %H:%M:%S")
  df.agg$DATETIME <- as.POSIXct(df.agg$DATETIME)
  
  #filter based on times when port authority buses are running
  df.agg <- with(df.agg, df.agg[hour(DATETIME) >= 5 & hour(DATETIME) < 24, ])
  
  df.agg <- left_join(df.agg, bus_count, by = c("DATETIME", "Name"))
  
  rm(bus_count)
  
  #remove of all points of interest without transit stops
  a <- list(unique(transit.poi.map$Name))
  a1 <- unlist(a, use.names = FALSE)
  
  b <- raw.surge[raw.surge$place_label %in% a1,]
  
  b$DATETIME <- strptime(as.character(b$round_date), format = "%Y-%m-%d %H:%M:%S", tz = "America/New_York")
  b$DATETIME <- as.POSIXct(b$round_date)
  b$round_date <- NULL
  
  df <- left_join(b, df.agg, by = c("DATETIME", "place_label" = "Name"))}

#remove all unwanted columns
{df$X <- NULL
  df$date <- NULL
  df$hr_min <- NULL
  df$hr <- NULL
  df$min <- NULL
  df$hr_str <- NULL
  df$min_str <- NULL
  df$time <- NULL
  df$temp <- NULL}

{df.new <- df
  df.new$DATE <- as.Date(df.new$DATETIME)
  df.new$TIME <- format(as.POSIXct(df.new$DATETIME) ,format = "%H:%M:%S") 
#===========================================================================================================  
  #add  other dependent variables to data set (weather, traffic, events, etc.)
#============================================================================================================
  df.new$day_of_week <- weekdays(df.new$DATETIME)
  df.new <- mutate(df.new, WEEKEND_IND = ifelse(day_of_week == "Saturday" | day_of_week == "Sunday", 1, 0))
  
  weather <- read.csv("Pitt_Hourly_Weather_16-17.csv")
  weather$Time <- as.character(weather$Time)
  weather$Date <- as.character(weather$Date)
  
  weather$DATETIME <- as.POSIXct(paste(weather$Date, weather$Time), format="%Y-%m-%d %H:%M")
  weather.df <- data.frame(TEMP = weather$Temperature, PC = weather$Pavement_condition,
                           RAIN = weather$Rain, SNOW = weather$Snow, DATETIME = weather$DATETIME,
                           WIND = weather$Wind_gust, HUM = weather$Humidity )
  
  df_full <- left_join(df.new, weather.df, by = c("DATETIME"))
  
  #all weather data is only hourly. This copies the hourly for all the 10 min timesteps
  #within the hour
  dff <- mutate(df_full, RAIN = zoo::na.locf(RAIN), SNOW = zoo::na.locf(SNOW),
                TEMP = zoo::na.locf(TEMP), PC = zoo::na.locf(PC),
                WIND = zoo::na.locf(WIND), HUM = zoo::na.locf(HUM))
  
  #Create an indicator variable that is one for any precipication condition.
  dff <- mutate(dff, PREC_IND = ifelse(RAIN > 0 | SNOW > 0, 1, 0))
  
  dff <- replace_na(dff, list(BUS_COUNT = 0))
  
  rm(df.agg, raw.transit, transit, transit.df, transit.new)}


#add penguins games to the consol events center POI for the 2016-2017 season
{pens.games <- read.csv("penguins_games.csv")
  pens.games$START_DATETIME <- strptime(as.character(pens.games$START_TIME), format = "%m/%d/%y %H:%M")
  pens.games$START_DATETIME <- as.POSIXct(pens.games$START_DATETIME)
  pens.games$END_DATETIME <- strptime(as.character(pens.games$END_TIME), format = "%m/%d/%y %H:%M")
  pens.games$END_DATETIME <- as.POSIXct(pens.games$END_DATETIME)
  
  pens.games$START_TIME <- NULL
  pens.games$END_TIME <- NULL
  pens.games$EVENT <- 1
  
  dff <- left_join(dff, pens.games, by = c("place_label" = "POI", "DATETIME" = "END_DATETIME"))
  
  dff$EVENT_IND <- NA
  
  dff <- replace_na(dff, list(EVENT = 0))
  
  for(i in 1:dim(dff)[1]){
    if(dff$EVENT[i] == 1){
      dff$EVENT_IND[i] <- 1
      dff$EVENT_IND[i + 1] <- 1
      dff$EVENT_IND[i + 2] <- 1
      dff$EVENT_IND[i + 3] <- 1
      dff$EVENT_IND[i + 4] <- 1
      dff$EVENT_IND[i + 5] <- 1
    }
  }
}
rm(df_full, df.new)

poi_considered <- c("Benedum Center", "CONSOL Energy Center", "Pitt", "CMU", "Wilkinsburg",
                    "South Side","North Side","Strip District", "Shadyside","East Liberty")

final_df <- filter(dff, place_label %in% poi_considered)


#================================= Add INRIX Data ===========================================
traffic <- read.csv("traffic_conditions.csv")
traffic$DATETIME <- as.POSIXct(traffic$measurement_tstamp, format="%Y-%m-%d %H:%M:%S")

dff1 <- left_join(final_df, traffic, by = "DATETIME")

{dff1$PC <- NULL
  dff1$HUM <- NULL
  dff1$START_DATETIME <- NULL
  dff1$X <- NULL
  dff1$measurement_tstamp <- NULL}

#-------------------------------------------------------------------------------------------------------------
#Intermediate step to write csv for constructed data set for unfiltered case
#write.csv(dff1, file = "Transit2000.csv")

#================================== Filter out data to only consider "surge events" ===================================
dff1 <- read.csv("Transit1500.csv")

# ======================================================================================================================
##check the number of filtered instances by POI that were filtered because of no bus arrivals
#dff1$ones <- 1

#x <- dff1 %>%
#  group_by(place_label, BUS_COUNT) %>%
#  summarize(total = sum(ones))

#x1 <- filter(x, BUS_COUNT == 0)

#x2 <- x %>%
#  group_by(place_label) %>%
#  summarize(tot = sum(total))

#x1$prop <- x1$total / 19836
#======================================================================================================================
# Filter dataset to only include a) time periods with no surge, b)time periods immediately before surge, and
# c) time periods immediately after surge
#======================================================================================================================
threshold <- 1.4
threshold2 <- 1.2
dff1$SURGE_IND2 <- NA
poi <- unique(dff1$place_label)

for(jj in 1:length(poi)){
  pitt1.df <- dff1 %>%
    filter(place_label == poi[jj])
  print(poi[jj])
  for(ii in 3:(dim(pitt1.df)[1]-1)){
    if(pitt1.df$surge_multiplier[ii] >= threshold & (!(is.na(pitt1.df$surge_multiplier[ii])))){
      pitt1.df$SURGE_IND[ii] <- 1
    }else if(is.na(pitt1.df$surge_multiplier[ii])){
      pitt1.df$SURGE_IND[ii] <- NA
    }else{
      pitt1.df$SURGE_IND[ii] <- 0}
    if(pitt1.df$surge_multiplier[ii] >= threshold2 & (!(is.na(pitt1.df$surge_multiplier[ii])))){
      pitt1.df$SURGE_IND2[ii] <- 2
    }else{
      pitt1.df$SURGE_IND2[ii] <- 0
    }
    #if(pitt1.df$surge_multiplier[ii - 1] < threshold & (!(is.na(pitt1.df$surge_multiplier[ii-1])))
    #  & pitt1.df$surge_multiplier[ii - 2] < threshold & (!(is.na(pitt1.df$surge_multiplier[ii-2])))){
    #pitt1.df$SURGE_IND2[ii - 2] <- 2}}
  }
  if(jj > 1){
    pitt1.df <- rbind(full1.df, pitt1.df)
  }
  full1.df <- pitt1.df
}

dff1 <- pitt1.df

{for(j in 1:length(poi)){
  pitt.df <- dff1 %>%
    filter(place_label == poi[j])
  print(poi[j])
  
  pitt.df <- replace_na(pitt.df, list(BUS_COUNT = 0))
  pitt.df <- replace_na(pitt.df, list(SURGE_IND = 99))
  pitt.df$t.lag <- NA
  pitt.df$t.current <- NA
  pitt.df$t.lead <- NA
  
  for(i in 4:(dim(pitt.df)[1]-2)){
    t.lag = pitt.df$SURGE_IND[i - 1]
    t.lag2 = pitt.df$SURGE_IND[i - 2]
    t.lag3 = pitt.df$SURGE_IND[i - 3]
    t.current = pitt.df$SURGE_IND[i]
    t.lead = pitt.df$SURGE_IND[i + 1]
    t.lead2 = pitt.df$SURGE_IND[i + 2]
    if(pitt.df$BUS_COUNT[i] > 0 & t.lag == 0 & t.current == 0 & t.lead == 0){
      pitt.df$t.lag[i] <- t.lag
      pitt.df$t.current[i] <- t.current
      pitt.df$t.lead[i] <- t.lead
    }else if(pitt.df$BUS_COUNT[i] > 0 & t.lag2 == 0 & t.lag == 0 & t.current == 1 & pitt.df$SURGE_IND2[i+1] == 2){
      pitt.df$t.lag[i] <- t.lag
      pitt.df$t.current[i] <- t.current
      pitt.df$t.lead[i] <- t.lead
      #}else if(pitt.df$BUS_COUNT[i] > 0 & t.lag == 1 & t.current == 0 & t.lead == 0 & t.lag2 == 1){
      #pitt.df$t.lag[i] <- t.lag
      #pitt.df$t.current[i] <- t.current
      #pitt.df$t.lead[i] <- t.lead
      #}else if(pitt.df$BUS_COUNT[i] > 0 & t.lag == 1 & t.current == 1 & t.lead == 1){
      #pitt.df$t.lag[i] <- t.lag
      #pitt.df$t.current[i] <- t.current
      #pitt.df$t.lead[i] <- t.lead
      #}else if(pitt.df$BUS_COUNT[i] > 0 & t.lag == 1 & t.current == 1 & t.lead == 0 & t.lead2 == 0){
      #pitt.df$t.lag[i] <- t.lag
      #pitt.df$t.current[i] <- t.current
      #pitt.df$t.lead[i] <- t.lead
    }else if(pitt.df$BUS_COUNT[i] > 0 & t.lag == 0 & t.current == 0 & t.lead == 1 & pitt.df$SURGE_IND2[i+2] == 2){
      pitt.df$t.lag[i] <- t.lag
      pitt.df$t.current[i] <- t.current
      pitt.df$t.lead[i] <- t.lead
    }else{ 
      pitt.df$t.current[i] <- 99
    }
  }
  if(j == 1){
    full.df <- pitt.df
  }else{
    full.df <- rbind(full.df, pitt.df)
  }
  
}
}


pitt.filter <- full.df %>%
  filter(t.current != 99)

#write.csv(pitt.filter, file = "Transit_df_1_2_1500.csv")

#====================================================================================================
#                      Create a lagged data set for robustness checks
#     Just moving the treatment to the time period before  the actual surge takes place
#====================================================================================================
dff1 <- read.csv("Transit1500.csv")

threshold <- 1.6
threshold2 <- 1.2
dff1$SURGE_IND2 <- NA
poi <- unique(dff1$place_label)

for(jj in 1:length(poi)){
  pitt1.df <- dff1 %>%
    filter(place_label == poi[jj])
  print(poi[jj])
  for(ii in 3:(dim(pitt1.df)[1]-1)){
    if(pitt1.df$surge_multiplier[ii] >= threshold & (!(is.na(pitt1.df$surge_multiplier[ii])))){
      pitt1.df$SURGE_IND[ii-1] <- 1
    }else if(is.na(pitt1.df$surge_multiplier[ii])){
      pitt1.df$SURGE_IND[ii-1] <- NA
    }else{
      pitt1.df$SURGE_IND[ii-1] <- 0}
    if(pitt1.df$surge_multiplier[ii] >= threshold2 & (!(is.na(pitt1.df$surge_multiplier[ii])))){
      pitt1.df$SURGE_IND2[ii-1] <- 2
    }else{
      pitt1.df$SURGE_IND2[ii-1] <- 0
    }
    #if(pitt1.df$surge_multiplier[ii - 1] < threshold & (!(is.na(pitt1.df$surge_multiplier[ii-1])))
    #  & pitt1.df$surge_multiplier[ii - 2] < threshold & (!(is.na(pitt1.df$surge_multiplier[ii-2])))){
    #pitt1.df$SURGE_IND2[ii - 2] <- 2}}
  }
  if(jj > 1){
    pitt1.df <- rbind(full1.df, pitt1.df)
  }
  full1.df <- pitt1.df
}

dff1 <- pitt1.df

{for(j in 1:length(poi)){
  pitt.df <- dff1 %>%
    filter(place_label == poi[j])
  print(poi[j])
  
  pitt.df <- replace_na(pitt.df, list(BUS_COUNT = 0))
  pitt.df <- replace_na(pitt.df, list(SURGE_IND = 99))
  pitt.df$t.lag <- NA
  pitt.df$t.current <- NA
  pitt.df$t.lead <- NA
  
  for(i in 4:(dim(pitt.df)[1]-2)){
    t.lag = pitt.df$SURGE_IND[i - 1]
    t.lag2 = pitt.df$SURGE_IND[i - 2]
    t.lag3 = pitt.df$SURGE_IND[i - 3]
    t.current = pitt.df$SURGE_IND[i]
    t.lead = pitt.df$SURGE_IND[i + 1]
    t.lead2 = pitt.df$SURGE_IND[i + 2]
    if(pitt.df$BUS_COUNT[i] > 0 & t.lag == 0 & t.current == 0 & t.lead == 0){
      pitt.df$t.lag[i] <- t.lag
      pitt.df$t.current[i] <- t.current
      pitt.df$t.lead[i] <- t.lead
    }else if(pitt.df$BUS_COUNT[i] > 0 & t.lag2 == 0 & t.lag == 0 & t.current == 1 & pitt.df$SURGE_IND2[i+1] == 2){
      pitt.df$t.lag[i] <- t.lag
      pitt.df$t.current[i] <- t.current
      pitt.df$t.lead[i] <- t.lead
      #}else if(pitt.df$BUS_COUNT[i] > 0 & t.lag == 1 & t.current == 0 & t.lead == 0 & t.lag2 == 1){
      #pitt.df$t.lag[i] <- t.lag
      #pitt.df$t.current[i] <- t.current
      #pitt.df$t.lead[i] <- t.lead
      #}else if(pitt.df$BUS_COUNT[i] > 0 & t.lag == 1 & t.current == 1 & t.lead == 1){
      #pitt.df$t.lag[i] <- t.lag
      #pitt.df$t.current[i] <- t.current
      #pitt.df$t.lead[i] <- t.lead
      #}else if(pitt.df$BUS_COUNT[i] > 0 & t.lag == 1 & t.current == 1 & t.lead == 0 & t.lead2 == 0){
      #pitt.df$t.lag[i] <- t.lag
      #pitt.df$t.current[i] <- t.current
      #pitt.df$t.lead[i] <- t.lead
    }else if(pitt.df$BUS_COUNT[i] > 0 & t.lag == 0 & t.current == 0 & t.lead == 1 & pitt.df$SURGE_IND2[i+2] == 2){
      pitt.df$t.lag[i] <- t.lag
      pitt.df$t.current[i] <- t.current
      pitt.df$t.lead[i] <- t.lead
    }else{ 
      pitt.df$t.current[i] <- 99
    }
  }
  if(j == 1){
    full.df <- pitt.df
  }else{
    full.df <- rbind(full.df, pitt.df)
  }
  
}
}

pitt.filter <- full.df %>%
  filter(t.current != 99)

#write.csv(pitt.filter, file = "Transit_df_1_6_1500_LAG.csv")
