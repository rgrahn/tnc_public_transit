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
  library(lmtest)
  library(sandwich)
  library(stargazer)
  library(corrplot)}

setwd("C:/Users/rickr/Desktop/Research/Uber_Surge/Data_Code")
#==============================================================
#  Load data - Data has already been filtered to include three conditions mentioned:
#              i) All periods immediately before a surge event
#             ii) All periods during the initialization of a surge multiplier
#            iii) All perios where no surge multiplier is observed
#
#  Surge events are indicated by a 1

# --------------- Load data -------------------
{surge_12 <- read.csv("Transit_df_1_2_1500.csv")
surge_14 <- read.csv("Transit_df_1_4_1500.csv")
surge_16 <- read.csv("Transit_df_1_6_1500.csv")}
#---------- Filter data ----------------------

all <- surge_16

{#check1 <- filter(all, WEEKEND_IND == 0 & day_of_week != "Friday") %>% # -------> MON-THURS
check1 <- filter(all, day_of_week == "Saturday" | day_of_week == "Sunday") %>% #---------> WEEKENDS
mutate(current = ifelse(t.lag == 0 & t.current == 1, 1, 0),
         temperature = ifelse(TEMP <= 45, 0, 1), ave_stops = total.stops/BUS_COUNT)
#filter out xmas week
y <- with(check1, check1[(date(DATETIME) < as.Date('2016-12-21') | date(DATETIME) > as.Date('2017-01-02')), ])
#filter out Thanksgiving week
y <- with(y, y[(date(DATETIME) < as.Date('2016-11-24') | date(DATETIME) >= as.Date('2016-11-27')), ])
#filter out MLK day
check <- with(y, y[(date(DATETIME) != as.Date('2017-01-16')), ])
#create a column for month
check$month <- format(as.Date(check$DATE), "%m")}


# ================ Select times of analysis =================================
#Morning Peak (7-10am)
#Evening Peak (4-7pm)
#Late night (7pm-12am)
#Weekend (5pm-9pm)

time.df <- with(check, check[hour(DATETIME) >= 17 & hour(DATETIME) < 22, ])
#-----------------------------------------------------------------------------
# Filter based on the surge threshold. Only want to compare values above the 
# threshold with periods of no surge. ONLY SELECT ONE!
# ----------------------------------------------------------------------------
#filter for when surge threshold == 1.2
time.df <- filter(time.df, surge_multiplier >= 1.2 | surge_multiplier < 1.05)
#filter for when surge threshold == 1.4
time.df <- filter(time.df, surge_multiplier >= 1.4 | surge_multiplier < 1.05)
#filter for when surge threshold == 1.6
time.df <- filter(time.df, surge_multiplier >= 1.6 | surge_multiplier < 1.05)
#-------------------- Chose the location of interest -------------------------
poi <- c("Benedum Center", "CONSOL Energy Center", "Pitt", "CMU", "Wilkinsburg",
         "South Side","North Side","Strip District", "Shadyside","East Liberty")

#change the index of poi (from 1,2,....,10)
{current_poi <- poi[5]
print(current_poi)
test <- filter(time.df, place_label == current_poi)}

#-------------------------------------------------------------------------------
# Need to use different models for the different locations because local traffic conditions
# Are different
if(current_poi %in% c("Wilkinsburg","East Liberty","Shadyside")){
  model <- lm(RIDERSHIP ~  current + BUS_COUNT + ave_stops + temperature + RAIN + SNOW +
                IN_north_376 + OUT_south_376 +
                IN_south_279 + OUT_north_279 +
                IN_west_376 + OUT_east_376 +
                #local traffic controls
                ES_Eastbound + ES_Westbound +
                factor(day_of_week) + factor(month) + factor(TIME), test)
}else if(current_poi %in% c("CMU","Pitt")){
  # xmas break
  y <- with(test, test[(date(DATETIME) < as.Date('2016-12-17') | date(DATETIME) > as.Date('2017-01-16')), ])
  # thanksgiving break
  y1 <- with(y, y[(date(DATETIME) < as.Date('2016-11-21') | date(DATETIME) >= as.Date('2016-11-27')), ])
  # spring break
  y2 <- with(y1, y1[(date(DATETIME) < as.Date('2017-03-10') | date(DATETIME) >= as.Date('2017-03-17')), ])
  test1 <- y2
  model <- lm(RIDERSHIP ~  current + BUS_COUNT + ave_stops + temperature + RAIN + SNOW +
                IN_north_376 + OUT_south_376 +
                IN_south_279 + OUT_north_279 +
                IN_west_376 + OUT_east_376 +
                factor(day_of_week) + factor(month) + 
                factor(TIME), test1) 
}else if(current_poi %in% c("Benedum Center","CONSOL Energy Center")){
  model <- lm(RIDERSHIP ~  current + BUS_COUNT + ave_stops + temperature + RAIN + SNOW +
                IN_north_376 + OUT_south_376 +
                IN_south_279 + OUT_north_279 +
                IN_west_376 + OUT_east_376 +
                #local traffic controls
                DT_Forbes_East + DT_Liberty_East + DT_PENN_East + DT_5th_West + DT_Liberty_West +
                #EVENT +
                factor(day_of_week) + factor(month) + factor(TIME), test) 
}else if(current_poi %in% c("Strip District")){
  model <- lm(RIDERSHIP ~  current + BUS_COUNT + ave_stops + temperature + RAIN + SNOW +
                IN_north_376 + OUT_south_376 +
                IN_south_279 + OUT_north_279 +
                IN_west_376 + OUT_east_376 +
                #local traffic controls
                DT_Liberty_East + DT_PENN_East + DT_Liberty_West +
                factor(day_of_week) + factor(month) + factor(TIME), test) 
}else{model <- lm(RIDERSHIP ~  current + BUS_COUNT + ave_stops + temperature + RAIN + SNOW + #North Side & South Side
                IN_north_376 + OUT_south_376 +
                IN_south_279 + OUT_north_279 +
                IN_west_376 + OUT_east_376 +
                factor(day_of_week) + factor(month) + factor(TIME), test) 
}

# assign models for all levels of surge to compile into a latex table
model1 <- model
model2 <- model
model3 <- model

#---------------------------------------------------------------------------------------
#                         Plot the jackknife residuals
#---------------------------------------------------------------------------------------
#png(filename = paste0(current_poi"_resid.png", width = 3000, height = 2500, res = 300))
{plot(fitted(model), rstudent(model),
      xlab = "Fitted Values",
      ylab = "Jackknife Residuals",
      cex.axis=1.5,
      cex.main=2,
      cex.lab=1.5,
      main = paste0(current_poi,": Weekdays (4pm-7pm)"))
  abline(h=0)}
#dev.off()

# --------------------------------------------------------------------------------------
#              Check Cook's Distance and Variance inflation factors
# --------------------------------------------------------------------------------------
influenceIndexPlot(model, vars = c("Cook", "Studentized","hat"))
#calculate variance inflation factors
vif(model)

#---------------------- Plot results in table for the report ------------------------------------
#          The labels must be altered based on the point of interest 
#------------------------------------------------------------------------------------------------
labels <- c("Intercept","Surge Indicator","Bus Count","Ave. Stop Count","Temperature",
              "Rain","Snow","North I-376 (Inbound)","South I-376 (Outbound)","South I-279 (Inbound)",
              "North I-279 (Outbound)","West I-376 (Inbound)","East I-376 (Outbound)"
              #----------------------for Benedum and CONSOL--------------------------------------
              #,"DT_Forbes_East","DT_Liberty_East","DT_PENN_East","DT_5th_West","DT_Liberty_West"
              #----------------------for Strip District------------------------------------------
              #,"DT_Liberty_East","DT_PENN_East","DT_Liberty_West"
              #-------------------Wilkinsburg, East Liberty, Shadyside---------------------------
              ,"ES_Eastbound","ES_Westbound"
              # ------------------------- For Weekdays ------------------------------------------
              #,"Thursday","Tuesday","Wednesday"
              # ------------------------- For Weekends -------------------------------------------
              ,"Sunday"
               # ------------------------- For Weekdays ------------------------------------------
              #,"February","March","September","October","November","December"
              # ------------------------- For Weekends -------------------------------------------
              ,"February","March","October","November","December"
               )

# creating talbes in Latex 
table <- stargazer(model1,model2,model3, type = "latex", omit = "TIME", single.row = TRUE,
                     intercept.bottom = FALSE, omit.stat = c("ser", "f"), dep.var.caption = c(paste0(current_poi," (7am-10am)")),
                     covariate.labels = labels, dep.var.labels   = "Bus Ridership Counts",
                     column.labels = c("Surge = 1.2","Surge = 1.4", "Surge = 1.6"))
 
  
