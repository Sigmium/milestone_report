# Load required libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
library(data.table) 
library(vtreat) 

# Comments listed as "TODO:" indicate work required for the subsiquent code

setwd("c:/myFiles/")

#### DATA IMPORT, WRANGLING & LOOKUP (TODO=1) ####

# Import Apple Device lookup data from URL
AppleDevices <- read_csv("https://raw.githubusercontent.com/Sigmium/capstone/master/original_AppleDevices.csv")
# AppleDevices <- read_csv("original_AppleDevices.csv")
  
# Wrangle and structure lookup data for later lookups and possible merging into larger device lookup table 
names(AppleDevices)[names(AppleDevices)=="DEVICE TYPE"] <- "DEVICE_MODEL"
names(AppleDevices)[names(AppleDevices)=="PRODUCT NAME"] <- "PRODUCT_NAME"
AppleDevices <- AppleDevices %>% 
  separate_('PRODUCT_NAME', into = c('FRIENDLY_PRODUCT_NAME','PRODUCT_MODEL_INFO'), sep = '\\(')
AppleDevices$PRODUCT_MODEL_INFO <- gsub('\\)','', AppleDevices$PRODUCT_MODEL_INFO)
AppleDevices$RETAIL_BRANDING <- c("Apple")
AppleDevices$DEVICE_MODEL_2 <- c("N/A")

# Save clean file for use in other scripts
# write_csv(AppleDevices, "clean_AppleDevices.csv")

# Import merged login data file 
Login <- read.csv("original_MobileLogin.csv")

# Conduct preliminary data filtering, wrangling and cleanup
# TODO: Resolve Login$Hour <- error (output is 00:00 for all)
Login <- filter(Login, DEVICE_TYPE != "Android") %>%
  filter(CHANNEL__TYPE == "MOBILE") %>%
  filter(DEVICE_MODEL!="x86_64") %>%
  filter(DEVICE_MODEL!="^iPad7.*")
names(Login)[names(Login)=="X_time"] <- "Timestamp"
names(Login)[names(Login)=="count"] <- "Volume"
names(Login)[names(Login)=="DEVICE_TYPE"] <- "APP_TYPE"
Login$RESULT_DISPOSITION <- as.character(Login$RESULT_DISPOSITION)
Login <- filter(RESULT_DISPOSITION!="UNKNOWN")
Login$Timestamp <- format(Login$Timestamp, tz="America/New_York",usetz=TRUE)
Login$Date <- as.Date(Login$Timestamp,  tz="America/New_York")
# Login$Hour <- strftime(Login$Timestamp, format = "%R")
Login$Weekday <- weekdays(as.Date(Login$Timestamp))
Login$Weekday <- format(Login$Weekday, tz="America/New_York",usetz=FALSE)
Login$WeekNumber <- strftime(Login$Timestamp, format = "%U")
Login$WeekNumber <- format(Login$WeekNumber, tz="America/New_York",usetz=FALSE)

# Lookup and merge device info then append device super group names
Login <- full_join(Login, AppleDevices, by = "DEVICE_MODEL")
Login$DEVICE_SUPERGROUP_NAME <- Login$FRIENDLY_PRODUCT_NAME
Login <- Login %>% mutate(DEVICE_SUPERGROUP_NAME = sub('^iPhone.*', 'iPhone', DEVICE_SUPERGROUP_NAME)) %>%
  mutate(DEVICE_SUPERGROUP_NAME = sub('^iPod.*', 'iPod Touch', DEVICE_SUPERGROUP_NAME)) %>%
  mutate(DEVICE_SUPERGROUP_NAME = sub('^iPad.*', 'iPad', DEVICE_SUPERGROUP_NAME)) %>%
  filter(!is.na(DEVICE_SUPERGROUP_NAME)) %>%
  filter(!is.na(Timestamp))

# Save/Load clean login data file from APPLE devices
# write_csv(Login, "clean_APPLE_MobileLogin.csv")
# Login <- read_csv("clean_APPLE_MobileLogin.csv")
                    
#### DATA QUALITY ANALYSIS & MEASUREMENT (TODO=1)####

# QC - Lookup functionality
lookup_quality <- Login %>%
  group_by(DEVICE_SUPERGROUP_NAME, FRIENDLY_PRODUCT_NAME, DEVICE_MODEL, APP_TYPE) %>%
  summarise(Total = sum(Volume))

# QC - Expected/unexpected values, min, max, mean, null
# TODO

# QC - Summary of Data Quality Observations (to be excluded from analysis)
dq_observations <- filter(Login, DEVICE_MODEL=="x86_64" | RESULT_DISPOSITION=="UNKNOWN" | is.na(FRIENDLY_PRODUCT_NAME))

#### TOP LEVEL LOGIN VOLUME, POLICY/FAILURE RATE & DISTRIBUTION (TODO=6)  ####

# Totals by Device (Super group)
total_VOLUME_by_device_supgroup <- Login %>% 
  filter(!is.na(RESULT_DISPOSITION)) %>%
  group_by(DEVICE_SUPERGROUP_NAME, RESULT_DISPOSITION) %>% 
  summarise(Total = sum(Volume)) %>% 
  spread(RESULT_DISPOSITION, Total)
total_VOLUME_by_super_device$SUCCESS [is.na(total_VOLUME_by_super_device$SUCCESS)] <- 0
total_VOLUME_by_super_device$POLICY [is.na(total_VOLUME_by_super_device$POLICY)] <- 0
total_VOLUME_by_super_device$DEFECT [is.na(total_VOLUME_by_super_device$DEFECT)] <- 0
total_VOLUME_by_super_device <- mutate(total_VOLUME_by_super_device, TOTAL = (SUCCESS + POLICY + DEFECT)) 
# TODO stack bar with totals by super group - ggplot(totals, aes(x = DEVICE_SUPERGROUP_NAME, y = TOTAL))+
# geom_line()

  
# Totals by Device (Sub group)
total_VOLUME_by_device_subgroup <- Login %>% 
  group_by(FRIENDLY_PRODUCT_NAME, RESULT_DISPOSITION) %>% 
  summarise(Total = sum(Volume)) %>% 
  spread(RESULT_DISPOSITION, Total)
total_VOLUME_by_device_subgroup$SUCCESS [is.na(total_VOLUME_by_device_subgroup$SUCCESS)] <- 0
total_VOLUME_by_device_subgroup$POLICY [is.na(total_VOLUME_by_device_subgroup$POLICY)] <- 0
total_VOLUME_by_device_subgroup$DEFECT [is.na(total_VOLUME_by_device_subgroup$DEFECT)] <- 0
total_VOLUME_by_device_subgroup <- mutate(total_VOLUME_by_device_subgroup, TOTAL = (SUCCESS + POLICY + DEFECT)) 
total_VOLUME_by_device_subgroup <- filter(total_VOLUME_by_device_subgroup, TOTAL > 0)
# TODO stack bar with totals by sub group - ggplot(totals, aes(x = DEVICE_SUPERGROUP_NAME, y = TOTAL))+
# geom_line()

# Hourly Totals by Device (Sub group)
total_hourly_VOLUME_by_sub_device <- Login %>% 
  group_by(Timestamp, FRIENDLY_PRODUCT_NAME, RESULT_DISPOSITION) %>% 
  summarise(Total = sum(Volume)) %>% 
  spread(RESULT_DISPOSITION, Total)
total_hourly_VOLUME_by_sub_device$SUCCESS [is.na(total_hourly_VOLUME_by_sub_device$SUCCESS)] <- 0
total_hourly_VOLUME_by_sub_device$POLICY [is.na(total_hourly_VOLUME_by_sub_device$POLICY)] <- 0
total_hourly_VOLUME_by_sub_device$DEFECT [is.na(totals$DEFECT)] <- 0
total_hourly_VOLUME_by_sub_device <- mutate(total_hourly_VOLUME_by_sub_device, TOTAL = (SUCCESS + POLICY + DEFECT)) 
total_hourly_VOLUME_by_sub_device <- filter(total_hourly_VOLUME_by_sub_device, TOTAL > 0)
total_hourly_VOLUME_by_sub_device$'<NA>' <- total_hourly_VOLUME_by_sub_device$'<NA>' <- NULL

# Hourly Totals by Device (Sub group) line chart
ggplot(total_hourly_VOLUME_by_sub_device, aes(x = Timestamp, y = TOTAL))+
  geom_line()

# Totals by Device (Sub group) plot
ggplot(Login, aes(x = Volume, y = FRIENDLY_PRODUCT_NAME, size = Volume, color = RESULT_DISPOSITION))+
  geom_point()
# TODO need to validate, clean up - Goal is to show high usage of 5c device

# Hourly Policy/Fail rate by device super group, chart over time and box plot
# TODO: Add group by day to chart daily failure rates

rate_hourly_RESULT_supergroup_device <- Login %>% 
  group_by(Timestamp, DEVICE_SUPERGROUP_NAME, RESULT_DISPOSITION) %>%
  summarise(Total = sum(Volume)) %>% 
  spread(RESULT_DISPOSITION, Total) 
rate_hourly_RESULT_supergroup_device$SUCCESS [is.na(rate_hourly_RESULT_supergroup_device$SUCCESS)] <- 0
rate_hourly_RESULT_supergroup_device$POLICY [is.na(rate_hourly_RESULT_supergroup_device$POLICY)] <- 0
rate_hourly_RESULT_supergroup_device$DEFECT [is.na(rate_hourly_RESULT_supergroup_device$DEFECT)] <- 0
rate_hourly_RESULT_supergroup_device <- rate_hourly_RESULT_supergroup_device %>%
  mutate(TOTAL = (SUCCESS + POLICY + DEFECT)) %>%
  mutate(POLICY_RATE = POLICY / (SUCCESS + POLICY + DEFECT)) %>%
  mutate(FAIL_RATE = DEFECT / (SUCCESS + POLICY + DEFECT)) %>%
  filter(!is.na(TOTAL)) %>%
  filter(TOTAL != 0) %>%
  filter(!is.na(DEVICE_SUPERGROUP_NAME))
rate_hourly_RESULT_supergroup_device$'<NA>' <- rate_hourly_RESULT_supergroup_device$'<NA>' <- NULL

rate_hourly_RESULT_supergroup_deviceiPhone <- rate_hourly_RESULT_supergroup_device %>% filter(DEVICE_SUPERGROUP_NAME == "iPhone")
rate_hourly_RESULT_supergroup_deviceiPad <- rate_hourly_RESULT_supergroup_device %>% filter(DEVICE_SUPERGROUP_NAME == "iPad")
rate_hourly_RESULT_supergroup_deviceiPod <- rate_hourly_RESULT_supergroup_device %>% filter(DEVICE_SUPERGROUP_NAME == "iPod Touch")

ggplot(rate_hourly_RESULT_supergroup_deviceiPhone, aes(x = Timestamp, y = POLICY_RATE))+
  geom_line(aes(col = DEVICE_SUPERGROUP_NAME))+
  geom_line(data = rate_hourly_RESULT_supergroup_deviceiPad, aes(color = DEVICE_SUPERGROUP_NAME))+
  geom_line(data = rate_hourly_RESULT_supergroup_deviceiPod, aes(color = DEVICE_SUPERGROUP_NAME))
# TBD Find out why is this now erroring out. Err: "Fo you need to adjust group asthetuc?"

ggplot(rate_hourly_RESULT_supergroup_deviceiPhone, aes(x = DEVICE_SUPERGROUP_NAME, y = POLICY_RATE))+
  geom_boxplot(aes())+
  geom_boxplot(data = rate_hourly_RESULT_supergroup_deviceiPad, aes()) +
  geom_boxplot(data = rate_hourly_RESULT_supergroup_deviceiPod, aes())

ggplot(rate_hourly_RESULT_supergroup_deviceiPhone, aes(x = Timestamp, y = FAIL_RATE))+
  geom_line(aes(color = DEVICE_SUPERGROUP_NAME))+
  geom_line(data = rate_hourly_RESULT_supergroup_deviceiPad, aes(color = DEVICE_SUPERGROUP_NAME))+
  geom_line(data = rate_hourly_RESULT_supergroup_deviceiPod, aes(color = DEVICE_SUPERGROUP_NAME))
# TBD Find out why is this now erroring out. Err: "Fo you need to adjust group asthetuc?"

ggplot(rate_hourly_RESULT_supergroup_deviceiPhone, aes(x = DEVICE_SUPERGROUP_NAME, y = FAIL_RATE))+
  geom_boxplot(aes())+
  geom_boxplot(data = rate_hourly_RESULT_supergroup_deviceiPad, aes()) +
  geom_boxplot(data = rate_hourly_RESULT_supergroup_deviceiPod, aes())




#### TOP LEVEL INVESTIGATION OF OBSERVATIONS (TODO=3) ####

# Create Policy/Fail rate dataframe by device super and sub group
rate_hourly_RESULT_subgroup_device <- Login %>% 
  group_by(Timestamp, DEVICE_SUPERGROUP_NAME,FRIENDLY_PRODUCT_NAME, RESULT_DISPOSITION) %>%
  summarise(Total = sum(Volume)) %>% 
  spread(RESULT_DISPOSITION, Total) 
rate_hourly_RESULT_subgroup_device$SUCCESS [is.na(rate_hourly_RESULT_subgroup_device$SUCCESS)] <- 0
rate_hourly_RESULT_subgroup_device$POLICY [is.na(rate_hourly_RESULT_subgroup_device$POLICY)] <- 0
rate_hourly_RESULT_subgroup_device$DEFECT [is.na(rate_hourly_RESULT_subgroup_device$DEFECT)] <- 0
rate_hourly_RESULT_subgroup_device <- rate_hourly_RESULT_subgroup_device %>%
  mutate(TOTAL = (SUCCESS + POLICY + DEFECT)) %>%
  mutate(POLICY_RATE = POLICY / (SUCCESS + POLICY + DEFECT)) %>%
  mutate(FAIL_RATE = DEFECT / (SUCCESS + POLICY + DEFECT)) %>%
  filter(!is.na(TOTAL)) %>%
  filter(TOTAL != 0) %>%
  filter(!is.na(DEVICE_SUPERGROUP_NAME))

# Filter and split into dataframes for each super group
rate_hourly_RESULT_subgroup_deviceiPhone <- rate_hourly_RESULT_subgroup_device %>% filter(DEVICE_SUPERGROUP_NAME == "iPhone")
rate_hourly_RESULT_subgroup_deviceiPad <- rate_hourly_RESULT_subgroup_device %>% filter(DEVICE_SUPERGROUP_NAME == "iPad")
rate_hourly_RESULT_subgroup_deviceiPod <- rate_hourly_RESULT_subgroup_device %>% filter(DEVICE_SUPERGROUP_NAME == "iPod Touch")

# POLICY RATE - Plot policy rate over time for each super group, stacked by subgroup
ggplot(rate_hourly_RESULT_subgroup_deviceiPhone, aes(x = Timestamp, y = POLICY_RATE))+
  geom_line(aes(col = FRIENDLY_PRODUCT_NAME))
# TODO correct plot to chart iPhone policy rate over time by sub group and create the same for iPad and iPod

# POLICY RATE - Box plot policy rate for each super group, split by subgroup
ggplot(rate_hourly_RESULT_subgroup_deviceiPhone, aes(x = FRIENDLY_PRODUCT_NAME, y = POLICY_RATE))+
  geom_boxplot(aes())

ggplot(rate_hourly_RESULT_subgroup_deviceiPad, aes(x = FRIENDLY_PRODUCT_NAME, y = POLICY_RATE))+
  geom_boxplot(aes())

ggplot(rate_hourly_RESULT_subgroup_deviceiPod, aes(x = FRIENDLY_PRODUCT_NAME, y = POLICY_RATE))+
  geom_boxplot(aes())

# FAIL RATE - Plot fail rate over time for each super group, stacked by subgroup

# TODO Plot failure rate over time for each super group, stacked by subgroup
# TODO create the same, for all three, for failure rate

# FAIL RATE - Box plot fail rate over time for each super group, stacked by subgroup
ggplot(rate_hourly_RESULT_subgroup_deviceiPhone, aes(x = FRIENDLY_PRODUCT_NAME, y = FAIL_RATE))+
  geom_boxplot(aes())

ggplot(rate_hourly_RESULT_subgroup_deviceiPad, aes(x = FRIENDLY_PRODUCT_NAME, y = FAIL_RATE))+
  geom_boxplot(aes())

ggplot(rate_hourly_RESULT_subgroup_deviceiPod, aes(x = FRIENDLY_PRODUCT_NAME, y = FAIL_RATE))+
  geom_boxplot(aes())

#### SECOND LEVEL INVESTIGATION OF OSERVATIONS (TODO=4+) ####

# Investigate observation - Elevated iPad Fail Rates

#iPad RESULT RATE by APP_TYPE
x <- Login %>% 
  filter(DEVICE_SUPERGROUP_NAME =="iPad") %>%
  group_by(Timestamp, APP_TYPE, RESULT_DISPOSITION) %>%
  summarise(Total = sum(Volume)) %>% 
  spread(RESULT_DISPOSITION, Total) 
x$SUCCESS [is.na(x$SUCCESS)] <- 0
x$POLICY [is.na(x$POLICY)] <- 0
x$DEFECT [is.na(x$DEFECT)] <- 0
x <- x %>%
  mutate(TOTAL = (SUCCESS + POLICY + DEFECT)) %>%
  mutate(POLICY_RATE = POLICY / (SUCCESS + POLICY + DEFECT)) %>%
  mutate(FAIL_RATE = DEFECT / (SUCCESS + POLICY + DEFECT)) %>%
  filter(!is.na(TOTAL)) %>%
  filter(TOTAL != 0)

# iPad Fail Rate Box Plot by APP_TYPE
ggplot(x, aes(x = APP_TYPE, y = FAIL_RATE))+
  geom_boxplot(aes())

#iPad (iPad App) RESULT RATE by AuthMethod
x <- Login %>% 
  filter(DEVICE_SUPERGROUP_NAME=="iPad") %>%
  filter(APP_TYPE=="iPad") %>%
  group_by(Timestamp, AUTH_METHOD, RESULT_DISPOSITION) %>%
  summarise(Total = sum(Volume)) %>% 
  spread(RESULT_DISPOSITION, Total) 
x$SUCCESS [is.na(x$SUCCESS)] <- 0
x$POLICY [is.na(x$POLICY)] <- 0
x$DEFECT [is.na(x$DEFECT)] <- 0
x <- x %>%
  mutate(TOTAL = (SUCCESS + POLICY + DEFECT)) %>%
  mutate(POLICY_RATE = POLICY / (SUCCESS + POLICY + DEFECT)) %>%
  mutate(FAIL_RATE = DEFECT / (SUCCESS + POLICY + DEFECT)) %>%
  filter(!is.na(TOTAL)) %>%
  filter(TOTAL != 0)

# iPad (iPad App) Fail RATE Box Plot by AUTH_METHOD
ggplot(x, aes(x = AUTH_METHOD, y = FAIL_RATE))+
  geom_boxplot(aes())

# iPad (iPad App) Fail RATE Box Plot by AUTH_METHOD for each sub group
x <- Login %>%
  filter(DEVICE_SUPERGROUP_NAME=="iPad") %>%
  filter(APP_TYPE=="iPad") %>%
  group_by(Timestamp, AUTH_METHOD, FRIENDLY_PRODUCT_NAME, RESULT_DISPOSITION) %>%
  summarise(Total = sum(Volume)) %>% 
  spread(RESULT_DISPOSITION, Total) 
x$SUCCESS [is.na(x$SUCCESS)] <- 0
x$POLICY [is.na(x$POLICY)] <- 0
x$DEFECT [is.na(x$DEFECT)] <- 0
x <- x %>%
  mutate(TOTAL = (SUCCESS + POLICY + DEFECT)) %>%
  mutate(POLICY_RATE = POLICY / (SUCCESS + POLICY + DEFECT)) %>%
  mutate(FAIL_RATE = DEFECT / (SUCCESS + POLICY + DEFECT)) %>%
  filter(!is.na(TOTAL)) %>%
  filter(TOTAL != 0)
x_pwd <- x %>% filter(AUTH_METHOD=="Password")
x_pat <- x %>% filter(AUTH_METHOD=="Pattern")
x_fin <- x %>% filter(AUTH_METHOD=="Finger Print")
  
ggplot(x_pwd, aes(x = FRIENDLY_PRODUCT_NAME, y = FAIL_RATE))+
  geom_boxplot(aes())
ggplot(x_pat, aes(x = FRIENDLY_PRODUCT_NAME, y = FAIL_RATE))+
  geom_boxplot(aes())
ggplot(x_fin, aes(x = FRIENDLY_PRODUCT_NAME, y = FAIL_RATE))+
  geom_boxplot(aes())
# TODO better way to vizualize? Show with the same x axis. Loosing something due to diff charts and scales? 

# TODO Investigate iPad Fail Rate - Correlation to iOS ver, App Version 


# Investigate observation - Elevated Policy Rates
# TODO Investigate Policy Rate - Split for each target sub group (iPhone 4s thru iPhone 5s) by Auth Method for high policy rate, then by App Type, iOS ver, App Version as needed

# Investigate observations - iPhone 5c
# TODO Improved view of 5c usage, when will App version catch up to this population? What iOS version are they stuck on and what is the impact? 


#### INVESTIGATION, REGRESSION, PROJECTION, ETC (TODO=1+) ####
# Statistics, regression, ? 

Login$RESULT_DISPOSITION2 <- Login$RESULT_DISPOSITION

Login <- Login %>% 
  mutate(RESULT_DISPOSITION2 = sub('SUCCESS', 'FALSE', RESULT_DISPOSITION2)) %>%
  mutate(RESULT_DISPOSITION2 = sub('POLICY', 'FALSE', RESULT_DISPOSITION2)) %>%
  mutate(RESULT_DISPOSITION2 = sub('DEFECT', 'TRUE', RESULT_DISPOSITION2)) 

dTrainC <- data.frame(x = c(Login$DEVICE_SUPERGROUP_NAME), z = c(Login$USER_STATUS_CODE), y = c(Login$RESULT_DISPOSITION2))
treatmentsC <- designTreatmentsC(dTrainC, colnames(dTrainC), 'y', TRUE)


#### MISC CODE - SAVE FOR LATER ####

# iOS Failure/Success/Policy volume by Device and Authentication Method

x <- filter(Login, RESULT_DISPOSITION=="SUCCESS" & DEVICE_TYPE!="Android" )

ggplot(data = x, aes(x = Volume, y = FRIENDLY_PRODUCT_NAME, size = Volume, color = AUTH_METHOD)) + 
  geom_jitter()
  geom_point()

# Success volume over time by Authentication Method

x <- filter(Login, RESULT_DISPOSITION=="SUCCESS") %>%
  group_by(Date, AUTH_METHOD) %>%
  summarise(Total = sum(Volume)) 

ggplot(data = x, aes(x = Date, y = Total, fill = AUTH_METHOD)) + 
    geom_area()


# totals by Device, Result & Failure Rate

total_by_DEVICE_RESULT_FAIL_RATE <- Login %>% 
  group_by(DEVICE_SUPERGROUP_NAME, APP_TYPE, RESULT_DISPOSITION) %>%
  summarise(Total = sum(Volume)) %>% 
  spread(RESULT_DISPOSITION, Total)
total_by_DEVICE_RESULT_FAIL_RATE$UNKNOWN <- total_by_DEVICE_RESULT_FAIL_RATE$UNKNOWN
total_by_DEVICE_RESULT_FAIL_RATE$SUCCESS [is.na(total_by_DEVICE_RESULT_FAIL_RATE$SUCCESS)] <- 0
total_by_DEVICE_RESULT_FAIL_RATE$POLICY [is.na(total_by_DEVICE_RESULT_FAIL_RATE$POLICY)] <- 0
total_by_DEVICE_RESULT_FAIL_RATE$DEFECT [is.na(total_by_DEVICE_RESULT_FAIL_RATE$DEFECT)] <- 0
total_by_DEVICE_RESULT_FAIL_RATE$UNKNOWN [is.na(total_by_DEVICE_RESULT_FAIL_RATE$UNKNOWN)] <- 0
total_by_DEVICE_RESULT_FAIL_RATE <- total_by_DEVICE_RESULT_FAIL_RATE %>%
  mutate(TOTAL = (SUCCESS + POLICY + DEFECT)) %>%
  mutate(FAIL_RATE = DEFECT / (SUCCESS + POLICY + DEFECT + UNKNOWN)) %>%
  filter(!is.na(TOTAL)) %>%
  filter(TOTAL != 0) %>%
  filter(!is.na(DEVICE_SUPERGROUP_NAME))
total_by_DEVICE_RESULT_FAIL_RATE$'<NA>' <- total_by_DEVICE_RESULT_FAIL_RATE$'<NA>' <- NULL


# totals by Device Type, Result & Failure Rate

total_by_DEVICE_TYPE_RESULT_FAIL_RATE <- Login %>% 
  group_by(FRIENDLY_PRODUCT_NAME, RESULT_DISPOSITION) %>%
  summarise(Total = sum(Volume)) %>% 
  spread(RESULT_DISPOSITION, Total)
total_by_DEVICE_TYPE_RESULT_FAIL_RATE$UNKNOWN <- total_by_DEVICE_TYPE_RESULT_FAIL_RATE$UNKNOWN
total_by_DEVICE_TYPE_RESULT_FAIL_RATE$SUCCESS [is.na(total_by_DEVICE_TYPE_RESULT_FAIL_RATE$SUCCESS)] <- 0
total_by_DEVICE_TYPE_RESULT_FAIL_RATE$POLICY [is.na(total_by_DEVICE_TYPE_RESULT_FAIL_RATE$POLICY)] <- 0
total_by_DEVICE_TYPE_RESULT_FAIL_RATE$DEFECT [is.na(total_by_DEVICE_TYPE_RESULT_FAIL_RATE$DEFECT)] <- 0
total_by_DEVICE_TYPE_RESULT_FAIL_RATE$UNKNOWN [is.na(total_by_DEVICE_TYPE_RESULT_FAIL_RATE$UNKNOWN)] <- 0
total_by_DEVICE_TYPE_RESULT_FAIL_RATE <- total_by_DEVICE_TYPE_RESULT_FAIL_RATE %>%
  mutate(TOTAL = (SUCCESS + POLICY + DEFECT)) %>%
  mutate(FAIL_RATE = DEFECT / (SUCCESS + POLICY + DEFECT + UNKNOWN)) %>%
  filter(!is.na(TOTAL)) %>%
  filter(TOTAL != 0) %>%
  filter(!is.na(FRIENDLY_PRODUCT_NAME))
total_by_DEVICE_TYPE_RESULT_FAIL_RATE$'<NA>' <- total_by_DEVICE_TYPE_RESULT_FAIL_RATE$'<NA>' <- NULL


# iOS version distribution by device over time

totals <- Login %>%
  group_by(Timestamp, DEVICE_OPERATING_SYSTEM_VERSION, DEVICE_SUPERGROUP_NAME) %>%
  summarise(Total = sum(Volume)) 

totals_iPad <- totals %>% filter(DEVICE_SUPERGROUP_NAME == "iPad") 
ggplot(totals_iPad, aes(x = Timestamp, y = Total, fill = DEVICE_OPERATING_SYSTEM_VERSION))+
  geom_area(position = "fill") 

totals_iPhone <- totals %>% filter(DEVICE_SUPERGROUP_NAME == "iPhone") 
ggplot(totals_iPhone, aes(x = Timestamp, y = Total, fill = DEVICE_OPERATING_SYSTEM_VERSION))+
  geom_area(position = "fill") 

totals_iPod <- totals %>% filter(DEVICE_SUPERGROUP_NAME == "iPhone") 
ggplot(totals_iPod, aes(x = Timestamp, y = Total, fill = DEVICE_OPERATING_SYSTEM_VERSION))+
  geom_area(position = "fill") 

# App version distribution by device over time

totals <- Login %>%
  group_by(Timestamp, APP_VERSION, DEVICE_SUPERGROUP_NAME) %>%
  summarise(Total = sum(Volume)) 

totals_iPad <- totals %>% filter(DEVICE_SUPERGROUP_NAME == "iPad") 
ggplot(totals_iPad, aes(x = Timestamp, y = Total, fill = APP_VERSION))+
  geom_area(position = "fill") 

totals_iPhone <- totals %>% filter(DEVICE_SUPERGROUP_NAME == "iPhone") 
ggplot(totals_iPhone, aes(x = Timestamp, y = Total, fill = APP_VERSION))+
  geom_area(position = "fill") 

totals_iPod <- totals %>% filter(DEVICE_SUPERGROUP_NAME == "iPhone") 
ggplot(totals_iPod, aes(x = Timestamp, y = Total, fill = APP_VERSION))+
  geom_area(position = "fill") 


# Result distribution by device over time

totals <- Login %>%
  group_by(Timestamp, RESULT_DISPOSITION, DEVICE_SUPERGROUP_NAME) %>%
  summarise(Total = sum(Volume)) %>%
  filter(!is.na(DEVICE_SUPERGROUP_NAME))

totals_POLICY <- totals %>% filter(RESULT_DISPOSITION == "POLICY") 
ggplot(totals_POLICY, aes(x = Timestamp, y = Total, fill = DEVICE_SUPERGROUP_NAME))+
  geom_area(position = "fill") 

totals_DEFECT <- totals %>% filter(RESULT_DISPOSITION == "DEFECT") 
ggplot(totals_DEFECT, aes(x = Timestamp, y = Total, fill = DEVICE_SUPERGROUP_NAME))+
  geom_area(position = "fill") 

totals_SUCCESS <- totals %>% filter(RESULT_DISPOSITION == "SUCCESS") 
ggplot(totals_SUCCESS, aes(x = Timestamp, y = Total, fill = DEVICE_SUPERGROUP_NAME))+
  geom_area(position = "fill") 

# Misc vizuals

wk_by_auth <- summ %>%
  filter(Login, RESULT_DISPOSITION=="SUCCESS") %>% 
  group_by(WeekNumber, AUTH_METHOD) %>% 
  summarize(Total = sum(Volume))




#### OBSERVATIONS ####

# OBSERVATION: Failure Rates are Higher for both iPad and iPhone apps

FailRate <- Login %>% group_by(Timestamp, FRIENDLY_PRODUCT_NAME, RESULT_DISPOSITION, APP_TYPE) %>%
  summarise(Count = sum(Volume)) %>%
  spread(RESULT_DISPOSITION, Count) %>%
  mutate(FAIL_RATE = DEFECT / (SUCCESS + POLICY + DEFECT)) %>%
  #  Change filter to iPhone for iPhone app, iPad for iPad app
  filter(APP_TYPE == "iPhone") %>%
  filter(!is.na(FAIL_RATE)) %>%
  filter(!is.na(FRIENDLY_PRODUCT_NAME))

ggplot(data = FailRate, aes(x = FRIENDLY_PRODUCT_NAME, y = FAIL_RATE)) +
  geom_boxplot()

# OBSERVATION: iPhone 5c outlier populations #

sum_by_device <- Login %>% group_by(FRIENDLY_PRODUCT_NAME) %>% 
  summarise(Count = sum(Volume))
# Create bar chart or plot showing distribution of login by device
ggplot(data = sum_by_device, aes(x = FRIENDLY_PRODUCT_NAME, y = Volume)) +
  geom_()

#### WORKSPACE ####
x <- Login %>% 
  group_by(Timestamp, DEVICE_SUPERGROUP_NAME, FRIENDLY_PRODUCT_NAME, AUTH_METHOD, RESULT_DISPOSITION) %>%
  summarise(Total = sum(Volume)) %>% 
  spread(RESULT_DISPOSITION, Total) 
x$SUCCESS [is.na(x$SUCCESS)] <- 0
x$POLICY [is.na(x$POLICY)] <- 0
x$DEFECT [is.na(x$DEFECT)] <- 0
x <- x %>%
  mutate(TOTAL = (SUCCESS + POLICY + DEFECT)) %>%
  mutate(POLICY_RATE = POLICY / (SUCCESS + POLICY + DEFECT)) %>%
  mutate(FAIL_RATE = DEFECT / (SUCCESS + POLICY + DEFECT))
x$Weekday <- weekdays(as.Date(x$Timestamp))
x$WeekNumber <- strftime(x$Timestamp, format = "%W")

xiPhone <- x %>% filter(DEVICE_SUPERGROUP_NAME == "iPhone")
xiPad <- x %>% filter(DEVICE_SUPERGROUP_NAME == "iPad")
xiPod <- x %>% filter(DEVICE_SUPERGROUP_NAME == "iPod Touch")

> model1_total = lm(FAIL_RATE ~ APP_VERSION, data=x)
> summary(model1_total)