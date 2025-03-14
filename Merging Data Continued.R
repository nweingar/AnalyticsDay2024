# Editing the person FINAL data #

  # this accounts for unknown (998/999) age
  # changed the name to NEWVARS to maintain different versions of the data

person_NEWVARS <- person_COMPLETE %>%
  group_by(ST_CASE_NEW) %>%
  summarise(
    DRTOTAL = max(PER_NO),
    DRTOTAL_MALE = sum(SEXNAME == "Male" & PER_NO == 1),
    DRTOTAL_FEMALE = sum(SEXNAME == "Female" & PER_NO == 1),
    DRAVG_AGE = mean(AGE[PER_NO == 1 & !AGE %in% c(998, 999)], na.rm = TRUE)
  )
  
head(person_FINAL)

# Editing the vehicle FINAL data #

  # also changed this one's name bc im neurotic

vehicle_NEWVARS <- vehicle_COMPLETE  %>%
  group_by(ST_CASE_NEW) %>%
  summarise(
    VHAVG_AGE = mean(MOD_YEAR),
    TOT_SPEED = sum(SPEEDREL >= 1 & SPEEDREL <= 5),
    TOT_L_VIO = sum(L_STATUS >= 1 & L_STATUS <= 4)
  )

head(vehicle_FINAL)

## Merging person and vehicle new vars with originals ##

# person #

person_temp <- person_COMPLETE %>% distinct(ST_CASE_NEW, .keep_all = TRUE)

person_FINAL <- left_join(person_temp, person_NEWVARS, by = "ST_CASE_NEW") %>%
  select(ST_CASE_NEW, DRTOTAL, DRTOTAL_MALE, DRTOTAL_FEMALE, DRAVG_AGE)

  # person_FINAL is the person dataset we will use to merge with accidents

# vehicle #

vehicle_temp <-  vehicle_COMPLETE %>% distinct(ST_CASE_NEW, .keep_all = TRUE)

vehicle_FINAL <- left_join(vehicle_temp, vehicle_NEWVARS, by = "ST_CASE_NEW") %>%
  select(ST_CASE_NEW, VSPD_LIM, VSPD_LIMNAME, VPROFILE, VPROFILENAME, VALIGN, VALIGNNAME,
         VSURCOND, VSURCONDNAME, VHAVG_AGE, TOT_SPEED, TOT_L_VIO)

  # vehicle_FINAL is the vehicle dataset we will use to merge with accidents
  # CHECK CAR AVG AGE VARIABLE

## Removing unnecessary vars from the main accidents data ##

accidents_FINAL <- accidents_COMPLETE %>%
  select(STATE, STATENAME,
         ST_CASE_NEW,
         PERNOTMVIT,    #(anyone involved not in car; bike, ped, etc)
         VE_FORMS,      #(num of vehicles involved)
         PERSONS,       #num of forms submitted of ppl involved
         PERMVIT,       #num ppl inside vehicle
         COUNTYNAME,    
         CITYNAME,
         MONTHNAME,
         DAY, DAY_WEEKNAME,
         YEAR,
         HOUR, HOURNAME,
         MINUTE,
         ROUTENAME,     #type of road- county rd, interstate
         RUR_URBNAME,   #rural or urban
         FUNC_SYSNAME,  #functional system (type of road?)
         RD_OWNERNAME,  #who owns the road (maybe)
         SP_JURNAME,    #special jurisdiction or not (maybe)
         LATITUDE, LONGITUD,
         HARM_EVNAME,   #first event causing the crash
         MAN_COLLNAME,  #angle of crash
         RELJCT1NAME,   #interchange
         RELJCT2NAME,   #specific junction
         TYP_INTNAME,   #type of interchange
         REL_ROADNAME,  #relation to trafficway
         WRK_ZONENAME,  #work zone
         LGT_CONDNAME,  #light condition
         WEATHERNAME,   #weather
         NOT_HOUR, NOT_HOURNAME, NOT_MIN, NOT_MINNAME, #(notification of emergency service)
         ARR_HOUR, ARR_HOURNAME, ARR_MIN, ARR_MINNAME, #(arrival of emergency service)
         HOSP_HR, HOSP_HRNAME, HOSP_MN, HOSP_MNNAME, #(arrival at hospital)
         FATALS         #(number of fatalities))
  )

# writing the finalized data to csv just in case #

write.csv(accidents_FINAL, "stat7240_fall24_team1/Finalized Datasets/accidents_FINAL.csv", row.names = FALSE)
write.csv(drimpair_FINAL, "stat7240_fall24_team1/Finalized Datasets/drimpair_FINAL.csv", row.names = FALSE)
write.csv(person_FINAL, "stat7240_fall24_team1/Finalized Datasets/person_FINAL.csv", row.names = FALSE)
write.csv(vehicle_FINAL, "stat7240_fall24_team1/Finalized Datasets/vehicle_FINAL.csv", row.names = FALSE)

### ~~~~ Merging all sub-data into THE Dataset ~~~~ ###

  # total obs should be 182347
  # accidents: 45 vars
  # drimpair: 2 vars (+ID)
  # person: 4 vars (+ID)
  # vehicle: 11 vars (+ID)
  # therefore, total vars should be 62

library(purrr)

data_list <- list(accidents_FINAL, drimpair_FINAL, person_FINAL, vehicle_FINAL)

FARS_2018_2022 <- reduce(data_list, left_join, by = "ST_CASE_NEW")


## Editing some vars - changing some obs to 'Unknown' ##

library(lubridate)

FARS_2018_2022 <- FARS_2018_2022 %>%
  mutate(across(c(WEATHERNAME, VSURCONDNAME, RUR_URBNAME, FUNC_SYSNAME,RD_OWNERNAME,
                  SP_JURNAME, HARM_EVNAME, MAN_COLLNAME,RELJCT1NAME, RELJCT2NAME,REL_ROADNAME, 
                  TYP_INTNAME, WRK_ZONENAME, LGT_CONDNAME, VPROFILENAME, VALIGNNAME),
    ~ ifelse(. %in% c("Trafficway Not in State Inventory", "Not Reported", "Unknown",
                      "Reported as Unknown", "Unknown Object Not Fixed",
                      "Unknown Fixed Object"), "Unknown", .)
  ))

table(FARS_2018_2022$WEATHERNAME)

write.csv(FARS_2018_2022, "stat7240_fall24_team1/FARS_2018_2022.csv", row.names = FALSE)

## ~~~  Usable Date Variable ~~~ ##

FARS_2018_2022 <- read_csv("stat7240_fall24_team1/FARS_2018_2022.csv")

test <- FARS_2018_2022 %>%
  mutate(DATE = ymd(paste(YEAR, MONTHNAME, DAY, sep = "-")))

table(FARS_2018_2022$HOUR)
table(FARS_2018_2022$HOURNAME)
table(FARS_2018_2022$MINUTE)

# Mode imputation of hour and minute #

# Hour
FARS_2018_2022$HOUR[FARS_2018_2022$HOUR == 99] <- NA

mode_HOUR <- FARS_2018_2022 %>%
  filter(!is.na(HOUR)) %>%
  count(HOUR) %>%
  arrange(desc(n)) %>%
  slice(1) %>%
  pull(HOUR)

FARS_2018_2022$HOUR[is.na(FARS_2018_2022$HOUR)] <- mode_HOUR

table(FARS_2018_2022$HOUR)

# Minute

FARS_2018_2022$MINUTE[FARS_2018_2022$MINUTE == 99] <- NA

mode_MIN <- FARS_2018_2022 %>%
  filter(!is.na(MINUTE)) %>%
  count(MINUTE) %>%
  arrange(desc(n)) %>%
  slice(1) %>%
  pull(MINUTE)

FARS_2018_2022$MINUTE[is.na(FARS_2018_2022$MINUTE)] <- mode_MIN

table(FARS_2018_2022$MINUTE)

# Making the date variable #

FARS_WDATE <- FARS_2018_2022 %>%
  mutate(DATE = ymd_hm(paste(YEAR, MONTHNAME, DAY, HOUR, MINUTE, sep = "-")))

## ~~~ Removing some other stuffs ~~~ ##

TEMP <- FARS_WDATE %>%
  select(-DAY, -YEAR, -HOUR, -HOURNAME, -MINUTE, -NOT_HOUR, -NOT_HOURNAME, -NOT_MIN, 
         -NOT_MINNAME, -ARR_HOUR, -ARR_HOURNAME, -ARR_MIN, -ARR_MINNAME, -HOSP_HR,
         -HOSP_HRNAME, -HOSP_MN, -HOSP_MNNAME, -VPROFILE, -VALIGN, -VSURCOND) 

TEMP_FARS <- TEMP %>%
  relocate(STATE, STATENAME, ST_CASE_NEW, DATE)

FARS_2018_2022_V2 <- TEMP_FARS

write.csv(FARS_2018_2022_V2, "stat7240_fall24_team1/FARS_2018_2022_V2.csv", row.names = FALSE)

## ~~~ Mode Imputation of Unknowns ~~~ ##

FARS_2018_2022_V3 <- FARS_2018_2022_V2 %>%
  mutate(across(
    c(WEATHERNAME, VSURCONDNAME, RUR_URBNAME, FUNC_SYSNAME, RD_OWNERNAME, SP_JURNAME, HARM_EVNAME, 
      MAN_COLLNAME, RELJCT1NAME, RELJCT2NAME, REL_ROADNAME, TYP_INTNAME, WRK_ZONENAME, LGT_CONDNAME, 
      VPROFILENAME, VALIGNNAME), 
    ~ {
      .[. == "Unknown"] <- NA
      mode_val <- names(which.max(table(.[!is.na(.)])))
      replace_na(., mode_val)
    }
  ))

table(temp$WEATHERNAME)
table(temp$FUNC_SYSNAME)
table(temp$WRK_ZONENAME)

write.csv(FARS_2018_2022_V3, "stat7240_fall24_team1/FARS_2018_2022_V3.csv", row.names = FALSE)

## Mode imputation of VHAVG_AGE and add season variable ##

FARS_2018_2022_V3$VHAVG_AGE[FARS_2018_2022_V3$VHAVG_AGE > 2023] <- NA

mode_AGE <- FARS_2018_2022_V3 %>%
  filter(!is.na(VHAVG_AGE)) %>%
  count(VHAVG_AGE) %>%
  arrange(desc(n)) %>%
  slice(1) %>%
  pull(VHAVG_AGE)

FARS_2018_2022$VHAVG_AGE[is.na(FARS_2018_2022$VHAVG_AGE)] <- mode_AGE

table(FARS_2018_2022_V3$VHAVG_AGE)

# MAKE AVG AGE ROUND TO WHOLE NUMBER #

FARS_2018_2022_V3 <- FARS_2018_2022_V3 %>%
  mutate(VHAVG_AGE = round(VHAVG_AGE))

table(FARS_2018_2022_V3$VHAVG_AGE)

# SEASON VARIABLE #

FARS_2018_2022_V3 <- FARS_2018_2022_V3 %>%
  mutate(SEASON = case_when(
    MONTHNAME %in% c("December", "January", "February") ~ "Winter",
    MONTHNAME %in% c("March", "April", "May") ~ "Spring",
    MONTHNAME %in% c("June", "July", "August") ~ "Summer",
    MONTHNAME %in% c("September", "October", "November") ~ "Fall",
  ))

table(FARS_2018_2022_V3$SEASON)

# Add back the year #

FARS_2018_2022_V3 <- FARS_2018_2022_V3 %>%
  mutate(YEAR = year(DATE)) %>%
  relocate(STATE, STATENAME, ST_CASE_NEW, DATE, YEAR, MONTHNAME, DAY_WEEKNAME)

write.csv(FARS_2018_2022_V3, "stat7240_fall24_team1/FARS_2018_2022_V3.csv", row.names = FALSE)

# Impute NAs once again because somehow they keep appearing #

mode_DRAGE <- FARS_2018_2022_V3 %>%
  filter(!is.na(DRAVG_AGE)) %>%
  count(DRAVG_AGE) %>%
  arrange(desc(n)) %>%
  slice(1) %>%
  pull(DRAVG_AGE)

FARS_2018_2022_V3$DRAVG_AGE[is.na(FARS_2018_2022_V3$DRAVG_AGE)] <- mode_DRAGE

summary(FARS_2018_2022_V3$DRAVG_AGE)

write.csv(FARS_2018_2022_V3, "stat7240_fall24_team1/FARS_2018_2022_V3.csv", row.names = FALSE)


