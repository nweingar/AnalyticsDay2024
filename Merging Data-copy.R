# ~~~ Import 2021 data ~~~ #

accident2021 <- read_csv("stat7240_fall24_team1/FARS2021/accident.csv")
drimpair2021 <- read_csv("stat7240_fall24_team1/FARS2021/drimpair.csv")
drugs2021 <- read_csv("stat7240_fall24_team1/FARS2021/drugs.csv")
nmcrash2021 <- read_csv("stat7240_fall24_team1/FARS2021/nmcrash.csv")
person2021 <- read_csv("stat7240_fall24_team1/FARS2021/person.csv")
vehicle2021 <- read_csv("stat7240_fall24_team1/FARS2021/vehicle.csv")


# ~~~ Import 2020 data ~~~ #

accident2020 <- read_csv("stat7240_fall24_team1/FARS2020/accident.csv")
drimpair2020 <- read_csv("stat7240_fall24_team1/FARS2020/drimpair.csv")
drugs2020 <- read_csv("stat7240_fall24_team1/FARS2020/drugs.csv")
nmcrash2020 <- read_csv("stat7240_fall24_team1/FARS2020/nmcrash.csv")
person2020 <- read_csv("stat7240_fall24_team1/FARS2020/person.csv")
vehicle2020 <- read_csv("stat7240_fall24_team1/FARS2020/vehicle.csv")

# ~~~ Import 2019 data ~~~ #

accident2019 <- read_csv("stat7240_fall24_team1/FARS2019/accident.CSV")
drimpair2019 <- read_csv("stat7240_fall24_team1/FARS2019/DrImpair.CSV")
drugs2019 <- read_csv("stat7240_fall24_team1/FARS2019/Drugs.CSV")
nmcrash2019 <- read_csv("stat7240_fall24_team1/FARS2019/NMCrash.CSV")
person2019 <- read_csv("stat7240_fall24_team1/FARS2019/Person.CSV")
vehicle2019 <- read_csv("stat7240_fall24_team1/FARS2019/vehicle.csv")

# ~~~ Import 2018 data ~~~ #

accident2018 <- read_csv("stat7240_fall24_team1/FARS2018/accident.csv")
drimpair2018 <- read_csv("stat7240_fall24_team1/FARS2018/DrImpair.csv")
drugs2018 <- read_csv("stat7240_fall24_team1/FARS2018/Drugs.csv")
nmcrash2018 <- read_csv("stat7240_fall24_team1/FARS2018/nmcrash.csv")
person2018 <- read_csv("stat7240_fall24_team1/FARS2018/person.csv")
vehicle2018 <- read_csv("stat7240_fall24_team1/FARS2018/vehicle.csv")

## ~~~ Add year suffix to ID variables ~~~ ##

# 2022 #
#Accidents
accident2022_clean <- accident2022 %>%
dplyr::mutate(ST_CASE_NEW = paste0("2022", "_", ST_CASE)) %>%
dplyr::select(ST_CASE_NEW, everything(), -ST_CASE)
#Drimpair
drimpair2022_clean <- drimpair2022 %>%
dplyr::mutate(ST_CASE_NEW = paste0("2022", "_", ST_CASE)) %>%
dplyr::select(ST_CASE_NEW, everything(), -ST_CASE)
#drugs
drugs2022_clean <- drugs2022 %>%
dplyr::mutate(ST_CASE_NEW = paste0("2022", "_", ST_CASE)) %>%
dplyr::select(ST_CASE_NEW, everything(), -ST_CASE)
#nmcrash
nmcrash2022_clean <- nmcrash2022 %>%
dplyr::mutate(ST_CASE_NEW = paste0("2022", "_", ST_CASE)) %>%
dplyr::select(ST_CASE_NEW, everything(), -ST_CASE)
#person
person2022_clean <- person2022 %>%
dplyr::mutate(ST_CASE_NEW = paste0("2022", "_", ST_CASE)) %>%
dplyr::select(ST_CASE_NEW, everything(), -ST_CASE)
#vehicle
vehicle2022_clean <- vehicle2022 %>%
dplyr::mutate(ST_CASE_NEW = paste0("2022", "_", ST_CASE)) %>%
dplyr::select(ST_CASE_NEW, everything(), -ST_CASE)


# 2021 #
#Accidents
accident2021_clean <- accident2021 %>%
dplyr::mutate(ST_CASE_NEW = paste0("2021", "_", ST_CASE)) %>%
dplyr::select(ST_CASE_NEW, everything(), -ST_CASE)
#drimpair
drimpair2021_clean <- drimpair2021 %>%
dplyr::mutate(ST_CASE_NEW = paste0("2021", "_", ST_CASE)) %>%
dplyr::select(ST_CASE_NEW, everything(), -ST_CASE)
#drugs
drugs2021_clean <- drugs2021 %>%
dplyr::mutate(ST_CASE_NEW = paste0("2021", "_", ST_CASE)) %>%
dplyr::select(ST_CASE_NEW, everything(), -ST_CASE)
#nmcrash
nmcrash2021_clean <- nmcrash2021 %>%
dplyr::mutate(ST_CASE_NEW = paste0("2021", "_", ST_CASE)) %>%
dplyr::select(ST_CASE_NEW, everything(), -ST_CASE)
#person
person2021_clean <- person2021 %>%
dplyr::mutate(ST_CASE_NEW = paste0("2021", "_", ST_CASE)) %>%
dplyr::select(ST_CASE_NEW, everything(), -ST_CASE)
#vehicle
vehicle2021_clean <- vehicle2021 %>%
dplyr::mutate(ST_CASE_NEW = paste0("2021", "_", ST_CASE),
              TRLR2VIN = as.numeric(TRLR2VIN)) %>%
dplyr::select(ST_CASE_NEW, everything(), -ST_CASE)


# 2020 #
#Accidents
accident2020_clean <- accident2020 %>%
dplyr::mutate(ST_CASE_NEW = paste0("2020", "_", ST_CASE)) %>%
dplyr::select(ST_CASE_NEW, everything(), -ST_CASE)
#drimpair
drimpair2020_clean <- drimpair2020 %>%
dplyr::mutate(ST_CASE_NEW = paste0("2020", "_", ST_CASE)) %>%
dplyr::select(ST_CASE_NEW, everything(), -ST_CASE)
#drugs
drugs2020_clean <- drugs2020 %>%
dplyr::mutate(ST_CASE_NEW = paste0("2020", "_", ST_CASE)) %>%
dplyr::select(ST_CASE_NEW, everything(), -ST_CASE)
#nmcrash
nmcrash2020_clean <- nmcrash2020 %>%
dplyr::mutate(ST_CASE_NEW = paste0("2020", "_", ST_CASE)) %>%
dplyr::select(ST_CASE_NEW, everything(), -ST_CASE)
#person
person2020_clean <- person2020 %>%
dplyr::mutate(ST_CASE_NEW = paste0("2020", "_", ST_CASE)) %>%
dplyr::select(ST_CASE_NEW, everything(), -ST_CASE)
#vehicle
vehicle2020_clean <- vehicle2020 %>%
dplyr::mutate(ST_CASE_NEW = paste0("2020", "_", ST_CASE),
              TRLR2VIN = as.numeric(TRLR2VIN)) %>%
dplyr::select(ST_CASE_NEW, everything(), -ST_CASE)


# 2019 #
#Accidents
accident2019_clean <- accident2019 %>%
dplyr::mutate(ST_CASE_NEW = paste0("2019", "_", ST_CASE),
              LATITUDE = as.numeric(LATITUDE),
              LONGITUD = as.numeric(LONGITUD),
              LATITUDENAME = as.numeric(LATITUDENAME),
              LONGITUDNAME = as.numeric(LONGITUDNAME)) %>%
dplyr::select(ST_CASE_NEW, everything(), -ST_CASE)
#drimpair
drimpair2019_clean <- drimpair2019 %>%
dplyr::mutate(ST_CASE_NEW = paste0("2019", "_", ST_CASE)) %>%
dplyr::select(ST_CASE_NEW, everything(), -ST_CASE)
#drugs
drugs2019_clean <- drugs2019 %>%
dplyr::mutate(ST_CASE_NEW = paste0("2019", "_", ST_CASE)) %>%
dplyr::select(ST_CASE_NEW, everything(), -ST_CASE)
#nmcrash
nmcrash2019_clean <- nmcrash2019 %>%
dplyr::mutate(ST_CASE_NEW = paste0("2019", "_", ST_CASE)) %>%
dplyr::select(ST_CASE_NEW, everything(), -ST_CASE)
#person
person2019_clean <- person2019 %>%
dplyr::mutate(ST_CASE_NEW = paste0("2019", "_", ST_CASE),
              MAK_MOD = as.numeric(MAK_MOD)) %>%
dplyr::select(ST_CASE_NEW, everything(), -ST_CASE)
#vehicle
vehicle2019_clean <- vehicle2019 %>%
dplyr::mutate(ST_CASE_NEW = paste0("2019", "_", ST_CASE)) %>%
dplyr::select(ST_CASE_NEW, everything(), -ST_CASE)


# 2018 #
#Accidents
accident2018_clean <- accident2018 %>%
dplyr::mutate(ST_CASE_NEW = paste0("2018", "_", ST_CASE),
              LATITUDE = as.numeric(LATITUDE),
              LONGITUD = as.numeric(LONGITUD),
              LATITUDENAME = as.numeric(LATITUDENAME),
              LONGITUDNAME = as.numeric(LONGITUDNAME)) %>%
dplyr::select(ST_CASE_NEW, everything(), -ST_CASE)
#drimpair
drimpair2018_clean <- drimpair2018 %>%
dplyr::mutate(ST_CASE_NEW = paste0("2018", "_", ST_CASE)) %>%
dplyr::select(ST_CASE_NEW, everything(), -ST_CASE)
#drugs
drugs2018_clean <- drugs2018 %>%
dplyr::mutate(ST_CASE_NEW = paste0("2018", "_", ST_CASE)) %>%
dplyr::select(ST_CASE_NEW, everything(), -ST_CASE)
#nmcrash
nmcrash2018_clean <- nmcrash2018 %>%
dplyr::mutate(ST_CASE_NEW = paste0("2018", "_", ST_CASE)) %>%
dplyr::select(ST_CASE_NEW, everything(), -ST_CASE)
#person
person2018_clean <- person2018 %>%
dplyr::mutate(ST_CASE_NEW = paste0("2018", "_", ST_CASE),
              MINUTENAME = as.character(MINUTENAME)) %>%
dplyr::select(ST_CASE_NEW, everything(), -ST_CASE)
#vehicle
vehicle2018_clean <- vehicle2018 %>%
dplyr::mutate(ST_CASE_NEW = paste0("2018", "_", ST_CASE)) %>%
dplyr::select(ST_CASE_NEW, everything(), -ST_CASE)

## ~~~ Creating one dataset for each sub-dataset ~~~ ##

# accidents #

accidents_COMPLETE <- bind_rows(accident2018_clean, accident2019_clean,
                                accident2020_clean, accident2021_clean,
                                accident2022_clean) %>%
arrange(STATE, ST_CASE_NEW)

# drimpair #

drimpair_COMPLETE <- bind_rows(drimpair2018_clean, drimpair2019_clean, 
                               drimpair2020_clean, drimpair2021_clean,
                               drimpair2022_clean) %>%
arrange(STATE, ST_CASE_NEW)

# drugs #

drugs_COMPLETE <- bind_rows(drugs2018_clean, drugs2019_clean,
                            drugs2020_clean, drugs2021_clean,
                            drugs2022_clean) %>%
arrange(STATE, ST_CASE_NEW)

# nmcrash #

nmcrash_COMPLETE <- bind_rows(nmcrash2018_clean, nmcrash2019_clean,
                              nmcrash2020_clean, nmcrash2021_clean,
                              nmcrash2022_clean) %>%
arrange(STATE, ST_CASE_NEW)

# person # ERROR

person_COMPLETE <- bind_rows(person2018_clean, person2019_clean,
                            person2020_clean, person2021_clean,
                            person2022_clean) %>%
arrange(STATE, ST_CASE_NEW)

# vehicle #

vehicle_COMPLETE <- bind_rows(vehicle2018_clean, vehicle2019_clean,
                              vehicle2020_clean, vehicle2021_clean,
                              vehicle2022_clean) %>%
arrange(STATE, ST_CASE_NEW)

# export to csv #

write.csv(accidents_COMPLETE, "stat7240_fall24_team1/Unfiltered Data 2018-2022/accidents_COMPLETE.csv", row.names = FALSE)
write.csv(drimpair_COMPLETE, "stat7240_fall24_team1/Unfiltered Data 2018-2022/drimpair_COMPLETE.csv", row.names = FALSE)
write.csv(drugs_COMPLETE, "stat7240_fall24_team1/Unfiltered Data 2018-2022/drugs_COMPLETE.csv", row.names = FALSE)
write.csv(person_COMPLETE, "stat7240_fall24_team1/Unfiltered Data 2018-2022/person_COMPLETE.csv", row.names = FALSE)
write.csv(vehicle_COMPLETE, "stat7240_fall24_team1/Unfiltered Data 2018-2022/vehicle_COMPLETE.csv", row.names = FALSE)


## ~~~ Creating and removing variables for each dataset ~~~ ##
library(dplyr)
# drimpair #

  # add a num of impaired variable - IMPAIR_NO; IMPAIR_NO_NAME (yes, none/unknown)
  # reduce to one record per incident
  # remove veh_no, then drimp and drimpname

drimpair_FINAL <- drimpair_COMPLETE %>%
  group_by(ST_CASE_NEW) %>%
  dplyr::mutate(
    IMPAIR_NO = sum(DRIMPAIR >= 1 & DRIMPAIR <= 10),  
    IMPAIR_NO_NAME = ifelse(any(DRIMPAIR >= 1 & DRIMPAIR <= 10), "Yes", "None/Unknown")) %>%
  filter(VEH_NO == 1) %>%
  dplyr::select(-VEH_NO, -DRIMPAIR, -DRIMPAIRNAME)

dataset1 <- accidents_COMPLETE %>% mutate(source = "accidents_COMPLETE")
dataset2 <- drimpair_FINAL %>% mutate(source = "drimpair_FINAL")

# Perform a full join on the identifier
full_data <- full_join(dataset1, dataset2, by = "ST_CASE_NEW", suffix = c("_1", "_2"))

# Filter for unique observations in each dataset
unique_to_dataset1 <- full_data %>% filter(is.na(source_2)) %>% select(ST_CASE_NEW, source_1)
unique_to_dataset2 <- full_data %>% filter(is.na(source_1)) %>% select(ST_CASE_NEW, source_2)

# View unique observations
unique_to_dataset1
unique_to_dataset2
# slightly more obs than accidents csv...  why?

#There are 269 observations not present in the drimpair dataset that is in the accidents data
#It would probably be easiest to track the st_case_news in unique_dataset_1 and remove those observations in the accidents data


  write.csv(drimpair_FINAL, "stat7240_fall24_team1/Finalized Datasets/drimpair_FINAL.csv", row.names = FALSE)


# person #

  # create a total male var (just for drivers - PER_NO = 1)
  # create a total female var
  # consider a variable for age of driver: average age, age range, or age bracket indicator vars? #decided on avg age for the sake of ease
  # we already have total in the vehicles/involved
  
person_FINAL <- person_COMPLETE %>%
  group_by(ST_CASE_NEW) %>%
  summarise(
    DRTOTAL = max(PER_NO),
    DRTOTAL_MALE = sum(SEXNAME == "Male" & PER_NO == 1),
    DRTOTAL_FEMALE = sum(SEXNAME == "Female" & PER_NO == 1),
    AVG_AGE = mean(AGE)
  )
summary(person_FINAL$DRTOTAL_MALE)
summary(person_FINAL$AGE)
summary(person_FINAL$AVG_AGE)

# vehicle #

  # we already have total occupants in vehicles
  # body type (car type) - would have to categorize these and then make binary vars; VPICBODYCLASSNAME?
    #we could instead just use the quantitative body_typ variable, it is naturally ordinal.
  # vehicle age range, maybe average age
    
  # number of speeders
  # number of invalid licenses (revoked, suspended, unlicensed)
  # only need one obs from these (they should be the same for all records):
  #   vtrafway
  #   vspeedlim
  #   valign
  #   vprofile
  #   vsurcon
  #   vtrafcon
  #   vtcontf


vehicle_FINAL <- vehicle_COMPLETE  %>%
  group_by(ST_CASE_NEW) %>%
  summarise(
    AVG_AGE = mean(MOD_YEAR),
    TOT_SPEED = sum(SPEEDREL >= 1 & SPEEDREL <= 5),
    TOT_L_VIO = sum(L_STATUS >= 1 & L_STATUS <= 4)
  )

summary(vehicle_FINAL$TOT_L_VIO)

vehicle_license <- vehicle_COMPLETE %>%
  select(c("ST_CASE_NEW", "VEH_NO", "L_STATUS"))
