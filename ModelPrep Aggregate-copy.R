# impute NAs in vhavg_age because somehow i missed them #

FARS_2018_2022_V3$VHAVG_AGE[FARS_2018_2022_V3$VHAVG_AGE > 2023] <- NA

mode_AGE <- FARS_2018_2022_V3 %>%
  filter(!is.na(VHAVG_AGE)) %>%
  count(VHAVG_AGE) %>%
  arrange(desc(n)) %>%
  slice(1) %>%
  pull(VHAVG_AGE)

FARS_2018_2022_V3$VHAVG_AGE[is.na(FARS_2018_2022_V3$VHAVG_AGE)] <- mode_AGE

table(FARS_2018_2022_V3$VHAVG_AGE)
sum(is.na(FARS_2018_2022_V3$VHAVG_AGE))

write.csv(FARS_2018_2022_V3, "stat7240_fall24_team1/FARS_2018_2022_V3.csv", row.names = FALSE)

## Aggregating ##

grouping <- recent_data %>%
  group_by(STATENAME, COUNTYNAME, YEAR) %>%
  summarise(
    fatal_accidents = n(),
    MONTHNAME = names(sort(table(MONTHNAME), decreasing = TRUE))[1],
    DAY_WEEKNAME = names(sort(table(DAY_WEEKNAME), decreasing = TRUE))[1],
    PERNOTMVIT = median(PERNOTMVIT, na.rm=T),
    VE_FORMS = median(VE_FORMS, na.rm=T),
    PERSONS = median(PERSONS, na.rm=T),
    PERMVIT = median(PERMVIT, na.rm=T),
    CITYNAME = names(sort(table(CITYNAME), decreasing = TRUE))[1],
    ROUTENAME = names(sort(table(ROUTENAME), decreasing = TRUE))[1],
    RUR_URBNAME = names(sort(table(RUR_URBNAME), decreasing = TRUE))[1],
    FUNC_SYSNAME = names(sort(table(FUNC_SYSNAME), decreasing = TRUE))[1],
    RD_OWNERNAME = names(sort(table(RD_OWNERNAME), decreasing = TRUE))[1],
    SP_JURNAME = names(sort(table(SP_JURNAME), decreasing = TRUE))[1],
    LATITUDE = median(LATITUDE, na.rm=T),
    LONGITUD = median(LONGITUD, na.rm=T),
    HARM_EVNAME = names(sort(table(HARM_EVNAME), decreasing = TRUE))[1],
    MAN_COLLNAME = names(sort(table(MAN_COLLNAME), decreasing = TRUE))[1],
    RELJCT1NAME = names(sort(table(RELJCT1NAME), decreasing = TRUE))[1],
    RELJCT2NAME = names(sort(table(RELJCT2NAME), decreasing = TRUE))[1],
    TYP_INTNAME = names(sort(table(TYP_INTNAME), decreasing = TRUE))[1],
    REL_ROADNAME = names(sort(table(REL_ROADNAME), decreasing = TRUE))[1],
    WRK_ZONENAME = names(sort(table(WRK_ZONENAME), decreasing = TRUE))[1],
    LGT_CONDNAME = names(sort(table(LGT_CONDNAME), decreasing = TRUE))[1],
    WEATHERNAME = names(sort(table(WEATHERNAME), decreasing = TRUE))[1],
    IMPAIR_NO = median(IMPAIR_NO, na.rm=T),
    IMPAIR_NO_NAME = names(sort(table(SP_JURNAME), decreasing = TRUE))[1],
    DRTOTAL = median(DRTOTAL, na.rm=T),
    DRTOTAL_MALE = median(DRTOTAL_MALE, na.rm=T),
    DRTOTAL_FEMALE = median(DRTOTAL_FEMALE, na.rm=T),
    DRAVG_AGE = mean(DRAVG_AGE, na.rm=T),
    VSPD_LIMNAME = names(sort(table(VSPD_LIMNAME), decreasing = TRUE))[1],
    VPROFILENAME = names(sort(table(VPROFILENAME), decreasing = TRUE))[1],
    VALIGNNAME = names(sort(table(VALIGNNAME), decreasing = TRUE))[1],
    VSURCONDNAME = names(sort(table(VSURCONDNAME), decreasing = TRUE))[1],
    VHAVG_AGE = median(VHAVG_AGE, na.rm=T),
    TOT_SPEED = median(TOT_SPEED, na.rm=T),
    TOT_L_VIO = median(TOT_L_VIO, na.rm=T),
    SEASON = names(sort(table(SEASON), decreasing = TRUE))[1]
  ) %>%
  ungroup()

hist(recent_data$TOT_L_VIO)
summary(recent_data$TOT_L_VIO)

FARS_grouping <- grouping

write.csv(FARS_grouping, "stat7240_fall24_team1/FARS_grouping.csv", row.names = FALSE)


