library(stringr)
library(dplyr)
library(tidyr)
recent_data <- read_csv("stat7240_fall24_team1/FARS_2018_2022_V3.csv")

co_popprecovid <- co_pop201819 %>%
  filter(CTYNAME != STNAME) %>%
  mutate(CountyState = paste(CTYNAME, STNAME, sep = ", "))

head(co_popprecovid)

co_poppostcovid <- co_est2023_pop %>%
  mutate(CountyState = substr(CountyState, 2, nchar(CountyState)))
head(co_poppostcovid)

popest_merge <- co_popprecovid %>%
  left_join(co_poppostcovid, by = "CountyState") %>%
  select(-c("CTYNAME", "STNAME")) %>%
  select("CountyState", everything())
head(popest_merge)

grouping2 <- grouping %>%
  mutate(cleaned_county = str_to_title(str_replace(COUNTYNAME, " \\(.*\\)", "")),
         STATENAME = str_trim(STATENAME),
         year = as.character(YEAR))
head(accidents_summary2)

population_data <- popest_merge %>%
  rename(CountyState = 1) %>%
  mutate(county = word(CountyState, 1, sep = ","),
         state = str_trim(word(CountyState, 2, sep = ",")),
         across(-c(CountyState, county, state), as.character))

population_long <- population_data %>%
  pivot_longer(-c(CountyState, county, state), names_to = "year", values_to = "popest") %>%
  mutate(year = substr(year, 12, nchar(year)))

merged_data <- grouping2 %>%
  rowwise() %>%
  mutate(
    popest = population_long %>%
      filter(
        STATENAME == state, 
        str_detect(county, cleaned_county),  # Check if population_long county contains accidents_data county
        YEAR == year
      ) %>%
      pull(popest) %>% 
      first()  # Use the first match, or you can handle multiple matches differently if needed
  ) %>%
  ungroup()

#Fix dataset and making target
merged_final <- merged_data %>%
  select(-c("COUNTYNAME", "year")) %>%
  mutate(popest = as.numeric(popest),
    target = fatal_accidents / popest)


#Find NAs
na_counts <- sapply(merged_final, function(x) sum(is.na(x)))
na_counts

#Imputation of missing variables
mode_AGE <- merged_final %>%
  filter(!is.na(VHAVG_AGE)) %>%
  count(VHAVG_AGE) %>%
  arrange(desc(n)) %>%
  slice(1) %>%
  pull(VHAVG_AGE)

merged_final2 <- merged_final

merged_final2$VHAVG_AGE[is.na(merged_final2$VHAVG_AGE)] <- mode_AGE

mode_DRAGE <- merged_final2 %>%
  filter(!is.na(DRAVG_AGE)) %>%
  count(DRAVG_AGE) %>%
  arrange(desc(n)) %>%
  slice(1) %>%
  pull(DRAVG_AGE)

merged_final2$DRAVG_AGE[is.na(merged_final2$DRAVG_AGE)] <- mode_DRAGE

#Removing NAs in target variable (only 343 observations)
merged_final3 <- merged_final2[!is.na(merged_final2$target),]

write.csv(merged_final3, "stat7240_fall24_team1/FARS_merged.csv", row.names = FALSE)

dummy_model <- model.matrix(target ~ ., data = train_data)
vif(dummy_model)





                                     