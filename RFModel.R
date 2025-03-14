library(caret)
library(randomForest)
library(car)

merged <- read_csv("stat7240_fall24_team1/FARS_merged.csv")

#Remove variables directly related to target variable
merged_mod <- merged %>%
  select(-c("popest", "fatal_accidents"))

#Convert character variables to factor variables
merged_fac <- merged_mod %>%
  mutate(across(where(is.character), as.factor))



sqrt(39)
my.mtry = c(3, 6, 9, 12)
my.rule = "gini"
my.nodes = c(1, 3, 5)

my.grid = expand.grid(mtry = my.mtry,
                      splitrule = my.rule,
                      min.node.size = my.nodes)

my.metric = "RMSE"

summary(merged_fac)
na_counts <- sapply(merged_fac, function(x) if(is.factor(x)) length(levels(x)) else NA)
na_counts

problematic_rows <- sapply(merged_fac_cln, function(col) any(grepl("[^ -~]", col)))
names(which(problematic_rows))

#Remove problematic variable
merged_fac_cln <- merged_fac %>%
  select(-c("CITYNAME"))

train_index <- createDataPartition(merged_fac_cln$target, p=0.8, list=F)
train_data <- merged_fac_cln %>% slice(as.vector(train_index))
test_data <- merged_fac_cln %>% slice(as.vector(-train_index))

set.seed(2)
rf.tune = caret::train(target ~ ., data = train_data, method = "ranger",
                       metric = 'RMSE',
                       importance = "impurity",
                       trControl = trainControl(method = "cv",
                                                number = 10),
                       tuneLength = 7
                       )

lm_model <- caret::train(target ~ ., data = train_data, method = "lm",
                         trControl = trainControl(method = "cv", number = 10))
summary(lm_model)

rf.tune$results %>% arrange(-desc(RMSE))

rf.tune$bestTune

pred.tune = predict(rf.tune, test_data, type = 'raw')
head(pred.tune)
#
#train_data_reduced <- subset(train_data, select = -cleaned_county)
#
#lm_model_simple <- lm(target ~ ., data=train_data_reduced)
alias_info <- alias(lm_model_simple)
print(alias_info)
vif_values <- vif(lm_model_simple)
print(vif_values)

# Re-fit the model with the reduced dataset
lm_model_reduced <- lm(target ~ ., data = train_data_reduced)

cor(train_data)

ridge_model <- caret::train(target ~ ., data = train_data, method = "glmnet",
                            tuneGrid = expand.grid(alpha = 0, lambda = seq(0.001, 0.1, by = 0.01)),
                            trControl = trainControl(method = "cv", number = 10))
summary(ridge_model)
ridge_model$results %>% arrange(-desc(RMSE))
ridge_model$bestTune

ridge.pred = predict(ridge_model, test_data, type = 'raw')
head(ridge.pred)

#pred.tune and ridge.pred

prob.tune = predict(rf.tune, test_data, type="prob")
prob.ridge = predict(ridge_model, test_data, type="prob")

#XGBoost
xgb.tune = caret::train(target ~ ., data = train_data, method = "xgbTree"
                        , metric = 'RMSE'
                        , verbosity = 0
                        , trControl = trainControl(method = "cv",
                                                   number = 10)
                        , tuneLength = 5
                        )
xgb.tune$results %>% arrange(-desc(RMSE))
xgb.tune$bestTune

pred.xgb = predict(xgb.tune, test_data, type = "raw")
#prob.xgb = predict(xgb.tune, test_data, type = 'prob')

df.xgbpred = data.frame(
  actual = test_data$target,
  prediction = pred.xgb
)
head(df.xgbpred)

conf1 = caret::confusionMatrix(data = pred.xgb, reference = test_data$target)

rf.vimp = caret::varImp(rf.tune, scale=F)
rf.vimp

xgb.imp = xgb.importance(model = xgb.tune$finalModel)
head(xgb.imp)
xgb.imp2 = xgb.imp
xgb.imp2$scaled_gain <- (xgb.imp$Gain - min(xgb.imp$Gain)) / (max(xgb.imp$Gain) - min(xgb.imp$Gain)) * 100
sum(xgb.imp$Gain)

df.rf.vimp = data.frame(rf.vimp$importance) %>%
  rownames_to_column("Variable") %>%
  arrange(desc(Overall)) %>%
  dplyr::slice(1:10)
head(df.rf.vimp)

gg.rf.vimp = ggplot() +
  geom_col(data = df.rf.vimp, aes(x = Overall, y =fct_reorder(Variable, Overall)), fill = "dodgerblue", alpha = 0.8) +
  ggtitle("Variable Importance: Random Forest Model") +
  xlab("Importance - Gini Impurity (scaled)") +
  ylab(element_blank())
gg.rf.vimp

xgb.imp = xgb.importance(model = xgb.tune$finalModel)
head(xgb.imp)
xgb.imp2 = xgb.imp
xgb.imp2$scaled_gain <- (xgb.imp$Gain - min(xgb.imp$Gain)) / (max(xgb.imp$Gain) - min(xgb.imp$Gain)) * 100
sum(xgb.imp$Gain)

df.xg.vimp = data.frame(xgb.imp2) %>%
  rownames_to_column("Variable") %>%
  arrange(desc(scaled_gain)) %>%
  dplyr::slice(1:10)
head(df.xg.vimp)

gg.xg.vimp = ggplot() +
  geom_col(data = df.xg.vimp, aes(x = scaled_gain, y =fct_reorder(Feature, scaled_gain)), fill = "dodgerblue", alpha = 0.8) +
  ggtitle("Variable Importance: XGBoost Model") +
  xlab("Importance - Gain (scaled)") +
  ylab(element_blank())
gg.xg.vimp

cor(thing)
corrplot(cor(hurr2 %>% dplyr::select(-hurricane_yn)))

table(merged_final$MAN_COLLNAME)

library(rstatix)
identify_outliers(
  data = merged,
  variable = "fatal_accidents"
)
data<-merged

# Define the column to remove outliers from
column_name <- "popest"  # replace with the actual column name

# Calculate Q1, Q3, and IQR for the specific column
Q1 <- quantile(data[[column_name]], 0.25, na.rm = TRUE)
Q3 <- quantile(data[[column_name]], 0.75, na.rm = TRUE)
IQR <- Q3 - Q1

# Filter out rows where the column value is an outlier
data_no_outliers <- data %>%
  filter(data[[column_name]] >= (Q1 - 1.5 * IQR) & data[[column_name]] <= (Q3 + 1.5 * IQR))
# Optionally, remove rows with any NA values 
data_no_outliers <- data_no_outliers %>% drop_na()

#Convert character variables to factor variables
data_fac <- data_no_outliers %>%
  mutate(across(where(is.character), as.factor)) %>%
  select(-c("CITYNAME", "popest", "fatal_accidents"))

train_index <- createDataPartition(data_fac$target, p=0.8, list=F)
train_data <- data_fac %>% dplyr::slice(as.vector(train_index))
test_data <- data_fac %>% dplyr::slice(as.vector(-train_index))

xgb.tune.noout = caret::train(target ~ ., data = train_data, method = "xgbTree"
                        , metric = 'RMSE'
                        , trControl = trainControl(method = "cv",
                                                   number = 10)
                        , tuneLength = 5
)
xgb.tune.noout$results %>% arrange(-desc(RMSE))
xgb.tune.noout$bestTune

xgb.imp.out = xgb.importance(model = xgb.tune.noout$finalModel)
head(xgb.imp.out)
xgb.imp2.out = xgb.imp.out
xgb.imp2.out$scaled_gain <- (xgb.imp.out$Gain - min(xgb.imp.out$Gain)) / (max(xgb.imp.out$Gain) - min(xgb.imp.out$Gain)) * 100
sum(xgb.imp.out$Gain)

df.xg.vimp.out = data.frame(xgb.imp2.out) %>%
  rownames_to_column("Variable") %>%
  arrange(desc(scaled_gain)) %>%
  dplyr::slice(1:10)
head(df.xg.vimp.out)

gg.xg.vimp.out = ggplot() +
  geom_col(data = df.xg.vimp.out, aes(x = scaled_gain, y =fct_reorder(Feature, scaled_gain)), fill = "dodgerblue", alpha = 0.8) +
  ggtitle("Variable Importance: XGBoost Model") +
  xlab("Importance - Gain (scaled)") +
  ylab(element_blank())
gg.xg.vimp.out

cleaned <- read_csv("stat7240_fall24_team1/fars_merged_RECAT.csv")

cleaned <- cleaned %>%
  select(-c("fatal_accidents", "popest"))

train_index <- createDataPartition(cleaned$target, p=0.8, list=F)
train_data <- cleaned %>% dplyr::slice(as.vector(train_index))
test_data <- cleaned %>% dplyr::slice(as.vector(-train_index))

xgb.tune.clean = caret::train(target ~ ., data = train_data, method = "xgbTree"
                              , metric = 'RMSE'
                              , trControl = trainControl(method = "cv",
                                                         number = 10)
                              , tuneLength = 5
)
xgb.tune.clean$results %>% arrange(-desc(RMSE))
xgb.tune.clean$bestTune

xgb.imp.clean = xgb.importance(model = xgb.tune.clean$finalModel)
head(xgb.imp.clean)
xgb.imp2.clean = xgb.imp.clean
xgb.imp2.clean$scaled_gain <- (xgb.imp.clean$Gain - min(xgb.imp.clean$Gain)) / (max(xgb.imp.clean$Gain) - min(xgb.imp.clean$Gain)) * 100
sum(xgb.imp.clean$Gain)

df.xg.vimp.clean = data.frame(xgb.imp2.clean) %>%
  rownames_to_column("Variable") %>%
  arrange(desc(scaled_gain)) %>%
  dplyr::slice(1:10)
head(df.xg.vimp.clean)

gg.xg.vimp.clean = ggplot() +
  geom_col(data = df.xg.vimp.clean, aes(x = scaled_gain, y =fct_reorder(Feature, scaled_gain)), fill = "dodgerblue", alpha = 0.8) +
  ggtitle("Variable Importance: XGBoost Model") +
  xlab("Importance - Gain (scaled)") +
  ylab(element_blank())
gg.xg.vimp.clean

nocounty <- cleaned %>%
  select(-c("cleaned_county"))

train_index <- createDataPartition(nocounty$target, p=0.8, list=F)
train_data <- nocounty %>% dplyr::slice(as.vector(train_index))
test_data <- nocounty %>% dplyr::slice(as.vector(-train_index))

xgb.tune.nocount = caret::train(target ~ ., data = train_data, method = "xgbTree"
                              , metric = 'RMSE'
                              , trControl = trainControl(method = "cv",
                                                         number = 10)
                              , tuneLength = 5
)

xgb.tune.nocount$results %>% arrange(-desc(RMSE))
xgb.tune.nocount$bestTune

xgb.imp.nocount = xgb.importance(model = xgb.tune.nocount$finalModel)
head(xgb.imp.nocount)
xgb.imp2.nocount = xgb.imp.nocount
xgb.imp2.nocount$scaled_gain <- (xgb.imp.nocount$Gain - min(xgb.imp.nocount$Gain)) / (max(xgb.imp.nocount$Gain) - min(xgb.imp.nocount$Gain)) * 100
sum(xgb.imp.nocount$Gain)

df.xg.vimp.nocount = data.frame(xgb.imp2.nocount) %>%
  rownames_to_column("Variable") %>%
  arrange(desc(scaled_gain)) %>%
  dplyr::slice(1:10)
head(df.xg.vimp.nocount)

gg.xg.vimp.nocount = ggplot() +
  geom_col(data = df.xg.vimp.nocount, aes(x = scaled_gain, y =fct_reorder(Feature, scaled_gain)), fill = "dodgerblue", alpha = 0.8) +
  ggtitle("Variable Importance: XGBoost Model") +
  xlab("Importance - Gain (scaled)") +
  ylab(element_blank())
gg.xg.vimp.nocount

rf.tune$results %>% arrange(-desc(RMSE))
ridge_model$results %>% arrange(-desc(RMSE))
xgb.tune$results %>% arrange(-desc(RMSE))
xgb.tune.noout$results %>% arrange(-desc(RMSE))
xgb.tune.clean$results %>% arrange(-desc(RMSE))
xgb.tune.nocount$results %>% arrange(-desc(RMSE))

gg.xg.vimp.

