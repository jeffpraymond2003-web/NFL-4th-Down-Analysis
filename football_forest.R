library(tidyverse)
library(tidymodels)
library(randomForest)
library(pROC)
library(rpart)
library(rpart.plot)
library(reshape2)

RNGkind(sample.kind = "default")
set.seed(4561)
train.idx <- sample(x = 1:nrow(fourth_final), size = floor(.7*nrow(fourth_final)))

train.df <- fourth_final[train.idx,]
train.df <- subset(train.df)
test.df <- fourth_final[-train.idx,]

rf_model <- rand_forest(mtry = tune(),
                        trees = 1000) %>% 
  set_mode("classification") %>% 
  set_engine("randomForest")

# (step 2) Create a recipe
rf_rec <- recipe(fourth_down_converted ~ ., data = train.df)

# (step 3) Create the workflow
rf_wf <- workflow() %>%
  add_model(rf_model) %>%
  add_recipe(rf_rec)

# (step 4) Create folds for cross validation (see previous illustration)
set.seed(172172)
folds <- vfold_cv(train.df, v = 5)

# (step 5) tune random forest
rf_tuned <- tune_grid(
  rf_wf, 
  resamples = folds, 
  grid = tibble(mtry = c(1:5,10,15,20)), 
  metrics = metric_set(roc_auc)
)

# (step 6) extract AUC and/or OOB error estimates
rf_results <- rf_tuned %>%
  collect_metrics()

ggplot(data = rf_results) +
  geom_line(aes(x = mtry, y = mean)) +
  labs(x = "m (mtry) value", y = "Area Under the Curve (AUC)")+
  theme_bw() +
  scale_x_continuous(breaks = c(1:20))

best_params <- select_best(rf_tuned, metric ="roc_auc")
final_forest<- randomForest(fourth_down_converted ~ .,
                            data = train.df,
                            ntree = 1000, #fix B at 1000!
                            mtry = best_params %>% pull(mtry),
                            importance = TRUE)

pi_hat_forest<- predict(final_forest, test.df, type = "prob")[, "Yes"]
rocCurve <- roc(response = test.df$fourth_down_converted,
                predictor = pi_hat_forest,
                levels = c("No", "Yes"))
plot(rocCurve,print.auc = TRUE, print.thres=TRUE)

vi <- varImpPlot(final_forest,type=1) %>% as.data.frame %>% 
  slice_max(order_by = MeanDecreaseAccuracy, n = 10)
vi$Variable <- rownames(vi)

ggplot(data = vi) +
  geom_bar(aes(x = reorder(Variable,MeanDecreaseAccuracy), weight = MeanDecreaseAccuracy),
           position ="identity") +
  coord_flip() +
  labs( x = "Variable Name",y = "Importance")
