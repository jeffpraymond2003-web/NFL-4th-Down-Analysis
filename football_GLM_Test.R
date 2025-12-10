library(tidyverse)
library(pROC)
library(glmnet) # for fitting ridge
library(lubridate) # for easily manipulating data




#fourth_final <- fourth_final %>% 
  mutate(fourth_down_converted_bin = ifelse(fourth_down_converted == "Yes", 1, 0 )) %>% 
  select(-c(fourth_down_converted))


#Setting seedfourth_down_converted_bin#Setting seed and getting our test and training data sets
RNGkind(sample.kind = "default")
set.seed(4561)
train.idx <- sample(x = 1:nrow(fourth_final), size = floor(.7*nrow(fourth_final)))

train.df <- fourth_final[train.idx,]
train.df <- subset(train.df)
test.df <- fourth_final[-train.idx,]


#only two games in Allianz Arena, both in test set and mess up the regression to keep sets as similar as possible across models dropping the two entries 
test.df <-  test.df[test.df$game_stadium != "Allianz Arena",]
#build X matrices for ridge

x.train <- model.matrix(fourth_down_converted_bin ~ ., data = train.df)[,-1] #to remove an unnecessary intercept column

x.test <-  model.matrix(fourth_down_converted_bin ~ ., data = test.df)[,-1]

# creating "vectorized" y vectors
y.train <- as.vector(train.df$fourth_down_converted_bin)
y.test <- as.vector(test.df$fourth_down_converted_bin)

#Using cross validation to quickly fit and assess many ridge regression models
lr_ridge_cv <- cv.glmnet(x.train, y.train, family = binomial(link = "logit"), alpha = 0)


plot(lr_ridge_cv, sign.lambda = 1 )


best_ridge_lambda <- lr_ridge_cv$lambda.min # this is the lambda that 
#minimizes out of sample error (based on cross validation)

lr_ridge_coefs <- coef(lr_ridge_cv, s=best_ridge_lambda )

#Fitting the final model 
final_ridge <- glmnet(x.train, y.train, family = binomial(link = "logit"), alpha = 0,
                      lambda = best_ridge_lambda)

test.df.preds <- test.df %>%  
  mutate(ridge_pred = predict(final_ridge, x.test, type = "response")[,1])
         
        
#creating an ROC Curve
ridge_rocCurve <- roc(response = as.factor(test.df.preds$fourth_down_converted_bin),
                      predictor = test.df.preds$ridge_pred,
                      levels = c("0","1"))


#plotting ROC Curve
plot(ridge_rocCurve,print.auc = TRUE, print.thres=TRUE)



#Table showing comparison of X and Ys

#looking more which coeff have the greatest impact

library(tibble)
library(dplyr)

cmat <- coef(lr_ridge_cv, s = best_ridge_lambda)

coef_tbl <- tibble(
  feature = rownames(cmat),
  coef    = as.vector(cmat)
) %>%
 filter(feature != "(Intercept)", abs(coef) > 1)

coef_tbl


#graphs...

ggplot(data = fourth_final) +
  geom_bar(aes(x = reorder(game_stadium,fourth_down_converted), fill = fourth_down_converted_bin), position = "fill") +
  scale_fill_brewer("Fourth Down Converted",palette = "Paired") +
  labs(x = "Conference", y = "Proportion") +
  coord_flip()

fourth_final$game_stadium
