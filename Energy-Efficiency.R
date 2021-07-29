# https://www.kaggle.com/elikplim/eergy-efficiency-dataset

# https://cran.r-project.org/
# https://www.rstudio.com/

# https://github.com/rmkunrath/weigao_group

# https://rafalab.github.io/dsbook/

# https://www.edx.org/professional-certificate/harvardx-data-science

# packages required
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")
if(!require(readxl)) install.packages("readxl", repos = "http://cran.us.r-project.org")

# import .xlsx file
data <- read_excel("data.xlsx")

#assign appropriate column names
colnames(data) <- c("relative_compactness","surface_area","wall_area","roof_area","overall_height",
                    "orientation","glazing_area", "glazing_area_distribution", "heating_load", "cooling_load")


#display first 6 rows of the updated data table
data %>% head() %>% knitr::kable(caption = "First 6 rows of the dataset")

#summary stats of data
summary(data)

#simple plot
data %>% ggplot(aes(heating_load, cooling_load, colour = roof_area)) +
  geom_point(alpha = 0.5) +
  xlab("Heating load") +
  ylab("Cooling load")
  # geom_smooth()

pairs(data, pch = 19, lower.panel = NULL)

my_cols <- c("#00AFBB", "#E7B800", "#FC4E07")  

# Customize upper panel
upper.panel<-function(x, y){
  points(x,y, pch=19, col = my_cols[data$orientation])
  r <- round(cor(x, y), digits=2)
  txt <- paste0("R = ", r)
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  text(0.5, 0.9, txt)
}
pairs(data[,c(1,2,9,10)], lower.panel = NULL, 
      upper.panel = upper.panel)

rm(my_cols, upper.panel)

############################
# Data Management
############################
#set seed to 1
set.seed(1, sample.kind = "Rounding")

# Droping cooling_load - it is correlated. Association is not causation!
data$cooling_load <- NULL

# Split energydat into test and train set. evaluation set will be 10% of energydat data set
index <- createDataPartition(data$heating_load, times = 1, p = 0.1, list = FALSE)
train <- data[-index,]
test <- data[index,]

# removes files and table that are no longer needed
rm(data, index)


# define function for computing RMSE for vectors of rating and corresponding predictors
RMSE <- function(true_load, predicted_load){sqrt(mean((true_load - predicted_load)^2))}

# https://en.wikipedia.org/wiki/Root-mean-square_deviation

############################
# Model 1: Using the average
############################
# compute the average of all the load
heat_avg <- mean(train$heating_load)

# RMSE obtained by predicting mu for all unknown rating 
guessing_rmse <- RMSE(test$heating_load, heat_avg)

RMSE_results <- data.frame(Model ="Guessing",
                           RMSE = guessing_rmse) 
RMSE_results


############################
# Machine learning
############################

# https://topepo.github.io/caret/available-models.html

# We select a set of suitable models and store them into the "models" variable.
models <- c("lm", "glm", "knn", "rpart", "Rborist")

# In the following steps, the selected models are fitted first. Afterwards
# predictions are made as part of an sapply statement. Finally, we will
# evaluate the results using different performance metrics.
# Attention: This takes several minutes or an hour depending on your hardware.

# Model Training & Prediction
predictions <- sapply(models, function(model) {
  print(model)
  fit <- train(heating_load ~ ., method = model, data = train)
  prediction <- predict(fit, test)
  data.frame(model = prediction)
})

# Transform predictions into data frame
predictions <- as.data.frame(predictions)

# Inspect predictions
View(predictions)

linear_RSME <- RMSE(test$heating_load, predictions$lm.model)
RMSE_results <- bind_rows(RMSE_results,
                          data.frame(Model ="Linear Regression",  
                                     RMSE = linear_RSME))


logistic_RSME <- RMSE(test$heating_load, predictions$glm.model)
RMSE_results <- bind_rows(RMSE_results,
                          data.frame(Model ="Logistic Regression",  
                                     RMSE = logistic_RSME))

knn_RSME <- RMSE(test$heating_load, predictions$knn.model)
RMSE_results <- bind_rows(RMSE_results,
                          data.frame(Model ="Knn",  
                                     RMSE = knn_RSME))

decisiont_RSME <- RMSE(test$heating_load, predictions$rpart.model)
RMSE_results <- bind_rows(RMSE_results,
                          data.frame(Model ="Decision Tree",  
                                     RMSE = decisiont_RSME))

rforest_RSME <- RMSE(test$heating_load, predictions$Rborist.model)
RMSE_results <- bind_rows(RMSE_results,
                          data.frame(Model ="Random Forest",  
                                     RMSE = rforest_RSME))


RMSE_results #print RMSE table

view(test %>% mutate(prediction_ln = predictions$lm.model))
view(test %>% mutate(prediction_rf = predictions$Rborist.model))

# comparison
dt <- test %>% mutate(prediction_rf = predictions$Rborist.model)
dt %>% ggplot(aes(heating_load, prediction_rf)) +
  geom_point()

# Decision tree x random forest
fit <- train(heating_load ~ ., method = "rpart", data = train)
plot(fit$finalModel, margin = 0.1)
text(fit$finalModel, cex = 0.75)

view(test %>% mutate(prediction_rpart = predictions$rpart.model))

# https://rafalab.github.io/dsbook/examples-of-algorithms.html#random-forests

# see the element size
fit <- train(heating_load ~ ., method = "Rborist", data = train)
