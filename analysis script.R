install.packages("ggplot2")

library(ggplot2)


earthquake <- read.table(file = "earthquake.txt", header = TRUE, sep = "", dec = ".")


library(ggplot2)
library(RColorBrewer)

# Assuming 'earthquake' is your dataframe and is already loaded
ggplot(earthquake, aes(x=body, y=surface, color=type, shape=type)) +
  geom_point(size=4, alpha=0.8) + # Slightly larger and semi-transparent points
  theme_light(base_size = 14) +   # A lighter theme with adjusted base font size
  labs(title = "Body-wave Magnitude vs. Surface-wave Magnitude",
       subtitle = "Comparing earthquake types",
       x = "Body-wave Magnitude (mb)",
       y = "Surface-wave Magnitude (Ms)",
       color = "Type",
       shape = "Type") +
  scale_color_brewer(palette = "Set1") + # A more accessible color palette
  theme(legend.position = "right") # Adjust legend position



########################part B first one ########################################

# Load necessary libraries
library(randomForest)
library(ggplot2)
library(caret)


data <- read.table(file = "earthquake.txt", header = TRUE, sep = "", dec = ".")
# 2. Preprocess the data (ensure correct data types)
data$type <- as.factor(data$type)

# 3. For hyperparameter tuning, normally you'd create a train-test split or use cross-validation.
# Since we're evaluating with leave-one-out cross-validation later, we'll skip splitting here.

# 4. Train the Random Forest model
# Perform a grid search for hyperparameter tuning (example: mtry)
# For simplicity, we'll skip the tuning part and use default parameters here
set.seed(123) # For reproducibility
rf_model <- randomForest(type ~ body + surface, data=data)

# 5. Model visualization
# Predict on a grid to visualize decision boundaries
plot_data <- with(data, expand.grid(body=seq(min(body), max(body), length.out=100),
                                    surface=seq(min(surface), max(surface), length.out=100)))
plot_data$type <- predict(rf_model, newdata=plot_data, type="class")

ggplot(data, aes(x=body, y=surface, color=type)) +
  geom_point() +
  geom_point(data=plot_data, aes(x=body, y=surface, color=type), alpha=0.5) +
  labs(title="Random Forest Decision Boundary with Data Points")

# 6. Model evaluation with leave-one-out cross-validation
loo_control <- trainControl(method="LOOCV")
loo_results <- train(type ~ body + surface, data=data, method="rf", trControl=loo_control)

# Print out the results
print(loo_results$results)

# Comment on results and graphs


################## part B 2nd one###########################################

# Load necessary libraries
library(e1071)
library(ggplot2)
library(caret)



# 2. Preprocess the data (ensure correct data types)
data$type <- as.factor(data$type)

# 3. Model tuning: Use cross-validation to find the best hyperparameters
set.seed(123) # For reproducibility
tune_result <- tune(svm, type ~ body + surface, data=data,
                    kernel="radial", ranges=list(cost=c(0.1, 1, 10, 100), gamma=c(0.1, 1, 10)))

# View best parameters
print(tune_result$best.parameters)

# 4. Train the SVM model with the best parameters
svm_model <- svm(type ~ body + surface, data=data, method="C-classification",
                 kernel="radial", cost=tune_result$best.parameters$cost,
                 gamma=tune_result$best.parameters$gamma)

# 5. Model visualization
# Predict on a grid to visualize decision boundaries
plot_data <- with(data, expand.grid(body=seq(min(body), max(body), length.out=100),
                                    surface=seq(min(surface), max(surface), length.out=100)))

plot_data$type <- predict(svm_model, newdata=plot_data)

ggplot(data, aes(x=body, y=surface, color=type)) +
  geom_point() +
  geom_point(data=plot_data, aes(x=body, y=surface, color=type), alpha=0.1) +
  labs(title="SVM Decision Boundary with Data Points")

# 6. Model evaluation with leave-one-out cross-validation
loo_control <- trainControl(method="LOOCV")
loo_results <- train(type ~ body + surface, data=data, method="svmRadial",
                     trControl=loo_control, tuneLength=5)

# Print out the results
print(loo_results$results)

# Comment on results and graphs

