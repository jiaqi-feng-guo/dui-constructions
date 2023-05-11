library(randomForest)
library(ranger)
library(ggplot2)
library(caret)
library(ranger)
library(e1071)
library(pdp)
library(randomForestExplainer)
library(dplyr)
library(readr)     
library(partykit)
library(vcd)
# ------ prepare data -------

setwd("/Users/jiaqiguo/Desktop/Data_Analysis11/R/")

data <- read_csv("RQ3_Tree.csv")

##clean data
data <- data %>% mutate(Functions = trimws(Functions))
data <- data %>% mutate(Semantics = trimws(Semantics))
data <- data %>% mutate(Verb = trimws(Verb))
data <- data %>% mutate(Reliability = trimws(Reliability))
data <- data %>% mutate(Accessibility = trimws(Accessibility))
data <- data %>% mutate(Native_COLL = trimws(Native_COLL))
data <- data %>% mutate(Textbook_COLL = trimws(Textbook_COLL))
data <- data %>% mutate(Mode = trimws(Mode))
data <- data %>% mutate(L1 = trimws(L1))


# Keep the categorical variables as factors or ordered factors
data$Functions <- factor(data$Functions,
                         levels = c("Theme", "Concerned", "Recipient", "Stimulus", "Target", "Scope"))

data$Semantics <- factor(data$Semantics,
                         levels = c("Functional", "Psych", "Attribute", "Social", "Communication", "Physical", "Manner"),
                         labels = c("Func", "Psych", "Attr", "Soc", "Comm", "Phys", "Man"))

data$Reliability <- factor(data$Reliability,
                           levels = c("Yes", "No"))

data$Accessibility <- as.factor(data$Accessibility)

# Convert the 'Proficiency' variable to a factor
data$Proficiency <- factor(data$Proficiency, levels = c("NNS1", "NNS2", "NNS3"))


data$Contingency<- factor(data$Native_COLL,
                          levels = c("Attracted", "Repelled"))

data$L1 <- as.factor(data$L1)


data$Mode<- factor(data$Mode,
                   levels = c("W", "S"))

data$Verb <- as.factor(data$Verb)

levels(data$L1)



# ------------------- check linearity -------------------------
# Check for association between Function and Semantics variables using Cramér's V
contingency_table <- table(data$Functions, data$Semantics)
cramers_v <- assocstats(contingency_table)$cramer
print(cramers_v)
# [1] 0.7224353

# Calculate association measures, including Goodman and Kruskal's Tau
association_stats <- assocstats(table(data$Functions, data$Semantics))

# Print association measures
print(association_stats)
# [2] Contingency Coeff.:  0.85  Cramer's V: 0.722 


#_____ ranger model ------------- 

# Split the data into training and testing sets
set.seed(123) # Set a seed for reproducibility
train_index <- sample(1:nrow(data), 0.8 * nrow(data))
train_data <- data[train_index, ]
test_data <- data[-train_index, ]

# Train the ranger model with the Verb column as the target variable and compute variable importance
rf_model <- ranger(Verb ~ Mode + Proficiency + L1 + Contingency + Native + Textbook + Reliability + Accessibility + Functions + Semantics,
                   data = train_data, importance = 'impurity')

# Show variable importance
importance(rf_model)

predictions <- predict(rf_model, data = test_data)$predictions

# Calculate accuracy by comparing the predicted Verb with the actual Verb in the test set
accuracy <- sum(predictions == test_data$Verb) / length(predictions)
print(accuracy)




# -------validation ----------------

# Set the number of folds
num_folds <- 5

# Set the number of repeated runs
num_repeats <- 3

# Set up the cross-validation strategy
cv_strategy <- trainControl(method = "repeatedcv",
                            number = num_folds,
                            repeats = num_repeats,
                            savePredictions = "final", # Save predictions for later use
                            classProbs = TRUE) # Calculate class probabilities

# Train the random forest model with k-fold cross-validation
model_cv <- train(Verb ~ Mode + Proficiency + L1 + Native_COLL + Textbook_COLL + Native + Textbook + Reliability + Accessibility + Functions + Semantics,
                  data = data,
                  method = "ranger",
                  trControl = cv_strategy,
                  importance = "impurity",
                  tuneLength = 1) # Use the default hyperparameters

predictions <- model_cv$pred$pred
actual_values <- model_cv$pred$obs

cm <- confusionMatrix(predictions, actual_values)

# Print evaluation metrics
print(cm$overall)

# Calculate and print mean accuracy
mean_accuracy <- cm$overall["Accuracy"]
print(paste0("Mean accuracy: ", mean_accuracy))




#----- 

set.seed(42) # Set a random seed for reproducibility
trainIndex <- createDataPartition(data$Verb, p = 0.8, list = FALSE)
train_data <- data[trainIndex, ]
test_data <- data[-trainIndex, ]

# Predict using the model_cv without the dependent variable (Verb)
predictions <- predict(model_cv, test_data[,-which(names(test_data) == "Verb")])

# Check the lengths
print(length(predictions))
print(length(test_data$Verb))

# If the lengths match, create the confusion matrix
confusionMatrix <- confusionMatrix(as.factor(predictions), as.factor(test_data$Verb))
print(confusionMatrix)

# Calculate and print the mean values for each evaluation metric
mean_sensitivity <- mean(confusionMatrix$byClass[,"Sensitivity"], na.rm = TRUE)
mean_specificity <- mean(confusionMatrix$byClass[,"Specificity"], na.rm = TRUE)
mean_precision <- mean(confusionMatrix$byClass[,"Pos Pred Value"], na.rm = TRUE)
mean_recall <- mean(confusionMatrix$byClass[,"Recall"], na.rm = TRUE)
mean_F1 <- mean(confusionMatrix$byClass[,"F1"], na.rm = TRUE)

print(paste0("Mean sensitivity: ", mean_sensitivity))
print(paste0("Mean specificity: ", mean_specificity))
print(paste0("Mean precision: ", mean_precision))
print(paste0("Mean recall: ", mean_recall))
print(paste0("Mean F1 score: ", mean_F1))



# ------- interactive effect --- worked

# Drop unused levels in the target variable
train_data$Verb <- droplevels(train_data$Verb)

# Fit a randomForest model
rf_model_rf <- randomForest(Verb ~ Mode + Proficiency + L1 + Contingency + Native + Textbook + Reliability + Accessibility + Functions + Semantics,
                            data = train_data)


# Calculate the interaction importance
interaction_importance <- measure_importance(rf_model_rf)

# Print the interaction importance
print(interaction_importance)



#  ------------ visualisation ------------

# Calculate variable importance
var_importance <- rf_model_rf$importance

# Create a data frame with variable names and their importance
importance_df <- data.frame(Variable = row.names(var_importance),
                            Importance = round(var_importance[, "MeanDecreaseGini"], 2))

# Sort the data frame by importance
importance_df <- importance_df[order(importance_df$Importance, decreasing = TRUE),]

# Create the feature importance plot
importance_plot <- ggplot(importance_df, aes(x = reorder(Variable, Importance), y = Importance, fill = Importance)) +
  geom_bar(stat = "identity") +
  labs(title = "Feature Importance Plot",
       x = "Variables",
       y = "Importance") +
  coord_flip() +
  theme_minimal() +
  scale_fill_gradient(low = "skyblue", high = "steelblue") +
  theme(axis.title.y = element_text(size = 14), # Change the font size for the y-axis title
        axis.text.y = element_text(size = 12))  # Change the font size for the y-axis labels
print(importance_plot)


# Partial Dependence Plots (PDP)
# You may choose the variables you want to examine for interactions
# In this example, we'll use 'Proficiency' and 'L1'
pdp_data <- pdp::partial(rf_model_rf, pred.var = c("Textbook", "Semantics"), train = train_data)


# Plot the PDP
p <- ggplot(pdp_data, aes(x = Textbook, y = Semantics, fill = yhat)) +
  geom_tile() +
  labs(title = "Partial Dependence Plot for Textbook and Semantics Interaction",
       x = "Textbook",
       y = "Semantics",
       fill = "Predicted Probability") +
  theme_minimal()
print(p)


#---- new visualisation 
# Function to create PDP for a given level of Accessibility
create_pdp_plot <- function(access_level) {
  unique_textbook <- unique(train_data$Textbook)
  unique_semantics <- unique(train_data$Semantics)
  
  pdp_data <- pdp::partial(rf_model_rf, pred.var = c("Textbook", "Semantics"),
                           train = train_data, plot = FALSE, chull = FALSE,
                           ice = FALSE, center = TRUE,
                           grid.resolution = length(unique_textbook) * length(unique_semantics),
                           pred.grid = expand.grid(Textbook = unique_textbook,
                                                   Semantics = unique_semantics,
                                                   Accessibility = access_level))
  
  p <- ggplot(pdp_data, aes(x = Textbook, y = Semantics, size = yhat, color = yhat)) +
    geom_point(alpha = 0.6) +
    scale_color_gradient(low = "#1f77b4", high = "#d62728") +
    labs(title = paste0("Partial Dependence Plot for Textbook, Semantics, and Accessibility Interaction (Accessibility = ", access_level, ")"),
         x = "Textbook",
         y = "Semantics",
         size = "Predicted Probability",
         color = "Predicted Probability") +
    theme_bw()
  
  return(p)
}

# Create a list to store the plots
pdp_plots <- list()

# Define the levels of Accessibility to create plots for
access_levels <- unique(train_data$Accessibility)

# Loop through the levels and create a plot for each
for (level in access_levels) {
  pdp_plots[[as.character(level)]] <- create_pdp_plot(level)
}

# Print the plots
for (level in access_levels) {
  print(pdp_plots[[as.character(level)]])
}


#--- six panel visualisation-----

# Function to create PDP data for all levels of Accessibility
create_pdp_data <- function() {
  unique_textbook <- unique(train_data$Textbook)
  unique_semantics <- unique(train_data$Semantics)
  access_levels <- unique(train_data$Accessibility)
  
  pdp_data_list <- list()
  
  for (access_level in access_levels) {
    pdp_data <- pdp::partial(rf_model_rf, pred.var = c("Textbook", "Semantics"),
                             train = train_data, plot = FALSE, chull = FALSE,
                             ice = FALSE, center = TRUE,
                             grid.resolution = length(unique_textbook) * length(unique_semantics),
                             pred.grid = expand.grid(Textbook = unique_textbook,
                                                     Semantics = unique_semantics,
                                                     Accessibility = access_level))
    pdp_data_list[[as.character(access_level)]] <- pdp_data
  }
  
  return(do.call(rbind, pdp_data_list))
}

# Create PDP data for all levels of Accessibility
pdp_data <- create_pdp_data()

# Plot the six-panel PDP
p <- ggplot(pdp_data, aes(x = Textbook, y = Semantics, size = yhat, color = yhat)) +
  geom_point(alpha = 0.6) +
  scale_color_gradient(low = "#1f77b4", high = "#d62728") +
  labs(title = "Partial Dependence Plot for Textbook, Semantics, and Accessibility Interaction",
       x = "Textbook",
       y = "Semantics",
       size = "Predicted Probability",
       color = "Predicted Probability") +
  theme_bw() +
  facet_wrap(~ Accessibility, ncol = 3)

print(p)



# ---- tree map --

install.packages("rpart")
install.packages("rpart.plot")
library(rpart)
library(rpart.plot)

# Fit a single decision tree
single_tree <- rpart(Verb ~ Mode + Proficiency + L1 + Native_COLL + Textbook_COLL + Native + Textbook + Reliability + Accessibility + Functions + Semantics,
                     data = train_data)
# Plot the decision tree
rpart.plot(single_tree)


