library(readxl)
library(dplyr)
library(tidyr)
library(nnet)
library(margins)
library(ggplot2)
library(tidyverse)
library(lme4)
library(nnet)


# Read data
data <- read_excel("/Users/jiaqiguo/Desktop/Data_Analysis11/R/Error/RQ2_Error.xlsx", sheet = "Error")

data$Error <- ifelse(data$Error == "Error", 1, 0)
data$Mode <- as.factor(data$Mode)
data$Proficiency <- as.factor(data$Proficiency)
data$Nationality <- as.factor(data$Nationality)
data$L1 <- as.factor(data$L1)
data$ErrorType <- as.factor(data$ErrorType)


# Create a GLMM with a random intercept for ID
error_model <- glmer(Error ~ Proficiency + (1 | ID), data = data, family = binomial())

# Print model summary
summary(error_model)
tab_model(error_model)


# ------ make prediction

# Load the required packages
library(lme4)
library(emmeans)

# Generate the estimated marginal means for the proficiency levels
emm_proficiency <- emmeans(error_model, ~ Proficiency)

# Back-transform the EMMs to the response scale (probability of error)
emm_proficiency_response <- summary(emm_proficiency, type = "response")

# Display the results
print(emm_proficiency_response)




error_plot <- ggplot(emm_proficiency_response_df, aes(x = Proficiency, y = prob, ymin = asymp.LCL, ymax = asymp.UCL, color = Proficiency)) +
  geom_point(size = 4, shape = 21, fill = "white") +
  geom_errorbar(width = 0.2, size = 1) +
  labs(title = "Figure 7-6: Estimated Marginal Means of Error by Proficiency Level", x = "Proficiency", y = "Probability of Error") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 8, face = "bold"),
    axis.text = element_text(size = 15),
    legend.position = "none"
  ) +
  scale_color_manual(values = c("NNS1" = "steelblue", "NNS2" = "firebrick", "NNS3" = "grey"))

print(error_plot)

# Save the plot as a file
ggsave("error_types_plot.png", plot = error_plot, width = 8, height = 6, dpi = 300)


confint(error_model, method = "Wald")










# ---------- build a full model ------------- 

# Factorize the categorical variables
data$L1 <- as.factor(data$L1)
data$Mode <- as.factor(data$Mode)
data$Functions <- as.factor(data$Functions)
data$Accessibility <- as.factor(data$Accessibility)
data$Semantics <- as.factor(data$Semantics)
data$Reliability <- as.factor(data$Reliability)
data$Native <- as.factor(data$Native)
data$Textbook <- as.factor(data$Textbook)

# Build the mixed-effects logistic regression model
#full_model <- glmer(Error ~ Proficiency + L1 + Mode + Functions + Accessibility + Semantics + Reliability + Native_COLL + Textbook_COLL + Native + Textbook + (1 | ID), data = data, family = binomial)

# Display the model summary
#summary(full_model)


#-------------Error Types ---------------

# Create a frequency table
error_type_proficiency <- table(data$ErrorType, data$Proficiency)

data_filtered <- data[data$ErrorType != "Correct", ]


# dispay error types by proficiency
error_by_proficiency <- table(data_filtered$ErrorType, data_filtered$Proficiency)
print(error_by_proficiency)


multinom_model_filtered <- multinom(formula = ErrorType ~ Proficiency, data = data_filtered)
summary(multinom_model_filtered)


predicted_probabilities <- predict(multinom_model_filtered, type = "probs")



# Generate the estimated marginal means for the proficiency levels by error type
emm_means <- emmeans(multinom_model_filtered, ~ Proficiency | ErrorType)

# Perform pairwise comparisons
pairwise_comparisons <- pairs(emm_means)

# Display the results with p-values and apply Bonferroni adjustment
summary(pairwise_comparisons, type = "response", adjust = "bonferroni")




library(ggplot2)

# Create a data frame with the summary results
summary_data <- data.frame(
  ErrorType = rep(c("Collocation Error", "Insertion", "Omission", "Preposition Misuse", "Word Order"), each = 3),
  Proficiency = rep(c("NNS1", "NNS2", "NNS3"), 5),
  Probability = c(0.0952, 0.3160, 0.3697, 0.0476, 0.0660, 0.1765, 0.2381, 0.0943, 0.0672, 0.5238, 0.4953, 0.3529, 0.0952, 0.0283, 0.0336),
  LowerCI = c(-0.0443, 0.2465, 0.2733, -0.0536, 0.0289, 0.1003, 0.0356, 0.0506, 0.0172, 0.2864, 0.4205, 0.2575, -0.0443, 0.0035, -0.0024),
  UpperCI = c(0.2348, 0.3856, 0.4662, 0.1489, 0.1032, 0.2526, 0.4406, 0.1381, 0.1172, 0.7613, 0.5701, 0.4484, 0.2348, 0.0531, 0.0696)
)

# Create the plot and assign it to a variable
plot <- ggplot(summary_data, aes(x = Proficiency, y = Probability, fill = ErrorType)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = LowerCI, ymax = UpperCI), width = 0.2, position = position_dodge(0.9)) +
  labs(title = "Figure: Marginal Means of Error Types by Proficiency", x = "Proficiency", y = "Probability") +
  theme_minimal() +
  theme(legend.title = element_blank())

# Save the plot as a file
ggsave("error_types_plot.png", plot = plot, width = 8, height = 6, dpi = 300)


