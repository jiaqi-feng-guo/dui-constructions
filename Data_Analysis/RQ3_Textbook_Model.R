# This is the final version of R script that I have worked on since 28th March 2023

# Load required packages
library(glmmTMB)
library(tidyverse)
library(lme4)
library(mice)
library(r2glmm)
library(Matrix)
library(ggeffects)
library(lmtest)
library(broom.mixed)
library(dotwhisker)
library(lattice)
library(ggplot2)
library(sjPlot)
library(usethis)
library(emmeans)
library(devtools)
library(MASS)
library(patchwork)
library(flexplot)
library (margins)
library(effects)
# Install and load broom.mixed

# Note to examiners:
# Please download the 'RQ3_Textbook_Model.csv' file from the GitHub repository
# (https://github.com/jiaqi-feng-guo/dui-constructions/blob/main/Input_Data/RQ3_Textbook_Model.csv)
# and place it in the same directory as this R script. If you choose to place it in a different directory,
# replace "RQ3_Textbook_Model.csv" in the read_csv function with the correct path to the file.

##read data
data <- read.csv("RQ3_Textbook_Model.csv", header = TRUE)


##clean data
data <- data %>% unique()
data <- data %>% mutate(Functions = trimws(Functions))
data <- data %>% mutate(Semantics = trimws(Semantics))
data <- data %>% mutate(Verb = trimws(Verb))
data <- data %>% mutate(Reliability = trimws(Reliability))
data <- data %>% mutate(Accessibility = trimws(Accessibility))

##clean data
data <- data %>% unique()
data <- data %>% mutate(Functions = trimws(Functions))
data <- data %>% mutate(Semantics = trimws(Semantics))
data <- data %>% mutate(Verb = trimws(Verb))
data <- data %>% mutate(Reliability = trimws(Reliability))
data <- data %>% mutate(Accessibility = trimws(Accessibility))

# Keep the categorical variables as factors or ordered factors
data$Functions <- factor(data$Functions,
                         levels = c("Theme", "Concerned", "Stimulus", "Target", "Scope"))

data$Semantics <- factor(data$Semantics,
                         levels = c("Functional", "Psych", "Attribute", "Social", "Communication", "Physical", "Manner"),
                         labels = c("Func", "Psych", "Attr", "Soc", "Comm", "Phys", "Man"))

data$Reliability <- factor(data$Reliability,
                           levels = c("Yes", "No"))

data$Contingency <- factor(data$Contingency,
                           levels = c("Attracted", "Repelled"))

data$Accessibility <- as.factor(data$Accessibility)


md.pattern(data)



# Choose a suitable statistical test

null_model <- glmer.nb(Learner ~ (1 | Verb), data = data)
tab_model(null_model)  

# Model 2: glm possion 
glm_poisson <- glm(Learner ~ Textbook + Accessibility + Functions + Semantics + Reliability + Contingency, data = data, family = poisson)
summary(glm_poisson)
drop1(glm_poisson, test = "Chisq")
tab_model(glm_poisson)  


# Model 1: Negative Binomial Regression
glmrnb_model <- glmer.nb(Learner ~ Textbook + Accessibility + Functions + Semantics + Reliability + Contingency + (1 | Verb), data = data)
tab_model(glmrnb_model)  

# Model 2: Gaussian distribution
glm_gaussian_model <- glmer(Learner ~ Textbook + Accessibility + Functions + Semantics + Reliability + Contingency + (1 | Verb), data = data, family = gaussian)
tab_model(glm_gaussian_model )  


# Model 3: Zero-Inflated Negative Binomial (ZINB) models
zinb_model <- glmmTMB(Learner ~ Textbook + Accessibility + Functions + Semantics + Reliability + Contingency + (1 | Verb), data = data, family = list(family = "nbinom2", link = "log"), ziformula = ~1)
tab_model(zinb_model)  


# Model4: Zero-Inflated Poisson (ZIP) models
zip_model <- glmmTMB(Learner ~ Textbook + Accessibility + Functions + Semantics + Reliability + Contingency + (1 | Verb), data = data, family = "poisson", ziformula = ~1)
tab_model(zip_model)  


# glmmTMB Poisson model
glmmTMB_poisson <- glmmTMB(Learner ~ Textbook + Accessibility + Functions + Semantics + Reliability + Contingency + (1 | Verb), data = data, family = poisson)
summary(glmmTMB_poisson)
drop1(glmmTMB_poisson, test = "Chisq")
tab_model(glmmTMB_poisson)   # 0.422 / 0.774


# Calculate AIC and BIC  values for each model
AIC(null_model, glm_poisson ,glmrnb_model, glm_gaussian_model,zinb_model, zip_model,glmmTMB_poisson)
BIC(null_model, glm_poisson, glmrnb_model, glm_gaussian_model,zinb_model, zip_model,glmmTMB_poisson)




# -------------------Choose Model ------------------------ #

# Choose model
model <- glmmTMB_poisson

# calculate R-squared for model
tab_model(model) 



# -------- main effect plots -----

effect <- allEffects(model)

# Create the plot
p <- plot(effect, as.table = TRUE, multiline = TRUE, rug = FALSE, type = "response", ylab = "L2 Output", xlab = NULL)

# Modify the plot using ggplot2 functions
p <- p +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1, size = 12)) +
  facet_wrap(~variable, nrow = 2, scales = "free_x", labeller = label_parsed)

# Print the plot
print(p)

# Save the plot
ggsave("my_plot_textbook.png", plot = p, width = 10, height = 6, dpi = 300)

# -------------------Predictions and Visualization1 ------------------------ 

# Filter out the problematic Textbook value
filtered_data <- data[data$Textbook != 385.55, ]

# Refit the model using filtered data
glmm.model15_filtered <- glmer.nb(Learner ~ Textbook * Functions 
                                  + Textbook * Semantics
                                  + Accessibility
                                  + Reliability + (1 | Verb),
                                  data = filtered_data, control = glmerControl(optimizer = "Nelder_Mead"))


# Print predictions using ggpredict() and the filtered model
textbook_functions_pred <- ggpredict(glmm.model15_filtered, c("Textbook", "Functions"))
cat("\nPredictions for Textbook & Functions interaction:\n")
print(textbook_functions_pred)

textbook_Semantics_pred <- ggpredict(glmm.model15_filtered, c("Textbook", "Semantics"))
cat("\nPredictions for Textbook * Semantics interaction:\n")
print(textbook_Semantics_pred)

accessibility_pred <- ggpredict(glmm.model15_filtered, c("Accessibility"))
cat("\nPredictions for Accessibility:\n")
print(accessibility_pred)

reliability_pred <- ggpredict(glmm.model15_filtered, c("Reliability"))
cat("\nPredictions for Reliability:\n")
print(reliability_pred)

textbook_pred <- ggpredict(glmm.model15_filtered, c("Textbook"))
cat("\nPredictions for Textbook:\n")
print(textbook_pred)

#---------------------- visualization --

# Create plots
plot_textbook_functions <- plot(textbook_functions_pred) +
  theme_minimal() +
  ggtitle("Figure 8-6: Effects of Textbook & Functions Interaction on Learner Output") +
  xlab("Textbook Frequency") +
  ylab("Predicted Learner Output") +
  labs(color = "Functions")


# Create plot
plot_textbook_Semantics <- plot(textbook_Semantics_pred) +
  theme_minimal() +
  ggtitle("Figure 8-7: Effects of Textbook & Semantics Interaction on Learner Output") +
  xlab("Textbook") +
  ylab("Predicted Learner") +
  ylim(0, 100)  # Adjust y-axis limits



# Create plots
plot_accessibility <- plot(accessibility_pred) +
  theme_minimal() +
  ggtitle("Accessibility") +
  xlab("Accessibility Scales") +
  ylab("Predicted Learner Output")

plot_reliability <- plot(reliability_pred) +
  theme_minimal() +
  ggtitle("Reliability") +
  xlab("Reliability") +
  ylab("Predicted Learner Output")


# Create plot
plot_textbook <- plot(textbook_pred) +
  theme_minimal() +
  ggtitle("Figure 8-4: Textbook Influence on Learner Output") +
  xlab("Textbook Frequency") +
  ylab("Predicted Learner Outout")


# Display plots
plot_accessibility
plot_reliability
plot_textbook
plot_textbook_functions
plot_textbook_Semantics


# save plots

ggsave("Textbook.png", plot_textbook, width = 6, height = 4, dpi = 300)
ggsave("Accessibility.png", plot_accessibility, width = 6, height = 4, dpi = 300)
ggsave("Textbook-Functions.png", plot_textbook_functions, width = 6, height = 4, dpi = 300)
ggsave("Reliability-ggpred2.png", reliability_plot, width = 6, height = 4, dpi = 300)
ggsave("Textbook-Semantics.png", plot_textbook_Semantics, width = 6, height = 4, dpi = 300)



##--------------- invidual prediction ======

##--------------- invidual prediction ======

# Extract p-values from the model summary
model_summary <- summary(glmmTMB_poisson)
p_values <- coef(model_summary)$cond[, "Pr(>|z|)"]


# Display the summary of the model
model_summary <- summary(model)
print(model_summary)


# Print p-values for each predictor
cat("P-value for Textbook:", p_values["Textbook"], "\n")
cat("P-value for Accessibility1:", p_values["Accessibility1"], "\n")
cat("P-value for Accessibility2:", p_values["Accessibility2"], "\n")
cat("P-value for Accessibility3:", p_values["Accessibility3"], "\n")
cat("P-value for Accessibility4:", p_values["Accessibility4"], "\n")
cat("P-value for Accessibility5:", p_values["Accessibility5"], "\n")
cat("P-value for Accessibility6:", p_values["Accessibility6"], "\n")
cat("P-value for FunctionsConcerned:", p_values["FunctionsConcerned"], "\n")
cat("P-value for FunctionsStimulus:", p_values["FunctionsStimulus"], "\n")
cat("P-value for FunctionsTarget:", p_values["FunctionsTarget"], "\n")
cat("P-value for FunctionsScope:", p_values["FunctionsScope"], "\n")
cat("P-value for FunctionsTheme:", p_values["FunctionsTheme"], "\n")
cat("P-value for SemanticsPsych:", p_values["SemanticsPsych"], "\n")
cat("P-value for SemanticsAttr:", p_values["SemanticsAttr"], "\n")
cat("P-value for SemanticsSoc:", p_values["SemanticsSoc"], "\n")
cat("P-value for SemanticsComm:", p_values["SemanticsComm"], "\n")
cat("P-value for SemanticsPhys:", p_values["SemanticsPhys"], "\n")
cat("P-value for SemanticsMan:", p_values["SemanticsMan"], "\n")
cat("P-value for SemanticsFunc:", p_values["SemanticsFunc"], "\n")
cat("P-value for ReliabilityNo:", p_values["ReliabilityNo"], "\n")
cat("P-value for ReliabilityYes:", p_values["ReliabilityYes"], "\n")
cat("P-value for ContingencyRepelled:", p_values["ContingencyRepelled"], "\n")
cat("P-value for ContingencyAttracted:", p_values["ContingencyAttracted"], "\n")


summary(glmmTMB_poisson)

# Print predictions using ggpredict()
Textbook_pred <- ggpredict(model, c("Textbook"))
cat("\nPredictions for Textbook:\n")
print(Textbook_pred)

functions_pred <- ggpredict(model, c("Functions"))
cat("\nPredictions for Functions:\n")
print(functions_pred)

semantics_pred <- ggpredict(model, c("Semantics"))
cat("\nPredictions for Semantics:\n")
print(semantics_pred)

accessibility_pred <- ggpredict(model, c("Accessibility"))
cat("\nPredictions for Accessibility:\n")
print(accessibility_pred)

reliability_pred <- ggpredict(model, c("Reliability"))
cat("\nPredictions for Reliability:\n")
print(reliability_pred)


contingency_pred <- ggpredict(model, c("Contingency"))
cat("\nPredictions for Contingency:\n")
print(contingency_pred)




# --------------------- visualise the whole dataset  ---------#

# Calculate main effect and crete a ggplot2 object
effect <- allEffects(model)
p <- plot(effect, as.table = TRUE, multiline = TRUE, rug = FALSE, type = "response", ylab = "Effect")
p <- p + theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1, size = 12))

citation("emmeans")

# Obtain the emmeans
emm <- emmeans(model, ~ Textbook + Functions + Reliability + Accessibility )


# Convert the emmeans object to a data frame
emm_df <- as.data.frame(emm)

# Create a new column to represent the rounded Textbook values
emm_df$Textbook_Rounded <- round(emm_df$Textbook, 1)

# Create a new column to represent the combination of factor levels with rounded Textbook values
emm_df$Factor <- paste(emm_df$Textbook_Rounded,emm_df$Functions, emm_df$Reliability,emm_df$Accessibility, sep = "-")

# Update the ggplot code with custom colors, points, and theme
ggplot(emm_df, aes(x = Factor, y = emmean)) +
  geom_point(size = 3, shape = 21, fill = "dodgerblue", color = "black") +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.2, color = "dodgerblue") +
  theme_minimal() +
  xlab("Factors: Textbook-Functions-Reliability-Accessibility") +
  ylab("Learner Verb Output") +
  ggtitle("Figure 8-13: Textbook Model Interaction Effects") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 30),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 0.5, size = 12),
    axis.text.y = element_text(size = 10),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "grey80"),
    panel.grid.major.y = element_line(color = "grey80")
  )

# Print the emm_df data frame
print(emm_df)

# Export the emm_df data frame to a CSV file
write.csv(emm_df, "GLMM_Textbook_emmeans_data.csv", row.names = FALSE)




