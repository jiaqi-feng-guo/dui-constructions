library(mice)
library(r2glmm)
library(Matrix)
library(lme4)
library(ggeffects)
library(lmtest)
library(glmmTMB)
library(broom.mixed)
library(dotwhisker)
library(lattice)
library(tidyverse)
library(ggplot2)
library(sjPlot)
library(usethis)
library(emmeans)
library(devtools)
library(MASS)
library(patchwork)
library (margins)
library(effects)
library(vcd)
library(pscl)

##read data
data <- read.csv("/Users/jiaqiguo/Desktop/Data_Analysis11/R/RQ3_NS_NNS/NS_NNS.csv", header = TRUE)



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

# check for missing data 
md.pattern(data)


data$Learner <- round(data$Learner)
data$Learner <- as.integer(data$Learner)

# ------------------- start the analysis -------------------------


# Check for association between Function and Semantics variables using Cramér's V
contingency_table <- table(data$Functions, data$Semantics)
cramers_v <- assocstats(contingency_table)$cramer
print(cramers_v)  # 0.6870797


## Choose a best-fitness model
# null model
null_model <- glmer.nb(Learner ~ (1 | Verb), data = data)
tab_model(null_model)  # 0.000 / 0.572

# GLM Poisson model
glm_poisson <- glm(Learner ~ Native + Accessibility + Functions + Semantics + Reliability + Contingency, data = data, family = poisson)
summary(glm_poisson)
drop1(glm_poisson, test = "Chisq")
tab_model(glm_poisson)  


# glmmTMB Poisson model
glmmTMB_poisson <- glmmTMB(Learner ~ Native + Accessibility + Functions + Semantics + Reliability + Contingency + (1 | Verb), data = data, family = poisson)
summary(glmmTMB_poisson)
drop1(glmmTMB_poisson, test = "Chisq")
tab_model(glmmTMB_poisson)   # 0.494 / 0.808

# Model 2: Gaussian distribution
glm_gaussian_model <- glmer(Learner ~ Native + Accessibility + Functions + Semantics + Reliability + Contingency + (1 | Verb), data = data, family = gaussian)
tab_model(glm_gaussian_model )  # 0.474 / 0.504


# Zero-Inflated Negative Binomial (ZINB) model
zinb_model <- glmmTMB(Learner ~ Native + Accessibility + Functions + Semantics + Reliability + Contingency + (1 | Verb),
                      zi = ~1|Verb,
                      family = nbinom2(link = "log"), data = data)

summary(zinb_model)
tab_model(zinb_model)  # 0.226 / 0.331


# Zero-Inflated Poisson (ZIP) model
library(pscl)
zip_model <- glmmTMB(Learner ~ Native + Accessibility + Functions + Semantics + Reliability + Contingency  + (1 | Verb), family = poisson(link = "log"), ziformula = ~1, data = data)
tab_model(zip_model) # 0.326 / 0.546

# GLM Negative Binomial model
glmrnb_model <- glmer.nb(Learner ~ Native + Accessibility + Functions + Semantics + Reliability + Contingency + (1 | Verb), data = data)
summary(glmrnb_model)
drop1(glmrnb_model, test = "Chisq")
tab_model(glmrnb_model)  # 0.493 / 0.805

# Model comparison
AIC(null_model,glm_poisson,glmmTMB_poisson,zinb_model,glmrnb_model,glm_gaussian_model)
BIC(null_model,glm_poisson,glmmTMB_poisson,zinb_model,glmrnb_model,glm_gaussian_model)


# -------------------Choose Model ------------------------ #

# Choose model
model <- glmmTMB_poisson

# calculate R-squared for model
tab_model(model)




# -------- main effect plots -----
# Calculate main effect and create a ggplot2 object
# Compute the effects
effect <- allEffects(glmmTMB_poisson)

# Create the plot
p <- plot(effect, as.table = TRUE, multiline = TRUE, rug = FALSE, type = "response", ylab = "L2 Output", xlab = NULL)

# Modify the plot using ggplot2 functions
p <- p +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1, size = 12)) +
  facet_wrap(~variable, nrow = 2, scales = "free_x", labeller = label_parsed)

# Print the plot
print(p)

# Save the plot
ggsave("my_plot_native.png", plot = p, width = 10, height = 6, dpi = 300)




# Visualisationx
plot_model <- function(model, var.list, save.path, ci=TRUE){
  pr <- ggpredict(model, var.list)
  pg <- plot(pr, ci = ci) + 
    labs(title = "Figure 8-9:Prediction of Learner Output") +
    theme(
      panel.background = element_rect(fill='white'),
      plot.background = element_rect(fill='white'),
    )
  ggsave(save.path, pg, width = 6, height = 4, dpi = 300)
}

# Generate visualizations
plot_model(model, c("Accessibility"), 'access.png')
plot_model(model, c("Accessibility", "Reliability"), 'access_relia_11.png')
plot_model(model, c("Native", "Functions", "Accessibility"), 'Native_Function_Access_11.png', ci = FALSE)
plot_model(model, c("Native", "Functions", "Reliability"), 'Native_Function_reliability_11.png', ci = FALSE)
plot_model(model, c("Native", "Functions", "Contingency"), 'Native_Function_contingency_11.png', ci = FALSE)
plot_model(model, c("Native", "Accessibility", "Reliability"), 'Native_Reliability_access_11.png', ci = FALSE)
plot_model(model, c("Functions", "Accessibility"), 'Functions_access_11.png')
plot_model(model, c("Functions", "Reliability"), 'fun_re_11.png')
plot_model(model, c("Reliability", "Native", "Functions"), 'rel_Native_Fun_11.png')
plot_model(model, c("Native", "Functions","Contingency" ), 'con_Native_Fun_11.png')



# Visualization function
# Load the necessary package
library(grid)


# Visualization function
plot_model <- function(model, var.list, ci=TRUE){
  pr <- ggpredict(model, var.list)
  pg <- plot(pr, ci = ci) + 
    labs(title = "Figure 8-9:                                 ") +
    theme(
      panel.background = element_rect(fill='white'),
      plot.background = element_rect(fill='white'),
      legend.text = element_text(size = 12), # Adjust the size as needed
      legend.key.size = unit(1.5, "lines"), # Adjust the size as needed
      legend.spacing.y = unit(0.5, "cm") # Adjust the size as needed
    ) +
    guides(color = guide_legend(override.aes = list(size = 2))) # Adjust the size as needed
  return(pg)
}



# Generate ggplot objects
access <- plot_model(model, c("Accessibility"))
access_relia <- plot_model(model, c("Accessibility", "Reliability"))
Native_Function_Access <- plot_model(model, c("Native", "Functions", "Accessibility"), ci = FALSE)
Native_Function_reliability <- plot_model(model, c("Native", "Functions", "Reliability"), ci = FALSE)
Native_Reliability_access <- plot_model(model, c("Native", "Accessibility", "Reliability"), ci = FALSE)
Functions_access <- plot_model(model, c("Functions", "Accessibility"))
fun_re <- plot_model(model, c("Functions", "Reliability"))
rel_Native_Fun <- plot_model(model, c("Reliability", "Native", "Functions"))
con_Native_Fun <- plot_model(model, c("Native","Functions","Contingency"))




# Load the necessary package
library(gridExtra)

# Display the plots in a grid layout
grid.arrange(access, access_relia, Native_Function_Access, Native_Function_reliability,
             Native_Reliability_access, con_Native_Fun,
             nrow = 2, ncol = 4)



# Display the 'Native_Function_Access' plot
print(Native_Function_Access)

# Print the results
pr_native_func_access <- ggpredict(model, c("Native", "Functions", "Accessibility"))
summary(pr_native_func_access)


# Display the 'Native_Function_reliability' plot
print(Native_Function_reliability)

# Print the results
pr_native_func_relia <- ggpredict(model, c("Native", "Functions", "Reliability"))
summary(pr_native_func_relia)


# Display the 'Native_Accessibility_reliability' plot
print(Native_Reliability_access)

# Print the results
pr_native_access_relia <- ggpredict(model, c("Native", "Accessibility","Reliability"))
summary(pr_native_access_relia)


# Display the 'Native_Accessibility_reliability' plot
print(con_Native_Fun)

# Print the results
pr_native_con_Fun <- ggpredict(model, c("Native", "Contingency","Functions"))
summary(pr_native_con_Fun)


# Display the 'Native_Accessibility_reliability' plot
print(con_Native_Fun)

# Print the results
pr_con_native_Fun <- ggpredict(model, c("Native","Functions","Contingency"))
summary(pr_con_native_Fun)

# Save the ggplot objects to files
ggsave("access.png", access, width = 6, height = 4, dpi = 300)
ggsave("access_relia.png", access_relia, width = 6, height = 4, dpi = 300)
ggsave("Native_Function_Access.png", Native_Function_Access, width = 6, height = 4, dpi = 300)
ggsave("Native_Function_reliability.png", Native_Function_reliability, width = 6, height = 4, dpi = 300)
ggsave("Native_Reliability_access.png", Native_Reliability_access, width = 6, height = 4, dpi = 300)
ggsave("Functions_access.png", Functions_access, width = 6, height = 4, dpi = 300)
ggsave("fun_re.png", fun_re, width = 6, height = 4, dpi = 300)
ggsave("rel_Native_Fun.png", rel_Native_Fun, width = 6, height = 4, dpi = 300)
ggsave("native_con_Fun.png", con_Native_Fun, width = 6, height = 4, dpi = 300)


# --------------------- visualise the whole dataset 

# Calculate main effect and crete a ggplot2 object
effect <- allEffects(model)
p <- plot(effect, as.table = TRUE, multiline = TRUE, rug = FALSE, type = "response", ylab = "Effect")
p <- p + theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1, size = 12))

citation("emmeans")

# Obtain the emmeans
emm <- emmeans(model, ~ Native  + Functions + Reliability + Accessibility )


# Convert the emmeans object to a data frame
emm_df <- as.data.frame(emm)

# Create a new column to represent the rounded Native values
emm_df$Native_Rounded <- round(emm_df$Native, 1)

# Create a new column to represent the combination of factor levels with rounded Native values
emm_df$Factor <- paste(emm_df$Native_Rounded,emm_df$Functions, emm_df$Reliability,emm_df$Accessibility, sep = "-")

# Update the ggplot code with custom colors, points, and theme
ggplot(emm_df, aes(x = Factor, y = emmean)) +
  geom_point(size = 3, shape = 21, fill = "dodgerblue", color = "black") +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.2, color = "dodgerblue") +
  theme_minimal() +
  xlab("Factors: Native-Functions-Reliability-Accessibility") +
  ylab("Learner Verb Output") +
  ggtitle("Figure 8-3: EMMeans Plot for Interaction Effects") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 20),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "grey80"),
    panel.grid.major.y = element_line(color = "grey80")
  )

# Print the emm_df data frame
print(emm_df)

# Export the emm_df data frame to a CSV file
write.csv(emm_df, "GLMM_NATIVE_emmeans_data.csv", row.names = FALSE)

# ---------------------------- Individual Predictor ---------------------

# Create a prediction dataset
data.pred <- expand.grid(Native = seq(0, 1, by = 1),
                         Accessibility = levels(data$Accessibility),
                         Functions = levels(data$Functions),
                         Semantics = levels(data$Semantics),
                         Contingency = levels(data$Contingency),
                         Reliability = levels(data$Reliability))


# Find the most frequent Verb in the original dataset
most_frequent_verb <- names(sort(table(data$Verb), decreasing = TRUE))[1]

# Add the most frequent Verb to the prediction dataset
data.pred$Verb <- most_frequent_verb

# Make predictions
data.pred$predicted_Learner <- predict(model, newdata = data.pred, type = "response")

# Save the predictions to a CSV file
write.csv(data.pred, file = "predictions.csv", row.names = FALSE)


## individual predictor visualization

plot_model <- function(model, var.list, ci=TRUE){
  pr <- ggpredict(model, var.list)
  pg <- plot(pr, ci = ci) + 
    labs(title = "Prediction of Learner") +
    theme(
      panel.background = element_rect(fill='white'),
      plot.background = element_rect(fill='white'),
      title = element_text(size=16, face="bold"),
      axis.title = element_text(size=14),
      axis.text = element_text(size=12)
    ) +
    geom_line(aes(color = group), size = 1) +
    geom_point(aes(color = group), size = 2) +
    scale_color_manual(values = c("blue", "red", "green", "purple", "orange"))
  
  return(pg)
}

native_plot <- plot_model(model, c("Native"))
print(native_plot)

functions_plot <- plot_model(model, c("Functions"))
print(functions_plot)

accessibility_plot <- plot_model(model, c("Accessibility"))
print(accessibility_plot)

reliability_plot <- plot_model(model, c("Reliability"))
print(reliability_plot)



# save plots
print(native_plot)
ggsave("Native.png", native_plot, width = 6, height = 4, dpi = 300)
ggsave("Accessibility.png", accessibility_plot, width = 6, height = 4, dpi = 300)
ggsave("Functions.png", functions_plot, width = 6, height = 4, dpi = 300)
ggsave("Reliability.png", reliability_plot, width = 6, height = 4, dpi = 300)


##--------------- invidual prediction ======

# Extract p-values from the model summary
model_summary <- summary(glmmTMB_poisson)
p_values <- coef(model_summary)$cond[, "Pr(>|z|)"]


# Display the summary of the model
model_summary <- summary(model)
print(model_summary)


# Print p-values for each predictor
cat("P-value for Native:", p_values["Native"], "\n")
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
native_pred <- ggpredict(model, c("Native"))
cat("\nPredictions for Native:\n")
print(native_pred)

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




