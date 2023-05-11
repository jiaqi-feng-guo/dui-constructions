# Load required libraries
library(ggplot2)
library(vcd)
library(descr)


# Create a contingency table
contingency_table <- matrix(c(277, 1114, 1115, 648, 1294), nrow = 1, ncol = 5,
                            dimnames = list("Frequency", c("NNS1", "NNS2", "NNS3", "Native", "Textbook")))

# Perform the Chi-square test
chi_square_test <- chisq.test(contingency_table)
print(chi_square_test)


# Frequency data
corpus_freq <- data.frame(
  Corpus = c("NNS1", "NNS2", "NNS3", "Native", "Textbook"),
  Relative_Frequency = c(277, 1114, 1115, 648, 1294)
)

# Pairwise comparisons
comparisons <- list(
  c("NNS1", "Native"),
  c("NNS1", "Textbook"),
  c("NNS2", "Native"),
  c("NNS2", "Textbook"),
  c("NNS3", "Native"),
  c("NNS3", "Textbook"),
  c("NNS1", "NNS2"),
  c("NNS2", "NNS3")
)

# Bonferroni correction
alpha <- 0.05
n_tests <- length(comparisons)
bonferroni_alpha <- alpha / n_tests

# Perform pairwise Chi-square tests and apply Bonferroni correction
for (comparison in comparisons) {
  corpus1 <- comparison[1]
  corpus2 <- comparison[2]
  
  freq1 <- corpus_freq$Relative_Frequency[corpus_freq$Corpus == corpus1]
  freq2 <- corpus_freq$Relative_Frequency[corpus_freq$Corpus == corpus2]
  
  contingency_table <- matrix(c(freq1, freq2), nrow = 2, byrow = TRUE)
  chi_square_test <- chisq.test(contingency_table)
  
  cat("Chi-square test for", corpus1, "vs", corpus2, "\n")
  cat("X-squared:", chi_square_test$statistic, "\n")
  cat("p-value:", chi_square_test$p.value, "\n")
  cat("Bonferroni-corrected alpha:", bonferroni_alpha, "\n")
  cat("Significant?", chi_square_test$p.value < bonferroni_alpha, "\n\n")
}




# ----------- effect size calculation

# Function to calculate Cramer's V
calc_cramers_v <- function(chi_squared, n, df) {
  phi <- sqrt(chi_squared / n)
  v <- phi / sqrt(min(df[1] - 1, df[2] - 1))
  return(v)
}

# Calculate Cramer's V values for each comparison
chi_squared_nns1_native <- 24.46
contingency_table_nns1_native <- matrix(c(277, 648), nrow = 2, byrow = TRUE)
chi_squared_nns1_textbook <- 5.37
contingency_table_nns1_textbook <- matrix(c(277, 1294), nrow = 2, byrow = TRUE)
chi_squared_nns2_native <- 16.39
contingency_table_nns2_native <- matrix(c(1114, 648), nrow = 2, byrow = TRUE)
chi_squared_nns2_textbook <- 6.24
contingency_table_nns2_textbook <- matrix(c(1114, 1294), nrow = 2, byrow = TRUE)
chi_squared_nns3_native <- 36.84
contingency_table_nns3_native <- matrix(c(1115, 648), nrow = 2, byrow = TRUE)
chi_squared_nns3_textbook <- 4.95
contingency_table_nns3_textbook <- matrix(c(1115, 1294), nrow = 2, byrow = TRUE)

cramers_v_data <- data.frame(
  Comparison = c("NNS1 vs Native", "NNS1 vs Textbook", "NNS2 vs Native", "NNS2 vs Textbook", "NNS3 vs Native", "NNS3 vs Textbook"),
  Cramers_V = c(
    calc_cramers_v(chi_squared_nns1_native, sum(contingency_table_nns1_native), c(2, 1)),
    calc_cramers_v(chi_squared_nns1_textbook, sum(contingency_table_nns1_textbook), c(2, 1)),
    calc_cramers_v(chi_squared_nns2_native, sum(contingency_table_nns2_native), c(2, 1)),
    calc_cramers_v(chi_squared_nns2_textbook, sum(contingency_table_nns2_textbook), c(2, 1)),
    calc_cramers_v(chi_squared_nns3_native, sum(contingency_table_nns3_native), c(2, 1)),
    calc_cramers_v(chi_squared_nns3_textbook, sum(contingency_table_nns3_textbook), c(2, 1))
  )
)

#Save the effect size data frame to a file
write.csv(cramers_v_data, file = "cramers_v_data.csv", row.names = FALSE)




# Define custom colors for each group
custom_colors <- c("NNS1" = "#3498DB", "NNS2" = "#E74C3C", "NNS3" = "#808080", "Native" = "#F1C40F", "Textbook" = "#2ECC71")

# Visualize the results
box_plot <- ggplot(corpus_freq, aes(x = Corpus, y = Relative_Frequency, fill = Corpus)) +
  geom_boxplot(alpha = 0.7, color = "black", outlier.color = "black", outlier.shape = 1) +
  scale_fill_manual(values = custom_colors) +
  labs(title = "Figure 6-2: Box Plot of Relative Frequency in Five Corpora",
       x = "Corpus",
       y = "Relative Frequency") +
  theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1))

# Print the boxplot
print(box_plot)

# Save the plot to a file
ggsave(filename = "RQ1_Frequency_box_plot.png", plot = box_plot, width = 10, height = 6, dpi = 300)


