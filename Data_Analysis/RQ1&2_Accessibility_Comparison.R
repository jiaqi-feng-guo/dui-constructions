library(ggplot2)
library(tidyr)
library(dplyr)
library(effsize)

# Input the data
data <- read.table(header = TRUE, text = "
Accessibility NNS1 NNS2 NNS3 Native Textbook
1 132 646 536  199 678
2 121 295 262  45 189
3 10 67 148  56 133
4 3 62 63 88 98
5 3 11 50  106 82
6 7 33 50 154 113
")

# Transform the data into a long format
data_long <- gather(data, key = "Group", value = "Count", -Accessibility)

# Perform Kruskal-Wallis test
kruskal_result <- kruskal.test(Count ~ Group, data = data_long)
print(kruskal_result)

# Define a function to perform pairwise comparisons
pairwise_comparison <- function(group1, group2) {
  data_df <- data_long %>% filter(Group %in% c(group1, group2))
  wilcox_test <- suppressWarnings(wilcox.test(Count ~ Group, data = data_df))
  cohen_d_value <- cohen.d(data_df %>% filter(Group == group1) %>% pull(Count), data_df %>% filter(Group == group2) %>% pull(Count))
  
  return(list(
    p_value = wilcox_test$p.value,
    cohen_d = cohen_d_value$estimate
  ))
}

# Perform pairwise comparisons for each NNS group vs Native, Textbook, and each other
comparison_results <- list()

nns_groups <- c("NNS1", "NNS2", "NNS3")
other_groups <- c("Native", "Textbook")

for (nns_group in nns_groups) {
  comparison_results[[nns_group]] <- list()
  
  for (other_group in other_groups) {
    comparison_results[[nns_group]][[other_group]] <- pairwise_comparison(nns_group, other_group)
  }
  
  for (other_nns_group in nns_groups) {
    if (nns_group != other_nns_group) {
      comparison_results[[nns_group]][[other_nns_group]] <- pairwise_comparison(nns_group, other_nns_group)
    }
  }
}

# Print the results
for (nns_group in nns_groups) {
  for (other_group in c(other_groups, nns_groups)) {
    if (nns_group != other_group) {
      cat("Comparison between", nns_group, "and", other_group, ":\n")
      cat("Wilcoxon rank-sum test P-value:", comparison_results[[nns_group]][[other_group]]$p_value, "\n")
      cat("Cohen's d:", comparison_results[[nns_group]][[other_group]]$cohen_d, "\n\n")
    }
  }
}


# ------------- Fisher exact test ----------------------------

# Function to create contingency tables for each Accessibility level
create_contingency_tables <- function(group1, group2) {
  lapply(1:nrow(data), function(i) {
    matrix(c(data[[group1]][i], sum(data[[group1]][-i]), data[[group2]][i], sum(data[[group2]][-i])),
           nrow = 2, byrow = TRUE)
  })
}

# Define all group names
all_groups <- c("NNS1", "NNS2", "NNS3", "Native", "Textbook")

# Create a list of all possible group combinations
group_combinations <- combn(all_groups, 2, simplify = FALSE)

# Generate contingency tables for each group combination
contingency_tables <- lapply(group_combinations, function(groups) {
  create_contingency_tables(groups[1], groups[2])
})

# Perform Fisher's Exact Test on each contingency table
fisher_tests <- lapply(contingency_tables, function(tables) {
  lapply(tables, fisher.test)
})

# Extract p-values from the test results
p_values <- lapply(fisher_tests, function(tests) {
  sapply(tests, function(x) x$p.value)
})

# Determine overuse and underuse for each group combination
overuse_underuse <- lapply(group_combinations, function(groups) {
  group1 <- groups[1]
  group2 <- groups[2]
  sapply(1:nrow(data), function(i) {
    if (data[[group1]][i] > data[[group2]][i]) {
      return(paste(group1, "Overuse"))
    } else {
      return(paste(group1, "Underuse"))
    }
  })
})

# Print the results of Fisher's Exact Test for each group combination
for (i in seq_along(group_combinations)) {
  cat("Fisher's Exact Test (", group_combinations[[i]][1], " vs ", group_combinations[[i]][2], "):\n", sep = "")
  print(p_values[[i]])
  cat("\n")
}

# Print overuse or underuse indications for each group combination
for (i in seq_along(group_combinations)) {
  cat("Overuse or Underuse Indication (", group_combinations[[i]][1], " vs ", group_combinations[[i]][2], "):\n", sep = "")
  for (j in 1:nrow(data)) {
    cat(data$Accessibility[j], ":", overuse_underuse[[i]][j], "\n")
  }
  cat("\n")
}



# ---------------Visualize the data using ggplot2 --------
# Define custom colors
# Order the factor levels
data_long$Group <- factor(data_long$Group, levels = c("NNS1", "NNS2", "NNS3", "Native", "Textbook"))

# Define custom colors
custom_colors <- c("NNS1" = "#3498DB", "NNS2" = "#E74C3C", "NNS3" = "#808080", "Native" = '#F1C40F', "Textbook" = "#2ECC71")

# Visualize the data using ggplot2
ggplot(data_long, aes(x = Group, y = Count, fill = Group)) +
  geom_boxplot(show.legend = FALSE, outlier.size = 3, outlier.color = "black") +
  scale_fill_manual(values = custom_colors) +
  theme_minimal() +
  theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        axis.title.x = element_text(size = 14, face = "bold"),
        axis.title.y = element_text(size = 14, face = "bold"),
        axis.text.x = element_text(size = 12, face = "bold", angle = 45, hjust = 1),
        axis.text.y = element_text(size = 12, face = "bold"),
        panel.grid.major = element_line(color = "gray", linetype = "dotted", size = 0.5),
        panel.grid.minor = element_blank()) +
  labs(title = "Figure 6-7: Box Plot of Accessibility Counts in Five Corpora",
       subtitle = paste("Kruskal-Wallis P-value =", round(kruskal_result$p.value, digits = 3)),
       x = "Group",
       y = "Frequency")

# Save the plot to a file
ggsave(filename = "RQ1_accessibility_plot.png", plot = last_plot(), width = 10, height = 6, dpi = 300)

