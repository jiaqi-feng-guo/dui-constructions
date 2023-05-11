library(ggplot2)
library(tidyr)
library(dplyr)
library(effsize)

# input data
data <- read.table(header = TRUE, text = "
Functions NNS1 NNS2 NNS3 Native Textbook
Concerned 35 285 198 66 171
Scope 0 5 71 145 106
Stimulus 83 311 349 58 237
Target 125 458 346 227 515
Theme 35 56 151 152 265
")

# prepare data- Transform the data into a long format
data_long <- gather(data, key = "Group", value = "Freq", -Functions)

#Kruskal-Wallis test
kruskal_result <- kruskal.test(Freq ~ Group, data = data_long)
print(kruskal_result)


# Perform pairwise comparisons for each NNS group vs Native, Textbook, and each other
# Define a function to perform pairwise comparisons
pairwise_comparison <- function(group1, group2) {
  data_df <- data_long %>% filter(Group %in% c(group1, group2))
  wilcox_test <- suppressWarnings(wilcox.test(Freq ~ Group, data = data_df))
  cohen_d_value <- cohen.d(data_df %>% filter(Group == group1) %>% pull(Freq), data_df %>% filter(Group == group2) %>% pull(Freq))
  
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

# Create contingency tables for each Function level
create_contingency_tables <- function(group1, group2) {
  lapply(1:nrow(data), function(i) {
    matrix(c(data[[group1]][i], sum(data[[group1]][-i]), data[[group2]][i], sum(data[[group2]][-i])),
           nrow = 2, byrow = TRUE)
  })
}

all_groups <- c("NNS1", "NNS2", "NNS3", "Native", "Textbook")

# Create the list of combinations
group_combinations <- combn(all_groups, 2, simplify = FALSE)

# Create contingency tables for each group combination
contingency_tables <- lapply(group_combinations, function(groups) {
  create_contingency_tables(groups[1], groups[2])
})

# Fisher's Exact Test
fisher_tests <- lapply(contingency_tables, function(tables) {
  lapply(tables, fisher.test)
})

# Extract p-values
p_values <- lapply(fisher_tests, function(tests) {
  sapply(tests, function(x) x$p.value)
})

# Calculate the overuse and underuse
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

# Display results
for (i in seq_along(group_combinations)) {
  cat("Fisher's Exact Test (", group_combinations[[i]][1], " vs ", group_combinations[[i]][2], "):\n", sep = "")
  print(p_values[[i]])
  cat("\n")
}

# Display overuse or underuse indications for each group combination
for (i in seq_along(group_combinations)) {
  cat("Overuse or Underuse Indication (", group_combinations[[i]][1], " vs ", group_combinations[[i]][2], "):\n", sep = "")
  for (j in 1:nrow(data)) {
    cat(data$Functions[j], ":", overuse_underuse[[i]][j], "\n")
  }
  cat("\n")
}


# ----------- Box Plot visualisation -------

# Visualize the data using ggplot2
ggplot(data_long, aes(x = Group, y = Freq, fill = Group)) +
  geom_boxplot(show.legend = FALSE, outlier.size = 3, outlier.color = "black") +
  scale_x_discrete(limits = c("NNS1", "NNS2", "NNS3", "Native", "Textbook")) +
  scale_fill_manual(values = c("NNS1" = "#3498DB", "NNS2" = "#E74C3C", "NNS3" = "#808080", "Native" = "#F1C40F", "Textbook" = "#2ECC71")) +
  theme_minimal() +
  theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        axis.title.x = element_text(size = 14, face = "bold"),
        axis.title.y = element_text(size = 14, face = "bold"),
        axis.text.x = element_text(size = 12, face = "bold", angle = 45, hjust = 1),
        axis.text.y = element_text(size = 12, face = "bold"),
        panel.grid.major = element_line(color = "gray", linetype = "dotted", linewidth = 0.5),
        panel.grid.minor = element_blank()) +
  labs(title = "Figure 6-4: Box Plot of Function Counts in Five Corpora",
       x = "Corpus",
       y = "Frequency") +
  annotate("text", x = 4, y = max(data_long$Freq) * 0.95, label = paste("Kruskal-Wallis H =", round(kruskal_result$statistic, 2), ", p =", format.pval(kruskal_result$p.value, digits = 2)), size = 5, fontface = "bold")




