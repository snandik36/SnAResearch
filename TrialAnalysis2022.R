#libraries
library(dplyr)
library(tidyverse)
library(ggplot2)
library(ggthemes)
library(RColorBrewer)
library(scales)
library(knitr)
library(kableExtra)

# loading local data as csv
data <- read.csv("/Users/surbhi/Desktop/RVU-COM/Research/SnAResearch/2022CSV.csv")

# replacing empty strings with NA and removing them
data <- data %>%
  mutate(across(starts_with("DRUGID"), ~na_if(., ""))) %>%
  mutate(across(starts_with("DRUGID"), ~na_if(., "None")))

# 1 - most common individual drugs (weighted by PATWT)
drug_columns <- grep("DRUGID", names(data), value = TRUE)

individual_drugs <- data %>%
  select(all_of(drug_columns), PATWT) %>%
  pivot_longer(cols = -PATWT, names_to = "Drug_Column", values_to = "Drug") %>%
  filter(!is.na(Drug)) %>%
  group_by(Drug) %>%
  summarize(Weighted_Count = sum(PATWT), 
            Unweighted_Count = n()) %>%
  arrange(desc(Weighted_Count))

# print top drugs
cat("top individual drugs (weighted by PATWT):\n")
print(individual_drugs)

# Visualization of top drugs
ggplot(individual_drugs %>% head(10), aes(x = reorder(Drug, Weighted_Count), y = Weighted_Count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Top 10 Most Frequently Administered Drugs (Weighted)",
       x = "Drug", y = "Weighted Count") +
  theme_minimal()

# 2. Most common drug combinations (accounting for PATWT)
# Create a column with all drugs given to each patient
data$All_Drugs <- apply(data[drug_columns], 1, 
                        function(x) paste(sort(na.omit(unique(x))), collapse = " + "))

# Calculate weighted frequencies of drug combinations
drug_combinations <- data %>%
  filter(All_Drugs != "") %>%
  group_by(All_Drugs) %>%
  summarize(Weighted_Count = sum(PATWT),
            Unweighted_Count = n()) %>%
  arrange(desc(Weighted_Count))

# Print top combinations
cat("\nTop Drug Combinations (Weighted by PATWT):\n")
print(drug_combinations %>% head(10))

# Visualization of top combinations
ggplot(drug_combinations %>% head(10), aes(x = reorder(All_Drugs, Weighted_Count), y = Weighted_Count)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  coord_flip() +
  labs(title = "Top 10 Most Frequent Drug Combinations (Weighted)",
       x = "Drug Combination", y = "Weighted Count") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 8))

# 3. Number of drugs administered per patient
data$Number_of_Drugs <- apply(data[drug_columns], 1, 
                              function(x) sum(!is.na(x)))

drugs_per_patient <- data %>%
  group_by(Number_of_Drugs) %>%
  summarize(Weighted_Count = sum(PATWT),
            Unweighted_Count = n()) %>%
  arrange(Number_of_Drugs)

# Print distribution
cat("\nNumber of Drugs Administered per Patient:\n")
print(drugs_per_patient)

# Visualization
ggplot(drugs_per_patient, aes(x = factor(Number_of_Drugs), y = Weighted_Count)) +
  geom_bar(stat = "identity", fill = "purple") +
  labs(title = "Number of Drugs Administered per Patient (Weighted)",
       x = "Number of Drugs", y = "Weighted Count") +
  theme_minimal()

# 4. Co-occurrence analysis - which drugs are often given together
# Create a binary matrix for drug co-occurrence
drug_list <- unique(unlist(data[drug_columns]))
drug_list <- drug_list[!is.na(drug_list)]

co_occurrence <- matrix(0, nrow = length(drug_list), ncol = length(drug_list),
                        dimnames = list(drug_list, drug_list))

for (i in 1:nrow(data)) {
  drugs <- na.omit(unlist(data[i, drug_columns]))
  if (length(drugs) > 1) {
    pairs <- combn(drugs, 2)
    for (j in 1:ncol(pairs)) {
      drug1 <- pairs[1, j]
      drug2 <- pairs[2, j]
      weight <- data$PATWT[i]
      
      # Add to both positions since co-occurrence is symmetric
      co_occurrence[drug1, drug2] <- co_occurrence[drug1, drug2] + weight
      co_occurrence[drug2, drug1] <- co_occurrence[drug2, drug1] + weight
    }
  }
}

# Convert to a long format for visualization
co_occurrence_df <- as.data.frame(as.table(co_occurrence)) %>%
  filter(Var1 != Var2 & Freq > 0) %>%
  arrange(desc(Freq))

# Print top co-occurring pairs
cat("\nTop Co-occurring Drug Pairs (Weighted by PATWT):\n")
print(co_occurrence_df %>% head(10))

# Heatmap visualization
ggplot(co_occurrence_df %>% head(20), aes(x = Var1, y = Var2, fill = Freq)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +
  labs(title = "Top 20 Drug Co-occurrences (Weighted)",
       x = "Drug 1", y = "Drug 2", fill = "Weighted Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 5. Time series analysis (if you have time data)
# This would require a date/time column in your actual data
# Example:
# data$DateTime <- as.POSIXct(data$YourTimeColumn)
# daily_counts <- data %>%
#   group_by(Date = as.Date(DateTime)) %>%
#   summarize(Count = sum(PATWT))
#
# ggplot(daily_counts, aes(x = Date, y = Count)) +
#   geom_line() +
#   labs(title = "Daily Drug Administration Trends") +
#   theme_minimal()