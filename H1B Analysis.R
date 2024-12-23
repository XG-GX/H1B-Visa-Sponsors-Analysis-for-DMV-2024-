# Load required libraries
library(dplyr)
library(data.table)
library(ggplot2)

# Define file path
file_path <- "data/H-1B_Disclosure_Data_FY24.csv"

# Load the data
data <- fread(file_path, header = TRUE)

# Step 1: Define job titles
jobs_include <- c("BUSINESS ANALYST", "DATA ANALYST")
pattern_include <- paste0(jobs_include, collapse = "|")

# Step 2: Filter and analyze the data for DC, VA, MD
dmv_data <- data %>%
  filter(EMPLOYER_STATE %in% c("DC", "VA", "MD") &
           grepl(pattern_include, JOB_TITLE, ignore.case = TRUE)) %>%
  mutate(JOB_CATEGORY = ifelse(grepl("BUSINESS ANALYST", JOB_TITLE, ignore.case = TRUE), 
                               "Business Analyst", "Data Analyst"))

# Step 3: Analyze data by state and job category
dmv_employer_stats <- dmv_data %>%
  group_by(EMPLOYER_STATE, JOB_CATEGORY, EMPLOYER_NAME) %>%
  summarise(NUM_H1B = n(), .groups = "drop") %>%
  arrange(EMPLOYER_STATE, JOB_CATEGORY, desc(NUM_H1B))

# Save results
write.csv(dmv_employer_stats, "outputs/DMV_H1B_Sponsors.csv", row.names = FALSE)

# Step 4: Visualize the data for each state and category
ggplot(data = dmv_employer_stats, aes(x = NUM_H1B, fill = JOB_CATEGORY)) +
  geom_histogram(binwidth = 1, position = "dodge", color = "black") +
  facet_wrap(~EMPLOYER_STATE, scales = "free") +
  labs(title = "H1B Sponsors in DMV (2024)", 
       x = "Number of H1B Applications", 
       y = "Count of Employers",
       fill = "Job Category") +
  theme_classic()

# Step 5: Print a summary for each state and category
print("Summary of H1B Sponsors in DMV (2024):")
print(dmv_employer_stats, n = 100)  # Replace 100 with the number of rows
write.csv(dmv_employer_stats, "/Users/hugoxu/Desktop/DMV_H1B_Sponsors.csv", row.names = FALSE)

