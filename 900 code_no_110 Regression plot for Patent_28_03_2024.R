# I will make regression analysis for K=98 topic model (STM_patent_K98)

setwd("C:/Users/ozbay/OneDrive - XXXX/R_patent_2024")

library(stm)
library(dplyr)
library(ggplot2)
library(tidyr)



# First I have to import patents id and other related data:
load("C:/Users/ozbay/OneDrive - XXXX/R_patent_2024/Patent_STM_Final_Code_K98_for_thesis_14_03_2024.RData") # NOTE FOR THESIS: Use the following file to reproduce
# 900 Patent_STM_Final_Code_K98_for_thesis_14_03_2024.RData
STM_patent_K98 # STM model

000000000000000000000000000000000000000000000000000000000000000
###############################################################
# NOTE FOR THESIS: Use the following file to reproduce
# 900 Patent_STM_Final_Code_K98_for_thesis_14_03_2024.RData
###############################################################
000000000000000000000000000000000000000000000000000000000000000

patents_cluster_in_text # includes necessary data
colnames(patents_cluster_in_text)
patent_id <- patents_cluster_in_text %>% select(key, application_id, Country, title_corrected, year)



###################################################################################################################### START
# STEP-1: Checking the order of documents in out and patents_cluster_in_text
# CAUTION: The order of the data I used for the STM model may not be in the same order as the patent original data.
# I will make it sure that the data is in the same order with below code (I did it for WOS data too)
# I will just check if the order is same
library(dplyr)
key_no_of_out <- as.data.frame(names(out[["documents"]])) # extract key no from "out"
names(key_no_of_out) <- "key"
key_no_of_out$key <- as.numeric(key_no_of_out$key)
STM_texts_REARRANGE_THE_ORDER <- merge(key_no_of_out, patents_cluster_in_text, by = "key", all.x = TRUE) # take columns of STM_text_Singular and insert them in key_no_of_out per key no.
# Check if the values in all columns are identical between the two data frames
all.equal(STM_texts_REARRANGE_THE_ORDER, patents_cluster_in_text, check.attributes = FALSE) # [1] TRUE
# The ordes is same of out. So I will remove un-necessary data:
rm(STM_texts_REARRANGE_THE_ORDER, key_no_of_out)
###################################################################################################################### END


# STEP 2: Topic proportions: ----
# Extract the theta matrix which contains topic proportions for each document
document_topic_proportions <- STM_patent_K98$theta
# Convert the matrix to a dataframe
df_topic_proportions <- as.data.frame(document_topic_proportions)
# Add the WoS numbers as a new column using the names from out$documents
df_topic_proportions$key <- names(out$documents)
# Rearrange the dataframe to have WoS_number as the first column
df_topic_proportions <- df_topic_proportions[, c(ncol(df_topic_proportions), 1:(ncol(df_topic_proportions)-1))]

topic_proportions <- df_topic_proportions
# Rename the columns from V1, V2, ... to T1, T2, ...
topic_proportions <- topic_proportions %>%
  rename_at(vars(starts_with("V")), ~ sub("V", "T", .))
rm(document_topic_proportions, df_topic_proportions)
############################################# END



############################################# START
# STEP-3 Add a new column publication_year to topic_proportions
# First, create a lookup table from patents_cluster_in_text
colnames(patent_id)

lookup <- patent_id %>% 
  select(key, application_id, year, Country)

# Then merge this lookup table with topic_proportions
topic_proportions$key <- as.numeric(topic_proportions$key)
topic_proportions <- topic_proportions %>%
  left_join(lookup, by = c("key" = "key"))

topic_proportions <- topic_proportions[, c(1, 100:102, 2:99)] # re-order columns
rm(lookup, patent_id)
############################################# END


library(dplyr)
library(ggplot2)
library(tidyr)

yearly_topic_proportions <- topic_proportions %>%
  group_by(year) %>%
  summarise(across(starts_with("T"), \(x) mean(x, na.rm = TRUE)))

# Calculating the publication count per year
publication_counts <- topic_proportions %>%
  group_by(year) %>%
  summarise(publication_count = n())


library(tidyverse)
# Prepare the data for regression analysis
long_data <- yearly_topic_proportions %>%
  pivot_longer(cols = starts_with("T"), 
               names_to = "Topic", 
               values_to = "Proportion")

# Function to fit both linear and quadratic models for a given topic and analyze them
analyze_topic <- function(topic) {
  # Filter data for the specific topic
  topic_data <- long_data %>% filter(Topic == topic)
  
  # Linear model
  lin_model <- lm(Proportion ~ year, data = topic_data)
  lin_summary <- summary(lin_model)
  b_lin <- coef(lin_summary)["year", "Estimate"]
  b_lin_significance <- coef(lin_summary)["year", "Pr(>|t|)"]
  
  # Quadratic model
  quad_model <- lm(Proportion ~ year + I(year^2), data = topic_data)
  quad_summary <- summary(quad_model)
  b_quad <- coef(quad_summary)["year", "Estimate"]
  c_quad <- coef(quad_summary)["I(year^2)", "Estimate"]
  c_quad_significance <- coef(quad_summary)["I(year^2)", "Pr(>|t|)"]
  
  # Peak time calculation
  peak_time <- ifelse(c_quad != 0, -b_quad / (2 * c_quad), NA)
  peak_time_within_period <- peak_time >= 2005 & peak_time <= 2021
  
  # Return a summary
  tibble(Topic = topic,
         B_Coefficient_Linear = b_lin,
         B_Significance_Linear = b_lin_significance,
         B_Coefficient_Quadratic = b_quad,
         C_Coefficient_Quadratic = c_quad,
         C_Significance_Quadratic = c_quad_significance,
         Peak_Time = peak_time,
         Peak_Time_Within_Period = peak_time_within_period)
}

# Apply the function to each topic
topic_analysis_results <- map_df(unique(long_data$Topic), analyze_topic)

# View the results
print(topic_analysis_results)

# Step 1.2: Defining trends over years
topic_trends <- topic_analysis_results %>%
  mutate(
    Trend_of_TOPIC = case_when(
      C_Significance_Quadratic < 0.05 & C_Coefficient_Quadratic > 0 ~ "Exponential Increase",
      C_Significance_Quadratic < 0.05 & C_Coefficient_Quadratic < 0 ~ "Exponential Decrease",
      C_Significance_Quadratic >= 0.05 & B_Significance_Linear < 0.05 & B_Coefficient_Linear > 0 ~ "Linear Increase",
      C_Significance_Quadratic >= 0.05 & B_Significance_Linear < 0.05 & B_Coefficient_Linear < 0 ~ "Linear Decrease",
      TRUE ~ "No Significant Change"
    )
  )

# openxlsx::write.xlsx(x = topic_trends, file = "Patent_STM_K98_for_thesis_topics_trends.xlsx") 


#############################################################################
# Plot single topic START ----
# yearly_topic_proportions and topic_analysis_results have been created as per previous steps.
# Function to plot data for a given topic, using 'topic_analysis_results' for annotations
plot_topic <- function(topic, yearly_topic_proportions, topic_analysis_results) {
  # Filter data for the specific topic
  topic_data <- yearly_topic_proportions %>%
    select(year, !!sym(topic)) %>%
    rename(Proportion = !!sym(topic))
  
  # Fit linear and quadratic models
  lin_model <- lm(Proportion ~ year, data = topic_data)
  quad_model <- lm(Proportion ~ year + I(year^2), data = topic_data)
  
  # Create a sequence of years for predictions
  years <- data.frame(year = seq(min(topic_data$year), max(topic_data$year)))
  
  # Predict values using both models
  lin_pred <- predict(lin_model, newdata = years)
  quad_pred <- predict(quad_model, newdata = years)
  
  # Extract the analysis results for the topic
  analysis_results <- topic_analysis_results %>%
    filter(Topic == topic)
  
  # Extracting p-values and formatting the significance texts
  c_p_value <- analysis_results$C_Significance_Quadratic
  b_p_value <- analysis_results$B_Significance_Linear
  c_signif_text <- ifelse(c_p_value < 0.05, paste("p of c=", formatC(c_p_value, format = "E", digits = 1)), "c= (p >= 0.05)")
  b_signif_text <- ifelse(b_p_value < 0.05, paste("p of b_lin)=", formatC(b_p_value, format = "E", digits = 1)), "b_lin= (p >= 0.05)")
  peak_time_text <- ifelse(!is.na(analysis_results$Peak_Time) && analysis_results$Peak_Time_Within_Period, paste("Peak ~", round(analysis_results$Peak_Time)), "Peak out")
  
  # Combine the legend annotations into a single string
  legend_annotations <- paste(c_signif_text, b_signif_text, peak_time_text, sep = " | ") # sep = "   " = I add additional spaces for a better view in the plot
  
  # Determine the maximum year in the data
  max_year <- max(topic_data$year) # WAS: publication_year
  
  # Create the plot
  p <- ggplot(topic_data, aes(x = year, y = Proportion)) +
    geom_line(aes(color = "Actual Proportions"), linewidth = 0.5) +
    geom_line(data = years, aes(x = year, y = lin_pred, color = "Linear Model"), linewidth = 0.5) +
    geom_line(data = years, aes(x = year, y = quad_pred, color = "Quadratic Model"), linetype = "dashed", linewidth = 0.5) +
    scale_x_continuous(breaks = seq(min(years$year), max_year, by = 1)) +
    scale_color_manual(
      name = "Legend",
      values = c("Actual Proportions" = "black", "Linear Model" = "blue", "Quadratic Model" = "red"),
      labels = c("Topic Prop.", "Linear(a+b*X)", "Quad.(a+b*X+c*X^2)")
    ) +
    labs(title = paste("Topic Analysis:", topic), # WAS= labs(title = paste("Topic", topic, "Analysis")
         x = "Year",
         y = "Topic Proportion",
         color = "Legend") +
    theme_minimal() +
    theme(legend.position = "bottom",
          legend.text = element_text(size = 10),
          legend.title = element_text(size = 10),
          plot.title = element_text(size = 12)) +
    guides(color = guide_legend(title = legend_annotations))
  
  # Adjust theme settings for text size
  p <- p + theme(
    axis.text.x = element_text(size = 7),  # Adjust x-axis text size
    axis.text.y = element_text(size = 8),  # Adjust y-axis text size
    legend.text = element_text(size = 6),  # Adjust legend text size
    legend.title = element_text(size = 6), # Adjust legend title size
    legend.margin = margin(t = -8, unit = "pt") # Negative top margin to move legend up
    )
  # Return the plot
}

# Example usage
# plot <- plot_topic("T60", yearly_topic_proportions, topic_analysis_results)
# plot  # Display the plot
# Plot single topic END ----
#############################################################################



getwd()


library(ggplot2)
library(dplyr)
library(tidyr)
library(gridExtra)

# Assuming 'yearly_topic_proportions' and 'topic_analysis_results' are already defined
# Assuming 'plot_topic' function is defined and creates a plot for a given topic

# Step 2: Create the PDF file
pdf("DENEME file name Patent_topic_regression_plots_for_thesis_28_03_2024.pdf", width = 5.5, height = 4)

# Calculate the number of pages needed
total_topics <- ncol(yearly_topic_proportions) - 1  # excluding 'publication_year' column

# Generate the plots page by page
for (topic_num in 1:total_topics) {
  # Create the plot for the current topic
  plot <- plot_topic(paste0("T", topic_num), yearly_topic_proportions, topic_analysis_results)
  # Print the plot to the PDF, each on a new page
  print(plot)
}

# Close the PDF plotting device
dev.off()

