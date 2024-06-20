library(stm)
library(dplyr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(gridExtra)
library(tidyverse)
library(scales)

# STEP 1:
# I will re-order M (i.e. WOS data according to out. Out is the data used in STM analysis)
#
load("C:/Users/ozbay/OneDrive - XXXX/robot_WOS/M_05_Feb_2023_Environment.RData") # import the original WOS data
# NOTE FOR THESIS: Use following file for reduction: 900 WOS_data_for_thesis_lean_version_of M_05_Feb_2023_Environment.RData

load("C:/Users/ozbay/OneDrive - XXXX/R_May_2023/STM_Model_May_2023.RData") # Import final version of STM Model
# NOTE FOR THESIS: Use following file for reproduction: 900 WOS_STM STM_Model_May_2023.RData

M <- M %>% select(UT, publication_year, TI, AB) # select only necessary columns
WOS_No_of_out <- as.data.frame(names(out[["documents"]])) # extract WOS numbers from "out"
names(WOS_No_of_out) <- "UT"

M_Original_reordered <- merge(WOS_No_of_out, M, by = "UT", all.x = TRUE) # take TI anda AB date and insert in WOS_No_of_out per WOS no.
names(WOS_No_of_out) <- "UT"
rm(M, dot_replacement_correction, dot_replacements, New, New_1, Old, Old_1, WOS_No_of_out, yedek) # removal of unnecessary data



# STEP 2: Topic proportions: ----
# Extract the theta matrix which contains topic proportions for each document
# 
# load("C:/Users/ozbay/OneDrive - XXXX/R_May_2023/STM_Model_May_2023.RData")
document_topic_proportions <- STM_robot$theta # WOS no is not included in this data
# Convert the matrix to a dataframe
df_topic_proportions <- as.data.frame(document_topic_proportions)
# Add the WoS numbers as a new column using the names from out$documents
df_topic_proportions$WoS_number <- names(out$documents)
# Rearrange the dataframe to have WoS_number as the first column
df_topic_proportions <- df_topic_proportions[, c(ncol(df_topic_proportions), 1:(ncol(df_topic_proportions)-1))]
# check_1 <- df_topic_proportions
# check_1 <- check_1[, -1]
# check_2 <- as.data.frame(STM_robot$theta)
# identical(check_1, check_2)
# rm(check_1, check_2)
topic_proportions <- df_topic_proportions
# Rename the columns from V1, V2, ... to T1, T2, ...
topic_proportions <- topic_proportions %>%
  rename_at(vars(starts_with("V")), ~ sub("V", "T", .))
rm(document_topic_proportions, df_topic_proportions)
############################################# END

############################################# START
# STEP-4 Add a new column publication_year to topic_proportions
# First, create a lookup table from M_Original_reordered
lookup <- M_Original_reordered %>% 
  select(UT, publication_year)

# Then merge this lookup table with topic_proportions
topic_proportions <- topic_proportions %>%
  left_join(lookup, by = c("WoS_number" = "UT"))

topic_proportions <- topic_proportions[, c(1,93,2:92)] # move publication_year to 2nd column
rm(lookup)
############################################# END



library(dplyr)
library(ggplot2)
library(tidyr)

yearly_topic_proportions <- topic_proportions %>%
  group_by(publication_year) %>%
  summarise(across(starts_with("T"), \(x) mean(x, na.rm = TRUE)))



# Sanırım oldu. Erol Hocanın istediği kod, topic proportions kısmı aşağıda
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
  lin_model <- lm(Proportion ~ publication_year, data = topic_data)
  lin_summary <- summary(lin_model)
  b_lin <- coef(lin_summary)["publication_year", "Estimate"]
  b_lin_significance <- coef(lin_summary)["publication_year", "Pr(>|t|)"]
  
  # Quadratic model
  quad_model <- lm(Proportion ~ publication_year + I(publication_year^2), data = topic_data)
  quad_summary <- summary(quad_model)
  b_quad <- coef(quad_summary)["publication_year", "Estimate"]
  c_quad <- coef(quad_summary)["I(publication_year^2)", "Estimate"]
  c_quad_significance <- coef(quad_summary)["I(publication_year^2)", "Pr(>|t|)"]
  
  # Peak time calculation
  peak_time <- ifelse(c_quad != 0, -b_quad / (2 * c_quad), NA)
  peak_time_within_period <- peak_time >= 2005 & peak_time <= 2022
  
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



# Assuming 'yearly_topic_proportions' and 'topic_analysis_results' have been created as per previous steps.
# Function to plot data for a given topic, using 'topic_analysis_results' for annotations
plot_topic <- function(topic, yearly_topic_proportions, topic_analysis_results) {
  # Filter data for the specific topic
  topic_data <- yearly_topic_proportions %>%
    select(publication_year, !!sym(topic)) %>%
    rename(Proportion = !!sym(topic))
  
  # Fit linear and quadratic models
  lin_model <- lm(Proportion ~ publication_year, data = topic_data)
  quad_model <- lm(Proportion ~ publication_year + I(publication_year^2), data = topic_data)
  
  # Create a sequence of years for predictions
  years <- data.frame(publication_year = seq(min(topic_data$publication_year), max(topic_data$publication_year)))
  
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
  b_signif_text <- ifelse(b_p_value < 0.05, paste("p of b_lin=", formatC(b_p_value, format = "E", digits = 1)), "b_lin= (p >= 0.05)")
  peak_time_text <- ifelse(!is.na(analysis_results$Peak_Time) && analysis_results$Peak_Time_Within_Period, paste("Peak ~", round(analysis_results$Peak_Time)), "Peak is out")
  
  # Combine the legend annotations into a single string
  legend_annotations <- paste(c_signif_text, b_signif_text, peak_time_text, sep = " | ") # sep = "   " = I add additional spaces for a better view in the plot
  
  # Determine the maximum year in the data
  max_year <- max(topic_data$publication_year)
  
  # Create the plot
  p <- ggplot(topic_data, aes(x = publication_year, y = Proportion)) +
    geom_line(aes(color = "Actual Proportions"), linewidth = 0.5) +
    geom_line(data = years, aes(x = publication_year, y = lin_pred, color = "Linear Model"), linewidth = 0.5) +
    geom_line(data = years, aes(x = publication_year, y = quad_pred, color = "Quadratic Model"), linetype = "dashed", linewidth = 0.5) +
    scale_x_continuous(breaks = seq(min(years$publication_year), max_year, by = 1)) +
    scale_color_manual(
      name = "Legend",
      values = c("Actual Proportions" = "black", "Linear Model" = "blue", "Quadratic Model" = "red"),
      labels = c("Topic Prop.", "Lin.(a+bX)", "Quad.(a+bX+cX^2)")
    ) +
    labs(title = paste("Topic Analysis:", topic), # WAS= labs(title = paste("Topic", topic, "Analysis")
         x = "Publication Year",
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
    legend.margin = margin(t = -5, unit = "pt") # Negative top margin to move legend up
  )
  

  # Return the plot
}





library(ggplot2)
library(dplyr)
library(tidyr)
library(gridExtra)

# Assuming 'yearly_topic_proportions' and 'topic_analysis_results' are already defined
# Assuming 'plot_topic' function is defined and creates a plot for a given topic

# Step 2: Create the PDF file
pdf("DENEME file name WOS_topic_regression_plots_for_thesis_28_03_2024.pdf", width = 5.5, height = 4)

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

