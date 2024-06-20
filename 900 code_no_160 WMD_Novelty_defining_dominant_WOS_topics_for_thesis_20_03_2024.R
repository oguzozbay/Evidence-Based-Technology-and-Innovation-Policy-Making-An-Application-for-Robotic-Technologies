# Her şeyi baştan yaptım...



# Tik-7 öncesi yazdığım koddan neredeyse eser kalmadı. Aşağıdaki kod Tik-7 öncesi yaptıklarım.
# C:/Users/ozbay/OneDrive - XXXX/R_new WMD Word Movers Distance/WMD similarity yearly topic proportions 20 Aralık düzeltme V2.R


#> NOTE: It is particularly important to emphasize that while calculating the novelty of topics, 
#> the proportions of th topics were multiplied by their respective novelty scores. 
#> This explains why the graphs depicting changes in topic proportions over the years and 
#> graphs depicting changes in novelty over the years exhibit similar patterns.


setwd("C:/Users/ozbay/OneDrive - XXXX/R_new WMD Word Movers Distance")

library(DescTools)
library(dplyr)
library(ggplot2)
library(grid)
library(gridExtra)
library(scales)
library(stm)
library(stringr)
library(tidyr)


# Import WMD similarity percentiles. It those are extracted from results of doc_similarity() function of library(text2map)
load("C:/Users/ozbay/OneDrive - XXXX/R_Ekim_Concept_Movers/1 UME bilgisayarı R sonucu WMD/WMD_and_Cosine_similarity_results.RData")
WMD_results_percentile_and_mean # WMD results.



# I am changing the column name to avoid any misunderstanding. "mean_Novelty_score" is the average of the calculated WMD value. 
# However, I named it in a way that could be misunderstood.
# df <- rename(df, newName = oldName) # SAMPLE CODE
WMD_results_percentile_and_mean <- rename(WMD_results_percentile_and_mean, mean_wmd = mean_Novelty_score)

# NOTE: While processing the cited titles references, I deleted  WOS numbers that did not have at least two references.
# I also checked that the words of the cited refs were existing the glove embedding (I deleted the words that did not exist)
# Details about those operations are in the R code below:
# C:/Users/ozbay/OneDrive - XXXX/R_Ekim_Concept_Movers/concept movers distance text preperation ekim 2023.R


load("C:/Users/ozbay/OneDrive - XXXX/R_Ekim_Concept_Movers/Cited_Titles_2_108_step5.RData") # cited titles up-to-date version
Cited_Titles_High_freq_deleted$high_freq_deleted # WMD was calculated on this column (also simple Cosine similarity was calculated on this column (i.e. cosine similarity without word embedding)) 


# First, make the order of the WOS data (i.e. cited ref.s' titles) the same as the STM model (this is M_Original_reordered).
# Then pull topic proportions from the STM model and insert WOS numbers and publication years.
# STEP 1:
# I will re-order M (i.e. WOS data according to out. Out is the data used in STM analysis)
load("C:/Users/ozbay/OneDrive - XXXX/robot_WOS/M_05_Feb_2023_Environment.RData") # import the original WOS data
load("C:/Users/ozbay/OneDrive - XXXX/R_May_2023/STM_Model_May_2023.RData") # Import final version of WOS STM Model
M <- M %>% select(UT, publication_year, TI, AB) # select only required columns
WOS_No_of_out <- as.data.frame(names(out[["documents"]])) # extract WOS numbers from "out"
names(WOS_No_of_out) <- "UT"

M_Original_reordered <- merge(WOS_No_of_out, M, by = "UT", all.x = TRUE) # take TI, AB, publication date and insert into WOS_No_of_out per WOS no.
names(WOS_No_of_out) <- "UT"
rm(M, dot_replacement_correction, dot_replacements, New, New_1, Old, Old_1, WOS_No_of_out, yedek) # removal of unnecessary data

# STEP 2: Topic proportions: ----
# Extract the theta matrix which contains topic proportions for each document
# load("C:/Users/ozbay/OneDrive - XXXX/R_May_2023/STM_Model_May_2023.RData")
document_topic_proportions <- STM_robot$theta # WOS no is not included in this data.

# Convert the matrix to a dataframe
df_topic_proportions <- as.data.frame(document_topic_proportions)

# Add the WoS numbers as a new column using the names from out$documents
df_topic_proportions$WoS_number <- names(out$documents)

# Rearrange the data_frame to have WoS_number as the first column
df_topic_proportions <- df_topic_proportions[, c(ncol(df_topic_proportions), 1:(ncol(df_topic_proportions)-1))]
topic_proportions <- df_topic_proportions

# Rename the columns from V1, V2, ... to T1, T2, ...
topic_proportions <- topic_proportions %>%
  rename_at(vars(starts_with("V")), ~ sub("V", "T", .))
rm(document_topic_proportions, df_topic_proportions)


############################################# START
# STEP-3 Add will a new column named publication_year into topic_proportions
# First, create a lookup table from M_Original_reordered
lookup <- M_Original_reordered %>% select(UT, publication_year)

# Then merge the lookup table with topic_proportions
topic_proportions <- topic_proportions %>% left_join(lookup, by = c("WoS_number" = "UT"))

topic_proportions <- topic_proportions[, c(1,93,2:92)] # move publication_year column into 2nd place
rm(lookup)
############################################# END



############################################# START
# STEP-4 
# Now I will remove WOS numbers that has not WMD value (Those had lacked of cited ref titles) & and those have only 2 cited refs.
# 
# Remove rows from topic_proportions if WoS_number DOES NOT exist in WMD_results_percentile_and_mean$Document_ID
WMD_available <- topic_proportions %>% filter(WoS_number %in% WMD_results_percentile_and_mean$Document_ID)

# I will also remove WOS numbers that has 2 cited refs (because, Novelty percentile value of WOS numbers with only 2 cited refs is relatively high).
load("C:/Users/ozbay/OneDrive - XXXX/R_new WMD Word Movers Distance/WOS_NO_of_2_cited_ref_documents_to_remove_from_novelty.Rdata")
WOS_NO_of_2_cited_ref_documents # WOS numbers that has 2 cited refs.
Topic_Prop_Selected <- WMD_available %>% filter(!(WoS_number %in% WOS_NO_of_2_cited_ref_documents$wos_id))

rm(WMD_available, topic_proportions, out, STM_robot, M_Original_reordered, Cited_Titles_High_freq_deleted, Cosine_similarity_percentile_and_mean)
############################################# END

#
###
##### START POINT FOR DIFFERENT TOPIC PROPORTION THRESHOLD TRIAL
###
#


# START HERE IF YOU TRY DIFFERENT THRESHOLDS
# Multiply columns 3-93 by 1000 and round to 0 decimal places
Topic_Prop_simplified <- Topic_Prop_Selected

Topic_Prop_simplified[, 3:93] <- round(Topic_Prop_simplified[, 3:93] * 1000, digits = 0)
# Calculate row sums for columns 3 to 93 and store in a new column 'row_sums'
Topic_Prop_simplified$row_sums <- rowSums(Topic_Prop_simplified[, 3:93], na.rm = TRUE)

############### PLOT - START ----
library(ggplot2)
# Topic_Prop_simplified$row_sums contains the sum values
ggplot(Topic_Prop_simplified, aes(x = row_sums)) +
  geom_histogram(binwidth = 1, fill = "grey", color = "black", boundary = 983.5) + 
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5, color = "black", size=4) + # Add counts above bars
  scale_x_continuous(breaks = 984:1011) + 
  scale_y_continuous(breaks = seq(0, 60000, by = 5000)) + # Y axis breaks at 1000 intervals
  theme_minimal() +
  theme(
    axis.line = element_line(size = 0.5, color = "black"), 
    axis.text.x = element_text(size = 14, angle = 90, vjust = 0.5, hjust=1),  # Bigger font and rotate x-axis text
    axis.text.y = element_text(size = 14), # Bigger font for y-axis labels
    axis.title.x = element_text(size = 16), # Bigger font for x-axis title
    axis.title.y = element_text(size = 16), # Bigger font for y-axis title
    plot.title = element_text(size = 18, hjust = 0.5) # Bigger font for plot title
  ) +
  labs(# title = "Distribution of row_sums",
       x = "Distrubution of topic proportions x 1000 after rounding 0 digits",
       y = "Frequency")
############### PLOT - END ----



# I will review the data.
# Calculate the mode for each row in columns 3 to 93
library(DescTools)
Topic_Prop_simplified$row_modes <- apply(Topic_Prop_simplified[, 3:93], 1, Mode, na.rm = TRUE)
# Select the smallest value if multiple modes exist
Topic_Prop_simplified$row_modes <- sapply(Topic_Prop_simplified$row_modes, min)

# calculate the median for each row
Topic_Prop_simplified$row_median <- apply(Topic_Prop_simplified[, 3:93], 1, median, na.rm = TRUE)





# I will reduce the topic proportions of some General Topics to zero (Not all, some topics):
# T12	General: Hybrid
# T19	General: Summary words
# T28	General: Challenges and Limitations
# T30	General: Robotic Applications (KEEP THIS i.e. do not reduce to 0)
# T44	General: Focus
# T45	General: Consideration of the Paper
# T51	General: Analysis and Evaluation
# T72	General: About Study
# T88	General: Mechatronic Design (KEEP THIS i.e. do not reduce to 0)


# Specify the columns to be converted to 0
columns_to_zero <- c("T12", "T19", "T28", "T44", "T45", "T51", "T72")

# Check if the specified columns exist in the data frame and convert their values to 0
Topic_Prop_simplified[columns_to_zero] <- lapply(Topic_Prop_simplified[columns_to_zero], function(x) 0)
# Now, all values in the specified columns of Topic_Prop_simplified have been set to 0.

# Re-calculate mode and median:
# Calculate the mode for each row in columns 3 to 93
Topic_Prop_simplified$row_modes_NoGeneral <- apply(Topic_Prop_simplified[, 3:93], 1, Mode, na.rm = TRUE)

# Select the smallest value if multiple modes exist
Topic_Prop_simplified$row_modes_NoGeneral <- sapply(Topic_Prop_simplified$row_modes_NoGeneral, min)

# calculate the median for each row
Topic_Prop_simplified$row_median_NoGeneral <- apply(Topic_Prop_simplified[, 3:93], 1, median, na.rm = TRUE)

# Calculate the frequencies
freq_mode <- Topic_Prop_simplified$row_modes_NoGeneral %>%
  table() %>% as.data.frame()

freq_median <- Topic_Prop_simplified$row_median_NoGeneral %>% 
  table() %>% as.data.frame()

# I supposed that in the real word an article may consist of 4-5 topics.
# So, for each document will reduce topic proportions to zero it a topic proportion is less than the threshold.
# I analysed modes, median (I started trial wilt threshold=6) and make trials and plot graphs to define a threshold.
# Finally I define %4.6 as the threshold value for the topic proportions of dominant topics.
# Why I chose 46, I made many trials. I tried for 15, 20, 25, 30, 46 and 47.
# %4.6 is the maximum threshold value that prevents any document's topic proportions from dropping to zero entirely.
# When it is set to 47, in two documents all topic proportion drop to zero.

# APPYING THRESHOLD (applying threshold to topic proportions)
# Then I decided to set the threshold as 46 (= means 0,046 because sum of a documents proportions is 1000.)
# Below code reduce values less than 46 to 0 in columns 3 to 93

Topic_Prop_simplified[, 3:93] <- lapply(Topic_Prop_simplified[, 3:93], function(x) ifelse(x < 46, 0, x))
# Now, all topic proportions less than 46 have been set to 0.

# Re-calculate the row sums.
Topic_Prop_simplified$row_sums_median_to_zero <- rowSums(Topic_Prop_simplified[, 3:93], na.rm = TRUE)
freq_row_sum_median_to_zero <- Topic_Prop_simplified$row_sums_median_to_zero %>% 
  table() %>%
  as.data.frame()


# I will count the number of values greater than 0 in each row of columns 3 to 93
Topic_Prop_simplified <- Topic_Prop_simplified %>%
  rowwise() %>%
  mutate(greater_than_0 = sum(c_across(3:93) > 0, na.rm = TRUE))


freq_remainig_topics <- Topic_Prop_simplified$greater_than_0 %>% 
  table() %>%
  as.data.frame()
names(freq_remainig_topics) <- c("Number_of_Remaining_Topics", "Freq")

# I saved various freq_remainig_topics for different threshold values in the below file: 
# load("C:/Users/ozbay/OneDrive - XXXX/R_new WMD Word Movers Distance/freq_remainig_topics.RData")

# Plot remaining topis with adjusted y-axis limits
par(mar = c(5, 5, 4, 6)) # Adjust the right margin (e.g., set it to 6)
with(freq_remainig_topics, barplot(Freq, names.arg = Number_of_Remaining_Topics,
                                  xlab = "Number of remaining topics (Threshold proportion < %4.6)",
                                  ylab = "Number of documents (197086 docs in total)",
                                  # main = "Distribution of remaining topics after the removal of relatively low topic proportions",
                                  width = 0.3,
                                  space = 1,
                                  ylim = c(0, 60000),
                                  cex.lab = 1.5))




#################################################
# At this point threshold value for dominant topics is defined and set to 46 (i.e 46/1000)

# Then I will do the following (19/03/2024):
# I will sort the data with 46 threshold applied and the original data from largest to smallest according to their "total topic proportions".
# Then I will calculate the rank correlation of these two data.
# My goal is to see how much my operation corrupts the content of the data.
# When Threshold was set to 46, the rank correlation was calculated as 0.70.
# spearman_total_prop # [1] 0.6793358
# cor_total_prop # [1] 0.6959399
# (see below codes for detail)
#################################################



Topic_Prop_simplified # threshold applied data.

# remove columns 94 to 100
Topic_Prop_simplified <- Topic_Prop_simplified[, -c(94:100)]

# I will Normalize each row sum to 1000.
# i.e. after applying the threshold, for each document I will make the total proportion of the remaining topics 1000.
# NOTE: At the beginning, I multiplied all proportions by 1000.
# I will calculate the sum of each row, then divide the elements in each row by the sum of that row.
# By this way I will generate Dominant Topics data.

Dominant_Topics <- Topic_Prop_simplified

# Calculate row sums for columns 3 to 93 and store in a new column 'row_sums'
Dominant_Topics$row_sums <- rowSums(Dominant_Topics[, 3:93], na.rm = TRUE)

# Normalize values in columns 3 to 93 by dividing by the row_sums for each row
Dominant_Topics[, 3:93] <- sweep(Dominant_Topics[, 3:93], 1, Dominant_Topics$row_sums, FUN = "/")

# I will check the result
Dominant_Topics$row_sum_ie_proportion <- rowSums(Dominant_Topics[, 3:93], na.rm = TRUE)
Dominant_Topics <- Dominant_Topics[, -c(94:95)] # Removing row_sums & row_sum_ie_proportion columns



# At this point, I get dominant topics, and topic proportions for each document.
Topic_Prop_Selected # Original topic proportions
Dominant_Topics # Dominant topic proportions those less than %4.6 reduced to 0, an then total porportions is normalized to 1)



# Now I will calculate the total proportions for the original and dominant topic data and the total topic proportions after 2018.
# My aim is to make a general comparison.
# STEP-1 ORIGINAL TOPIC PROPORTIONS:----
# Create a data frame
Total_Prop_original <- data.frame(Topic = colnames(Topic_Prop_Selected)[3:93], stringsAsFactors = FALSE)

# Calculate the sum for each topic (column) in Topic_Prop_Selected and add it to Total_Prop in a new column
Total_Prop_original$Total_Prop <- colSums(Topic_Prop_Selected[, 3:93], na.rm = TRUE)

# Filter rows in Topic_Prop_Selected where publication_year is greater than 2018 (selected dediğim WMD olan ve vcited ref >2)
year_2019_up <- Topic_Prop_Selected[Topic_Prop_Selected$publication_year > 2018, ]

# Calculate the sum for each topic in the filtered data
topic_sums_after_years_1 <- colSums(year_2019_up[, 3:93], na.rm = TRUE)

# Add these sums to Total_Prop_original in a new column "after_year_2018"
Total_Prop_original$after_year_2018 <- topic_sums_after_years_1
Total_Prop_original$Ratio_last_n_years_over_total <- with(Total_Prop_original, after_year_2018 / Total_Prop)



# STEP-2 DOMINANT TOPIC PROPORTIONS:----
# Step : Create a data frame
Total_Prop_dominant <- data.frame(Topic = colnames(Dominant_Topics)[3:93], stringsAsFactors = FALSE)

# Calculate the sum for each topic (column) in Dominant_Topics and add it to Total_Prop in a new column
Total_Prop_dominant$Total_Prop <- colSums(Dominant_Topics[, 3:93], na.rm = TRUE)

# Filter rows in Dominant_Topics where publication_year is greater than 2018 (selected dediğim WMD olan ve vcited ref >2)
year_2019_up_dom <- Dominant_Topics[Dominant_Topics$publication_year > 2018, ]
# Calculate the sum for each topic in the filtered data
topic_sums_after_years_d <- colSums(year_2019_up_dom[, 3:93], na.rm = TRUE)

# Add these sums to Total_Prop_original in a new column "after_year_2018"
Total_Prop_dominant$after_year_2018 <- topic_sums_after_years_d
Total_Prop_dominant$Ratio_last_n_years_over_total <- with(Total_Prop_dominant, after_year_2018 / Total_Prop)

rm(year_2019_up, topic_sums_after_years_1, year_2019_up_dom, topic_sums_after_years_d)


# STEP-3: At this point I have 2 data frames:
Total_Prop_original # Original topic proportions and ratio of sum(2019-2022)/sum(2005-2022) 
Total_Prop_dominant # Dominant topic proportions and ratio of sum(2019-2022)/sum(2005-2022) 

# I will remove general topics (T12, T19, T28, T44, T45, T51, T72)
# Specify the topics to be removed
topics_to_remove <- c("T12", "T19", "T28", "T44", "T45", "T51", "T72")

# Remove rows from Total_Prop_original
Props_Original <- Total_Prop_original[!Total_Prop_original$Topic %in% topics_to_remove, ]

# Remove rows from Total_Prop_dominant
Props_Dominant <- Total_Prop_dominant[!Total_Prop_dominant$Topic %in% topics_to_remove, ]

# Compute Pearson correlation for Total_Prop
#> It is important to ensure that the topics in both data frames are aligned so that each topic's proportions
#> and ratios in one data frame correspond to the same topic in the other data frame

cor_total_prop <- cor(Props_Original$Total_Prop, Props_Dominant$Total_Prop, method = "pearson")

# Compute Pearson correlation for Ratio_last_n_years_over_total
cor_ratio_last_n_years <- cor(Props_Original$Ratio_last_n_years_over_total, 
                              Props_Dominant$Ratio_last_n_years_over_total, method = "pearson")

# Compute Pearson correlation for after_year_2018
cor_after_year_2018 <- cor(Props_Original$after_year_2018, 
                           Props_Dominant$after_year_2018, method = "pearson")

# cor_total_prop # [1] 0.6959399
# cor_ratio_last_n_years # [1] 0.9876008
# cor_after_year_2018#  [1] 0.7955889
rm(cor_total_prop, cor_ratio_last_n_years, cor_after_year_2018)

# Calculate Spearman's rank correlation for Total_Prop
spearman_total_prop <- cor(Props_Original$Total_Prop, Props_Dominant$Total_Prop, method = "spearman")

# If you also need to compare the rankings based on Ratio_last_n_years_over_total:
spearman_ratio_last_n_years <- cor(Props_Original$Ratio_last_n_years_over_total, 
                                   Props_Dominant$Ratio_last_n_years_over_total, method = "spearman")

# spearman_total_prop # [1] 0.6793358
# spearman_ratio_last_n_years # [1] 0.9793257
rm(spearman_total_prop, spearman_ratio_last_n_years)




# At this point, I get dominant topics, and topic proportions for each document.
# Now I will calculate novelty scores with topic proportions.
# I will use WPD 99 percentile for novelty score. Because:

#> Shibayama et al. (2021) ... tested different percentile values (q). 
#> The result shows greater performance with higher q’s both in the correlation with self-reported novelty
#> measures and in the prediction of future citation. 
#> Thus, the novelty of scientific documents is determined by a small number of distant recombination. 
#> This contrasts with the previous recombinant novelty measures based on more average distances [9] (Shibayama et al. 2021)).

 
qX_WMD <- WMD_results_percentile_and_mean %>% select(Document_ID, q100) 
names(qX_WMD) <- c("Document_ID", "q_selected")

# Load the scales library
library(scales)

# Scale qX_WMD$q_selected values using the rescale function from the scales library
qX_WMD$scaled_q <- rescale(qX_WMD$q_selected)

# In order to check the results: Divide qX_WMD$q_selected by qX_WMD$scaled_q and store the result in a new column named "ratio"
qX_WMD$ratio <- qX_WMD$q_selected / qX_WMD$scaled_q
# Since q_selected WMD values vary between 1 and 0.018, the effect of "scale" is extremely low.
qX_WMD$ratio <- NULL

# Create Novelty Score (It van be thinked as similarity distance, i.e 0 is more similar, and 1 is un-similar)
# Subtract each value in the scaled_q column from 1 to compute the Novelty Score
qX_WMD$Novelty_Score <- 1 - qX_WMD$scaled_q


# Merge Novelty_Score from qX_WMD to Dominant_Topics
Dominant_Topics$Novelty_Score <- qX_WMD$Novelty_Score[match(Dominant_Topics$WoS_number, qX_WMD$Document_ID)]
# Now, Dominant_Topics includes the Novelty_Score from qX_WMD matched by Document_ID to WoS_number.

Dominant_Topics <- Dominant_Topics %>% relocate(Novelty_Score, .after = publication_year) # for ease of display

# Generate a data frame as a copy of Dominant_Topics and multiply Novelty_scores with dominant topic proportions.
Novelty_of_Topics <- Dominant_Topics

# Multiply the Novelty_Score with each value in columns 4 to 94 for each row
Novelty_of_Topics[, 4:94] <- sweep(Novelty_of_Topics[, 4:94], 1, Novelty_of_Topics$Novelty_Score, `*`)
# Just a simple check after filtering Novelty_Score = 0 in R studio view screen
# sum(Novelty_of_Topics[Novelty_of_Topics$WoS_number == "WOS:000209409100003", 4:94], na.rm = TRUE)

# Now I will calculate the total novelty scores for all years and the total topic proportions after 2018.
# First Step : Create a data frame named Total_Novelty to enter required information
Total_Novelty <- data.frame(Topic = colnames(Novelty_of_Topics)[4:94], stringsAsFactors = FALSE)

# Calculate the sum for each topic (column) in Novelty_of_Topics and add it to Total_Novelty in a new column named Topics_total_novelty
Total_Novelty$Topics_total_novelty <- colSums(Novelty_of_Topics[, 4:94], na.rm = TRUE)

################################## CHECKING THE CODE - START
topic_no <- 80
sum(Novelty_of_Topics[,(topic_no+3)])
Total_Novelty[topic_no,2]
sum(Novelty_of_Topics[,(topic_no+3)]) - Total_Novelty[topic_no,2]
rm(topic_no)
################################## CHECKING THE CODE - END

# Filter rows in Novelty_of_Topics where publication_year is greater than 2018
selected_year <- Novelty_of_Topics[Novelty_of_Topics$publication_year > 2018, ]

# Calculate the sum for each topic in the filtered data and then add these sums to Total_Novelty in a new column "after_year_2018"
Total_Novelty$after_year_2018 <- colSums(selected_year[, 4:94], na.rm = TRUE)

Total_Novelty$Ratio_last_n_years_over_total <- with(Total_Novelty, after_year_2018 / Topics_total_novelty)
# Just a simple check
# sil <- 45
# (Total_Novelty[sil,3]/Total_Novelty[sil,2])/Total_Novelty[sil,4]
# remove(sil)

topics_to_remove <- c("T12", "T19", "T28", "T44", "T45", "T51", "T72")

# Remove rows from Total_Novelty
Total_Novelty <- Total_Novelty[!Total_Novelty$Topic %in% topics_to_remove, ]
rm(selected_year)

##########################################
# cor(Total_Novelty$Topics_total_novelty, Total_Novelty$after_year_2018, method = "spearman")
# [1] 0.8633796
# cor(Total_Novelty$Topics_total_novelty, Total_Novelty$after_year_2018, method = "pearson")
# [1] 0.783031
##########################################





# NOW GET BELOW DATA
Total_Novelty # Novelty Scores: including the ratio of "sum of years after 2018" to "sum of all years" (General topics removed)
Props_Dominant # Dominant topics: including the ratio of "sum of years after 2018" to "sum of all years" (General topics removed)
Props_Original # Original topics: including the ratio of sum years after 2018 to all years (General topics removed)


####################
# For test purpose I generated an additional data for years after 2015 and check the correlation as follows:
# Calculate the ranks for Ratio_last_n_years_over_total columns of Total_Novelty_2018 within and Total_Novelty_2015
# Calculate Spearman's rank correlation for Total_Prop
# spearman_2015_2018 <- cor(Total_Novelty_2015$Ratio_last_n_years_over_total, Total_Novelty_2018$Ratio_last_n_years_over_total, method = "spearman")
# spearman_2015_2018
# [1] 0.9526577
# openxlsx::write.xlsx(x = Total_Novelty_2018, file = "Novelty_Scores_of_Topics_and_Raio_of_years_after_2018.xlsx") 
####################


##
####
#######
#########
########### PLOTS (single Plot)
#########
######
####
##


#> NOTE: It is particularly important to emphasize that while calculating the novelty of topics, 
#> the proportions of th topics were multiplied by their respective novelty scores. 
#> This explains why the graphs depicting changes in topic proportions over the years and 
#> graphs depicting changes in novelty over the years exhibit similar patterns.


##################################################################### START 
# I will divide the total Novelty value of each year by the number of publications in that year 
# (i.e. I will get the average "Novelty_Score x Topic Proportion" values of each topic on an annual basis)
library(dplyr)
yearly_novelty_per_topic <- Novelty_of_Topics %>%
  group_by(publication_year) %>%
  summarise(across(starts_with("T"), function(x) mean(x, na.rm = TRUE)))


# Now I will calculate the proportion of each topic according to years.
# In other words, I will calculate the total proportion of the topics by year and 
# divide the result  by the number of documents in that year.
yearly_proportions_per_topic_of_dominant_topics <- Dominant_Topics %>%
  group_by(publication_year) %>%
  summarise(across(starts_with("T"), function(x) mean(x, na.rm = TRUE)))


# Count the number of documents for each year (Not necess)
# yearly_doc_counts <- Novelty_of_Topics %>% group_by(publication_year) %>% summarise(document_count = n())

topics_to_remove <- c("T12", "T19", "T28", "T44", "T45", "T51", "T72")

yearly_novelty_per_topic # Data frame of Novelty proportions
yearly_proportions_per_topic_of_dominant_topics  # Data frame of topic proportions

# Assuming the first topic is the first column after `publication_year`, typically "T1"
ggplot(data = yearly_novelty_per_topic, aes(x = publication_year, y = T7)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(
    title = "Yearly Novelty Changes for Topic T7",
    x = "Publication Year",
    y = "Total novelty / Number of documents that year"
  )


# Assuming the first topic is the first column after `publication_year`, typically "T1"
ggplot(data = yearly_proportions_per_topic_of_dominant_topics, aes(x = publication_year, y = T7)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(
    title = "Yearly Topic Proportion changes for Topic T7",
    x = "Publication Year",
    y = "Total topic proportion / Number of documents that year"
  )



##
####
#######
#########
########### PLOTS (Plotting all topics)
#########
######
####
##


#> NOTE: It is particularly important to emphasize that while calculating the novelty of topics, 
#> the proportions of th topics were multiplied by their respective novelty scores. 
#> This explains why the graphs depicting changes in topic proportions over the years and 
#> graphs depicting changes in novelty over the years exhibit similar patterns.


# Plot all together (3x2, NOTE 4x2 code is also available below)

library(ggplot2)
library(gridExtra)
library(dplyr)

# Before plotting, multiply the novelty values by 10
yearly_novelty_per_topic_adjusted <- yearly_novelty_per_topic
cols_to_adjust <- names(yearly_novelty_per_topic_adjusted)[2:ncol(yearly_novelty_per_topic_adjusted)]
yearly_novelty_per_topic_adjusted[cols_to_adjust] <- yearly_novelty_per_topic_adjusted[cols_to_adjust] * 10 # for a better plot ratio

# Define the function to create a plot for a single topic
create_plot_for_topic <- function(topic) {
  plot_novelty <- ggplot(data = yearly_novelty_per_topic_adjusted, aes(x = publication_year)) +
    geom_line(aes(y = get(topic), color = "Novelty x 10")) +
    geom_point(aes(y = get(topic), color = "Novelty x 10")) +
    geom_line(data = yearly_proportions_per_topic_of_dominant_topics, aes(x = publication_year, y = get(topic), color = "Proportion")) +
    geom_point(data = yearly_proportions_per_topic_of_dominant_topics, aes(x = publication_year, y = get(topic), color = "Proportion")) +
    scale_color_manual(values = c("Novelty x 10" = "blue", "Proportion" = "red")) +
    theme_minimal() +
    labs(
      title = paste("Yearly Data for", topic),
      x = "Publication Year",
      y = "Value",
      color = "Legend"
    ) +
    theme(axis.line = element_line(color = "black"))
  
  return(plot_novelty)
}

# Generate and save the plots to a PDF, with updated dimensions and legends
pdf("yearly_topic_data.pdf_3x2", width = 11, height = 11) # was 8.5 x 11
plots <- lapply(setdiff(names(yearly_novelty_per_topic_adjusted)[-1], topics_to_remove), create_plot_for_topic)

# Handling the plots arrangement with pagination
num_plots_per_page <- 6
for (i in seq(0, length(plots) - 1, num_plots_per_page)) {
  page_plots <- plots[(i + 1):min(i + num_plots_per_page, length(plots))]
  if (length(page_plots) > 0) {
    do.call("grid.arrange", c(page_plots, nrow = 3, ncol = 2))
  }
}

dev.off()




######## PLOT CODE FOR 4x2 per page

#> NOTE: It is particularly important to emphasize that while calculating the novelty of topics, 
#> the proportions of th topics were multiplied by their respective novelty scores. 
#> This explains why the graphs depicting changes in topic proportions over the years and 
#> graphs depicting changes in novelty over the years exhibit similar patterns.


library(ggplot2)
library(gridExtra)
library(dplyr)

# Before plotting, multiply the novelty values by 10
yearly_novelty_per_topic_adjusted <- yearly_novelty_per_topic
cols_to_adjust <- names(yearly_novelty_per_topic_adjusted)[2:ncol(yearly_novelty_per_topic_adjusted)]
yearly_novelty_per_topic_adjusted[cols_to_adjust] <- yearly_novelty_per_topic_adjusted[cols_to_adjust] * 10 # for a better plot ratio of novelty scores

# Define the function to create a plot for a single topic
create_plot_for_topic <- function(topic) {
  plot_novelty <- ggplot(data = yearly_novelty_per_topic_adjusted, aes(x = publication_year)) +
    geom_line(aes(y = get(topic), color = "Novelty x 10")) +
    geom_point(aes(y = get(topic), color = "Novelty x 10")) +
    geom_line(data = yearly_proportions_per_topic_of_dominant_topics, aes(x = publication_year, y = get(topic), color = "Proportion")) +
    geom_point(data = yearly_proportions_per_topic_of_dominant_topics, aes(x = publication_year, y = get(topic), color = "Proportion")) +
    scale_color_manual(values = c("Novelty x 10" = "blue", "Proportion" = "red")) +
    theme_minimal() +
    labs(
      title = paste("Topic:", topic),
      x = "Publication Year",
      y = "(Prop. & Nov.)/Yearly Docs.",
      color = "Legend"
    ) +
    theme(axis.line = element_line(color = "black"))
  
  return(plot_novelty)
}

# Generate and save the plots to a PDF, with updated dimensions and legends
pdf("yearly_topic_data_4x2.pdf", width = 11, height = 11)
plots <- lapply(setdiff(names(yearly_novelty_per_topic_adjusted)[-1], topics_to_remove), create_plot_for_topic)

# Handling the plots arrangement with pagination for 4 rows and 2 columns
num_plots_per_page <- 8
for (i in seq(0, length(plots) - 1, num_plots_per_page)) {
  page_plots <- plots[(i + 1):min(i + num_plots_per_page, length(plots))]
  if (length(page_plots) > 0) {
    do.call("grid.arrange", c(page_plots, nrow = 4, ncol = 2))
  }
}

dev.off()



####################################################
### PLOT CODE FOR 1 x 1 pdf(one page pf)
library(ggplot2)
library(dplyr)

# Assume yearly_novelty_per_topic_adjusted and the create_plot_for_topic function are defined as above

# Generate and save the plots to a PDF, with one plot per page
pdf("SIL_sil_yearly_topic_data_1per_page.pdf", width = 6, height = 3)  # Adjust size as needed
plots <- lapply(setdiff(names(yearly_novelty_per_topic_adjusted)[-1], topics_to_remove), create_plot_for_topic)

# Loop through each plot and output it on a separate page
for (plot in plots) {
  print(plot)  # Print each plot to its own page in the PDF
}

dev.off()
####################################################

























#
##
####
######
########
########## NEW ANALYSIS
########
######
####
##
#


#> NOTE: It is particularly important to emphasize that while calculating the novelty of topics, 
#> the proportions of th topics were multiplied by their respective novelty scores. 
#> This explains why the graphs depicting changes in topic proportions over the years and 
#> graphs depicting changes in novelty over the years exhibit similar patterns.


# A new idea came to my mind.
# Find total novelty for each WOS number.
# Look at the total novelty distribution and select documents greater than "mean + 2 * standard deviation"
# Examine the topic distribution of these selected documents.
# The documents with the highest proportion has the possibility of being novel.

Novelty_of_Topics # Each column includes Novelty Score x Topic proportions of dominant topics.

Doc_Novelty <- Novelty_of_Topics %>% filter(Novelty_Score > 0) # filter those Novelty Score > 0

# Calculate the sum of columns starting with "T" for each row
Doc_Novelty$sum_of_doc <- rowSums(Doc_Novelty[, startsWith(names(Doc_Novelty), "T")], na.rm = TRUE)

Doc_Novelty$sum_of_doc <- round(Doc_Novelty$sum_of_doc, digits = 4)
dim(Doc_Novelty) # 137919

Doc_Novelty <- Doc_Novelty %>% filter(sum_of_doc > 0)
dim(Doc_Novelty) # 137072

# Plotting the histogram of sum_of_doc values
library(ggplot2)
ggplot(Doc_Novelty, aes(x = sum_of_doc)) +
  geom_histogram(bins = 100, fill = "grey", color = "black") + # Adjust the number of bins as needed
  theme_minimal() +
  labs(title = "Distribution of sum_of_doc Values",
       x = "sum_of_doc",
       y = "Count")


# Calculate the mean and standard deviation
mean_value <- mean(Doc_Novelty$sum_of_doc, na.rm = TRUE)
std_dev <- sd(Doc_Novelty$sum_of_doc, na.rm = TRUE)

# Calculate thresholds
threshold_2sd <- mean_value + 2 * std_dev
threshold_3sd <- mean_value + 3 * std_dev


# Plot the histogram with the threshold lines
ggplot(Doc_Novelty, aes(x = sum_of_doc)) +
  geom_histogram(bins = 100, fill = "grey", color = "black") +
  geom_vline(xintercept = threshold_2sd, color = "black", linetype = "solid", size = 0.5) +
  geom_vline(xintercept = threshold_3sd, color = "black", linetype = "solid", size = 0.5) +
  theme_minimal() +
  
  theme(
    axis.line = element_line(size = 0.5, color = "black"),
    axis.text.x = element_text(size = 14),  # Bigger text font for x axis
    axis.text.y = element_text(size = 14),   # Bigger text font for y axis
    axis.title.x = element_text(size = 16), # Bigger text font for x-axis title
    axis.title.y = element_text(size = 16)  # Bigger text font for y-axis title
    ) +
  
  labs(# title = "Distribution of the sums of the novelty values of the documents",
       x = "Total novelty values of documents",
       y = "Count") +
  annotate("text", x = threshold_2sd, y = 1000, label = paste("   (Mean + 2 Std.Dev) \n ", round(threshold_2sd, 3), " "), 
           vjust = -8, color = "black", hjust = 1, size = 5) +
  annotate("text", x = threshold_3sd, y = 1000, label = paste("  (Mean + 3 Std.Dev)\n ", round(threshold_3sd, 3), "  "),
           vjust = 1, color = "black", hjust = 0, size = 5)


# Define the threshold
threshold <- mean_value + 2 * std_dev

# Filter the data
filtered_Doc_Novelty <- Doc_Novelty[Doc_Novelty$sum_of_doc > threshold, ]

# Step : Create a data frame
Total_Novelty_per_topic_in_filtered_docs <- data.frame(Topic = colnames(filtered_Doc_Novelty)[4:94], stringsAsFactors = FALSE)

# Calculate the sum for each topic (column) in Dominant_Topics and add it to Total_Prop in a new column
Total_Novelty_per_topic_in_filtered_docs$Total_Prop <- colSums(filtered_Doc_Novelty[, 4:94], na.rm = TRUE)

# openxlsx::write.xlsx(x = Total_Novelty_per_topic_in_filtered_docs, file = "Total_Novelty_per_topic_in_filtered_High_Novelty_docs.xlsx") 
# save.image("C:/Users/ozbay/OneDrive - XXXX/R_new WMD Word Movers Distance/WMD_Novelty_defining_dominant_WOS_topics_for_thesis_20_03_2024.RData")



# Additional calculations (22/03/2023:
# I run above codes for q95. And I generated  below data frames for comparison:
q95_46thresh_Total_Novelty_per_topic_in_filtered_docs # Table for q95 and threshold 46
q100_46thresh_Total_Novelty_per_topic_in_filtered_docs # Table for q100 and threshold 46


# Calculate Spearman's rank correlation for Total_Prop
cor(q95_46thresh_Total_Novelty_per_topic_in_filtered_docs$Total_Prop,
    q100_46thresh_Total_Novelty_per_topic_in_filtered_docs$Total_Prop,
    method = "spearman")
# [1] 0.9036519 # The correlation result.


