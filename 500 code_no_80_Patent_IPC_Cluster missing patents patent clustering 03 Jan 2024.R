# I downloaded the missing patents from January 2024 and 2021 to 2023.
# The missing ones are as follows: I did not download some years (there was an error while downloading).
# I downloaded the patent data incompletely.
# 2007, 2009, 2012, 2014 # MISSING
# Thereupon, I downloaded the missing parts and additionally downloaded the period between 2021-2023 (all of 2023).
# Additionally, the numbers of 2013 and 2015 were more than the patents I had, so I downloaded them too (the database must have been updated over time).
# "C:\Users\ozbay\OneDrive - XXXX\R partner\patent STM 13 july 2023\missing_patents"


# As a result, I will re-STM the patents.
# For STM, I will first cluster the IPC codes.
# In my previous work, I reduced the IPC codes to a single letter. That is, I made "G05D 1/02; A47L 11/24" = "b; a".
# This time I reduce it to "g05 and a47" (that is, I reduce it to 3 characters).
# I used the old code in the same way. The second stage of the reduction process in the old code is unnecessary here.
# But since I didn't do the second reduction, that is, I did substr(x, 1, 3)) in the code below, it has no effect.
# df$IPC_LEVEL_1 <- lapply(df$IPC_LEVEL_1, function(x) substr(x, 1, 3))
# If you want to make an additional reduction in the future, you can do substr(x, 1, 1)) and only the first letters remain.



load("C:/Users/ozbay/OneDrive - XXXX/R ortak/patent STM 13 temmuz 2023/STM patent 13 temmuz_ENVIRONMENT.RData")
load("C:/Users/ozbay/OneDrive - XXXX/R ortak/patent STM 13 temmuz 2023/missing_patents/missing_patents_V3.RData")

setwd("C:/Users/ozbay/OneDrive - XXXX/R ortak/patent IPC clustering")

patents_IPC_part1 <- patents_for_STM %>% select(key, appication_id, IPC)
patents_IPC_missings <- missing_patents_V2 %>% select(key_oguz, Application_Id, I_P_C)
colnames(patents_IPC_missings)
names(patents_IPC_missings) <- c("key", "appication_id", "IPC")

library(dplyr)

# Assuming data frames are patents_IPC_missings and patents_IPC_part1

# Combine the two data frames
combined_patents <- rbind(patents_IPC_part1, patents_IPC_missings)
dim(combined_patents) # [1] 353854      3
combined_patents <- as.data.frame(combined_patents)
names(combined_patents) <- c("key", "application_id", "IPC") # correct "application_id" (WAS= appication_id)
# Remove duplicate rows based on the application_id column
combined_patents <- combined_patents %>% distinct(application_id, .keep_all = TRUE)
dim(combined_patents) # [1] 352958      3

# Delete all except for combined_patents
# save.image("C:/Users/ozbay/OneDrive - XXXX/R ortak/patent IPC clustering/combine unite all patents ie first ones and the missing patents for IPC clustering 04_01_2024.RData")

load("C:/Users/ozbay/OneDrive - XXXX/R ortak/patent IPC clustering/combine unite all patents ie first ones and the missing patents for IPC clustering 04_01_2024.RData") # Casper
load("D:/oguz_ozbay/patent missing patents cluster analysis/combine unite all patents ie first ones and the missing patents for IPC clustering 04_01_2024.RData") # UME workstation

any(is.na(combined_patents$IPC)) # [1] FALSE


sil <- combined_patents %>% filter(key == "10019322") # This line to be deleted. I will remove this patent from STM analysis also
combined_patents <- combined_patents %>% filter(key != "10019322")

df <- combined_patents
df$IPC_Original <- df$IPC
df <- df[complete.cases(df$IPC), ]
df$IPC <- tolower(df$IPC)
df$IPC <- df$IPC %>% stringr::str_squish()

df$IPC <- strsplit(df$IPC, "; ")
df$IPC <- lapply(df$IPC, function(x) substr(x, 1, 3)) # Extract first three characters
df$IPC <- lapply(df$IPC, unique) # Remove duplicates
df$IPC <- sapply(df$IPC, paste, collapse = "; ") # Convert back to a single string

# split all IPC codes into a list
ipc_list <- unlist(strsplit(df$IPC, "; "))
unique(ipc_list)
# Create a table of IPC code counts
ipc_counts <- table(ipc_list) # Convert the table to a data frame
df_ipc_counts <- as.data.frame.table(ipc_counts)
names(df_ipc_counts) <- c("IPC_Code", "Frequency") # Rename the columns

# openxlsx::write.xlsx(x = ipc_counts, file = "ipc_counts_all_patend_including_missingd_04_Jan_2024.xlsx") 


# Correction list (I checked from: https://patentscope.wipo.int/search/en/result.jsf?_vid=P20-LQZD6A-66705 )
#   / to be replaced as follows: b25
# aig to be replaced as follows: a63
# b52 to be replaced as follows: to be deleted
# bij to be replaced as follows: b25
# bik to be replaced as follows: b25
# bil to be replaced as follows: b01
# g0v to be replaced as follows: g05
# gic to be replaced as follows: g01
# hiw to be replaced as follows: h04
# gir to be replaced as follows: g01


df$IPC_corrected <- df$IPC
library(stringr)
df$IPC_corrected <- str_replace_all(df$IPC_corrected, c("/" = "b25",
                                                        "aig" = "a63",
                                                        "b52; " = "",
                                                        "bij" = "b25",
                                                        "bik" = "b25",
                                                        "bil" = "b01",
                                                        "g0v" = "g05",
                                                        "gic" = "g01",
                                                        "hiw" = "h04",
                                                        "gir" = "g01"))



############################################
############################################
# checking replacements
colnames(df)
sil <- df %>% select (IPC, IPC_corrected)
# Comparing original and replaced text
sil$check_1 <- mapply(function(x, y) paste0(setdiff(x, y), collapse = " "), strsplit(sil[,1], '\\s'), strsplit(sil[,2], '\\s'))
sil$check_2 <- mapply(function(x, y) paste0(setdiff(x, y), collapse = " "), strsplit(sil[,2], '\\s'), strsplit(sil[,1], '\\s'))
sil_filtered <- sil %>% filter(check_1 != "" | check_2 != "")
############################################
############################################


# Now I will check IPC codes again
# split all IPC codes into a list
ipc_list_2 <- unlist(strsplit(df$IPC_corrected, "; "))
unique(ipc_list_2)
# Create a table of IPC code counts
ipc_counts_2 <- table(ipc_list_2) # Convert the table to a data frame
df_ipc_counts_2 <- as.data.frame.table(ipc_counts_2)



# I will find IPC combinations of patents.
# Split the IPC codes into a list, sort each list, and collapse back into a string
df$IPC_corrected <- sapply(lapply(strsplit(df$IPC_corrected, "; "), sort), paste, collapse = "; ")
# Calculate frequency of each combination
ipc_combinations_freq <- table(df$IPC_corrected)
# Convert table to a data frame
ipc_combinations_freq <- as.data.frame(ipc_combinations_freq)
# Rename the columns
names(ipc_combinations_freq) <- c("IPC_Combinations", "Frequency")


# IPC Level-1 ----
# Now I will extract level 1 values of IPC Codes (it is possible to tune the number of characters by following substr(x, 1, 3))
# In my previous trials I used function(x) substr(x, 1, 2)). I that case below code will be necessary. However for 3 blow line has no effect.
# But I did not changed the code.
df$IPC_LEVEL_1 <- strsplit(df$IPC_corrected, "; ")
df$IPC_LEVEL_1 <- lapply(df$IPC_LEVEL_1, function(x) substr(x, 1, 3)) # Extract first 3 characters # WAS= substr(x, 1, 2)
df$IPC_LEVEL_1 <- lapply(df$IPC_LEVEL_1, unique) # Remove duplicates
df$IPC_LEVEL_1 <- sapply(df$IPC_LEVEL_1, paste, collapse = "; ") # Convert back to a single string
# split all IPC-Level-1 codes into a list
ipc_list_LEV_1 <- unlist(strsplit(df$IPC_LEVEL_1, "; "))
unique(ipc_list_LEV_1)
# Create a table of IPC code counts
ipc_counts_IPC_LEV_1 <- table(ipc_list_LEV_1) # Convert the table to a data frame
df_ipc_counts_IPC_LEV_1 <- as.data.frame.table(ipc_counts_IPC_LEV_1)
names(df_ipc_counts_IPC_LEV_1) <- c("IPC_Code", "Frequency") # Rename the columns


# Split the level 1 IPC codes into a list, sort each list, and collapse back into a string
df$IPC_LEVEL_1 <- sapply(lapply(strsplit(df$IPC_LEVEL_1, "; "), sort), paste, collapse = "; ")
# Calculate frequency of each combination
ipc_LEVEL_1_combinations_freq <- table(df$IPC_LEVEL_1)
# Convert table to a data frame
ipc_LEVEL_1_combinations_freq <- as.data.frame(ipc_LEVEL_1_combinations_freq)
# Rename the columns
names(ipc_LEVEL_1_combinations_freq) <- c("IPC_Combinations", "Frequency")



# CLUSTERING IPC CODES
library(dplyr)
df <- as.data.frame(df)
df <- df %>% select(key, IPC_corrected, IPC_LEVEL_1) #

# K-means clustering
head(df)
library(tidyverse)
library(cluster)
library(factoextra)

# Backup dataframe before separation
df_original <- df

# Separate the IPC_LEVEL_1 column into multiple rows
df <- df %>%
  separate_rows(IPC_LEVEL_1, sep = "; ") 

# Create dummy variables
df_dummy <- df %>%
  mutate(across(starts_with("IPC_LEVEL_1"), ~as.factor(.))) %>%
  pivot_wider(names_from = IPC_LEVEL_1, values_from = IPC_LEVEL_1, 
              values_fill = 0, values_fn = function(x) 1)

# Remove columns that are not needed for clustering (if not removed the next code will not work)
df_dummy <- df_dummy[,!(names(df_dummy) %in% c("key", "IPC_corrected"))]
# save.image("D:/oguz_ozbay/patent missing patents cluster analysis/cluster_analysis_of_all_patents_Jan_2024.RData")

# STEP-1 ( a good number of clusters)----
# trying to find a good number of clusters (elbow method)
system.time({
  set.seed(123)
  # Compute total within-cluster sum of square 
  wss <- sapply(1:25, function(k){
    kmeans(df_dummy, k, nstart=5000, iter.max=1000000000)$tot.withinss
  })
}) # elapsed 105 hours 11/01/2024

# I run above code in workstation, saved it to workstaion than I took it to mu laptop:
# save.image("D:/oguz_ozbay/patent missing patents cluster analysis/cluster_analysis_of_all_patents_Jan_2024.RData") # Workstation

# load("C:/Users/ozbay/OneDrive - XXXX/R ortak/patent IPC clustering/cluster_analysis_of_all_patents_Jan_2024.RData") # Casper
# setwd("D:/OneDrive - XXXX/R ortak/patent IPC clustering") # PC-UME
# load("D:/OneDrive - XXXX/R ortak/patent IPC clustering/cluster_analysis_of_all_patents_Jan_2024.RData") # PC-UME


par(new = TRUE, mar = c(5, 10, 10, 5))
# Plot wss versus the number of clusters
plot(1:25, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares (WSS)")


k_values <- 1:(length(wss) + 1) # Create a vector for the number of clusters (I had used below code for calculating WSS change versus number of clusters)
max_k <- max(k_values) # Define maximum x values for the plot
# Add thin grey vertical lines
for (k in k_values) {
  abline(v = 13, lwd = 0.05, col = "grey") # WAS= v=k
}

# STEP-4 Cluster Number ----
# A good number of cluster (i.e. elbow point) is selected as 13
# Perform clustering

system.time({
  set.seed(123)
  clusters_for_k_13 <- kmeans(df_dummy, centers = 13, nstart=10000, iter.max=1000000000) # denemek için 10.0000 yaptım. Çünkü 100BİN hata verdi (05/02/2024)
  # Add the cluster assignments back to the original dataframe (before separation)
  df_original$cluster <- clusters_for_k_13$cluster
}) # elapsed 265804 / 08.02.2024 = 78 hours (üç gün sürdü bu yzden 10.000'de bırakıyorum)

# save.image("D:/OneDrive - XXXX/R ortak/patent IPC clustering/cluster_analysis_of_all_patents_Jan_2024_V2_K_13.RData")

df_original # key versus cluster no.

# Attention, the following situation happened:
# While analyzing the cluster, I did not delete the duplicate patents in the old patents, that is, "STM patent 13 temmuz_ENVIRONMENT.RData".
# I delete these (i.e. duplicate patents) when doing STM because it distorts the result and also makes it harder to analyze with fingthoughs.
# Also, logically, I can delete them because these are applications for the same patent in different countries.
# As a result, the df_original here is 352957 lines, unless I took a wrong note.
# But the "all patents from 2005 to end of 2023 ready for STM 05_01_2024_V6.RData" data to be used for stm is 342565 lines. But since I defined the key, there is no problem.



###############################################################
# Above code took 3 days so I skip the code below --- START
# system.time({
#   set.seed(123)
#   clusters_for_k_13_100K <- kmeans(df_dummy, centers = 13, nstart=100000, iter.max=1000000000)
#  # Add the cluster assignments back to the original dataframe (before separation)
#  df_original$cluster_100K <- clusters_for_k_13_100K$cluster
# }) # elapsed ??? 11.05.2024
# Above code took 3 days so I skip the code below --- END
###############################################################

cluster_summary <- df_original %>%
  group_by(cluster, IPC_LEVEL_1) %>%
  summarise(freq = n())


