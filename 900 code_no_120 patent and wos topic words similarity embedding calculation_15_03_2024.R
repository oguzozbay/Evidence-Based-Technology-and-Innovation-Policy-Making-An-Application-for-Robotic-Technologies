# I will write a code for comparing topics words's similarities.
# I am thinking to find most similar topics of WOS and Patent Topic Models.

library(stm)
library(dplyr)
library(text2map)
library(tidytext)
library(dplyr)


setwd("C:/Users/ozbay/OneDrive - XXXX/R tez")
# Load updated word embedding:
load("C:/Users/ozbay/OneDrive - XXXX/R topic words similarity/glove_embedding_wiki_xmax_100_new_patents_included.Rdata") # Updated embedding with new patents

# Final STM Model
load("C:/Users/ozbay/OneDrive - XXXX/R_May_2023/STM_Model_May_2023.RData")
STM_robot # Final STM Model

# Final Patent Model
load("C:/Users/ozbay/OneDrive - XXXX/R_patent_2024/Patent_STM_Final_Code_K98_for_thesis_14_03_2024.RData")
STM_patent_K98 # Final Patent Model
rm(out, Patent_docvars, patents_cluster_in_text, read_me_UPDATED_5_patents_cluster_in_text)

# NOTE for THESIS
# NOTE: Skıp above and use following two R data for reproduction: 
# 900 Analysis of Results/STM models WOS and STM.RData
# glove_embedding_wiki_xmax_100_new_patents_included.Rdata
# This data include STM_patent_K98, STM_robot and glove_embedding



# Extract topic words
WOS_labels <- labelTopics(STM_robot, topics=NULL, n=50) 
# str(WOS_labels)
Patent_labels <- labelTopics(STM_patent_K98, topics=NULL, n=50)


############################################# START
# frex_topc_labels <- as.data.frame(topic_labes[["frex"]]) # exracting frex words
# prob_topc_labels <- as.data.frame(topic_labes[["prob"]]) # exracting frex words

# frex_labels <- data.frame(topicnums = topic_labels$topicnums, frex = topic_labels$frex)
# prob_labels <- data.frame(topicnums = topic_labels$topicnums, prob = topic_labels$prob)
############################################# END

# FOR WOS: Extract the values from columns 1 to 25 (or as needed) for each row
WOS_frex <- apply(WOS_labels$frex, 1, function(row) paste(row[1:25], collapse = " "))
# Create the new data frame with topicnums and the extracted frex_words
WOS_frex_united <- data.frame(
  topicnums = WOS_labels$topicnums,
  frex_words = WOS_frex)
rm(WOS_frex)


# FOR WOS: Extract the values from columns 1 to 25 (or as needed) for each row
Patent_frex <- apply(Patent_labels$frex, 1, function(row) paste(row[1:25], collapse = " "))
# Create the new data frame with topicnums and the extracted frex_words
Patent_frex_united <- data.frame(
  topicnums = Patent_labels$topicnums,
  frex_words = Patent_frex)
rm(Patent_frex)

# I will remove Patent frex words that are not available in the word embedding.
# Because while I was running STM model I created new N-Gram that are not available in the word embedding.
# This process is not necessary for WOS data.

# Process rows in Patent_frex_united$frex_words and remove words that do not exist in glove_embedding
for (i in seq_along(Patent_frex_united$frex_words)) {
  # Split the words in the current string
  words_list <- str_split(Patent_frex_united$frex_words[i], " ")[[1]]
  
  # Filter out the words that are not in glove_embedding's row names
  filtered_words <- words_list[words_list %in% rownames(glove_embedding)]
  
  # Combine the remaining words back into a single string
  Patent_frex_united$frex_words[i] <- paste(filtered_words, collapse = " ")
} # Now Patent_frex_united$frex_words is updated, with non-existing words removed from each string


# Now check how many words remainde in Patent FREX words:
count_words <- Patent_frex_united
# Load the stringr library for the str_count function
library(stringr)
# Count the words in each entry of count_words$frex_words
count_words$n_words <- str_count(count_words$frex_words, "\\S+") 
# Now the count_words data frame has an n_words column with the word counts.
# Minimum word count is 19 (in 6 rows)


# I will remove un-necessary words from word-embedding.
all_words <- paste(c(WOS_frex_united$frex_words, Patent_frex_united$frex_words), collapse = " ")
words_list <- strsplit(all_words, " ")[[1]]
unique_topic_words <- unique(words_list)

# Subset glove_embedding to keep only rows corresponding to unique_tokens (for faster process and data size reduction)
reduced_glove_embedding <- glove_embedding[unique_topic_words, , drop = FALSE]


# Replace topic Numbers stored in WOS_frex_united and Patent_frex_united $topicnums columns
WOS_frex_united$topicnums <- paste("WOS_T", WOS_frex_united$topicnums, sep = "")
Patent_frex_united$topicnums <- paste("Patent_T", Patent_frex_united$topicnums, sep = "")

# I will remove General Labeled topics ow WOS and Patents.
# General WOS topics are  12, 19, 28, 30, 44, 45, 51, 72, and 88
# General Patent topics are 9, 17, 18, 23, 30, 63, 88, 89, 91 and 98).

# Define the topics you want to remove
topics_to_remove_W <- c("WOS_T12", "WOS_T19", "WOS_T28", "WOS_T30", 
                      "WOS_T44", "WOS_T45", "WOS_T51", "WOS_T72", 
                      "WOS_T88")
# Remove rows where topicnums matches any of the topics in topics_to_remove
WOS_frex_united <- WOS_frex_united[!WOS_frex_united$topicnums %in% topics_to_remove_W, ]


# Define the topics you want to remove
topics_to_remove_P <- c("Patent_T9", "Patent_T17", "Patent_T18", "Patent_T23", 
                      "Patent_T30", "Patent_T63", "Patent_T88", "Patent_T89", 
                      "Patent_T91", "Patent_T98")
# Remove rows where topicnums matches any of the topics in topics_to_remove
Patent_frex_united <- Patent_frex_united[!Patent_frex_united$topicnums %in% topics_to_remove_P, ]


# İki ayrı DTM girince sonuç vermedi nedense? 
# Ben de çare olarak WOS ile Patent'i alt alta koyuyorum:
# names(frex_patent_united) <- names(frex_united) # Sütun isimleri aynı olması gerekiyor.
combined_WOS_Patent <- rbind(WOS_frex_united, Patent_frex_united) # WOS ve Patent topic words are combined (added below the other)
names(combined_WOS_Patent) <- c("topicnums", "topic_words")

# Create DTM of combined topic of WOS and Patents.
dtm <- combined_WOS_Patent |> dtm_builder(topic_words, doc_id = topicnums) # text2map package's dtm builder
dsm <- doc_similarity(dtm, method = "wmd", wv = reduced_glove_embedding) # for WMD
# dsm <- doc_similarity(dtm, method = "centroid", wv = reduced_glove_embedding) # Cosine centroid


# Now I will keep only WOS vs Patent similarity values
dsm_reduced <- dsm[-(1:82), ]
dsm_reduced <- dsm_reduced[, -(83:170)]
# dsm_reduced <- dsm[-(1:91), -(92:182)]  # to do in one step
# dsm_reduced <- scale(dsm_reduced). This is not necessary


# I will extract for example top 4 similar topics for each WOS topic
# Initialize lists to store the top 4 Patent_T topics and their values for each WOS_T topic
top_4_patents_per_wos <- list()
top_4_values_per_wos <- list()

# Iterate over each column in dsm_reduced
for (col in 1:ncol(dsm_reduced)) {
  column_name <- colnames(dsm_reduced)[col] # WOS_T topic name
  column_values <- dsm_reduced[, col] # Similarity values for this WOS_T topic
  
  # Get indices of the top 4 highest values
  top_4_indices <- order(column_values, decreasing = TRUE)[1:4] # set the number as necessary
  
  # Store the top 4 Patent_T topic names and their values
  top_4_patents_per_wos[[column_name]] <- rownames(dsm_reduced)[top_4_indices]
  top_4_values_per_wos[[column_name]] <- column_values[top_4_indices]
}

# Convert the list of Patent_T topics into a data frame
top_4_patents_df <- do.call(rbind, lapply(top_4_patents_per_wos, function(x) {
  df <- as.data.frame(t(as.character(x)), stringsAsFactors = FALSE)
  colnames(df) <- paste("Top_Patent_T", 1:4, sep = "")
  return(df)
}))
rownames(top_4_patents_df) <- names(top_4_patents_per_wos)

# Convert the list of values into a data frame
top_4_patents_df_values <- do.call(rbind, lapply(top_4_values_per_wos, function(x) {
  df <- as.data.frame(t(x), stringsAsFactors = FALSE)
  colnames(df) <- paste("Top_Value", 1:4, sep = "")
  return(df)
}))
rownames(top_4_patents_df_values) <- names(top_4_values_per_wos)

View(top_4_patents_df)
View(top_4_patents_df_values)

top_4_patents_ORIGINAL <- top_4_patents_df

# I will insert Labels into top_4_patents_df
getwd() # "C:/Users/ozbay/OneDrive - XXXX/R tez"
library(readxl)
labels_for_R_input_WOS <- read_excel("Topic labels for R input.xlsx", sheet = "WOS_Labels") # Use "900 Topic labels for R input.xlsx" for regeration
labels_for_R_input_Patent <- read_excel("Topic labels for R input.xlsx", sheet = "Patent_Labels") # Use "900 Topic labels for R input.xlsx" for regeration

#> Below code will go through each row of top_4_patents_df and replace its name 
#> with the corresponding topic_label from labels_for_R_input_WOS where there's a match on topic_code.

# Iterate over the row names of top_4_patents_df
for (i in 1:nrow(top_4_patents_df)) {
  # Find the matching topic_code in labels_for_R_input_WOS
  matching_label <- labels_for_R_input_WOS$topic_label[which(labels_for_R_input_WOS$topic_code == rownames(top_4_patents_df)[i])]
  
  # Check if there is a matching label and update the row name
  if (length(matching_label) > 0) {
    rownames(top_4_patents_df)[i] <- matching_label
  }
} # Now top_4_patents_df should have its row names updated based on matching topic_code and topic_label from labels_for_R_input_WOS

# Now I will replace Patent Topic_codes with Patent Topic labels.
# Define the columns to update in top_4_patents_df
columns_to_update <- c("Top_Patent_T1", "Top_Patent_T2", "Top_Patent_T3", "Top_Patent_T4")

# Iterate over the specified columns
for (col in columns_to_update) {
  # For each value in the column, find the corresponding label and replace it
  top_4_patents_df[[col]] <- sapply(top_4_patents_df[[col]], function(x) {
    # Find the matching topic_label
    label <- labels_for_R_input_Patent$topic_label[which(labels_for_R_input_Patent$topic_code == x)]
    
    # Replace the value if a match is found, otherwise keep the original value
    if (length(label) > 0) {
      return(label)
    } else {
      return(x)
    }
  })
} # At this point, the relevant columns in top_4_patents_df should have updated values based on the mapping.

# Create a new column "WOS_Topics" in top_4_patents_df containing the row names
top_4_patents_df$WOS_Topic_label <- rownames(top_4_patents_df)
# Move the "WOS_Topics" column to be the first column
top_4_patents_df <- top_4_patents_df[, c(5, 1:4)]

# Rename the 2nd to 5th columns
colnames(top_4_patents_df)[2:5] <- c("Most_Similar_Patent_Label_1st", "Most_Similar_Patent_Label_2nd", 
                                     "Most_Similar_Patent_Label_3rd", "Most_Similar_Patent_Label_4th")



# openxlsx::write.xlsx(x = top_4_patents_df, file = "WOS_Topic_Labels_versus_Most_similar_Patent_Labels_WMD.xlsx")

# Extract row names
top_4_patents_ORIGINAL$WOS_Topic_No <- rownames(top_4_patents_ORIGINAL)
top_4_patents_ORIGINAL <- top_4_patents_ORIGINAL[, c(5, 1:4)]
# openxlsx::write.xlsx(x = top_4_patents_ORIGINAL , file = "WOS_Topic_No_versus_Most_similar_Patent_Topic_No_WMD.xlsx")


