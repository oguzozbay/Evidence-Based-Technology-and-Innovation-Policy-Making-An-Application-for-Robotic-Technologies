# I will manually check the WMD results. I will extract the reference titles.
# I will manually look the highest and lowest values.


library(dplyr)

load("C:/Users/ozbay/OneDrive - XXXX/R_Ekim_Concept_Movers/Cited_Titles_2_108_step5.RData") # Titles data.

# Below is the results of doc_similarity() function of library(text2map)
load("C:/Users/ozbay/OneDrive - XXXX/R_Ekim_Concept_Movers/1 UME bilgisayarı R sonucu WMD/WMD_and_Cosine_similarity_results.RData")
# NOTE FOR THESIS: Use the following data for reproduction 800 WMD_similarity_results.RData

#########################
# NOTE: DO NOT USE the cosine_similarity in the original Environment (i.ei, WMD_and_Cosine_similarity_results.RData! 
# Because:
# 1. If I remember correctly, I calculated cosine similarty using a method that does not use word embedding (i.e. cosine similarty that only looks at the frequency).
# 2. "text2map" package calculates cosine similarity incorrectly using word embedding (i.e., method= centroid). 
# I mailed with the author of the package for this issue Afterwards the author fixed the code and updated the package.
# i.e doc_similarity(x, y = NULL, method, wv = NULL) # method= centroid (for cosine similarty)
#########################



# I will select only analysed titles
titles <- Cited_Titles_High_freq_deleted %>% select(key, wos_id, high_freq_deleted)

rm(Cited_Titles_High_freq_deleted, high_freq_word_to_delete_Nov_2023, read_me_Environment_Cited_Titles_2_108_step5.RData)

setwd("C:/Users/ozbay/OneDrive - XXXX/R_new WMD Word Movers Distance")




# # There are two codes below. They both basically do the same job. I prepared the second one to take images while writing the thesis:
# CODE-1
# Task 1: Create WMD_q95_descending data frame
WMD_q95_descending <- WMD_results_percentile_and_mean[, c("Document_ID", "q95")]
WMD_q95_descending <- WMD_q95_descending[order(-WMD_q95_descending$q95), ]

# Task 2 (for future use): Calculate the number of cited references for each WOS number and create a new data frame
number_of_cited_ref <- titles %>%
  group_by(wos_id) %>%
  summarise(number_of_cited_ref = n()) %>%
  ungroup()

# Add the number_of_cited_ref column to the existing titles data frame by joining
titles <- titles %>%
  left_join(number_of_cited_ref, by = "wos_id")

# Join this information with the WMD_q95_descending data frame
WMD_q95_descending <- WMD_q95_descending %>%
  left_join(number_of_cited_ref, by = c("Document_ID" = "wos_id"))

# Task 3: Function to extract titles based on the order in WMD_q95_descending
extract_titles_by_order <- function(order) {
  # Get the WOS number from WMD_q95_descending using the specified order (row number)
  wos_number <- WMD_q95_descending$Document_ID[order]
  
  # Extract the titles from the titles data frame corresponding to the WOS number
  titles_subset <- titles[titles$wos_id == wos_number, "high_freq_deleted"]
  
  # return(titles_subset) # just plots titles but do not plot WOS number
  return(list(wos_id = wos_number, titles = titles_subset)) # WOS number added
}

# Example usage:
# Let's say you want to extract titles for the first row in WMD_q95_descending
order <- 200003
extracted_titles <- extract_titles_by_order(2)
extract_titles_by_order(65)
# Print the extracted titles
print(extracted_titles)
# CODE-END


# CODE-2
# Updated function to extract titles and return WOS number and q95 as well
extract_titles_by_order <- function(order) {
  # Get the row from WMD_q95_descending using the specified order (row number)
  selected_row <- WMD_q95_descending[order, ]
  
  # Extract the WOS number and q95 value from the selected row
  wos_number <- selected_row$Document_ID
  q95_value <- selected_row$q95
  
  # Extract the titles from the titles data frame corresponding to the WOS number
  titles_subset <- titles[titles$wos_id == wos_number, "high_freq_deleted"]
  
  # Return a list containing the WOS number, q95 value, and the titles
  return(list(wos_id = wos_number, q95 = q95_value, titles = titles_subset))
}

# Example usage:
extract_titles_by_order(200801)
# CODE-END



# Below code is for calculating the average WMD values for each number of cited references.
# I suspect that documents with low number_of_cited_ref may have poor (i.e. incorrect) q95 value. 
# What I specifically suspect is that if the number_of_cited_ref is low (e.g. 2) the q95 value will be lower.

library(dplyr)
# Group by number_of_cited_ref and calculate average q95 and count of documents for each group
avg_q95_count <- WMD_q95_descending %>%
  group_by(number_of_cited_ref) %>%
  summarize(avg_q95 = mean(q95),
            count = n())

std_dev_q95 <- WMD_q95_descending %>%
  group_by(number_of_cited_ref) %>%
  summarize(std_dev_q95 = sd(q95))

# Scatter plot
plot(avg_q95_count$number_of_cited_ref, avg_q95_count$avg_q95,
     xlab = "Number of Cited References", ylab = "Average q95",
     main = "Scatter Plot of Average q95 vs. Number of Cited References")

# I will extract wos numbers that has 2 citer references for future use
WOS_NO_of_2_cited_ref_documents <- number_of_cited_ref %>% filter (number_of_cited_ref == 2)
# save(WOS_NO_of_2_cited_ref_documents, file = "WOS_NO_of_2_cited_ref_documents_to_remove_from_novelty.Rdata")
# load("C:/Users/ozbay/OneDrive - XXXX/R_new WMD Word Movers Distance/WOS_NO_of_2_cited_ref_documents_to_remove_from_novelty.Rdata")


#############
# q95'in mean_Novelty_score'a oranına bakacağım.
# mean_Novelty_score düşük ise başlıkların çoğu birbirine benzemez demektir.
# q95 küçük ise yine başlıklar birbirine bezemez demektir.
# Ama q95 büyükse birbirine bazeyen başlıklar var demek ama gerisi hakkında bir bilgimiz yok


WMD_results_percentile_and_mean$ratio_q95_to_mean <- WMD_results_percentile_and_mean$q95 / WMD_results_percentile_and_mean$mean_Novelty_score
# I will edit excel and use in my thesis for explaining qualitative analysis
# openxlsx::write.xlsx(x = WMD_results_percentile_and_mean, file = "WMD_results_ve_q95_mean_ratio_FOR_THESIS.xlsx") 

# I will delete rows in the WMD_results_percentile_and_mean, 
# where the Document_ID exists in the WOS_NO_of_2_cited_ref_documents$wos_id
# To remove rows from WMD_results_percentile_and_mean where Document_ID exists in WOS_NO_of_2_cited_ref_documents$wos_id

WMD_results_2_removed <- WMD_results_percentile_and_mean[!WMD_results_percentile_and_mean$Document_ID %in% WOS_NO_of_2_cited_ref_documents$wos_id, ]

# Adding a new column q100_99_95_to_q90_90_50_ratio
# The new column is the ratio of the sum of the 2nd, 3rd, and 4th columns to the sum of the 5th, 6th, and 7th columns for each row
WMD_results_2_removed$q100.99.95_to_q90.90.50_ratio <- (
  rowSums(WMD_results_2_removed[, 2:4]) / 
    rowSums(WMD_results_2_removed[, 5:7])
)

# Yukarıdakilerden bir şey çıkaramadım.Yani q95/mean oranından ve q100.99.95_to_q90.90.50_ratio oranından nitel bir yorum yapmadım.
# I could not deduce anything from the above. So, I did not make a qualitative comment from the q95/mean ratio and the q100.99.95_to_q90.90.50_ratio ratio.







