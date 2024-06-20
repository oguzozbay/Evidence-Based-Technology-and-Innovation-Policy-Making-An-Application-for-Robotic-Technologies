# This code is the WMD calculation code.
# I ran this code in parallel processing on the workstation.
# I got results after 2 days.
# Again, in this code, I convert the result of the 2-day process into a data frame.
# (that is, I obtain a data frame named WMD_results_percentile).
# This dataframe I mentioned can be used to plot the WMD value according to years.
# Later, I added "mean novely" to the code. WMD_results_percentile_and_mean

##############################################
# A mail from Dustin S. Stoltz"
# One thing to note: technically WMD is symmetrical, but underneath is an efficient solver which is not always symmetrical. While CMDist takes care of this for you, currently the doc_similarity function used in the attached script does not  -- we are considering updating that soon. But, as you are taking the average similarity, you probably do not need to worry about it.
# Kimden: "Dustin S. Stoltz" <dss219@lehigh.edu>
# Kime: "Oguz. Ozbay" <oguz.ozbay@XXXX>
# Gönderilenler: 29 Kasım Çarşamba 2023 22:41:09
##############################################


# load("C:/Users/ozbay/OneDrive - XXXX/R_Ekim_Concept_Movers/Cited_Titles_2_108_step5.RData") # data of cited titles
# load("C:/Users/ozbay/OneDrive - XXXX/R_TIK_6_GloVe/glove_embedding_wiki_xmax_100.Rdata")

# load("D:/oguz_ozbay/Cited_Titles_2_108_step5.RData") # I used this in work station
# load("D:/oguz_ozbay/glove_embedding_wiki_xmax_100.Rdata") # I used this in work station


# USE BELOW DATA FOR DEMONSTRATION (DEMO DATA):
load("C:/Users/ozbay/OneDrive - XXXX/R tez/R codes for thesis 02 Nisan 2024/Novelty calculation demo data ie WMD calculation demonstration data.RData")

gc()

# Import necessary libraries
library(text2map)
library(text2vec)
library(dplyr)
library(parallel)

test_data <- Cited_Titles_High_freq_deleted
colnames(test_data)
test_data <- test_data %>% select(wos_id, high_freq_deleted)


# I changed the column names to be compatible with the code I wrote before.
test_data <- rename(test_data, WOS_No = wos_id) # (test_data, newName = oldName)
test_data <- rename(test_data, NO_Missing_word = high_freq_deleted) # (test_data, newName = oldName)


# I will reduce my pre-trained embedding (i.e. glove_embedding) size by removing words that do not appear in the current text (i.e. cited titles).
# This way the code runs faster
# Extract unique tokens from test_data$NO_Missing_word
unique_tokens <- unique(unlist(strsplit(tolower(test_data$NO_Missing_word), " ")))

# NOTE: SKIP BELOW LINE WHEN TO USE DEMO DATA
# Subset glove_embedding to keep only rows corresponding to unique_tokens
reduced_glove_embedding <- glove_embedding[unique_tokens, , drop = FALSE] # SKIP THIS IF YOU USE DEMO DATA

# SKIP: I have to exclude articles (i.e., wos_id values) from my data that have only 1 cited title. (I did it before in Cited_Titles_2_108_step5.RData)
# SKIP: NOW I WILL REMOVE WORDS, THOSE DO NOT EXISIT IN MY WORDEMBEDDING, FROM THE TITLES. (I did it before in Cited_Titles_2_108_step5.RData)

sample_data_titles <- test_data # A variable name that I use while testing the code. Afterwards, I continued with the same name so as not to change the code.

# sample_data_titles <- test_data[1:11,]
# Yazarın kodu ile karşılaştırmak için örnek alıyorum
# setwd("C:/Users/ozbay/OneDrive - XXXX/R_Ekim_Concept_Movers/1 UME bilgisayarı R sonucu WMD")
# openxlsx::write.xlsx(x = sample_data_titles, file = "sample_data_titles.xlsx") 



# Import necessary libraries
library(text2map)
library(text2vec)
library(dplyr)


# Tokenize the titles
tokens <- word_tokenizer(tolower(sample_data_titles$NO_Missing_word))
# Create vocabulary
v <- create_vocabulary(itoken(tokens))
# Create vectorizer
vectorizer <- vocab_vectorizer(v)
# Create Document-Term Matrix # NOTE text2map
it <- itoken(tokens)
dtm <- create_dtm(it, vectorizer) #


# Alternative method for dtm creation
# dtm_alternative_method <- sample_data_titles %>% dtm_builder(NO_Missing_word)

################################## NOTE: START
# If it is wanted to apply  TF-IDF weighting, following code can be implemented.
# I don't think it is a good idea because there many titles like: "the pantoscope: a spherical remote-center-of-motion parallel manipulator for force reflection"
# If TF-IDF weighting is used, then term "pantoscope", possibly which has poor word embedding vector, becomes unnecessarily important (If it is still present in the text analyzed. Because it may have been deleted if it is low frequency). 
# Calculate IDF and apply TF-IDF weighting to the DTM
# idf <- log10(nrow(dtm) / colSums(dtm != 0))
# dtm <- dtm * idf # Bu konuyu d?s?nmek gerek. Basliklari tekil yapip idf hesaplamak daha mantikli sanki? Onu nasis yapabilirim?
################################## NOTE: END


# Now I will calculate WMD by Parallel computing
library(parallel)
main_WMDist_within_sample_groups_parallel <- function(dtm, reduced_glove_embedding, sample_data_titles) {
  unique_docs <- unique(sample_data_titles$WOS_No)
  
  # Setting up parallel backend
  # no_cores <- detectCores() - 22  # Adjust based on your system (there are 36 cores in UM PC)
  # cl <- makeCluster(no_cores)
  cl <- makeCluster(12)
  clusterExport(cl, varlist = c("sample_data_titles", "dtm", "reduced_glove_embedding", "doc_similarity", "which")) # OLD VERSION WAS: glove_embedding
  
  # Distribute WMD computation for each WOS_No group in the sample
  result <- parLapply(cl, unique_docs, function(dc) {
    doc_ids <- which(sample_data_titles$WOS_No == dc)
    doc_dtm <- dtm[doc_ids, , drop = FALSE]
    wmd_matrix <- doc_similarity(x = doc_dtm, method = "wmd", wv = reduced_glove_embedding) # OLD VERSION WAS: glove_embedding
    return(wmd_matrix)
  })
  
  stopCluster(cl)
  
  # Combine results into a named list
  names(result) <- unique_docs
  return(result)
}

start_time <- Sys.time()
system.time({
  results_parallel_full <- main_WMDist_within_sample_groups_parallel(dtm, reduced_glove_embedding, sample_data_titles) # OLD VERSION WAS: glove_embedding
}) # elapsed 925.12 for 30 cores / 544.69 for 16 cores / 500.70 for 12 cores / 521.05 for 8 cores / 522 for 14 cores

# elapsed 374.28 for 12 cores when I used reduced_glove_embedding
# elapsed 452.59 in casper (my laptop)

finish_time <- Sys.time()
process_time <- finish_time-start_time # process_time: Time difference of 2.151353 days
# I run above code on the workstation, an ordinary workstation with 128 GB Ram and 36 cores.
# save(results_parallel_full, start_time, finish_time, process_time, file= "D:/oguz_ozbay/results_parallel_full.RData")

##
####
###### I run the cod and get the results ----
####
##

# Check for NA or NaN values in each matrix
results_parallel_full # is the output of parallel processed dat which includes WMD values
any(sapply(results_parallel_full, function(x) any(is.na(x) | is.nan(x)))) # [1] FALSE

# After I got the results. I import the results to my laptop as follows
# load("C:/Users/ozbay/OneDrive - XXXX/R_Ekim_Concept_Movers/1 UME bilgisayarı R sonucu WMD/results_parallel_full.RData")


# Now I will process results_parallel_full data and I will get the WDM values as percentiles. 

# Function to calculate percentiles ----
# In the following function will remoce diagonal values ow each WOS numbers WMD result matrice.
# Because in other case percentile 100 will be 1 for all WOS_No.
# Note that  WMD is not-reversabe I took all pairs in WMD calculation of cited titles references (eg. title 1-1, 1-2, 1-3, 2-1 2-2, 2-3, 3-1, 3-2, 3-3)
# And because of the related function (i.e. doc_similarity(x = doc_dtm, method = "wmd", wv = reduced_glove_embedding)) I did not skip diagonals.
# As a result I will keep 12 and 2- but I have to remove diagonals.

calculate_percentiles_parallel <- function(results_parallel, q_list) {
  result <- list()
  
  for (doc_id in names(results_parallel)) {
    # Extract the matrix for the document
    dist_matrix <- results_parallel[[doc_id]]
    
    # Exclude the diagonal (self-similarity) elements by setting them to NA
    diag(dist_matrix) <- NA
    
    # Flatten the matrix to a vector, excluding NA values
    dist_vector <- as.numeric(dist_matrix)
    dist_vector <- dist_vector[!is.na(dist_vector)]
    
    if (length(dist_vector) > 0) {
      # Calculate the quantiles
      percentiles <- setNames(quantile(dist_vector, probs = q_list / 100, na.rm = TRUE), paste0("q", q_list))
      result[[doc_id]] <- cbind(Document_ID = doc_id, t(percentiles))
    } else {
      # Create NA entries for documents with no data
      result[[doc_id]] <- cbind(Document_ID = doc_id, setNames(rep(NA, length(q_list)), paste0("q", q_list)))
    }
  }
  
  # Combine all the results into one data frame
  result_df <- do.call(rbind, result)
  
  # Convert to a data frame and update column types
  result_df <- as.data.frame(result_df, stringsAsFactors = FALSE)
  result_df$Document_ID <- as.character(result_df$Document_ID)
  
  # Ensure all percentile columns are numeric
  for (q in paste0("q", q_list)) {
    result_df[[q]] <- as.numeric(as.character(result_df[[q]]))
  }
  
  return(result_df)
}

# This is for testing: Apply the function to the first 50 results for testing the code:
# first_50_results <- results_parallel_full[1:50]
# test <- calculate_percentiles_parallel(first_50_results, q_list)

# Specify the percentiles. list
q_list <- c(100, 99, 95, 90, 80, 50)
WMD_results_percentile <- calculate_percentiles_parallel(results_parallel_full, q_list) # elapsed 13 minutes
# save.image("C:/Users/ozbay/OneDrive - XXXX/R_Ekim_Concept_Movers/1 UME bilgisayarı R sonucu WMD/WMD_similarity_results_percentiles.RData")



# 000000000000000000000000000000000000000000000000000000000000000000000000000 START
# 00000000000000 Testing the outputs of WMD calculation Code 0000000000000000
# 000000000000000000000000000000000000000000000000000000000000000000000000000
# I used below code to check the result of WMD calculation code with manual WMD calculation.
# I count an WMD value manually and than compare what value with the result of my code.

# 1. Manual Calculation of WMD for a Single WOS_No Group
#
# If not available import necessary data and crate sample_data_titles and dtm as follows:
# load("C:/Users/ozbay/OneDrive - XXXX/R_Ekim_Concept_Movers/Cited_Titles_2_108_step5.RData")
# load("C:/Users/ozbay/OneDrive - XXXX/R_TIK_6_GloVe/glove_embedding_wiki_xmax_100.Rdata")
# test_data <- Cited_Titles_High_freq_deleted
# test_data <- test_data %>% select(wos_id, high_freq_deleted)
# test_data <- rename(test_data, WOS_No = wos_id) # (test_data, newName = oldName)
# test_data <- rename(test_data, NO_Missing_word = high_freq_deleted) # (test_data, newName = oldName)
# rm(test_data)
# sample_data_titles <- test_data
# Tokenize the titles
# tokens <- word_tokenizer(tolower(sample_data_titles$NO_Missing_word))
# Create vocabulary
# v <- create_vocabulary(itoken(tokens))
# Create vectorizer
# vectorizer <- vocab_vectorizer(v)
# Create Document-Term Matrix # NOTE text2map
# it <- itoken(tokens)
# dtm <- create_dtm(it, vectorizer) #
# Alternative method for dtm creation
# dtm_alternative_method <- sample_data_titles %>% dtm_builder(NO_Missing_word)

# Select a specific WOS_No group, e.g., the first unique WOS_No in my dataset
length(unique(sample_data_titles$WOS_No)) # [1] 200995
specific_wos_no <- unique(sample_data_titles$WOS_No)[1] # try different wos_no 1, 2, 3...
doc_ids_manual <- which(sample_data_titles$WOS_No == specific_wos_no)
# Compute WMD manually for this group
doc_dtm_manual <- dtm[doc_ids_manual, , drop = FALSE]
# wmd_matrix_manual <- doc_similarity(x = doc_dtm_manual, method = "wmd", wv = glove_embedding)
wmd_matrix_manual <- doc_similarity(x = doc_dtm_manual, method = "wmd", wv = reduced_glove_embedding) # For demonstration of thesis Appendix code, I added this line. In fact it is same using glove_embedding. Because reduced_glove_embedding covers all words in the available corpus

# 2. Calculate WMD Using main_WMDist_within_groups
# Calculate WMD using my function
# results_parallel_full <- main_WMDist_within_sample_groups_parallel(dtm, reduced_glove_embedding, sample_data_titles)
# Extract the result for the same WOS_No group
wmd_matrix_function <- results_parallel_full[[specific_wos_no]]

# Compare the two matrices
are_matrices_equal <- all.equal(wmd_matrix_manual, wmd_matrix_function)
print(are_matrices_equal) # [1] TRUE
# 00000000000000 Testing the outputs of WMD calculation Code 0000000000000000 END



#
##
####
#####
######
#######
# I modified code and added "mean novelty" values.
calculate_percentiles_and_mean <- function(results_parallel, q_list) {
  result <- list()
  
  for (doc_id in names(results_parallel)) {
    # Extract the matrix for the document
    dist_matrix <- results_parallel[[doc_id]]
    
    # Exclude the diagonal (self-similarity) elements by setting them to NA
    diag(dist_matrix) <- NA
    
    # Flatten the matrix to a vector, excluding NA values
    dist_vector <- as.numeric(dist_matrix)
    dist_vector <- dist_vector[!is.na(dist_vector)]
    
    if (length(dist_vector) > 0) {
      # Calculate the quantiles
      percentiles <- setNames(quantile(dist_vector, probs = q_list / 100, na.rm = TRUE), paste0("q", q_list))
      # Calculate the mean novelty score (In fact mean_novelty_score is the mean of WMD values. I didn't change the name later to avoid confusion.)
      mean_novelty_score <- mean(dist_vector, na.rm = TRUE)
      # Combine the quantiles and mean novelty score
      result[[doc_id]] <- cbind(Document_ID = doc_id, t(percentiles), mean_Novelty_score = mean_novelty_score)
    } else {
      # Create NA entries for documents with no data
      result[[doc_id]] <- cbind(Document_ID = doc_id, setNames(rep(NA, length(q_list) + 1), c(paste0("q", q_list), "mean_Novelty_score")))
    }
  }
  
  # Combine all the results into one data frame
  result_df <- do.call(rbind, result)
  
  # Convert to a data frame and update column types
  result_df <- as.data.frame(result_df, stringsAsFactors = FALSE)
  result_df$Document_ID <- as.character(result_df$Document_ID)
  result_df$mean_Novelty_score <- as.numeric(as.character(result_df$mean_Novelty_score))
  
  # Ensure all percentile columns are numeric
  for (q in paste0("q", q_list)) {
    result_df[[q]] <- as.numeric(as.character(result_df[[q]]))
  }
  
  return(result_df)
}

# Apply the function to my results_parallel_full data
q_list <- c(100, 99, 95, 90, 80, 50)
# NOTE In fact mean_novelty_score column is "mean of WMD values". I didn't change the name later to avoid confusion.
WMD_results_percentile_and_mean <- calculate_percentiles_and_mean(results_parallel_full, q_list) # NOTE In fact mean_novelty_score column is "mean of WMD values". I didn't change the name later to avoid confusion.
WMD_results_percentile_and_mean$mean_Novelty_score # NOTE In fact mean_novelty_score column is "mean of WMD values". I didn't change the name later to avoid confusion.

# 00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000 START
# 00000000000000 Testing the outputs of NEW WMD calculation Code (includes mean) 0000000000
# 00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
# I used below code to check the result of WMD calculation code with manual WMD calculation.
# I count an WMD value manually and than compare what value with the result of my code.

# TESTING the result of new code:
# simple check: Check if the specified columns are identical in both data frames (new code nd old code)
columns_to_check <- c("Document_ID", "q100", "q99", "q95", "q90", "q80", "q50")
are_data_frames_equal <- all.equal(WMD_results_percentile[columns_to_check], WMD_results_percentile_and_mean[columns_to_check])
print(are_data_frames_equal)


# 1. Manual Calculation of WMD for a Single WOS_No Group (checking the results)
#
# If not available import necessary data and crate sample_data_titles and dtm as folloes:
# load("C:/Users/ozbay/OneDrive - XXXX/R_Ekim_Concept_Movers/Cited_Titles_2_108_step5.RData")
# load("C:/Users/ozbay/OneDrive - XXXX/R_TIK_6_GloVe/glove_embedding_wiki_xmax_100.Rdata")
# test_data <- Cited_Titles_High_freq_deleted
# test_data <- test_data %>% select(wos_id, high_freq_deleted)
# test_data <- rename(test_data, WOS_No = wos_id) # (test_data, newName = oldName)
# test_data <- rename(test_data, NO_Missing_word = high_freq_deleted) # (test_data, newName = oldName)
# rm(test_data)
# sample_data_titles <- test_data
# Tokenize the titles
# tokens <- word_tokenizer(tolower(sample_data_titles$NO_Missing_word))
# Create vocabulary
# v <- create_vocabulary(itoken(tokens))
# Create vectorizer
# vectorizer <- vocab_vectorizer(v)
# Create Document-Term Matrix # NOTE text2map
# it <- itoken(tokens)
# dtm <- create_dtm(it, vectorizer) #
# Alternative method for dtm creation
# dtm_alternative_method <- sample_data_titles %>% dtm_builder(NO_Missing_word)

# STEP -1 Manual check of results
length(unique(sample_data_titles$WOS_No)) # [1] 200995
specific_wos_no <- unique(sample_data_titles$WOS_No)[1] # try different wos_no 1, 2, 3...
doc_ids_manual <- which(sample_data_titles$WOS_No == specific_wos_no)
# Compute WMD manually for this group
doc_dtm_manual <- dtm[doc_ids_manual, , drop = FALSE]
# wmd_matrix_manual <- doc_similarity(x = doc_dtm_manual, method = "wmd", wv = glove_embedding)
wmd_matrix_manual <- doc_similarity(x = doc_dtm_manual, method = "wmd", wv = reduced_glove_embedding) # For demonstration of thesis Appendix code, I added this line. In fact it is same using glove_embedding. Because reduced_glove_embedding covers all words in the available corpus



# 2. Calculate WMD Using main_WMDist_within_groups
# Calculate WMD using my function
# results_parallel_full <- main_WMDist_within_sample_groups_parallel(dtm, reduced_glove_embedding, sample_data_titles)
# Extract the result for the same WOS_No group
wmd_matrix_function <- results_parallel_full[[specific_wos_no]]

# Compare the two matrices
are_matrices_equal <- all.equal(wmd_matrix_manual, wmd_matrix_function)
print(are_matrices_equal) # [1] TRUE

# Function to calculate mean from a WMD matrix
calculate_mean_wmd <- function(wmd_matrix) {
  # Exclude diagonal elements by setting them to NA
  diag(wmd_matrix) <- NA
  
  # Flatten the matrix to a vector, excluding NA values
  flat_vector <- as.numeric(wmd_matrix)
  flat_vector <- flat_vector[!is.na(flat_vector)]
  
  # Calculate and return the mean
  mean(flat_vector, na.rm = TRUE)
}

# Manually calculate mean WMD for a specific WOS_No
mean_wmd_manual <- calculate_mean_wmd(wmd_matrix_manual)

# Calculate mean WMD using the function result for the same WOS_No
mean_wmd_function <- calculate_mean_wmd(wmd_matrix_function)

# Compare the two mean values
are_means_equal <- all.equal(mean_wmd_manual, mean_wmd_function)
print(are_means_equal)
# 00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000 
# 00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000 END


