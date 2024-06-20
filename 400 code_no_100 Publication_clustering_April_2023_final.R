


setwd("C:/Users/ozbay/OneDrive - XXXX/R_deneme_feb_2023")

library(dplyr)
library(ggplot2)
library(tm)
library(wordcloud)
library(tidytext)
library(stats)


# START
load("C:/Users/ozbay/OneDrive - XXXX/R_deneme_feb_2023/M_24_02_2023.RData")
WoS_numbers_STM <- M %>% select(UT) # This M is text processed 211585 articles which I used for STM
rm(M)

load("C:/Users/ozbay/OneDrive - XXXX/R_TIK_5_STM/word_preprocess_Haz_2022/united_new_wos_ready_to_use_just_united_not_processed.RData")
# Above imported M is original WoS downloaded articles' data
# Delete All environment except WoS_numbers_STM & M

objects() # show everything in environment
ls() ## or similarly
setdiff(ls(), c("M", "WoS_numbers_STM")) # the setdiff() function shows the difference between two sets
rm(list = setdiff(ls(), c("M", "WoS_numbers_STM")))

#> Explanations of WoS terms
#> SO Publication Name
#> PT Publication Type (J=Journal; B=Book; S=Series; P=Patent)
#> SC Research Areas
#> SE Book Series Title
#> TI Document Title
#> WC Web of Science Categories
#> PG Page Count
#> DE Author Keywords


#> Explanations:
#> Two areas in WOS, one is SC and the other is WC. These are very similar to each other.
#> Research Area information is entered in these two fields.
#> The word groups in this fields separated by ";" in the original text.
#> I tokenized both fields in the original text with R and according to separator (;).
#> Then I cleaned unnecessary white spaces (stringr::str_squish()).
#> Then I exported the SC and WC values to excel.
#> 
#> What I did in Excel:
#>   First I simplified the word groups in SC I deleted the words that may affect the meaning (e.g. "computer science"	to "computer").
#>   I painted the SC data blue in excel. I painted the WC red.
#>   I pasted the SC blow the WC.
#>   Then I sorted rows according to word column (i.e. original token) alphabetically and 
#>   checked if same original tokens (words column) assigned to same replacement (to column) and corrected if necessary.
#>   In this way, I have created a replacement list for SC's and WC's original values (i.e. original tokens)
#>   I will change the SC and WC values of all articles according to the replacement data.
#>   Then I will combine (untidy) SC and WC separately according to replaced values.
#>   Then I will unite SC and WC to a single cell (Call SC_WC_united).
#>   Then make will k-mean clustering to SC_WC_united.
#>   My goal is to create 5-6 clusters (I hope 5-6 will be a good OK in terms of "Optimal Number Of Clusters".
#>   Each artice will have a SC_WC_cluster value.
#>   I think, the topics of the articles will be effected from these clusters. In other words,
#>   I will use this SC_WC_cluster variable as covariate (prevalence) in Structual Topic Model.


M <- M %>% select (UT,SO,PT,SC,SE,TI,DE,WC,PG)

Publication_Group <- WoS_numbers_STM %>% left_join(M)

Publication_Group$SC <- tolower(Publication_Group$SC)
Publication_Group$WC <- tolower(Publication_Group$WC)

Publication_Group$SC <- Publication_Group$SC %>% stringr::str_squish()
Publication_Group$WC <- Publication_Group$WC %>% stringr::str_squish()

SC_WC <- Publication_Group %>% select(UT, SC, WC)

tidy_SC <- Publication_Group %>% unnest_tokens(output= word, input= SC, token = stringr::str_split, pattern = ";")
tidy_SC$word <- tidy_SC$word %>% stringr::str_squish()
tidy_SC %>% count(word, sort = TRUE)
# write to excel:
# tidy_SC %>% count(word, sort = TRUE) %>% openxlsx::write.xlsx("tidy_SC.xlsx")

tidy_WC <- Publication_Group %>% unnest_tokens(output= word, input= WC, token = stringr::str_split, pattern = ";")
tidy_WC$word <- tidy_WC$word %>% stringr::str_squish()
tidy_WC %>% count(word, sort = TRUE)
# write to excel:
# tidy_WC %>% count(word, sort = TRUE) %>% openxlsx::write.xlsx("tidy_WC.xlsx")

# First I will check if there are any rows in which both SC and WC is NA
library(dplyr)
SC_WC_NA <- Publication_Group %>% filter(is.na(SC)) %>% filter(is.na(WC))
# write to excel:
# SC_WC_NA %>% openxlsx::write.xlsx("SC_WC_NA_rows.xlsx")

# I manually defined SC and WC values for NA cells
library(readxl)
SC_WC_NA_rows_new_values <- read_excel("SC_WC_NA_rows.xlsx", 
                                       col_types = c("text", "skip", "skip", "text", "skip", "skip", "skip", "text", "skip"))


Publication_Group$SC_NA_fill <- ""
Publication_Group$WC_NA_fill <- ""
colnames(SC_WC_NA_rows_new_values) # [1] "UT"      "SC_OGUZ" "WC_OGUZ"
Publication_Group <- merge(Publication_Group, SC_WC_NA_rows_new_values, by = "UT", all.x = TRUE)

# Now I will make replacements in SC_NA_fill column. 
# After merge, SC_OGUZ includes replacements (36 rows) and NA (211,585-36=  211.549 rows)
# Below code takes SC_OGUZ value if SC_NA_fill is not NA
Publication_Group$SC_NA_fill <- ifelse(!is.na(Publication_Group$SC_OGUZ), Publication_Group$SC_OGUZ, Publication_Group$SC)
Publication_Group$WC_NA_fill <- ifelse(!is.na(Publication_Group$WC_OGUZ), Publication_Group$WC_OGUZ, Publication_Group$WC)


# Now I will check the replacements - STEP 1
Publication_Group$replace_check_SC_1  <- ""
Publication_Group$replace_check_SC_2  <- ""

Publication_Group$replace_check_SC_1 <- mapply(function(x, y) paste0(setdiff(x, y), collapse = " "), 
                                               strsplit(Publication_Group$SC_NA_fill, '\\s'), strsplit(Publication_Group$SC, '\\s'))

Publication_Group$replace_check_SC_2 <- mapply(function(x, y) paste0(setdiff(x, y), collapse = " "), 
                                               strsplit(Publication_Group$SC, '\\s'), strsplit(Publication_Group$SC_NA_fill, '\\s'))

# Now I will check the replacements - STEP 2
Publication_Group$replace_check_WC_1  <- ""
Publication_Group$replace_check_WC_2  <- ""

Publication_Group$replace_check_WC_1 <- mapply(function(x, y) paste0(setdiff(x, y), collapse = " "), 
                                               strsplit(Publication_Group$WC_NA_fill, '\\s'), strsplit(Publication_Group$WC, '\\s'))

Publication_Group$replace_check_WC_2 <- mapply(function(x, y) paste0(setdiff(x, y), collapse = " "), 
                                               strsplit(Publication_Group$WC, '\\s'), strsplit(Publication_Group$WC_NA_fill, '\\s'))

Publication_Group$SC <- Publication_Group$SC_NA_fill
Publication_Group$WC <- Publication_Group$WC_NA_fill

Publication_Group$SC_NA_fill <- NULL
Publication_Group$WC_NA_fill <- NULL
Publication_Group$SC_OGUZ <- NULL
Publication_Group$WC_OGUZ <- NULL
Publication_Group$replace_check_SC_1 <- NULL
Publication_Group$replace_check_SC_2 <- NULL
Publication_Group$replace_check_WC_1 <- NULL
Publication_Group$replace_check_WC_2 <- NULL

colnames(Publication_Group)
# [1] "UT" "SO" "PT" "SC" "SE" "TI" "DE" "WC" "PG"

#
##
###
####
#####
######
#####
###
##
#

# Now I will tokenize SC and WC again
Publication_Group$SC <- tolower(Publication_Group$SC)
Publication_Group$WC <- tolower(Publication_Group$WC)

Publication_Group$SC <- Publication_Group$SC %>% stringr::str_squish()
Publication_Group$WC <- Publication_Group$WC %>% stringr::str_squish()

library(tidytext)
tidy_SC <- Publication_Group %>% unnest_tokens(output= word, input= SC, token = stringr::str_split, pattern = ";")
tidy_SC$word <- tidy_SC$word %>% stringr::str_squish()
tidy_SC %>% count(word, sort = TRUE)

tidy_WC <- Publication_Group %>% unnest_tokens(output= word, input= WC, token = stringr::str_split, pattern = ";")
tidy_WC$word <- tidy_WC$word %>% stringr::str_squish()
tidy_WC %>% count(word, sort = TRUE)

#
##
###
####
#####
######
#####
###
##
#


library(readxl) # 20 April 2023
tidy_WC_SC_united_replace_list <- read_excel("tidy_WC_SC_united_replace_list_April_2023.xlsx", 
                                             sheet = "united_SC_WC", col_types = c("skip", 
                                                                                   "skip", "text", "skip", "skip", "text", 
                                                                                   "skip", "text", "skip", "skip"))

SC_tidy_replacement_list <- tidy_WC_SC_united_replace_list %>% filter(type == "SC")
SC_tidy_replacement_list$type <- NULL

WC_tidy_replacement_list <- tidy_WC_SC_united_replace_list %>% filter(type == "WC")
WC_tidy_replacement_list$type <- NULL


colnames(SC_tidy_replacement_list) # [1] "from" "to"
colnames(SC_tidy_replacement_list)[1] <- "word"

tidy_SC <- merge(tidy_SC, SC_tidy_replacement_list, by = "word", all.x = TRUE)

#> EXPLANATION: I made tidy_SC tidy according to SC. The name of the column created during this process is "word"
#> Therefore, in tidy_SC, there is a column named "word" instead of the SC column.
#> At the end of the process, the "word" column appears as the first column of tidy_SC. The "to" column was added at the end.
tidy_SC$to <- tidy_SC$to %>% stringr::str_squish()
Untidy_SC <- tidy_SC %>% group_by(UT) %>% summarise(SC_new = paste0(to, collapse = ' '))


colnames(WC_tidy_replacement_list) # [1] "from" "to"
colnames(WC_tidy_replacement_list)[1] <- "word"

tidy_WC <- merge(tidy_WC, WC_tidy_replacement_list, by = "word", all.x = TRUE)
#> At the end of the process, the "word" column appears as the first column of tidy_SC. The "to" column was added at the end.
tidy_WC$to <- tidy_WC$to %>% stringr::str_squish()
Untidy_WC <- tidy_WC %>% group_by(UT) %>% summarise(WC_new = paste0(to, collapse = ' '))


# Now I will manually check the results 
check_1 <- Publication_Group %>% select (UT, SC, WC)
check_final <- merge(check_1, Untidy_SC, by = "UT", all.x = TRUE)
check_final <- merge(check_final, Untidy_WC, by = "UT", all.x = TRUE)
check_final <- check_final %>% relocate(SC_new, .before = WC) # it is OK, or replacements are done, no NA in SC_new or WV_new
rm(check_1)
rm(check_final)


#
##
###
####
#####
###### At this point I have Untidy_SC and Untidy_WC, which I will use for clustering.
###### I will first check tokens.
#####
###
##
#

tokens_Untidy_SC <- Untidy_SC %>%
  unnest_tokens(output= SC_tokens, input= SC_new, token = "words") # %>% add_count(SC_tokens)

tokens_Untidy_SC$SC_tokens <- tokens_Untidy_SC$SC_tokens %>% stringr::str_squish()
SC_cluster_words <- tokens_Untidy_SC %>% count(SC_tokens, sort = TRUE) # it seem OK. I checked in alphabetical order and compared with original SC
#> "pharmacology & pharmacy" are coocuuring; "biomedical & biomedicine" are on different topic, etc.
#> As a reuslt tokens seem OK. No need to replace any of them. And also there are no NAs.

tokens_Untidy_WC <- Untidy_WC %>%
  unnest_tokens(output= WC_tokens, input= WC_new, token = "words") # %>% add_count(SC_tokens)

tokens_Untidy_WC$WC_tokens <- tokens_Untidy_WC$WC_tokens %>% stringr::str_squish()
WC_cluster_words <- tokens_Untidy_WC %>% count(WC_tokens, sort = TRUE)
#> As a result tokens seem OK. No need to replace any of them. And also there are no NAs.

rm(tokens_Untidy_SC) # removal of unnecessary data
rm(tokens_Untidy_WC) # removal of unnecessary data


#
##
###
####
#####
###### Now I will unite  Untidy_SC$SC_new and Untidy_WC$WC_new columns.
#####
###
##
#

clustering_data <- merge(Untidy_SC, Untidy_WC, by = "UT", all.x = TRUE)
any(is.na(clustering_data)) # [1] FALSE
colnames(clustering_data) # [1] "UT"     "SC_new" "WC_new"
library(tidyr)
clustering_data <- unite(data= clustering_data, SC_new, WC_new, col= "united_SC_WC", sep = " ", remove = TRUE, na.rm = FALSE)

# create a document term matrix from the article names
library(tm)
dtm <- DocumentTermMatrix(Corpus(VectorSource(clustering_data$united_SC_WC)))
max(dtm) # [1] 6

# Now I will remoce duplicated terms, and make clustering again (i.e. term frequencies higher than 1 will be reduced to 1)
dtm_max_freq_1 <- as.matrix(dtm)
dtm_max_freq_1[dtm_max_freq_1 != 0] <- 1 # removal of duplication of terms (i.e. term frequencies grater than 1 is reduced to 1 )

max(dtm_max_freq_1) # örnek kod (en büyük maris elemanının görmek için)
# highest_valueS <- unique(sort(dtm, decreasing = TRUE))[1:8]
# highest_valueS <- sort(dtm_original, decreasing = TRUE)[1:1000]


##### Now I will create a new clustering_data because I removed duplications
####
###
##
#
# I will use dtm_max_freq_1 in which max term frequencies is replaced to 1
library(tidyverse)
# Convert the matrix to a dataframe
data_frame_of_dtm_max_freq_1 <- as.data.frame(dtm_max_freq_1)

# Add a column with row numbers to serve as document IDs
data_frame_of_dtm_max_freq_1$document_id <- row.names(data_frame_of_dtm_max_freq_1)
data_frame_of_dtm_max_freq_1$document_id <- as.numeric(data_frame_of_dtm_max_freq_1$document_id)
data_frame_of_dtm_max_freq_1 <- data_frame_of_dtm_max_freq_1 %>% relocate(document_id, .before = aerospace) # <- for ease of understant when open data

# Reshape the dataframe to a long format
long_df <- data_frame_of_dtm_max_freq_1 %>% gather(key = "term", value = "frequency", -document_id)
#>  The gather() is used to reshape a dataframe from a wide format to a long format.
#>  In the context of my document-term matrix, the wide format has one row per document and one column per term.
#>  The goal of reshaping the dataframe to a long format is to create a new dataframe where each row represents 
#>  a unique combination of a document and a term, along with the frequency of that term in the document.

# The breakdown of the code:
# gather(key = "term", value = "frequency", -document_id): This line calls the gather() function to reshape the dataframe. 
# The key parameter specifies the name of the new column that will hold the term names (column names from the original document-term matrix).
# The value parameter specifies the name of the new column that will hold the frequencies.
# -document_id tells the function not to gather the document_id column; it should remain as-is in the reshaped dataframe.
# After running this code, long_df will be a dataframe in long format, with one row per document-term combination, and three columns: document_id, term, and frequency.


#> The value parameter in the gather() function specifies the name of the new column that will hold the frequencies of each term in each document. 
#> In this case, I set value = "frequency", which means that the new reshaped dataframe will have a column named "frequency" containing the term frequencies.
#> When using the gather() function, it is needed to specify which columns wanted to gather into the new long-format dataframe.
#> By default, the gather() function will gather all columns in the input dataframe. 
#> However, I don't want to gather the document_id column because it should remain as a separate column in the reshaped dataframe. 
#> To exclude the document_id column from being gathered, I use -document_id in the function.
#> So, -document_id tells the gather() function to exclude the document_id column from the reshaping process, 
#> and to keep it as a separate column in the output long-format dataframe.
#> After executing the gather() function with these parameters, the resulting long_df dataframe will have the following structure:
#> document_id column: Contains the document IDs (row numbers from the original document-term matrix)
#> term column: Contains the term names (column names from the original document-term matrix)
#> frequency column: Contains the frequencies of each term in each document
#> Each row in the long_df dataframe represents a unique combination of a document and a term, along with the frequency of that term in the document.


# Her doküman için 241 satır ekliyor, 241 satır kelimeler içim (241 çeşit kelime var).
#> Özetle document term matrisinin işl satırını transpoze ediyor, yanına bir sütun ekliyor. Bu sütunun tüm elemanları 1.
#> Sonra doküman term matirsinin ikinci satırını tranpose ediyor, ve yanına bir sütun ekliyor. Bu sütunun elemanları 2. Sonra bu sütunu yukarıda oluşturduğuğınun altına ekliyor.
# İşlem tamamlandığında doküman sayısı x kelime sayısı kadar satılı bir tablo oluşuyor.
# Bu tablonun ilk sütunu doküman numarası 1,1,1,..1 (247 satır); 2,2,2..., 3,3,3... İkinci sütum sürekli 247 kelieme aynı sırada tekar ediyor. 
# Üçüncü sütun ise 1 veya 0. Eğer ki dokümda o kelime gçiyorsa 1.


# Adds 241 lines for each document, 241 lines for words (there are 241 different).
#> In summary, it transposes the first row of the document term matrix and adds a column next to it. All elements of this column are 1.
#> Then, it transposes the second row of the "document term matrix" and adds a column next to it. The elements of this column are 2. Then this column is added below the one created above.
# When the process is completed, a table with the number of rows multiplied by the number of documents and the number of words is generated.
# The first column of this table is document number 1,1,1,..1 (247 rows); 2,2,2..., 3,3,3... Second column constantly repeats 247 words in the same order.
# The third column is 1 or 0. If that word appears in the document, 1.


# Filter out terms with zero frequency
filtered_df <- long_df %>% filter(frequency > 0)

# Expand the rows based on frequency
expanded_df <- filtered_df %>% unnest(n = frequency) # bu kısım aslında şu an için gerekli değil çünkü bende frekanslar en fazla bir.

#> The purpose of the unnest(n = frequency) function is to replicate each row in the filtered_df dataframe based on the frequency value in the "frequency" column. 
#> This way, we essentially expand the dataframe so that each term appears in the dataframe as many times as its frequency in the corresponding document.
#> unnest(n = frequency): This line calls the unnest() function to replicate each row based on the value in the "frequency" column.
#> The n = frequency part tells the function to use the "frequency" column as the number of times each row should be replicated.
#> After running this code, expanded_df will be a new dataframe where each term is repeated as many times as its frequency in the corresponding document.
#> This dataframe will still have the same three columns as filtered_df: document_id, term, and frequency.


# Combine terms for each document
original_text_df <- expanded_df %>% group_by(document_id) %>% summarise(text = paste(term, collapse = " "))

original_text_tibble <- as_tibble(original_text_df)
original_text_tibble$freq_bigger_1 <- clustering_data$united_SC_WC

# Manuel Ckeck for replacements
original_text_tibble$difference <- mapply(function(x, y) paste0(setdiff(x, y), collapse = " "),
                                          strsplit(original_text_tibble$freq_bigger_1, '\\s'), strsplit(original_text_tibble$text, '\\s'))

original_text_tibble$difference_2 <- mapply(function(x, y) paste0(setdiff(x, y), collapse = " "),
                                            strsplit(original_text_tibble$text, '\\s'), strsplit(original_text_tibble$freq_bigger_1, '\\s'))



clustering_data_max_term_freq_1 <- original_text_tibble # In order to give more understandable name.
clustering_data_max_term_freq_1$freq_bigger_1 <- NULL # removal of unnecessary column
clustering_data_max_term_freq_1$difference <- NULL # removal of unnecessary column
clustering_data_max_term_freq_1$difference_2 <- NULL # removal of unnecessary column

colnames(clustering_data) # [1] "UT"           "united_SC_WC"
clustering_data_max_term_freq_1$UT <- clustering_data$UT # To get WoS numbers.
colnames(clustering_data_max_term_freq_1) # [1] "document_id" "text"        "UT"  
names(clustering_data_max_term_freq_1)[2] <- "united_SC_WC"
clustering_data_max_term_freq_1 <- clustering_data_max_term_freq_1 %>% relocate(UT, .before = united_SC_WC) # <- for ease of understanding data
clustering_data_max_term_freq_1$document_id <- NULL

clustering_data_max_term_freq_1 # The domument term matrix in which maximum term frequency is 1 is converter text format, so that
# each row is a document and includes united_SC_WC values of each document in one cell.

# Now I will delete unnecessary data
rm(data_frame_of_dtm_max_freq_1, long_df, expanded_df, original_text_df, original_text_tibble)


# Elbow method
n_clusters <- data.frame()
system.time({
  for(i in 1:25){
    k_check <- kmeans(dtm_max_freq_1, centers = i, iter.max = 1000000000, nstart = 5000)
    print(i)
    n_clusters <- rbind(n_clusters, cbind(i, k_check$tot.withinss))
  }
}) # elapsed 315225 = 87 hours 24/04/2023

names(n_clusters) <- c("cluster", "total")


# plot elbow method graphic
# NOTE: A good number of clusters (k) seems to be ???
plot_WCSS_k <- ggplot(data = n_clusters, aes(x=cluster, y=total, group=1))+ 
  theme_bw(base_family = "Arial")+ geom_line(colour = "darkgreen")+
  scale_x_continuous(breaks = seq(from=0, to=50, by=1)) +
  labs(x = "k (number of clusters)",
       y = NULL, # within groups sum of squares"
       title = "a good number of clusters (k) seems to be 8",
       subtitle = "iter.max = 1.000.000.000 / nstart = 5000 # computation time: aprox 87 hours / dtm max term freq = 1") + 
  theme(text = element_text(size = 7)) +
  # + theme(axis.text = element_text(size = 20))       # Axis text size
  theme(text=element_text(family="serif"))

#> With the above ggplot, WCSS (Within-Cluster Sum of Square) values of 25 clusters obtained with "kmeans(dtm_max_freq_1, centers = i, iter.max = 1000000000, nstart = 5000)" are plotted.
#> According to the Elbow method, the point where there is no significant change in the WCSS value is determined as the optimal number of clusters. 
#> However, there is no elbow point in the graph.
#> As the number of lines in "kmeans(dtm_max_freq_1, centers = i, iter.max = 1000000000, nstart = 3000)" increases, the sharpness of the output curve decreases.
# I wrote the following code to see the Elbow point.
# I determined how much WCSS increased for each number of clusters and plotted the graph.
# Also, I had the WCSS chart drawn according to the number of clusters as above (I had the same chart drawn with different color formats, etc., and I tinkered with ggplot a bit).

df <- n_clusters

n <- nrow(df)
df$diff_2_1 <- numeric(n)
df$diff_2_1[1] <- NA
for (i in 2:n) {
  df$diff_2_1[i] <- df$total[i] - df$total[i - 1]
}

df$diff_1_2 <- numeric(n)
for (i in 1:(n - 1)) {
  df$diff_1_2[i] <- df$total[i] - df$total[i + 1]
}
df$diff_1_2[n] <- NA

colnames(df)
# [1] "cluster"  "total"    "diff_1_2" "diff_2_1"
names(df)[2] <- "WCSS"

df$explanation_1_2 <- character(n)
for (i in 1:(n - 1)) {
  df$explanation_1_2[i] <- paste("WCSS", i, "-", i + 1)
}
df$explanation_1_2[n] <- NA

df$explanation_2_1 <- character(n)
df$explanation_2_1[1] <- NA
for (i in 2:n) {
  df$explanation_2_1[i] <- paste("WCSS", i, "-", i - 1)
}


library(ggplot2)
library(gridExtra)
# the following is not actually needed, I had it drawn to tamper with ggplot
plot_1 <- ggplot(df, aes(x = cluster, y = WCSS)) +
  geom_point(color = "red", shape = "+", size = 5) +
  geom_vline(xintercept = seq(0, max(df$cluster+1), by = 1), linetype = "solid", color = "green", alpha = 0.2) +
  geom_hline(yintercept = seq((min(df$WCSS)-20000), max(df$WCSS), by = 20000), linetype = "solid", color = "red", alpha = 0.2) +
  scale_x_continuous(expand = c(0, 0), breaks = seq(min(df$cluster), max(df$cluster), by = 1)) +
  scale_y_continuous(expand = c(0, 0))  +
  # scale_y_continuous(expand = c(0, 0), breaks = seq(min(df$WCSS), max(df$WCSS), by = 50000))  +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 1, vjust = 0.5),
    axis.text.y = element_text(angle = 0, hjust = 1),
    # panel.grid.major = element_blank(),
    panel.grid.major.x = element_line(color = "yellow", linetype = "solid", linewidth = 0.1), # linetype = "dashed"
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black", linewidth = 0.1),
    panel.border = element_blank(),
    # panel.background = element_blank(),
    plot.background = element_rect(fill = "white"), # fill = "light blue"
    plot.margin = margin(1, 1, 1, 1, "cm"),
    axis.title = element_text(size = 12),
    plot.title = element_text(size = 14, hjust = 0.5, face = "plain", margin = margin(b = 10)), # face = "plain",
    plot.subtitle = element_text(size = 10, hjust = 0.5, face = "bold", margin = margin(b = 10)),
  ) +
  geom_line() +
  labs(x = "Number of clusters", 
       y = "WCSS (Within-Cluster Sum of Square)",
       title = "Variation of WCSS with respect to the number of clusters",
       subtitle = "iter.max = 10xE8 / nstart = 5000 / comput. time: approx. 87 hours/ doc. term matrix max term freq = 1",
       caption = "WCSS (Within-Cluster Sum of Square)") +
  theme(text = element_text(size = 10)) +
  # + theme(axis.text = element_text(size = 20))       # Axis text size
  theme(text=element_text(family="serif")) +
  theme(plot.caption = element_text(size = 10, hjust = 0.5, family = "serif")) # family = "Helvetica"



# Aşağıda WCSS farkların grafiğini çizdirdim.
plot_2 <- ggplot(df, aes(x = explanation_2_1, y = diff_2_1)) +
  geom_line(aes(group = 1), color = "blue") +
  #> geom_line() is trying to connect data points, but there is only one data point for each group. 
  #> This can happen when the x-axis variable is categorical, and there is only one observation for each category.
  #> To adjust the group aesthetic, add group=1 inside aes() in geom_line().
  #> This will group all the data points together and connect them with a line.
  geom_point(color = "red", shape = "+", size= 5) +
  scale_x_discrete(limits = unique(df$explanation_2_1)) +
  # scale_y_continuous(expand = c(0, 0))  +
  theme_minimal() +
  theme(
    axis.line = element_line(color = "black", linewidth = 0.1), # axis.line = element_line(color = "black", size = 0.5) # size is depriciated,
    panel.grid.major.x = element_line(color = "black", linetype = "solid", linewidth = 0.1),
    # panel.grid.minor = element_blank(),
    panel.grid.minor =  element_line(color = "black", linetype = "solid", linewidth = 0.1),
    axis.text.x = element_text(angle = 90, vjust = 0.5),
    axis.text.y = element_text(angle = 0, hjust = 0.5),
    axis.title = element_text(size = 12),
    plot.title = element_text(size = 14, hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    plot.caption = element_text(size = 10, hjust = 0),
    plot.margin = unit(c(1, 1, 1, 1), "cm"),
    plot.background = element_rect(fill = "white")
  ) +
  labs(x = "Explanation 2-1", y = "Diff 2-1", title = "Change of WCSS between consecutive clusters",
       subtitle = "Variations in WCSS differences between each cluster and its succeeding cluster.",
       caption = "WCSS 8-7 represents the change in the WCSS (Within-Cluster Sum of Square) value when the number of clusters is increased from seven to eight.\nSimilarly, WCSS 9-8 represents the change in WCSS when the number of clusters is increased from eight to nine.\nUpon examining the graph, it can be seen that when the number of clusters is increased beyond nine, the amount of change in WCSS decreases.\nTherefore, according to the elbow method, it has been decided that the appropriate number of clusters is 9.")


grid.arrange(plot_1, plot_2, nrow = 2, heights = c(1, 1)) # altlı üstlu çıktı almak için
grid.arrange(plot_WCSS_k, plot_2, nrow = 2, heights = c(1, 1)) # altlı üstlu çıktı almak için


# I decided that 9 cluster seems OK

# NOTE: Then I thought I would take 65000 samples, make 4-5 silhouettes and determine the number of clusters. And I DID THIS.
# BUT the number of clusters was 25 in two examples. When the number of rows (that is, the number of articles) exceeds 65,000, memory is not enough.
# I tried for a week, but I couldn't get any results.

# RESULT: 9 clusters seems suitable.


#
###
####
###
#



# The codes below are to draw word-clouds for the words contained in the clusters.
# Now I will separate my data to 9 clusters.
# Clustering the Research Areas using the k-means algorithm for K=9
set.seed(2023) # for reproducibility
dtm_max_freq_1 # Is the data in which max term frequency higher than 1 is reduced to 1.
system.time({
k9 <- kmeans(dtm_max_freq_1, centers = 9, iter.max = 1000000000, nstart = 10000)
}) # elapsed ???? 


# Now I will create a data frame that includes WoS numbers and cluster No, and cluster terms.
k9_df <- as.data.frame(cbind(clustering_data_max_term_freq_1$UT, clustering_data_max_term_freq_1$united_SC_WC, k9$cluster))
names(k9_df) <- c("UT", "united_SC_WC", "Cluster")


# Now I will assign each cluster to a data frame by using function_for_cluster_filtering
# First I will write a function
function_for_cluster_filtering <- function(clusters_data_frame, cluster_No) {
  cluster_N <- subset(clusters_data_frame, Cluster == cluster_No)
  # clusterFour <- subset(k8_df, Cluster == 4)
  cluster_N_Corpus <- Corpus(VectorSource(cluster_N$united_SC_WC))
  # clusterFour_Corpus <- Corpus(VectorSource(clusterFour$united_SC_WC))
  tdm_N <- TermDocumentMatrix(cluster_N_Corpus, control = list(verbose = FALSE,
                                                               asPlain = FALSE,
                                                               stopwords = FALSE,
                                                               tolower = TRUE,
                                                               removeNumbers = FALSE,
                                                               stemWords = FALSE,
                                                               removePunctuation = FALSE,
                                                               removeSeparators = FALSE,
                                                               stem = FALSE,
                                                               stripWhitespace = FALSE))
  
  matrix_N <- as.matrix(tdm_N)
  vN <- sort(rowSums(matrix_N), decreasing = TRUE)
  dN <- data.frame(word= names(vN), freq=vN)
  return(dN)  
}



d1 <- function_for_cluster_filtering(k9_df, 1) # to get d1 data frame
d2 <- function_for_cluster_filtering(k9_df, 2)
d3 <- function_for_cluster_filtering(k9_df, 3)
d4 <- function_for_cluster_filtering(k9_df, 4)
d5 <- function_for_cluster_filtering(k9_df, 5)
d6 <- function_for_cluster_filtering(k9_df, 6)
d7 <- function_for_cluster_filtering(k9_df, 7)
d8 <- function_for_cluster_filtering(k9_df, 8)
d9 <- function_for_cluster_filtering(k9_df, 9)

# Now let us see the elements of clusters.
# Aşağıdaki gibi yapmazsam _'leri siliyor!...'
# https://www.rdocumentation.org/packages/wordcloud/versions/2.6/topics/wordcloud

function_for_cluster_cloud_plot <- function(clusters_data_frame, cluster_No) {
  cluster_N <- subset(clusters_data_frame, Cluster == cluster_No)
  # clusterFour <- subset(k8_df, Cluster == 4) # this line is an example for what am I doing
  cluster_N_Corpus <- Corpus(VectorSource(cluster_N$united_SC_WC))
  # clusterFour_Corpus <- Corpus(VectorSource(clusterFour$united_SC_WC)) # this line is an example for what am I doing
  
# NOT: Aşağıdaki gibi yapmazsam _'leri siliyor (yani TermDocumentMatrix() ile yapmazsam) !...
# https://www.rdocumentation.org/packages/wordcloud/versions/2.6/topics/wordcloud
  tdm_N <- TermDocumentMatrix(cluster_N_Corpus, control = list(verbose = FALSE,
                                                               asPlain = FALSE,
                                                               stopwords = FALSE,
                                                               tolower = TRUE,
                                                               removeNumbers = FALSE,
                                                               stemWords = FALSE,
                                                               removePunctuation = FALSE,
                                                               removeSeparators = FALSE,
                                                               stem = FALSE,
                                                               stripWhitespace = FALSE))
  
  matrix_N <- as.matrix(tdm_N)
  vN <- sort(rowSums(matrix_N), decreasing = TRUE)
  dN <- data.frame(word= names(vN), freq=vN)
  
  result <- wordcloud(words=dN$word, freq=dN$freq, scale = c(2,0.7), max.words = 20, rot.per = 0, 
                      random.order = FALSE, random.color = F, colors = brewer.pal(8, "Dark2"), 
                      family = "Calibri", font=1, circular = F, fixed.asp = T, min.freq=0)
  return(result)  
} # end of function

#  Let us see terms of clusters
library(wordcloud)
library(tm)
# NOTE: Be suere that all necessary libraries are  started
function_for_cluster_cloud_plot(k9_df, 1) # to plot wordcloud of cluster 1
function_for_cluster_cloud_plot(k9_df, 2)
function_for_cluster_cloud_plot(k9_df, 3)
function_for_cluster_cloud_plot(k9_df, 4)
function_for_cluster_cloud_plot(k9_df, 5)
function_for_cluster_cloud_plot(k9_df, 6)
function_for_cluster_cloud_plot(k9_df, 7)
function_for_cluster_cloud_plot(k9_df, 8)
function_for_cluster_cloud_plot(k9_df, 9)


# Merge the data frames based on the 'word' column
combined_df <- merge(d1, d2, by = "word", all = TRUE)
combined_df <- merge(combined_df, d3, by = "word", all = TRUE)
combined_df <- merge(combined_df, d4, by = "word", all = TRUE)
combined_df <- merge(combined_df, d5, by = "word", all = TRUE)
combined_df <- merge(combined_df, d6, by = "word", all = TRUE)
combined_df <- merge(combined_df, d7, by = "word", all = TRUE)
combined_df <- merge(combined_df, d8, by = "word", all = TRUE)
combined_df <- merge(combined_df, d9, by = "word", all = TRUE)

# View the combined data frame of each cluster and its words
combined_df

colnames(combined_df)

# Rename the columns in the combined data frame using a loop
for(i in 1:9) {
  col_name <- paste0("cluster_", i)
  names(combined_df)[i+1] <- col_name
}

# View the renamed data frame
combined_df




# Below, I wrote a code for automatic ceration of d1, d2 ... d9 data frames
cluster_list <- c(1, 2, 3, 4, 5, 6, 7, 8, 9)
# Apply the function to each cluster number using lapply()
result_list <- lapply(cluster_list, function_for_cluster_filtering, clusters_data_frame = k9_df)
result_list[[1]] # elements of cluster 1
# Merge the resulting data frames based on their "word" column
combined_df_of_clusters_terms <- result_list[[1]]
for (i in 2:length(result_list)) {
  combined_df_of_clusters_terms <- merge(combined_df_of_clusters_terms, result_list[[i]], by = "word", all = TRUE)
}

# Rename the columns in the combined data frame
names(combined_df_of_clusters_terms) <- c("word", paste0("cluster_", cluster_list))

# save.image("C:/Users/ozbay/OneDrive - XXXX/R_deneme_feb_2023/Publication_clustering_April_2023_final_Environment.RData") # 24/04/2023
# save(k9_df, file ="C:/Users/ozbay/OneDrive - XXXX/R_deneme_feb_2023/WoS_per_Cluster_No_April_2023.RData")


