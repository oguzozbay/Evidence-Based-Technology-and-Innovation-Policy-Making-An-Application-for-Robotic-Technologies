getwd()
setwd("C:/Users/ozbay/OneDrive - XXXX/R_TIK_6_GloVe")

library(ggplot2)
library(data.table)
filepath <- "C:/Users/ozbay/OneDrive - XXXX/R_TIK_6_GloVe/glove_840B_300d.txt"
# https://nlp.stanford.edu/projects/glove/
# Common Crawl (840B tokens, 2.2M vocab, cased, 300d vectors, 2.03 GB download): glove.840B.300d.zip
glove_840B_300d <- fread(filepath, header = FALSE)
words <- glove_840B_300d$V1
glove_840B_300d <- as.matrix(glove_840B_300d[, -1, with = FALSE])
rownames(glove_840B_300d) <- words
colnames(glove_840B_300d) <- NULL # delete column names

# Import my own word embedding marix (3 word embedding witw x_mas 20, 50 and 100)
# load("C:/Users/ozbay/OneDrive - XXXX/R_TIK_6_GloVe/Glove_Models_Embeddings_Big_Corpus_xmax_100_15_Aug_2023.RData")

filepath_google <- "C:/Users/ozbay/OneDrive - XXXX/R_TIK_6_GloVe/GoogleNews_vectors_negative300.txt"
GoogleNews <- fread(filepath_google, header = FALSE)
words <- GoogleNews$V1
GoogleNews <- as.matrix(GoogleNews[, -1, with = FALSE])
rownames(GoogleNews) <- words
colnames(GoogleNews) <- NULL # delete column names

# Existing word embeddings ar as follows: ----
GoogleNews
glove_840B_300d


#> I convert all words to lowercase when analyzing. 
#> I also tokenize and lemmatize words and strip them of punctuation. 
#> Therefore, words containing only lowercase letters and _ can be useful for analysis.
#> I will reduce size of GoogleNews and glove_840B_300d_filtered by removing above mentioned words.


# STEP -1 I will delete worthless words GoogleNews ----
# Now I will keep  only the words that are all lowercase and may contain underscores but not two or more consecutive underscores. 
# Then, I will filter out any word that contains a period (dot).
# I will keep only rows where row-names are entirely in lowercase (may include _ )
GoogleNews_filtered <- GoogleNews[grepl("^[a-z_]+$", rownames(GoogleNews)),]
# Remove rows with two or more consecutive underscores
GoogleNews_filtered <- GoogleNews_filtered[!grepl("__+", rownames(GoogleNews_filtered)),]
# Remove rows containing any period (dot)
GoogleNews_filtered <- GoogleNews_filtered[!grepl("\\.", rownames(GoogleNews_filtered)),]
dim(GoogleNews_filtered)

# STEP -2 I will delete worthless words glove_840B_300d ----
# Now I will keep  only the words that are all lowercase and may contain underscores but not two or more consecutive underscores. 
# Then, I will filter out any word that contains a period (dot).
# I will keep only rows where row-names are entirely in lowercase (may include _ )
glove_840B_300d_filtered <- glove_840B_300d[grepl("^[a-z_]+$", rownames(glove_840B_300d)),]
# Remove rows with two or more consecutive underscores
glove_840B_300d_filtered <- glove_840B_300d_filtered[!grepl("__+", rownames(glove_840B_300d_filtered)),]
# Remove rows containing any period (dot)
glove_840B_300d_filtered <- glove_840B_300d_filtered[!grepl("\\.", rownames(glove_840B_300d_filtered)),]
dim(glove_840B_300d_filtered)

rm(GoogleNews, glove_840B_300d)


# Visualization of GoogleNews_filtered and glove_840B_300d_filtered
library(umap)
system.time({
umap_GoogleNews <- umap(GoogleNews_filtered, n_components = 2, metric = "cosine", n_neighbors = 25, min_dist = 0.1, spread=2)
save.image("C:/Users/ozbay/OneDrive - XXXX/R_TIK_6_GloVe/UMAP of reduced sized google and stanford glove embeddings.RData")
# GloVe dimension reduction
umap_glove_840B_300d <- umap(glove_840B_300d_filtered, n_components = 2, metric = "cosine", n_neighbors = 25, min_dist = 0.1, spread=2)
save.image("C:/Users/ozbay/OneDrive - XXXX/R_TIK_6_GloVe/UMAP of reduced sized google and stanford glove embeddings.RData")
}) # elapsed 453713 (5,2 days) 24/08/2023


# Put results in a dataframe for ggplot, starting with umap_xmax_100
umap <- umap_GoogleNews
df_x <- as.data.frame(umap$layout, stringsAsFactors = FALSE)
df_x$word <- rownames(df_x)
colnames(df_x) <- c("UMAP1", "UMAP2", "word")
df_umap_GoogleNews <- df_x
rm(umap)
rm(df_x)
df_umap_GoogleNews$definiton <- 'umap_xmax_100'

# Put results in a dataframe for ggplot, umap_xmax_50
umap <- umap_glove_840B_300d
df_x <- as.data.frame(umap$layout, stringsAsFactors = FALSE)
df_x$word <- rownames(df_x)
colnames(df_x) <- c("UMAP1", "UMAP2", "word")
df_umap_glove_840B_300d <- df_x
rm(umap)
rm(df_x)
umap_glove_840B_300d$definiton <- 'umap_xmax_50'



# I will remove all except for below UMAP data frames than save 
df_umap_glove_840B_300d
df_umap_GoogleNews
# save.image("C:/Users/ozbay/OneDrive - XXXX/R_TIK_6_GloVe/UMAP_GoogleNews_and_glove_840B_300d.RData")

