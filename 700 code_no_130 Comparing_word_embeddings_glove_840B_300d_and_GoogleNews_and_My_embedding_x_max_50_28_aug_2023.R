# 24 August 2023
# I will compare UMAPS of My embedding, Google embedding, and Stanford embedding.

library(dplyr)
library(stringr)
library(stopwords)
library(ggplot2)
library(gridExtra)
library(ggrepel)
library(textTinyR)
library(text2vec)

# Importing my word embeddings
load("C:/Users/ozbay/OneDrive - XXXX/R_TIK_6_GloVe/comparing_embeddings_x_max_20_50_100_with_UMAP_ENVIRONMENT.RData")
#> When I compared three different x-max values of 20, 50, and 100, I did not observe a significant difference. 
#> The word embedding with x-max 50 was selected.
# Environment operations: I will remove all environment except for "df_50", "glove_embedding_xmax_50"
ls() # show everything in environment
setdiff(ls(), c("df_50", "glove_embedding_xmax_50")) # the setdiff() funciton shows the difference between two sets
rm(list = setdiff(ls(), c("df_50", "glove_embedding_xmax_50")))

# Importing "glove_840B_300d_filtered" and "GoogleNews_filtered"     
load("C:/Users/ozbay/OneDrive - XXXX/R_TIK_6_GloVe/UMAP_GoogleNews_and_glove_840B_300d.RData")
glove_840B_300d_filtered # I kept only rows where row-names are entirely lowercase in the original glove_840B_300d (may include _ )
GoogleNews_filtered # I kept only rows where row-names are entirely lowercase in the original GoogleNews (may include _ )

My_embedding <- glove_embedding_xmax_50
rm(glove_embedding_xmax_50)

# I will created a data frame containing words from my STM text both from WOS and Patent that include an underscore.
# Importing patent_text used in STM
load("C:/Users/ozbay/OneDrive - XXXX/R ortak/patent STM 13 temmuz 2023/STM_Patents_Aug_2023_STM_Step.RData")
ls() # show everything in environment
setdiff(ls(), c("df_50","glove_embedding_xmax_50", "df_umap_glove_840B_300d", "df_umap_GoogleNews", "glove_840B_300d_filtered",
                "GoogleNews_filtered", "My_embedding", "STM_text_final_version")) # the setdiff() funciton shows the difference between two sets
rm(list = setdiff(ls(), c("df_50","glove_embedding_xmax_50", "df_umap_glove_840B_300d", "df_umap_GoogleNews", "glove_840B_300d_filtered",
                          "GoogleNews_filtered", "My_embedding", "STM_text_final_version")))

patent_stm_text <- STM_text_final_version %>% select(STM_text)
rm(STM_text_final_version)

# Importing WOS_text used in STM
load("C:/Users/ozbay/OneDrive - XXXX/R_March_2023/M_24_02_2023.RData")
WOS_stm_text <- M %>% select(STM_text_16_02_2023)
rm(M)

text_patents <- patent_stm_text
text_wos <- WOS_stm_text
rm(patent_stm_text, WOS_stm_text)
text_patents <- as.data.frame(text_patents)

names(text_patents) <- "stm_text"
names(text_wos) <- "stm_text"
text_united_STM <- data.frame(rbind(text_patents[1], text_wos[1]))
rm(text_patents, text_wos)


# Now I will extract under_scored words
prefix_extraction <- str_extract_all(text_united_STM$stm_text, "\\b\\w+_\\w+\\b")
df_pref_ext <- data.frame(matrix(unlist(prefix_extraction), 
                                 nrow = sum(sapply(prefix_extraction, length)),
                                 byrow = TRUE), stringsAsFactors=FALSE)
colnames(df_pref_ext)[1] <- "term" 
df_pref_ext  <- df_pref_ext  %>% group_by(term) %>% summarise(number = n()) %>% arrange(desc(number))
# openxlsx::write.xlsx(df_pref_ext, file = "NGrams_of_WOS_and_Patents_STM.xlsx") 
N_Grams <- df_pref_ext %>% filter(number > 10)


# Now I have two steps:
# Replace the underscores in these words with spaces and store this in a new column.
# Check if any words includes stopwords, and record this information.
# Then I will select some of them as stopword to remove from embeddings while comparing the embeddings.

# Create a new column named 'no_underscore'
N_Grams$no_underscore <- gsub("_", " ", N_Grams$term)
# stop_words <- stopwords::stopwords("en")
length(stopwords(source = "smart"))
length(stopwords(source = "snowball"))
length(stopwords(source = "stopwords-iso"))
setdiff(stopwords(source = "smart"), stopwords(source = "snowball"))
# Will remove stopwords. I will use "smart"
stop_words <- stopwords(source = "stopwords-iso")
stop_words <- setdiff(stop_words, c("no", "non", "not"))

# Check if any word in 'no_underscore' column is a stopword
N_Grams$stopword <- sapply(strsplit(N_Grams$no_underscore, " "), function(x) any(x %in% stop_words))
N_Grams <- N_Grams %>% filter (stopword == TRUE)
# openxlsx::write.xlsx(N_Grams, file = "stopword_NGrams.xlsx") 

# I manually select NGram stopwords from stopword_NGrams.xlsx
stopword_NGrams <- read_excel("stopword_NGrams.xlsx", 
                                sheet = "Ngram_stopwords", col_types = c("skip", "text", "skip", "skip", "skip"))
stopword_NGrams <- stopword_NGrams$term # Stopwords extracted and selected manually.

# Now I will create a stopwords list to use in bechmarking.
stop_words <- stopwords(source = "smart") # import original "smart" stopwords
# Below I add some manula word.
stop_words <- c(stop_words, "therefore", "s", "also", "can", "one", "two", "in_this_paper", 
                "thereof", "accord_to", "well", "wherein", "base_on", "first", "second", "thus", "can_be",
                "s", "another", "via", "etc", "many", "whole", "moreover", "however")

stop_words_Final <- unique(c(stop_words, stopword_NGrams))

# Now I will delete stopwords from UMAP data frames and word embeddings.
# save.image("C:/Users/ozbay/OneDrive - XXXX/R_TIK_6_GloVe/Comparing_word_embeddings_glove_840B_300d_and_GoogleNews_and_My_embedding_x_max_50_28_aug_2023_ENVIRONMENT.RData")

# load("C:/Users/ozbay/OneDrive - XXXX/R_TIK_6_GloVe/Comparing_word_embeddings_glove_840B_300d_and_GoogleNews_and_My_embedding_x_max_50_28_aug_2023_ENVIRONMENT.RData")




noSTOP_df_50 <- df_50[!(rownames(df_50) %in% stop_words_Final), , drop = FALSE]
noSTOP_df_840B_300d <- df_umap_glove_840B_300d[!(rownames(df_umap_glove_840B_300d) %in% stop_words_Final), , drop = FALSE]
noSTOP_df_GoogleNews <- df_umap_GoogleNews[!(rownames(df_umap_GoogleNews) %in% stop_words_Final), , drop = FALSE]

NoSTOP_My_embedding <- My_embedding[!(rownames(My_embedding) %in% stop_words_Final), , drop = FALSE]
NoSTOP_glove_840B_300d <- glove_840B_300d_filtered[!(rownames(glove_840B_300d_filtered) %in% stop_words_Final), , drop = FALSE]
NoSTOP_GoogleNews <- GoogleNews_filtered[!(rownames(GoogleNews_filtered) %in% stop_words_Final), , drop = FALSE]

# At This point I have removed my specific stopwords from word embedding matirices and UMAP data frames.
# Below is the function for comparing word embeddings. By using below function results of UMAP can be compared
plot_word_embedding <- function(word_name, embedding_matrix, umap_data, plot_title){
  # STEP-1: Extracting Words close to selected word (cosine similarity)
  word <- embedding_matrix[word_name, , drop = FALSE] 
  cos_sim <- sim2(x = embedding_matrix, y = word, method = "cosine", norm = "l2")
  select <- data.frame(rownames(as.data.frame(head(sort(cos_sim[,1], decreasing = TRUE), 75))))
  colnames(select) <- "word"
  selected_words <- umap_data %>% inner_join(select, by = "word")
  # STEP-2: Plot for selected word in the embedding
  plot <- ggplot(selected_words, aes(x = UMAP1, y = UMAP2, colour = word == word_name)) + 
    geom_point(show.legend = FALSE, size = 0.01) +  # NOTE: size = dot size of words
    scale_color_manual(values = c('black', 'red')) +
    geom_text_repel(aes(UMAP1, UMAP2, label = word), box.padding = 0.4, point.padding = 0.2, size = 2, # NOTE: size = text size of words
                    max.overlaps= 140, segment.size = 0.1, segment.color = 'grey85') +
        labs(title = plot_title) +
    # theme(plot.title = element_text(hjust = .5, size = 14)) + # ORIGINAL
    theme(plot.title = element_text(hjust = .5, size = 5),
          plot.margin = margin(5, 5, 1, 1, "pt"), #top, right, bottom, left
          axis.text.x = element_text(size = 5),  # Adjust this size as needed for x-axis labels
          axis.text.y = element_text(size = 5),
          axis.title.x = element_text(size = 6),  # Adjust the size for x-axis title
          axis.title.y = element_text(size = 6)
          ) + # (top, left, bottom, right ?)
    guides(colour = "none")
  return(plot)
}

# noSTOP_df_50
# noSTOP_df_GoogleNews
# noSTOP_df_840B_300d

# NoSTOP_My_embedding
# NoSTOP_GoogleNews
# NoSTOP_glove_840B_300d

# Generate the plots for a selected word.
plot_My_embedding <- plot_word_embedding("robot", NoSTOP_My_embedding, noSTOP_df_50, "(My_embedding) - Closest 75 words to 'robot'")
plot_GoogleNews <- plot_word_embedding("robot", NoSTOP_GoogleNews, noSTOP_df_GoogleNews, "(GoogleNews) - Closest 75  words to 'robot'")
plot_840B_300d <- plot_word_embedding("robot", NoSTOP_glove_840B_300d, noSTOP_df_840B_300d, "(glove_840B_300d) - Closest 75 words to 'robot'")

plot_My_embedding <- plot_My_embedding +  theme(plot.title = element_text(size = 10)) # resizing "(My_embedding) - Closest words to 'robot'..." 
plot_GoogleNews <- plot_GoogleNews +  theme(plot.title = element_text(size = 10))
plot_840B_300d <- plot_840B_300d +  theme(plot.title = element_text(size = 10))

# Put the results side-by-side for a comparison
library(gridExtra)
grid.arrange(plot_840B_300d,plot_GoogleNews, plot_My_embedding, nrow=2)
gc()


## Function for ploting a list of words -------------
##
##
## Below is the function for automatizing plotting various words:
library(gridExtra)
generate_plot_and_save <- function(word) {
    # Generate the plots
  plot_My_embedding <- plot_word_embedding(word, NoSTOP_My_embedding, noSTOP_df_50, paste0("(My_embedding) - Closest 75 words to '", word, "'"))
  plot_GoogleNews <- plot_word_embedding(word, NoSTOP_GoogleNews, noSTOP_df_GoogleNews, paste0("(GoogleNews) - Closest 75 words to '", word, "'"))
  plot_840B_300d <- plot_word_embedding(word, NoSTOP_glove_840B_300d, noSTOP_df_840B_300d, paste0("(glove_840B_300d) - Closest 75 words to '", word, "'"))
    # Adjust the titles
  plot_My_embedding <- plot_My_embedding +  theme(plot.title = element_text(size = 6))
  plot_GoogleNews <- plot_GoogleNews +  theme(plot.title = element_text(size = 6))
  plot_840B_300d <- plot_840B_300d +  theme(plot.title = element_text(size = 6))
    # Put the results side-by-side for a comparison
  g <- grid.arrange(plot_840B_300d, plot_My_embedding, plot_GoogleNews, ncol=2)
    # Save as PDF
  pdf_filename <- paste0(word, "_comparison.pdf")
  ggsave(filename = pdf_filename, g, width = 11.69, height = 8.27)
}

#> The quality of a word embedding created for the robotics corpus is aimed to be evaluated. 
#> To achieve this, a set of basic words, representative of various domains related to robotics, has been selected.
#> For each of these words, the closest 100 words in the embedding space will be identified and
#> visualized using UMAP (Uniform Manifold Approximation and Projection). 
#> The selected words are intended to span across different sectors, physical structures, autonomy levels, 
#> and relevant scientific domains. This approach is designed to assess how well the word embedding captures 
#> semantic relationships in the field of robotics.
#> Below are the 30 words that have been chosen to span across different sectors and domains related to robotics

# List of words
words_list <- c("actuator",
                "adaptive",
                "aerospace",
                "agriculture",
                "algorithm",
                "autonomous",
                "avionic",
                "bipedal",
                "calibration",
                #    "cobot", # absent in NoSTOP_glove_840B_300d and NoSTOP_GoogleNews
                "collaborative",
                "cybernetic",
                "dynamics",
                "electromechanical",
                #    "endeffector", # absent in NoSTOP_glove_840B_300d and NoSTOP_GoogleNews
                "exoskeleton",
                "fabrication",
                "feedback",
                "firmware",
                "forestry",
                "gripper",
                "haptic",
                "humanoid",
                "inference",
                "integration",
                "intelligence",
                "interface",
                "kinematics",
                "learning",
                "localization",
                "locomotion",
                "manipulator",
                "mapping",
                "mechatronics",
                "microcontroller",
                "modular",
                "navigation",
                "neural",
                #       "neural_network", # absent in NoSTOP_glove_840B_300d and NoSTOP_GoogleNews
                "optimization",
                "orthotic",
                "perception",
                "plc",
                "programming",
                "prosthetic",
                "recognition",
                "reinforcement",
                "robot",
                "sensor",
                "submersible",
                "supervised",
                "swarm",
                "telematics",
                #           "teleoperation", # absent in NoSTOP_GoogleNews
                "tethered",
                "tracked",
                "trajectory",
                "underwater",
                "unsupervised",
                "vision",
                "wheeled")

existence_check <- words_list %in% rownames(NoSTOP_My_embedding)
results <- data.frame(word = words_list, exists = existence_check)
print(results)

existence_check <- words_list %in% rownames(NoSTOP_glove_840B_300d)
results <- data.frame(word = words_list, exists = existence_check)
print(results)

existence_check <- words_list %in% rownames(NoSTOP_GoogleNews)
results <- data.frame(word = words_list, exists = existence_check)
print(results)

# Apply the function to each word in the list
for (word in words_list) {
  generate_plot_and_save(word)
}

gc()



# After analyzing output "generate_plot_and_save" I decided to remove underscores in NoSTOP_GoogleNews, because plots include lots of underscored word.
# It is obvious that, because I will do text preparation before analyses, 
# I will not have any underscored word of NoSTOP_GoogleNews embedding word in my texts.
# For this reason I decidet to remove underscored words from NoSTOP_GoogleNews matrices which I use for comparing.
# Assuming NoSTOP_GoogleNews is a matrix or data frame
rows_to_remove <- grep("_", rownames(NoSTOP_GoogleNews))
NoSTOP_NoUnderscore_GoogleNews <- NoSTOP_GoogleNews[-rows_to_remove, ]
underscore_words <- grep("_", rownames(NoSTOP_GoogleNews), value = TRUE)
# underscore_words <- as.data.frame(underscore_words)
noSTOP_NoUnderscore_df_GoogleNews <- noSTOP_df_GoogleNews[!(rownames(noSTOP_df_GoogleNews) %in% underscore_words), , drop = FALSE]


words_list_1 <- c("actuation", "biomechanics", "cartesian", "dexterity", "encoder", "loop",
                  "genetic", "heuristic", "inertial", "joint", "kinetic", "locomotion", "morphology",
                  "nanobot", "oscillation", "proprioception", "quaternion", "redundancy", "servo",
                  "torque", "virtual", "workspace", "yaw", "gyroscope")


existence_check <- words_list_1 %in% rownames(NoSTOP_My_embedding)
results <- data.frame(word = words_list_1, exists = existence_check)
print(results)

existence_check <- words_list_1 %in% rownames(NoSTOP_glove_840B_300d)
results <- data.frame(word = words_list_1, exists = existence_check)
print(results)

existence_check <- words_list_1 %in% rownames(NoSTOP_NoUnderscore_GoogleNews)
results <- data.frame(word = words_list_1, exists = existence_check)
print(results)


library(gridExtra)
generate_plot_and_save_2 <- function(word) {
  # Generate the plots
  plot_My_embedding <- plot_word_embedding(word, NoSTOP_My_embedding, noSTOP_df_50, paste0("(My_embedding) - Closest 75 words to '", word, "'"))
  plot_GoogleNews <- plot_word_embedding(word, NoSTOP_NoUnderscore_GoogleNews, noSTOP_NoUnderscore_df_GoogleNews, paste0("(GoogleNews_No_) - Closest 75 words to '", word, "'"))
  plot_840B_300d <- plot_word_embedding(word, NoSTOP_glove_840B_300d, noSTOP_df_840B_300d, paste0("(glove_840B_300d) - Closest 75 words to '", word, "'"))
  # Adjust the titles
  plot_My_embedding <- plot_My_embedding +  theme(plot.title = element_text(size = 6))
  plot_GoogleNews <- plot_GoogleNews +  theme(plot.title = element_text(size = 6))
  plot_840B_300d <- plot_840B_300d +  theme(plot.title = element_text(size = 6))
  # Put the results side-by-side for a comparison
  g <- grid.arrange(plot_840B_300d, plot_My_embedding, plot_GoogleNews, ncol=2)
  # Save as PDF
  pdf_filename <- paste0(word, "_comparison.pdf")
  ggsave(filename = pdf_filename, g, width = 11.69, height = 8.27)
}



# Apply the function to each word in the list
for (word in words_list) {
  generate_plot_and_save_2(word)
}


# Apply the function to each word in the list
for (word in words_list_1) {
  generate_plot_and_save_2(word)
}











