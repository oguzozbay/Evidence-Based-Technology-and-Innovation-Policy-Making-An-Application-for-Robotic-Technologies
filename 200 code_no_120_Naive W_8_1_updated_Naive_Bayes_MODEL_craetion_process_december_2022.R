# I realized that I included 125 "aerosol robotic network" articles in STM analysis. 
# I updated my "related to robotics or not list".
# Now I will update my Naive Bayes Model.
# NOTE: k_fold models results are much more accurate than %70 trainde model.

# load("C:/Users/ozbay/OneDrive - XXXX/robot_WOS/W_8_0_Naive_Bayes_updated_text_process_Environment_update_december_2022.RData")
# setwd("C:/Users/ozbay/OneDrive - XXXX/robot_WOS")

final_data_naive_bayes # ready to use processed data for Naive Bayes MODEL creation (teaching) (ie. "*robot*", Stopwords and single characters deleted)
# I deleted all data except "final_data_naive_bayes"

# save.image("C:/Users/ozbay/OneDrive - XXXX/robot_WOS/W_8_1_Naive_Bayes_MODEL_creation_Environment_updated_december_2022.RData")


### NAIVE BAYES starts here ----
###### W_6_0_data_for_naive_bayes.R was for for text cleaning processes and text pre-processes
#### Now I will create Corpus
# KAYNAK: https://rdrr.io/github/quanteda/quanteda.classifiers/man/crossval.html
# crossval: Cross-validate a fitted textmodel
# https://github.com/quanteda/quanteda.classifiers



colnames(final_data_naive_bayes)
library(quanteda)
Corpus <- corpus(final_data_naive_bayes, docid_field = "UT", text_field = "robot_deleted_no_stopword", unique_docnames = TRUE)
summary(Corpus, 5)


install.packages("remotes")
remotes::install_github("quanteda/quanteda.classifiers")
library("quanteda")
library("quanteda.textmodels")
library("quanteda.classifiers")

# https://rdrr.io/cran/quanteda/f/vignettes/quickstart.Rmd
# TOPIC: Constructing a document-feature matrix

# METHOD-1 k-fold cross validation in naive bayes classifier (to my knowledge better method ) ----

dfmat <- tokens(Corpus) %>% dfm()

Navive_B_text_model_30312_K_fold <- textmodel_nb(dfmat, dfmat$robot_relation)
crossval(Navive_B_text_model_30312_K_fold, k = 10, by_class = TRUE)
crossval(Navive_B_text_model_30312_K_fold, k = 10, by_class = FALSE)
crossval(Navive_B_text_model_30312_K_fold, k = 10, by_class = TRUE, verbose = TRUE)
summary(Navive_B_text_model_30312_K_fold)



Navive_B_text_model_30312_K_fold # 10 fold cross validated Model


# I will use:
Navive_B_text_model_30312_K_fold
# precision    recall        f1  accuracy balanced_accuracy
# NO  0.8630457 0.8584786 0.8656228 0.8696183         0.8630055
# YES 0.9327596 0.9358408 0.9278890 0.9241826         0.9313169
# precision            recall                f1          accuracy balanced_accuracy 
# 0.8980856         0.8953488         0.8982250         0.9000708         0.8976254 


# Below is old model's summary: ie Navive_B_text_model_30187_K_fold
#       precision    recall        f1      accuracy      balanced_accuracy
# NO    0.8665972   0.8592777   0.8680621   0.8729572     0.8608498
# YES   0.9340480   0.9357971   0.9279643   0.9281310     0.9336782


# Below is old model's summary: ie Navive_B_text_model_27536_K_fold
# crossval(Navive_B_text_model_27536_K_fold, k = 10, by_class = TRUE)
#       precision    recall        f1     accuracy      balanced_accuracy
# NO    0.8134669 0.7932275   0.8197107   0.8353691     0.8067815
# YES   0.9235693 0.9277491   0.9145079   0.9090141     0.9277619


# save.image("C:/Users/ozbay/OneDrive - XXXX/robot_WOS/W_8_1_Naive_Bayes_MODEL_creation_Environment_updated_december_2022.RData")



