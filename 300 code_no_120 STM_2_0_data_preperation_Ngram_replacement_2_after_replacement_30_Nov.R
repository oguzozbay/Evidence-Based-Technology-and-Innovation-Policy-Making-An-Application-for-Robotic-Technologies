memory.limit()
memory.limit(size = NA)
memory.limit(size=9999999999999)
gc()

library(dplyr)
library(stringi)
library(stringr)



# Değiştirme için hatalı bir kod yazmıştım ya. Onu düzelttim.
# (NOT: Burada sözünü ettiğim hata öznüne mühim değil. Hatalı değiştirme değil, eksik değiştirme yapıyordu.)
# Sonuç olarak değiştirme sorrası Ngram çıkarttım. O Ngramları kontrol ettim ve excele değiştirme listesi hazrıladım.
# Şimdi son düzenlediğim Ngram değiştirme listesini değiştireceğim.


load("C:/Users/ozbay/OneDrive - XXXX/robot_WOS/STM_2_0_data_preperation_Ngram_replacement_1_CORRECTION_9.RData")

colnames(M_final_STM)

# Gereksiz sütunları siliyorum.

M <- M_final_STM
rm(M_final_STM)
M_final_STM <- M %>% select(UT, PY_corrected, key_TI_AB, key_TI_AB_ngram_9)
rm(M)

N_gram_list_4 #  Aşağıdaki exceli okutmuştum. Şu an Environmentte var zaten
# library(readxl)
# N_gram_list_4 <- read_excel("word_pairs_step4_text2vec_Ngrams_Unite.xlsx", 
#                             sheet = "unite_all")

N_gram_list_4$from <- N_gram_list_4$from %>% stringr::str_squish()
N_gram_list_4$to <- N_gram_list_4$to %>% stringr::str_squish()

Old_4 <- N_gram_list_4$from
New_4 <- N_gram_list_4$to

# Environmenti kaydediyorum. Orijinal metin ve değişiklik listeleri var sadece:
save.image("C:/Users/ozbay/OneDrive - XXXX/robot_WOS/STM_2_0_data_preperation_Ngram_replacement_9_9.RData")


# loop 1.1 ----
M_final_STM$key_TI_AB_ngram_10 <- "" 
system.time({
  M_final_STM$key_TI_AB_ngram_10 <- stri_replace_all_regex(M_final_STM$key_TI_AB_ngram_9,
                                                                       paste0("\\b", Old_4, "\\b"), 
                                                                       paste0(" ", New_4, " "), vectorize_all = FALSE)
}) # elapsed:1500 doğru bursa 

M_final_STM$key_TI_AB_ngram_10 <- gsub(pattern= "OGUZOZBAYDELETE", replacement = " ", M_final_STM$key_TI_AB_ngram_10)
M_final_STM$key_TI_AB_ngram_10 <- str_squish(M_final_STM$key_TI_AB_ngram_10)
save.image("C:/Users/ozbay/OneDrive - XXXX/robot_WOS/STM_2_0_data_preperation_Ngram_replacement_10.RData")



# loop 1.2
M_final_STM$key_TI_AB_ngram_11 <- ""
system.time({
  M_final_STM$key_TI_AB_ngram_11 <- stri_replace_all_regex(M_final_STM$key_TI_AB_ngram_10,
                                                          paste0("\\b", Old_4, "\\b"), 
                                                          paste0(" ", New_4, " "), vectorize_all = FALSE)
}) # elapsed


M_final_STM$key_TI_AB_ngram_11 <- gsub(pattern= "OGUZOZBAYDELETE", replacement = " ", M_final_STM$key_TI_AB_ngram_11)
M_final_STM$key_TI_AB_ngram_11 <- str_squish(M_final_STM$key_TI_AB_ngram_11)
save.image("C:/Users/ozbay/OneDrive - XXXX/robot_WOS/STM_2_0_data_preperation_Ngram_replacement_11.RData")


# loop 1.3
M_final_STM$key_TI_AB_ngram_12 <- ""
system.time({
  M_final_STM$key_TI_AB_ngram_12 <- stri_replace_all_regex(M_final_STM$key_TI_AB_ngram_11,
                                                          paste0("\\b", Old_4, "\\b"), 
                                                          paste0(" ", New_4, " "), vectorize_all = FALSE)
}) # elapsed 

M_final_STM$key_TI_AB_ngram_12 <- gsub(pattern= "OGUZOZBAYDELETE", replacement = " ", M_final_STM$key_TI_AB_ngram_12)
M_final_STM$key_TI_AB_ngram_12 <- str_squish(M_final_STM$key_TI_AB_ngram_12)
save.image("C:/Users/ozbay/OneDrive - XXXX/robot_WOS/STM_2_0_data_preperation_Ngram_replacement_12.RData")




# Değişiklik kontrolü
colnames(M_final_STM)
# Now I will check the replacements
M_final_STM$replace_check_1 <- mapply(function(x, y) paste0(setdiff(x, y), collapse = " "), 
                                      strsplit(M_final_STM$key_TI_AB_ngram_9, '\\s'), strsplit(M_final_STM$key_TI_AB_ngram_12, '\\s')) # BU GÖSTERİYOR FARKI

M_final_STM$replace_check_2 <- mapply(function(x, y) paste0(setdiff(x, y), collapse = " "), 
                                      strsplit(M_final_STM$key_TI_AB_ngram_12, '\\s'), strsplit(M_final_STM$key_TI_AB_ngram_9, '\\s'))


# Şimdi yeniden ngram çıkaracağım.
# şu anda stop words temizlenmemiş durumda
# collocation analysis (n-grams)
library(text2vec)
library(dplyr)

sample_ind = 1:211585
tokens = word_tokenizer(M_final_STM$key_TI_AB_ngram_12[sample_ind]) # 211585 satırı tokenize etti 
it = itoken(tokens, ids = M_final_STM$UT[sample_ind]) # it= An input itoken or itoken_parallel iterator

system.time(v <- create_vocabulary(it))

v = prune_vocabulary(v, term_count_min = 10) # throws out very frequent and very infrequent terms.


# Dikkat!...: Aşağıda pmi_min = 5 yapınca "human robot" dizisini yakalamıyor.
model = Collocations$new(collocation_count_min = 15, pmi_min = 5, sep = "=") # model= A Collocation model object (orijinal 5 ve 5 idi)

# model = Collocations$new(vocabulary = NULL, collocation_count_min = 50, pmi_min = 5, gensim_min = 0, lfmd_min = -Inf, llr_min = 0, sep = "_") # help page definiton
model$fit(it, n_iter = 100)
model$collocation_stat # görmek için. Environemnt içinde model içinden de görebilirsin

it2 = model$transform(it)
v2 = create_vocabulary(it2)
v2 = prune_vocabulary(v2, term_count_min = 5) # v2 kelime dizileri içinde tek kelime olanlar ve alt çizgili olanlar var
# check what phrases model has learned
setdiff(v2$term, v$term) #  setdiff= which elements of a vector or data frame X are not existent in a vector or data frame Y.
v2 <- within(v2,{underline = as.numeric(grepl("=", fixed = FALSE, v2$term))}) # = sayısını sütuna yazdım. =0 ise tek kelimedir.
word_pairs <- v2 %>% filter(underline == 1)


# save.image("C:/Users/ozbay/OneDrive - XXXX/robot_WOS/STM_2_0_data_preperation_Ngram_replacement_13.RData")

# openxlsx::write.xlsx(x = word_pairs, file = "word_pairs_step4.xlsx")
# openxlsx::write.xlsx(x = v2, file = "all_words_and_word_pairs_step4.xlsx"

getwd()

# rm(tokens) # çok yer tutuyor diye sildim
# save.image("C:/Users/ozbay/OneDrive - XXXX/robot_WOS/M_ngram_step3_after_correction.RData")





