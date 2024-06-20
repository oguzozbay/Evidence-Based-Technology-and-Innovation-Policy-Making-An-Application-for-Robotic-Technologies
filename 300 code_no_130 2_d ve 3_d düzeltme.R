memory.limit()
memory.limit(size = NA)
memory.limit(size=9999999999999)
gc()

library(dplyr)
library(stringi)
library(stringr)

getwd()


# En başta 2-d ve 3-d olanları ayırmamışım. 
# Bunlar d olarak kalmış (yani lemma ve düzeltme işlemleri sonrası 2 ve 3 silinince)

# load("C:/Users/ozbay/OneDrive - XXXX/robot_WOS/STM_2_0_data_preperation_Ngram_replacement_13.RData")

M_processed_01_dec_2022 <- M_final_STM %>% select(UT, PY_corrected, key_TI_AB, key_TI_AB_ngram_12)

colnames(M_final_STM)

# load("C:/Users/ozbay/OneDrive - XXXX/robot_WOS/STM_3_0_data_preperation.RData")

colnames(meta)

M_original_robot_related <- meta %>% select(UT, TI, AB, PY_corrected, ID, DE)

# Environmentdeki gereksiz verileri sildim.

# save.image("C:/Users/ozbay/OneDrive - XXXX/robot_WOS/2_d ve 3_d düzeltme_Environment_1.RData")

# NOT: Kontrol ettim. WOS numaraları aynı (excele attıp iki veriyi de kontrol ettim)


# Şimdi sütunları kontrol edip 2-d ve 3-d var ise bir sütuna 1 yazdıracağım
M_2d_3d <- M_original_robot_related %>% left_join( M_processed_01_dec_2022, by= "UT") # M_2d_3d analize giren tüm WOS'lar
# M_2d_3d analize giren tüm WOS'lar. 2-d/3d düzeltme yapacağım için bu ismi verdim.
# NOT: M_2d_3d$key_TI_AB_ngram_12 EN SON (yani en güncel) Ngram değiştirilmiş sütun. 

M_2d_3d$is_TI_inc_2d <- 0
M_2d_3d$is_TI_inc_3d <- 0
M_2d_3d$is_AB_inc_2d <- 0
M_2d_3d$is_AB_inc_3d <- 0


# save.image("C:/Users/ozbay/OneDrive - XXXX/robot_WOS/2_d ve 3_d düzeltme_Environment_2.RData")


M_2d_3d <- within(M_2d_3d, {is_TI_inc_2d = as.numeric(grepl("2-d ", fixed = FALSE, M_2d_3d$TI))})
M_2d_3d <- within(M_2d_3d, {is_TI_inc_3d = as.numeric(grepl("3-d ", fixed = FALSE, M_2d_3d$TI))})
M_2d_3d <- within(M_2d_3d, {is_AB_inc_2d = as.numeric(grepl("2-d ", fixed = FALSE, M_2d_3d$AB))})
M_2d_3d <- within(M_2d_3d, {is_AB_inc_3d = as.numeric(grepl("3-d ", fixed = FALSE, M_2d_3d$AB))})


colnames(M_2d_3d)
sil <- M_2d_3d %>% select(UT, is_TI_inc_2d, is_TI_inc_3d, is_AB_inc_2d, is_AB_inc_3d)
# openxlsx::write.xlsx(x = sil, file = "2d_3d_içeren_WOS.xlsx") 
rm(sil)


# 2-d ve 3-d geçenlerin excel listesini çekiyorum:
library(readxl)
only_2d <- read_excel("2d_3d_included_WOS.xlsx", sheet = "only_2d") # sadece 2-d geçen metin
only_3d <- read_excel("2d_3d_included_WOS.xlsx", sheet = "only_3d") # sadece 3-d geçen metin
both_2d_3d <- read_excel("2d_3d_included_WOS.xlsx", sheet = "both_2d_3d", col_types = c("text", "skip")) # 2-d ve 3-d birlikte geçen metin
full_list <- read_excel("2d_3d_included_WOS.xlsx", sheet = "full_list") # tamamı, yani içinde en azından 2-d veya 3-d geçen

# save.image("C:/Users/ozbay/OneDrive - XXXX/robot_WOS/2_d ve 3_d düzeltme_Environment_2.RData")
# load("C:/Users/ozbay/OneDrive - XXXX/robot_WOS/2_d ve 3_d düzeltme_Environment_2.RData")

# Önce değişirme yapılamayacak WOS kısmını ayıracağım.
# Yani içinde 2-d yada 3-d geçmeyenler

# STEP-1 
remove_list <- full_list$full_list
M_no_need_to_change <- M_2d_3d[-match(remove_list, M_2d_3d$UT),]
M_no_need_to_change # değiştirilmeyecek WOS numaraları


# STEP-2
# Şimdi sadece 2-d içerenleri çekiyorum
extract_list_2d <- only_2d$only_2d
M_2D_only <- M_2d_3d[match(extract_list_2d, M_2d_3d$UT),] # sadece 2-d içerenler


# STEP-3
# Şimdi sadece 3-d içerenleri çekiyorum
extract_list_3d <- only_3d$only_3d
M_3D_only <- M_2d_3d[match(extract_list_3d, M_2d_3d$UT),] # sadece 3-d içerenler



# STEP-3
# Şimdi sadece 3-d içerenleri çekiyorum
extract_list_both <- both_2d_3d$both_2d_3d
M_botd_2d_3d <- M_2d_3d[match(extract_list_both, M_2d_3d$UT),] # 2-d veya 3-d'den en az birini içerenler

# save.image("C:/Users/ozbay/OneDrive - XXXX/robot_WOS/2_d ve 3_d düzeltme_Environment_2.RData")



# Şimdi ayırdığım data tablolarında 2-d ve 3-d değişikliklerini yapacağım

M_2D_only # key_TI_AB_ngram_12
M_3D_only # key_TI_AB_ngram_12
M_botd_2d_3d # key_TI_AB_ngram_12



# loop 2d ----
Old <- "d"
New <- "2d"
M_2D_only$d_corrected <- "" 
system.time({
  M_2D_only$d_corrected <- stri_replace_all_regex(M_2D_only$key_TI_AB_ngram_12,
                                                           paste0("\\b", Old, "\\b"), 
                                                           paste0(" ", New, " "), vectorize_all = FALSE)
}) 
M_2D_only$d_corrected <- str_squish(M_2D_only$d_corrected)

# loop 3d ----
Old <- "d"
New <- "3d"
M_3D_only$d_corrected <- "" 
system.time({
  M_3D_only$d_corrected <- stri_replace_all_regex(M_3D_only$key_TI_AB_ngram_12,
                                                  paste0("\\b", Old, "\\b"), 
                                                  paste0(" ", New, " "), vectorize_all = FALSE)
}) 
M_3D_only$d_corrected <- str_squish(M_3D_only$d_corrected)


# loop 2d and 3d ----
Old <- "d"
New <- "2d 3d"
M_botd_2d_3d$d_corrected <- "" 
system.time({
  M_botd_2d_3d$d_corrected <- stri_replace_all_regex(M_botd_2d_3d$key_TI_AB_ngram_12,
                                                  paste0("\\b", Old, "\\b"), 
                                                  paste0(" ", New, " "), vectorize_all = FALSE)
}) 
M_botd_2d_3d$d_corrected <- str_squish(M_botd_2d_3d$d_corrected)


# 0000000000000000000000 key_TI_AB sütununu da düzelteyim diyorum 00000000000000000000000000000000000


# loop 2d key_TI_AB ----
Old <- "d"
New <- "2d"
M_2D_only$d_corrected_key_TI_AB <- "" 
system.time({
  M_2D_only$d_corrected_key_TI_AB <- stri_replace_all_regex(M_2D_only$key_TI_AB,
                                                  paste0("\\b", Old, "\\b"), 
                                                  paste0(" ", New, " "), vectorize_all = FALSE)
}) 
M_2D_only$d_corrected_key_TI_AB <- str_squish(M_2D_only$d_corrected_key_TI_AB)


# loop 3d key_TI_AB ----
Old <- "d"
New <- "3d"
M_3D_only$d_corrected_key_TI_AB <- "" 
system.time({
  M_3D_only$d_corrected_key_TI_AB <- stri_replace_all_regex(M_3D_only$key_TI_AB,
                                                  paste0("\\b", Old, "\\b"), 
                                                  paste0(" ", New, " "), vectorize_all = FALSE)
}) 
M_3D_only$d_corrected_key_TI_AB <- str_squish(M_3D_only$d_corrected_key_TI_AB)


# loop 2d and 3d key_TI_AB ----
Old <- "d"
New <- "2d 3d"
M_botd_2d_3d$d_corrected_key_TI_AB <- "" 
system.time({
  M_botd_2d_3d$d_corrected_key_TI_AB <- stri_replace_all_regex(M_botd_2d_3d$key_TI_AB,
                                                     paste0("\\b", Old, "\\b"), 
                                                     paste0(" ", New, " "), vectorize_all = FALSE)
}) 

M_botd_2d_3d$d_corrected_key_TI_AB <- str_squish(M_botd_2d_3d$d_corrected_key_TI_AB)

# save.image("C:/Users/ozbay/OneDrive - XXXX/robot_WOS/2_d ve 3_d düzeltme_Environment_2.RData")



# CORRECTIONS: -------------------------------------------------------------------

M_2d_3d <- M_original_robot_related %>% left_join( M_processed_01_dec_2022, by= "UT") # M_2d_3d analize giren tüm WOS'lar
# M_2d_3d analize giren tüm WOS'lar. 2-d/3d düzeltme yapacağım için bu ismi verdim.

# Şimdi M_2d_3d içinde WOS numaralarına göre düzeltmeleri işleyeceğim

M_2d_3d$key_TI_AB_CORRECTED_d <- M_2d_3d$key_TI_AB
M_2d_3d$key_TI_AB_ngram_12_CORRECTED_d <- M_2d_3d$key_TI_AB_ngram_12
# NOT: M_2d_3d$key_TI_AB_ngram_12 EN SON (yani en güncel) Ngram değiştirilmiş sütun. 



00000000000000000000 ÖRNEK KOD 00000000000000000000000000
data$couples <- replace_list$to[match(data$key, replace_list$key)] # DİKKAT: Eşleşmeyenler için NA yazıyor
# Yukarıdaki kod ne yapıyor? = (data$key == replace_list$key) =>  "replace_list$to" değerini "data$couples" yerine yazıyor.
# AMA BİR SIKINTI VAR. (data$key != replace_list$key) => data$couples değerini NA yapıyor!
# Bu nedenle bir işlem daha yapmak gerekiyor.
data$replaced_OK <- ifelse(is.na(data$couples), data$text, data$couples) # NA ise "text" sütunundaki değeri yazoyor
# Şu anda değişiklik yapılmış oldu = data$replaced_OK sütunu isteiğim sonuç.
# Diğer gereksiz sütunları silebilirim.
# SONRADAN YAZDIĞIM ---- SON ----
00000000000000000000 ÖRNEK KOD / SON 00000000000000000000000000

# AÇIKLAMALAR
M_2D_only$d_corrected_key_TI_AB # key_TI_AB sütununun düzeltilmişi
M_3D_only$d_corrected_key_TI_AB # key_TI_AB sütununun düzeltilmişi
M_botd_2d_3d$d_corrected_key_TI_AB # key_TI_AB sütununun düzeltilmişi

M_2D_only$d_corrected # key_TI_AB_ngram_12 sütununun düzletilmişi
M_3D_only$d_corrected # key_TI_AB_ngram_12 sütununun düzletilmişi
M_botd_2d_3d$d_corrected # key_TI_AB_ngram_12 sütununun düzletilmişi
# AÇIKLAMALAR - SON

# DİKKAT: Ayrı Ayrı işlem yapmak zor oluyor. Bu nedenle düzletme verisini birleştirmekte fayda var
correction_list <- rbind(M_2D_only, M_3D_only, M_botd_2d_3d)
correction_list <- correction_list %>% select(UT, d_corrected_key_TI_AB, d_corrected)



# STEP-1 NGramlı sütunun düzeltilmesi (YANİ: key_TI_AB_ngram_12_CORRECTED_d sütunu)
M_2d_3d$key_TI_AB_ngram_12_CORRECTED_d <- correction_list$d_corrected[match(M_2d_3d$UT, correction_list$UT)] # DİKKAT: Eşleşmeyenler için NA yazıyor
M_2d_3d$key_TI_AB_ngram_12_CORRECTED_d <- ifelse(is.na(M_2d_3d$key_TI_AB_ngram_12_CORRECTED_d), 
                                                 M_2d_3d$key_TI_AB_ngram_12, M_2d_3d$key_TI_AB_ngram_12_CORRECTED_d) # NA olanları key_TI_AB_ngram_12 yaptım



# Now I will check the replacements
M_2d_3d$replace_check_0 <- mapply(function(x, y) paste0(setdiff(x, y), collapse = " "), 
                                      strsplit(M_2d_3d$key_TI_AB_ngram_12_CORRECTED_d, '\\s'), strsplit(M_2d_3d$key_TI_AB_ngram_12, '\\s')) # BU GÖSTERİYOR FARKI
M_2d_3d$replace_check_1 <- mapply(function(x, y) paste0(setdiff(x, y), collapse = " "), 
                                  strsplit(M_2d_3d$key_TI_AB_ngram_12, '\\s'), strsplit(M_2d_3d$key_TI_AB_ngram_12_CORRECTED_d, '\\s'))




# STEP-2 key_TI_AB sütunun düzeltilmesi (key_TI_AB_CORRECTED_d)
M_2d_3d$key_TI_AB_CORRECTED_d  <- correction_list$d_corrected_key_TI_AB[match(M_2d_3d$UT, correction_list$UT)] # DİKKAT: Eşleşmeyenler için NA yazıyor
M_2d_3d$key_TI_AB_CORRECTED_d <- ifelse(is.na(M_2d_3d$key_TI_AB_CORRECTED_d), 
                                                 M_2d_3d$key_TI_AB, M_2d_3d$key_TI_AB_CORRECTED_d) # NA olanları key_TI_AB_ngram_12 yaptım


M_2d_3d$replace_check_2 <- mapply(function(x, y) paste0(setdiff(x, y), collapse = " "), 
                                  strsplit(M_2d_3d$key_TI_AB_CORRECTED_d, '\\s'), strsplit(M_2d_3d$key_TI_AB, '\\s')) # BU GÖSTERİYOR FARKI
M_2d_3d$replace_check_3 <- mapply(function(x, y) paste0(setdiff(x, y), collapse = " "), 
                                  strsplit(M_2d_3d$key_TI_AB, '\\s'), strsplit(M_2d_3d$key_TI_AB_CORRECTED_d, '\\s'))



M_2d_3d_corrected <-M_2d_3d
M_2d_3d_corrected <- M_2d_3d_corrected %>% select (UT, TI, AB, key_TI_AB, key_TI_AB_CORRECTED_d, key_TI_AB_ngram_12, key_TI_AB_ngram_12_CORRECTED_d)

# save.image("C:/Users/ozbay/OneDrive - XXXX/robot_WOS/2_d ve 3_d düzeltme_Environment_3.RData")







