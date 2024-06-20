
memory.limit()
memory.limit(size = NA)
memory.limit(size=9999999999999)
gc()



# load("C:/Users/ozbay/OneDrive - XXXX/robot_WOS/M_NoStopword_STM_ready_text_december_2022.RData")


# Sonradan fark ettim ki bazı "ros" "ro" olarak lemmatize edilmiş.
# Bu nedenle orijinali ro olanların wos numarasını bulmaya çalışıyorum 
# Sonradan adam => adams düzeltmesi de yaptım

M$ro_check_AB <- 0
M <- within(M,{ro_check_AB = as.numeric(grepl("\\bro\\b", fixed = FALSE, M$AB))})


M$ro_check_TI <- 0
M <- within(M,{ro_check_TI = as.numeric(grepl("\\bro\\b", fixed = FALSE, M$TI))})

sil <- M %>% select(UT, TI, AB, ro_check_AB, ro_check_TI)

sil$sum <- sil$ro_check_AB + sil$ro_check_TI
ro_WOS <- sil %>% filter(sum > 0)

openxlsx::write.xlsx(x = ro_WOS_no, file = "ro_WOS_no.xlsx")



ro_WOS_no <- ro_WOS %>% select(UT)

M_ro_WOS <- left_join(M, )

M_ro_WOS <- merge(x=ro_WOS_no, y=M, by="UT", all.x=FALSE) # orijinali "ro" olan WOS numaraları


# DİKKAT: 
# İlk önce aşağıdaki düzeltmeler yapılacak
# "ro botic" => "robotic"
# "ro bot" => "robot"


# STEP-1
colnames(M_ro_WOS)
colnames(M)

M_ro_WOS$key_TI_AB_CORRECTED_d <- gsub(pattern= "\\bro botic\\b", replacement = "robotic", 
                                          M_ro_WOS$key_TI_AB_CORRECTED_d)
M_ro_WOS$key_TI_AB_CORRECTED_d <- gsub(pattern= "\\bro bot\\b", replacement = "robot", 
                                          M_ro_WOS$key_TI_AB_CORRECTED_d)

# STEP-2
M_ro_WOS$STM_text <- gsub(pattern= "\\bro botic\\b", replacement = "robotic", 
                                          M_ro_WOS$STM_text)
M_ro_WOS$STM_text <- gsub(pattern= "\\bro bot\\b", replacement = "robot", 
                                          M_ro_WOS$STM_text)

# STEP-3
M_ro_WOS$STM_no_stop_text <- gsub(pattern= "\\bro botic\\b", replacement = "robotic", 
                          M_ro_WOS$STM_no_stop_text)
M_ro_WOS$STM_no_stop_text <- gsub(pattern= "\\bro bot\\b", replacement = "robot", 
                          M_ro_WOS$STM_no_stop_text)




# STEP-4
# orijina "ro" olanları "ro_OGUZOZBAY_CORRECT" olarak değiştireceğim.
# Daha sonra ana metinde "ro" olanrı "ros" olarak deiştireceğim.
# En sonra "ro_OGUZOZBAY_CORRECT" olanları geriye "ro" olarak düzleteceğim.
# Böylece orijinali "ros" olup da "ro" olarak lemmatize ettiklerimi düzeltmiş olacağım. 

M_ro_WOS$key_TI_AB_CORRECTED_d_ro <- gsub(pattern= "\\bro\\b", replacement = "ro_OGUZOZBAY_CORRECT", 
                                          M_ro_WOS$key_TI_AB_CORRECTED_d)


# STEP-4.1
colnames(M_ro_WOS)
colnames(M)

M_ro_WOS$key_TI_AB_CORRECTED_d <- gsub(pattern= "\\bro\\b", replacement = "ro_OGUZOZBAY_CORRECT", 
                                       M_ro_WOS$key_TI_AB_CORRECTED_d)

# STEP-4.2
M_ro_WOS$STM_text <- gsub(pattern= "\\bro\\b", replacement = "ro_OGUZOZBAY_CORRECT", 
                          M_ro_WOS$STM_text)

# STEP-4.3
M_ro_WOS$STM_no_stop_text <- gsub(pattern= "\\bro\\b", replacement = "ro_OGUZOZBAY_CORRECT", 
                                  M_ro_WOS$STM_no_stop_text)


# openxlsx::write.xlsx(x = M_ro_WOS, file = "M_ro_WOS_replaced_original_ro_december_2022.xlsx")


# M_ro_WOS içinde düzeltimiş metinler var.
# Bu metinler düzeltme için kullanılacak
# M ve M_ro_Wos hariç tüm environmenti sildim
# save.image("C:/Users/ozbay/OneDrive - XXXX/robot_WOS/CORRECTION for ros finding ro ENVIRONMENT.R.RData")


0000000000000000000000000000000000000

# Hücreyi olduğu gibi değiştiren kod

# STEP-1 (correction of key_TI_AB_CORRECTED_d column)
# Caution: This code converts mismatches to NA. Therefore, it is necessary to replace the NA ones with the original abstract.
M_ro_WOS$UT # replace list
colnames(M)         # ... "key_TI_AB_CORRECTED_d" "STM_text" "STM_no_stop_text" "ro_check_AB" "ro_check_TI"
colnames(M_ro_WOS)  # ... "key_TI_AB_CORRECTED_d" "STM_text" "STM_no_stop_text" "ro_check_AB" "ro_check_TI"          

M$key_TI_AB_14_december <- M_ro_WOS$key_TI_AB_CORRECTED_d[match(M$UT, M_ro_WOS$UT)]
M <- M %>% relocate(key_TI_AB_14_december, .after = key_TI_AB_CORRECTED_d) # for the ease of checking
M$key_TI_AB_14_december <- ifelse(is.na(M$key_TI_AB_14_december), M$key_TI_AB_CORRECTED_d, M$key_TI_AB_14_december) # 
# if $key_TI_AB_14_december is NA, then NA is changed to "key_TI_AB_CORRECTED_d" value
M$key_TI_AB_14_december <- str_squish(M$key_TI_AB_14_december)

# save.image("C:/Users/ozbay/OneDrive - XXXX/robot_WOS/CORRECTION for ros finding ro ENVIRONMENT.RData")



M$replace_check_1 <- mapply(function(x, y) paste0(setdiff(x, y), collapse = " "), 
                            strsplit(M$key_TI_AB_CORRECTED_d, '\\s'), strsplit(M$key_TI_AB_14_december, '\\s')) 
M$replace_check_2 <- mapply(function(x, y) paste0(setdiff(x, y), collapse = " "), 
                            strsplit(M$key_TI_AB_14_december, '\\s'), strsplit(M$key_TI_AB_CORRECTED_d, '\\s')) 

# Aşağıdaki kelime düzeltilecek (silinecek)

# "er edu.ro_OGUZOZBAY_CORRECT.co"

M$key_TI_AB_14_december <- gsub(pattern= "\\ber edu.ro_OGUZOZBAY_CORRECT.co\\b", replacement = " ", M$key_TI_AB_14_december)
M$key_TI_AB_14_december <- str_squish(M$key_TI_AB_14_december)

# save.image("C:/Users/ozbay/OneDrive - XXXX/robot_WOS/CORRECTION for ros finding ro ENVIRONMENT.RData")


# STEP-2 (correction of STM_text column)
colnames(M)         # ... "key_TI_AB_CORRECTED_d" "STM_text" "STM_no_stop_text" "ro_check_AB" "ro_check_TI"
colnames(M_ro_WOS)  # ... "key_TI_AB_CORRECTED_d" "STM_text" "STM_no_stop_text" "ro_check_AB" "ro_check_TI"          

M$STM_text_14_december <- M_ro_WOS$STM_text[match(M$UT, M_ro_WOS$UT)]
M <- M %>% relocate(STM_text_14_december, .after = STM_text) # for the ease of checking
M$STM_text_14_december <- ifelse(is.na(M$STM_text_14_december), M$STM_text, M$STM_text_14_december)
# if $STM_text_14_december is NA, then NA is changed to "STM_text" value
M$STM_text_14_december <- str_squish(M$STM_text_14_december)

M$replace_check_1 <- NULL
M$replace_check_2 <- NULL

M$replace_check_1 <- mapply(function(x, y) paste0(setdiff(x, y), collapse = " "), 
                            strsplit(M$STM_text, '\\s'), strsplit(M$STM_text_14_december, '\\s')) 
M$replace_check_2 <- mapply(function(x, y) paste0(setdiff(x, y), collapse = " "), 
                            strsplit(M$STM_text_14_december, '\\s'), strsplit(M$STM_text, '\\s')) 


M$STM_text_14_december <- gsub(pattern= "\\ber edu.ro_OGUZOZBAY_CORRECT.co\\b", replacement = " ", M$STM_text_14_december)
M$STM_text_14_december <- str_squish(M$STM_text_14_december)

# save.image("C:/Users/ozbay/OneDrive - XXXX/robot_WOS/CORRECTION for ros finding ro ENVIRONMENT.RData")



# STEP-3 (correction of STM_no_stop_text column)
STM_text
colnames(M)         # ... "key_TI_AB_CORRECTED_d" "STM_text" "STM_no_stop_text" "ro_check_AB" "ro_check_TI"
colnames(M_ro_WOS)  # ... "key_TI_AB_CORRECTED_d" "STM_text" "STM_no_stop_text" "ro_check_AB" "ro_check_TI"          

M$STM_no_stop_text_14_december <- M_ro_WOS$STM_no_stop_text[match(M$UT, M_ro_WOS$UT)]
M <- M %>% relocate(STM_no_stop_text_14_december, .after = STM_no_stop_text) # for the ease of checking
M$STM_no_stop_text_14_december <- ifelse(is.na(M$STM_no_stop_text_14_december), M$STM_no_stop_text, M$STM_no_stop_text_14_december) # 
# if $STM_text_14_december is NA, then NA is changed to "STM_text" value
M$STM_no_stop_text_14_december <- str_squish(M$STM_no_stop_text_14_december)

M$replace_check_1 <- NULL
M$replace_check_2 <- NULL

M$replace_check_1 <- mapply(function(x, y) paste0(setdiff(x, y), collapse = " "), 
                            strsplit(M$STM_no_stop_text, '\\s'), strsplit(M$STM_no_stop_text_14_december, '\\s')) 
M$replace_check_2 <- mapply(function(x, y) paste0(setdiff(x, y), collapse = " "), 
                            strsplit(M$STM_no_stop_text_14_december, '\\s'), strsplit(M$STM_no_stop_text, '\\s')) 


M$STM_no_stop_text_14_december <- gsub(pattern= "\\ber edu.ro_OGUZOZBAY_CORRECT.co\\b", replacement = " ", M$STM_no_stop_text_14_december)
M$STM_no_stop_text_14_december <- str_squish(M$STM_no_stop_text_14_december)


# save.image("C:/Users/ozbay/OneDrive - XXXX/robot_WOS/CORRECTION for ros finding ro ENVIRONMENT.RData")


# NOW I will replace "ro" to "ros" -----
colnames(M) # .... key_TI_AB_14_december STM_text_14_december STM_no_stop_text

# STEP 1- (replacement of "ro" to "ros" in key_TI_AB_14_december column)
M$key_TI_AB_ROS_14_dec <- gsub(pattern= "\\bro\\b", replacement = "ros", M$key_TI_AB_14_december)
M$key_TI_AB_ROS_14_dec <- str_squish(M$key_TI_AB_ROS_14_dec)
M$replace_check_1 <- NULL
M$replace_check_2 <- NULL

M$replace_check_1 <- mapply(function(x, y) paste0(setdiff(x, y), collapse = " "), 
                            strsplit(M$key_TI_AB_14_december, '\\s'), strsplit(M$key_TI_AB_ROS_14_dec, '\\s')) 
M$replace_check_2 <- mapply(function(x, y) paste0(setdiff(x, y), collapse = " "), 
                            strsplit(M$key_TI_AB_ROS_14_dec, '\\s'), strsplit(M$key_TI_AB_14_december, '\\s')) 

M <- M %>% relocate(key_TI_AB_ROS_14_dec, .after = key_TI_AB_14_december) # for the ease of checking


# STEP 2- (replacement of "ro" to "ros" in STM_text_14_december column)
M$STM_text_ROS_14_dec <- gsub(pattern= "\\bro\\b", replacement = "ros", M$STM_text_14_december)
M$STM_text_ROS_14_dec <- str_squish(M$STM_text_ROS_14_dec)
M$replace_check_1 <- NULL
M$replace_check_2 <- NULL

M$replace_check_1 <- mapply(function(x, y) paste0(setdiff(x, y), collapse = " "), 
                            strsplit(M$STM_text_14_december, '\\s'), strsplit(M$STM_text_ROS_14_dec, '\\s')) 
M$replace_check_2 <- mapply(function(x, y) paste0(setdiff(x, y), collapse = " "), 
                            strsplit(M$STM_text_ROS_14_dec, '\\s'), strsplit(M$STM_text_14_december, '\\s')) 

M <- M %>% relocate(STM_text_ROS_14_dec, .after = STM_text_14_december) # for the ease of checking


# STEP 3 - (replacement of "ro" to "ros" in STM_no_stop_text_14_december column)
M$STM_no_stop_ROS_14_dec <- gsub(pattern= "\\bro\\b", replacement = "ros", M$STM_no_stop_text_14_december)
M$STM_no_stop_ROS_14_dec <- str_squish(M$STM_no_stop_ROS_14_dec)
M$replace_check_1 <- NULL
M$replace_check_2 <- NULL

M$replace_check_1 <- mapply(function(x, y) paste0(setdiff(x, y), collapse = " "), 
                            strsplit(M$STM_no_stop_text_14_december, '\\s'), strsplit(M$STM_no_stop_ROS_14_dec, '\\s')) 
M$replace_check_2 <- mapply(function(x, y) paste0(setdiff(x, y), collapse = " "), 
                            strsplit(M$STM_no_stop_ROS_14_dec, '\\s'), strsplit(M$STM_no_stop_text_14_december, '\\s')) 

M <- M %>% relocate(STM_no_stop_ROS_14_dec, .after = STM_no_stop_text_14_december) # for the ease of checking



00000000000000000000000000000000000

# Finally I will replace back "ro_OGUZOZBAY_CORRECT" to "ro" -----
# STEP-1 (replacements in key_TI_AB_ROS_14_dec column)
M$key_TI_AB_ROS_14_dec <- gsub(pattern= "\\bro_OGUZOZBAY_CORRECT\\b", replacement = "ro", M$key_TI_AB_ROS_14_dec)
M$key_TI_AB_ROS_14_dec <- str_squish(M$key_TI_AB_ROS_14_dec)
M$replace_check_1 <- NULL
M$replace_check_2 <- NULL

M$replace_check_1 <- mapply(function(x, y) paste0(setdiff(x, y), collapse = " "), 
                            strsplit(M$key_TI_AB_14_december, '\\s'), strsplit(M$key_TI_AB_ROS_14_dec, '\\s')) 
M$replace_check_2 <- mapply(function(x, y) paste0(setdiff(x, y), collapse = " "), 
                            strsplit(M$key_TI_AB_ROS_14_dec, '\\s'), strsplit(M$key_TI_AB_14_december, '\\s')) 


# STEP-2 (replacements in STM_text_ROS_14_dec column)
M$STM_text_ROS_14_dec <- gsub(pattern= "\\bro_OGUZOZBAY_CORRECT\\b", replacement = "ro", M$STM_text_ROS_14_dec)
M$STM_text_ROS_14_dec <- str_squish(M$STM_text_ROS_14_dec)
M$replace_check_1 <- NULL
M$replace_check_2 <- NULL

M$replace_check_1 <- mapply(function(x, y) paste0(setdiff(x, y), collapse = " "), 
                            strsplit(M$STM_text_14_december, '\\s'), strsplit(M$STM_text_ROS_14_dec, '\\s')) 
M$replace_check_2 <- mapply(function(x, y) paste0(setdiff(x, y), collapse = " "), 
                            strsplit(M$STM_text_ROS_14_dec, '\\s'), strsplit(M$STM_text_14_december, '\\s')) 


# STEP-3 (replacements in STM_no_stop_ROS_14_dec column)
M$STM_no_stop_ROS_14_dec <- gsub(pattern= "\\bro_OGUZOZBAY_CORRECT\\b", replacement = "ro", M$STM_no_stop_ROS_14_dec)
M$STM_no_stop_ROS_14_dec <- str_squish(M$STM_no_stop_ROS_14_dec)
M$replace_check_1 <- NULL
M$replace_check_2 <- NULL

M$replace_check_1 <- mapply(function(x, y) paste0(setdiff(x, y), collapse = " "), 
                            strsplit(M$STM_no_stop_text_14_december, '\\s'), strsplit(M$STM_no_stop_ROS_14_dec, '\\s')) 
M$replace_check_2 <- mapply(function(x, y) paste0(setdiff(x, y), collapse = " "), 
                            strsplit(M$STM_no_stop_ROS_14_dec, '\\s'), strsplit(M$STM_no_stop_text_14_december, '\\s')) 


# save.image("C:/Users/ozbay/OneDrive - XXXX/robot_WOS/CORRECTION for ros finding ro ENVIRONMENT.RData")

# Açıklama ROS_14_dec geçen sütunlar STM analizine hazır.
# Yani ro => ros düzeltmekeri yapılmış.
# Şu an vaktim yok. Gereksiz sütunları daha sonra sileceğim.


# load("C:/Users/ozbay/OneDrive - XXXX/robot_WOS/CORRECTION for ros finding ro ENVIRONMENT.RData")

# Robot ilgisiz olan 125 makale vardı onları da çekeyim
# library(readxl)
# aerosol_robotic_CORRECTION_WOS_december_2022 <- read_excel("aerosol_robotic_CORRECTION_WOS_december_2022.xlsx", 
#                                                           sheet = "not_robot_related")

# save.image("C:/Users/ozbay/OneDrive - XXXX/robot_WOS/CORRECTION for ros finding ro ENVIRONMENT.RData")


1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111
00000000000000000000000 STM analizi için son Halini veriyorum 00000000000000000000000000000



# Şimdi 125 satırı sileceğim
delete_list_1 <- aerosol_robotic_CORRECTION_WOS_december_2022$UT # bunu yapmayınca çalışmıyor.
# KAYNAK: https://stackoverflow.com/questions/13012509/how-to-delete-rows-from-a-data-frame-based-on-an-external-list-using-r
clean <- M[-match(delete_list_1, M$UT),]
colnames(clean)
library(dplyr)
clean_deleted_rows <- (select(clean, -c(key_TI_AB_CORRECTED_d, key_TI_AB_14_december, STM_text, STM_text_14_december, STM_no_stop_text,
                                    STM_no_stop_text_14_december, ro_check_AB, ro_check_TI, replace_check_1, replace_check_2)))



colnames(clean_deleted_rows)
# [1] "UT"                     "publication_year"       "TI"                     "AB"                     "key_TI_AB_ROS_14_dec"  
# [6] "STM_text_ROS_14_dec"    "STM_no_stop_ROS_14_dec"
library("dplyr")
# Rename multiple columns
clean_deleted_rows <- clean_deleted_rows %>% rename("key_TI_AB" = "key_TI_AB_ROS_14_dec",
                                                    "STM_key_TI_AB_nGram" = "STM_text_ROS_14_dec",
                                                    "NoStop_STM_key_TI_AB_nGram" = "STM_no_stop_ROS_14_dec")


rm(aerosol_robotic_CORRECTION_WOS_december_2022)
rm(clean)
rm(M)
rm(delete_list_1)
rm(M_ro_WOS) 

M_15_dec <- clean_deleted_rows
rm(clean_deleted_rows)

# Sadece M'i kaydediyorum.STM analizi yapmaya hazır düzeltilmiş veri = M_15_dec

M_15_dec # M analizi yapmaya hazır düzeltilmiş veri. Robot ilgisizi olan 125 makale silinmiş, ro = ros düzetlmesi yapılmış
# save.image("C:/Users/ozbay/OneDrive - XXXX/robot_WOS/M_corrected_15_dec_2022.RData")



#####################################
#### adams => adam hatası da yapılmış
#####################################

# orijinal metinde 49 tane "adam" kelimesi geçiyor.
# Bu adam'ları adam_OGUZOZBAY yapacağım. Sonra kalan tüm "adam"ları adams yapacağım.
# Sonra adam_OGUZOZBAY'ları adam'a geri çevireceğim.

load("C:/Users/ozbay/OneDrive - XXXX/robot_WOS/M_corrected_15_dec_2022.RData")


M_15_dec$adam_check_AB <- 0
M_15_dec <- within(M_15_dec,{adam_check_AB = as.numeric(grepl("\\badam\\b", fixed = FALSE, M_15_dec$AB))})


M_15_dec$adam_check_TI <- 0
M_15_dec <- within(M_15_dec,{adam_check_TI = as.numeric(grepl("\\badam\\b", fixed = FALSE, M_15_dec$TI))})

adam_wos <- M_15_dec
adam_wos$sum <- adam_wos$adam_check_AB + adam_wos$adam_check_TI
adam_wos <- adam_wos %>% filter(sum > 0)

# Kontrol etmesi kolay olsun diye adam_wos içindeki tüm adam'ları adam_OGUZOZBAY yapacağım.
colnames(adam_wos)
adam_wos[(1:30),5] <- gsub(pattern= "\\badam\\b", replacement = "adam_OGUZOZBAY", adam_wos[(1:30),5])
adam_wos[(1:30),6] <- gsub(pattern= "\\badam\\b", replacement = "adam_OGUZOZBAY", adam_wos[(1:30),6])
adam_wos[(1:30),7] <- gsub(pattern= "\\badam\\b", replacement = "adam_OGUZOZBAY", adam_wos[(1:30),7])
adam_wos <- adam_wos %>% select(UT, key_TI_AB, STM_key_TI_AB_nGram, NoStop_STM_key_TI_AB_nGram)

# replacement of all "adam" to "adams"
M_16_dec <- M_15_dec
colnames(M_16_dec)
# key_TI_AB, STM_key_TI_AB_nGram ve NoStop_STM_key_TI_AB_nGram sütunlarındaki tüm adam'ları adams yaptım
M_16_dec[(1:211460),5] <- gsub(pattern= "\\badam\\b", replacement = "adams", M_16_dec[(1:211460),5])
M_16_dec[(1:211460),6] <- gsub(pattern= "\\badam\\b", replacement = "adams", M_16_dec[(1:211460),6])
M_16_dec[(1:211460),7] <- gsub(pattern= "\\badam\\b", replacement = "adams", M_16_dec[(1:211460),7])

delete <- M_16_dec %>% select(UT, NoStop_STM_key_TI_AB_nGram) # for checking replacemnt (OK, there is no adam)
rm(delete)



# Now I will take adam_wos dataframe's row and paste them to M_16_dec
# Caution: This code converts mismatches to NA.
M_16_dec$key_TI_AB_OLD <- M_15_dec$key_TI_AB
M_16_dec$key_TI_AB <- adam_wos$key_TI_AB[match(M_16_dec$UT, adam_wos$UT)]
M_16_dec$key_TI_AB <- ifelse(is.na(M_16_dec$key_TI_AB), M_16_dec$key_TI_AB_OLD, M_16_dec$key_TI_AB)
        

M_16_dec$STM_key_TI_AB_nGram_OLD <- M_15_dec$STM_key_TI_AB_nGram
M_16_dec$STM_key_TI_AB_nGram <- adam_wos$STM_key_TI_AB_nGram[match(M_16_dec$UT, adam_wos$UT)]
M_16_dec$STM_key_TI_AB_nGram <- ifelse(is.na(M_16_dec$STM_key_TI_AB_nGram), M_16_dec$STM_key_TI_AB_nGram_OLD,
                                       M_16_dec$STM_key_TI_AB_nGram)



M_16_dec$NoStop_STM_key_TI_AB_nGram_OLD <- M_15_dec$NoStop_STM_key_TI_AB_nGram
M_16_dec$NoStop_STM_key_TI_AB_nGram <- adam_wos$NoStop_STM_key_TI_AB_nGram[match(M_16_dec$UT, adam_wos$UT)]
M_16_dec$NoStop_STM_key_TI_AB_nGram <- ifelse(is.na(M_16_dec$NoStop_STM_key_TI_AB_nGram), M_16_dec$NoStop_STM_key_TI_AB_nGram_OLD,
                                       M_16_dec$NoStop_STM_key_TI_AB_nGram)





# str_squish - START
M_16_dec$key_TI_AB <- str_squish(M_16_dec$key_TI_AB)
M_16_dec$STM_key_TI_AB_nGram <- str_squish(M_16_dec$STM_key_TI_AB_nGram)
M_16_dec$NoStop_STM_key_TI_AB_nGram <- str_squish(M_16_dec$NoStop_STM_key_TI_AB_nGram)
# str_squish - END

M_16_dec$adam_check_AB <- NULL
M_16_dec$adam_check_TI <- NULL
M_16_dec$replace_check_1 <- mapply(function(x, y) paste0(setdiff(x, y), collapse = " "), 
                            strsplit(M_16_dec$key_TI_AB_OLD, '\\s'), strsplit(M_16_dec$key_TI_AB, '\\s')) 
M_16_dec$replace_check_2 <- mapply(function(x, y) paste0(setdiff(x, y), collapse = " "), 
                                   strsplit(M_16_dec$key_TI_AB, '\\s'), strsplit(M_16_dec$key_TI_AB_OLD, '\\s')) 


M_16_dec$replace_check_1 <- NULL
M_16_dec$replace_check_2 <- NULL
M_16_dec$replace_check_1 <- mapply(function(x, y) paste0(setdiff(x, y), collapse = " "), 
                                   strsplit(M_16_dec$STM_key_TI_AB_nGram_OLD, '\\s'), strsplit(M_16_dec$STM_key_TI_AB_nGram, '\\s')) 
M_16_dec$replace_check_2 <- mapply(function(x, y) paste0(setdiff(x, y), collapse = " "), 
                                   strsplit(M_16_dec$STM_key_TI_AB_nGram, '\\s'), strsplit(M_16_dec$STM_key_TI_AB_nGram_OLD, '\\s'))


M_16_dec$replace_check_1 <- NULL
M_16_dec$replace_check_2 <- NULL
M_16_dec$replace_check_1 <- mapply(function(x, y) paste0(setdiff(x, y), collapse = " "), 
                                   strsplit(M_16_dec$NoStop_STM_key_TI_AB_nGram_OLD, '\\s'), strsplit(M_16_dec$NoStop_STM_key_TI_AB_nGram, '\\s')) 
M_16_dec$replace_check_2 <- mapply(function(x, y) paste0(setdiff(x, y), collapse = " "), 
                                   strsplit(M_16_dec$NoStop_STM_key_TI_AB_nGram, '\\s'), strsplit(M_16_dec$NoStop_STM_key_TI_AB_nGram_OLD, '\\s'))


M_16_dec$replace_check_1 <- NULL
M_16_dec$replace_check_2 <- NULL
colnames(M_16_dec)
M_16_dec$key_TI_AB_OLD <- NULL
M_16_dec$STM_key_TI_AB_nGram_OLD <- NULL
M_16_dec$NoStop_STM_key_TI_AB_nGram_OLD <- NULL


# key_TI_AB, STM_key_TI_AB_nGram ve NoStop_STM_key_TI_AB_nGram sütunlarındaki tüm adam_OGUZOZBAY'ları adam yapıyorum
M_16_dec[(1:211460),5] <- gsub(pattern= "\\badam_OGUZOZBAY\\b", replacement = "adam", M_16_dec[(1:211460),5])
M_16_dec[(1:211460),6] <- gsub(pattern= "\\badam_OGUZOZBAY\\b", replacement = "adam", M_16_dec[(1:211460),6])
M_16_dec[(1:211460),7] <- gsub(pattern= "\\badam_OGUZOZBAY\\b", replacement = "adam", M_16_dec[(1:211460),7])

rm(adam_wos)
rm(M_15_dec)


M_16_dec # M analizi yapmaya hazır düzeltilmiş veri. Robot ilgisizi olan 125 makale silinmiş, ro = ros düzetlmesi yapılmış, adams düzeltilmiş
# save.image("C:/Users/ozbay/OneDrive - XXXX/robot_WOS/M_CORRECTED_16_dec_2022.RData")







