##IMPORT LIBRARIES
library(data.table)
library(data.table)
library(stringi)
library(stringr)
library(ggplot2)
library(ggridges)
library(DescTools)

##GRAMNEGATIVE
#Download dataframes with sequences
mix_gramneg1 <- as.data.frame(new_data_filtered_gramneg[,11])
colnames(mix_gramneg1) <- "sequence"
write.csv(mix_gramneg1, "/home/victoria/Desktop/Signal_peptides_project/Thesis/mix_gramneg2.csv", quote= FALSE)

#Running python script in Google Colaboratory. It produces mixed sequences

name_gramneg <- new_data_filtered_gramneg$name
min_max_gramneg_mix <- as.data.frame(name_gramneg)
min_max_gramneg_mix$ID <- new_data_filtered_gramneg$ID
min_max_gramneg_mix$genus <-new_data_filtered_gramneg$genus
min_max_gramneg_mix$cleav_site <- new_data_filtered_gramneg$cleav_site #сайт отщепления сигнального пептида
min_max_gramneg_mix$cleved_protein <- new_data_filtered_gramneg$cleaved_protein
min_max_gramneg_mix$mixed_protein <- `mixed_proteins_gramneg.(1)`$combined
#Loop
count_charge <- function(sequence, window){
  sequence <- substr(sequence, 1, window)
  charge <- str_count(sequence, "R") + str_count(sequence, "K") -
    str_count(sequence, "D") - str_count(sequence, "E")
  return (charge)
}

count_charge <- Vectorize(count_charge, SIMPLIFY = T, USE.NAMES = F)

for(i in 6:50) {
  min_max_gramneg_mix$new <- count_charge(min_max_gramneg_mix$mixed_protein, i)
  setnames(min_max_gramneg_mix, "new", paste0("charge_", i))
}

#using dataset data_for cor from the script correlation between charge and cleav_site
min_max_gramneg_mix$min <- names(min_max_gramneg_mix[,7:51])[apply(min_max_gramneg_mix[,7:51], MARGIN = 1, FUN = which.min)] #количество АК, при котором наблюдается минимальных заряд для конкретного белка
min_max_gramneg_mix$max <- names(min_max_gramneg_mix[,7:51])[apply(min_max_gramneg_mix[,7:51], MARGIN = 1, FUN = which.max)] #количество АК, при котором наблюдается максимальный заряд для конкретного белка
min_max_gramneg_mix$charge_min <- as.numeric(sapply(min_max_gramneg_mix$min, function(x) strsplit(x, "_")[[1]][2]))#отделяю номер charge_ (без этого получается неверный график)
min_max_gramneg_mix$charge_max <- as.numeric(sapply(min_max_gramneg_mix$max, function(x) strsplit(x, "_")[[1]][2]))

#splitting dataset for 4 datasets in order to get perceptible graphs

#1 DATASET
min_max_gramneg_mix1 <- rbind(min_max_gramneg_mix[min_max_gramneg_mix$genus=="Alkalilacustris",], min_max_gramneg_mix[min_max_gramneg_mix$genus=="Ameyamaea",], 
                              min_max_gramneg_mix[min_max_gramneg_mix$genus=="Aquidulcibacter",],
                      min_max_gramneg_mix[min_max_gramneg_mix$genus=="Cucumibacter",])

#график корреляции, чтобы посмотреть примерную зависимость переменных(сайта отщепления и charge_ , где заряд минимальный)
ggplot(min_max_gramneg_mix1, aes(x = cleav_site, y = charge_min))+ 
  geom_point(col="blue", alpha=.2)+
  xlab("Длина сигнального пептида, АК остатки")+
  ylab("Длина окна АК последовательности с минимальным зарядом, АК остатки")+
  facet_wrap(~genus)
#график корреляции, чтобы посмотреть примерную зависимость переменных(сайта отщепления и charge_ , где заряд максимальный)
ggplot(min_max_gramneg_mix1, aes(x = cleav_site, y = charge_max))+ 
  geom_point(col="blue", alpha=.2)+
  xlab("Длина сигнального пептида, АК остатки")+
  ylab("Длина окна АК последовательности с максимальным зарядом, АК остатки")+
  facet_wrap(~genus)

#2 DATASET
min_max_gramneg_mix2 <- rbind(min_max_gramneg_mix[min_max_gramneg_mix$genus=="Halovulum",], min_max_gramneg_mix[min_max_gramneg_mix$genus=="Hellea",], 
                      min_max_gramneg_mix[min_max_gramneg_mix$genus=="Limibacillus",],
                      min_max_gramneg_mix[min_max_gramneg_mix$genus=="Luteithermobacter",])

#график корреляции, чтобы посмотреть примерную зависимость переменных(сайта отщепления и charge_ , где заряд минимальный)
ggplot(min_max_gramneg_mix2, aes(x = cleav_site, y = charge_min))+ 
  geom_point(col="blue", alpha=.2)+
  xlab("Длина сигнального пептида, АК остатки")+
  ylab("Длина окна АК последовательности с минимальным зарядом, АК остатки")+
  facet_wrap(~genus)
#график корреляции, чтобы посмотреть примерную зависимость переменных(сайта отщепления и charge_ , где заряд максимальный)
ggplot(min_max_gramneg_mix2, aes(x = cleav_site, y = charge_max))+ 
  geom_point(col="blue", alpha=.2)+
  xlab("Длина сигнального пептида, АК остатки")+
  ylab("Длина окна АК последовательности с максимальным зарядом, АК остатки")+
  facet_wrap(~genus)

#3 DATASET
min_max_gramneg_mix3 <- rbind(min_max_gramneg_mix[min_max_gramneg_mix$genus=="Neptunicoccus",],
                              min_max_gramneg_mix[min_max_gramneg_mix$genus=="Nguyenibacter",],
                              min_max_gramneg_mix[min_max_gramneg_mix$genus=="Phaeovibrio",],
                              min_max_gramneg_mix[min_max_gramneg_mix$genus=="Parapontixanthobacter",])

#график корреляции, чтобы посмотреть примерную зависимость переменных(сайта отщепления и charge_ , где заряд минимальный)
ggplot(min_max_gramneg_mix3, aes(x = cleav_site, y = charge_min))+ 
  geom_point(col="blue", alpha=.2)+
  xlab("Длина сигнального пептида, АК остатки")+
  ylab("Длина окна АК последовательности с минимальным зарядом, АК остатки")+
  facet_wrap(~genus)
#график корреляции, чтобы посмотреть примерную зависимость переменных(сайта отщепления и charge_ , где заряд максимальный)
ggplot(min_max_gramneg_mix3, aes(x = cleav_site, y = charge_max))+ 
  geom_point(col="blue", alpha=.2)+
  xlab("Длина сигнального пептида, АК остатки")+
  ylab("Длина окна АК последовательности с максимальным зарядом, АК остатки")+
  facet_wrap(~genus)

#4 DATASET
min_max_gramneg_mix4<- rbind(min_max_gramneg_mix[min_max_gramneg_mix$genus=="Rhizorhapis",], min_max_gramneg_mix[min_max_gramneg_mix$genus=="Rubricella",], 
                      min_max_gramneg_mix[min_max_gramneg_mix$genus=="Thermithiobacillus",],
                      min_max_gramneg_mix[min_max_gramneg_mix$genus=="Woodsholea",])

#график корреляции, чтобы посмотреть примерную зависимость переменных(сайта отщепления и charge_ , где заряд минимальный)
ggplot(min_max_gramneg_mix4, aes(x = cleav_site, y = charge_min))+ 
  geom_point(col="blue", alpha=.2)+
  xlab("Длина сигнального пептида, АК остатки")+
  ylab("Длина окна АК последовательности с минимальным зарядом, АК остатки")+
  facet_wrap(~genus)
#график корреляции, чтобы посмотреть примерную зависимость переменных(сайта отщепления и charge_ , где заряд максимальный)
ggplot(min_max_gramneg_mix4, aes(x = cleav_site, y = charge_max))+ 
  geom_point(col="blue", alpha=.2)+
  xlab("Длина сигнального пептида, АК остатки")+
  ylab("Длина окна АК последовательности с максимальным зарядом, АК остатки")+
  facet_wrap(~genus)

##E.COLI REFERENCE 
#Download dataframes with sequences
mix_ecoli_ref <- as.data.frame(data_filtered_ecol[,12])
colnames(mix_ecoli_ref) <- "sequence"
write.csv(mix_ecoli_ref, "/home/victoria/Desktop/Signal_peptides_project/Thesis/mix_ecoli_ref.csv", quote= FALSE)
#E.coli reference
name_ecolii <- new_data_filtered_ecoli$name
min_max_ecoli_mix <- as.data.frame(name_ecolii)
min_max_ecoli_mix$ID <- new_data_filtered_ecoli$ID
min_max_ecoli_mix$genus <-new_data_filtered_ecoli$genus
min_max_ecoli_mix$cleav_site <- new_data_filtered_ecoli$cleav_site #сайт отщепления сигнального пептида
min_max_ecoli_mix$cleved_protein <- new_data_filtered_ecoli$cleaved_protein
min_max_ecoli_mix$mixed_protein <- `mixed_proteins_ecoli.(1)`$combined
#Loop
count_charge <- function(sequence, window){
  sequence <- substr(sequence, 1, window)
  charge <- str_count(sequence, "R") + str_count(sequence, "K") -
    str_count(sequence, "D") - str_count(sequence, "E")
  return (charge)
}

count_charge <- Vectorize(count_charge, SIMPLIFY = T, USE.NAMES = F)

for(i in 6:50) {
  min_max_ecoli_mix$new <- count_charge(min_max_ecoli_mix$mixed_protein, i)
  setnames(min_max_ecoli_mix, "new", paste0("charge_", i))
}

min_max_ecoli_mix$min <- names(min_max_ecoli_mix[,6:50])[apply(min_max_ecoli_mix[,6:50], MARGIN = 1, FUN = which.min)] #количество АК, при котором наблюдается минимальных заряд для конкретного белка
min_max_ecoli_mix$max <- names(min_max_ecoli_mix[,6:50])[apply(min_max_ecoli_mix[,6:50], MARGIN = 1, FUN = which.max)] #количество АК, при котором наблюдается максимальный заряд для конкретного белка
min_max_ecoli_mix$charge_min <- as.numeric(sapply(min_max_ecoli_mix$min, function(x) strsplit(x, "_")[[1]][2]))#отделяю номер charge_ (без этого получается неверный график)
min_max_ecoli_mix$charge_max <- as.numeric(sapply(min_max_ecoli_mix$max, function(x) strsplit(x, "_")[[1]][2]))

##корреляция между макс/мин и сайтом отщепления
library(ggplot2)
#график корреляции, чтобы посмотреть примерную зависимость переменных(сайта отщепления и charge_ , где заряд минимальный)
ggplot(min_max_ecol_mix, aes(x = cleav_site, y = charge_min))+ 
  geom_point(col="red",alpha=.2, position = "jitter")+
  xlab("Длина сигнального пептида, АК остатки")+
  ylab("Длина окна АК последовательности с минимальным зарядом, АК остатки")
#график корреляции, чтобы посмотреть примерную зависимость переменных(сайта отщепления и charge_ , где заряд максимальный)
ggplot(min_max_ecol_mix, aes(x = cleav_site, y = charge_max))+ 
  geom_point(col="red",alpha=.2, position="jitter")+
  xlab("Длина сигнального пептида, АК остатки")+
  ylab("Длина окна АК последовательности с максимальным зарядом, АК остатки")



