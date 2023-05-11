##Корреляция между сайтом отщепления сигнального пептида и кол-вом АК, при которм наблюдается макс/мин заряд

#нахождение кол-ва аминокислот после обрезания сигнального пептида, при котором наблюдается мин и макс заряд
#для удобства сначала отделяем из основного датасета data_filtered (полученного после в основном скрипте) имя последовательности, ее ID, и род бактерии
#создается новый датасет min_max
library(ggplot2)
#Gramnegative bacteria
name_gramneg <- new_data_filtered_gramneg$name
new_min_max_gramneg <- as.data.frame(name_gramneg)
new_min_max_gramneg$ID <- new_data_filtered_gramneg$ID
new_min_max_gramneg$genus <-new_data_filtered_gramneg$genus
new_min_max_gramneg$cleav_site <- new_data_filtered_gramneg$cleav_site #сайт отщепления сигнального пептида
new_min_max_gramneg$n_reg <- new_data_filtered_gramneg$n_end
new_min_max_gramneg$h_reg <- as.numeric(new_data_filtered_gramneg$h_end - new_data_filtered_gramneg$h_start +1 )
new_min_max_gramneg$c_reg <- as.numeric(new_data_filtered_gramneg$cleav_site - new_data_filtered_gramneg$h_end)
#using dataset data_for cor from the script correlation between charge and cleav_site
new_min_max_gramneg$min <- names(new_data_filtered_gramneg[,12:56])[apply(new_data_filtered_gramneg[,12:56], MARGIN = 1, FUN = which.min)] #количество АК, при котором наблюдается минимальных заряд для конкретного белка
new_min_max_gramneg$max <- names(new_data_filtered_gramneg[,12:56])[apply(new_data_filtered_gramneg[,12:56], MARGIN = 1, FUN = which.max)] #количество АК, при котором наблюдается максимальный заряд для конкретного белка
new_min_max_gramneg$charge_min <- as.numeric(sapply(new_min_max_gramneg$min, function(x) strsplit(x, "_")[[1]][2]))#отделяю номер charge_ (без этого получается неверный график)
new_min_max_gramneg$charge_max <- as.numeric(sapply(new_min_max_gramneg$max, function(x) strsplit(x, "_")[[1]][2]))
new_min_max_gramneg$min_charge_value <-apply(new_data_filtered_gramneg[,12:56], MARGIN = 1, FUN = min)#поиск минимального заряда
new_min_max_gramneg$max_charge_value <-apply(new_data_filtered_gramneg[,12:56], MARGIN = 1, FUN = max)#поиск максимального заряда

#splitting dataset for 4 datasets in order to get perceptible graphs
#1 DATASET
new_min_max_gramneg1<- rbind(new_min_max_gramneg[new_min_max_gramneg$genus=="Alkalilacustris",], new_min_max_gramneg[new_min_max_gramneg$genus=="Ameyamaea",], 
                             new_min_max_gramneg[new_min_max_gramneg$genus=="Aquidulcibacter",],
                      new_min_max_gramneg[new_min_max_gramneg$genus=="Cucumibacter",])

#график корреляции, чтобы посмотреть примерную зависимость переменных(сайта отщепления и charge_ , где заряд минимальный)
ggplot(new_min_max_gramneg1, aes(x = cleav_site, y = charge_min))+ 
  geom_point(col="blue", alpha=.2)+
  xlab("Signal peptide length, aminoacids")+
  ylab("Window with minimal charge, aminoacids")+
  facet_wrap(~genus)
#график корреляции, чтобы посмотреть примерную зависимость переменных(сайта отщепления и charge_ , где заряд максимальный)
ggplot(new_min_max_gramneg1, aes(x = cleav_site, y = charge_max))+ 
  geom_point(col="blue", alpha=.2)+
  xlab("Signal peptide length, aminoacids")+
  ylab("Window with maximum charge, aminoacids")+
  facet_wrap(~genus)


#2 DATASET
new_min_max_gramneg2<- rbind(new_min_max_gramneg[new_min_max_gramneg$genus=="Halovulum",], new_min_max_gramneg[new_min_max_gramneg$genus=="Hellea",], 
                      new_min_max_gramneg[new_min_max_gramneg$genus=="Limibacillus",],
                      new_min_max_gramneg[new_min_max_gramneg$genus=="Luteithermobacter",])

#график корреляции, чтобы посмотреть примерную зависимость переменных(сайта отщепления и charge_ , где заряд минимальный)
ggplot(new_min_max_gramneg2, aes(x = cleav_site, y = charge_min))+ 
  geom_point(col="blue", alpha=.2)+
  xlab("Signal peptide length, aminoacids")+
  ylab("Window with minimal charge, aminoacids")+
  facet_wrap(~genus)
#график корреляции, чтобы посмотреть примерную зависимость переменных(сайта отщепления и charge_ , где заряд максимальный)
ggplot(new_min_max_gramneg2, aes(x = cleav_site, y = charge_max))+ 
  geom_point(col="blue", alpha=.2)+
  xlab("Signal peptide length, aminoacids")+
  ylab("Window with maximum charge, aminoacids")+
  facet_wrap(~genus)
count(new_data_filtered_gramneg, genus)

#3 DATASET
new_min_max_gramneg3<- rbind(new_min_max_gramneg[new_min_max_gramneg$genus=="Neptunicoccus",], 
                             new_min_max_gramneg[new_min_max_gramneg$genus=="Nguyenibacter",],
                             new_min_max_gramneg[new_min_max_gramneg$genus=="Parapontixanthobacter",], 
                             new_min_max_gramneg[new_min_max_gramneg$genus=="Phaeovibrio",])

#график корреляции, чтобы посмотреть примерную зависимость переменных(сайта отщепления и charge_ , где заряд минимальный)
ggplot(new_min_max_gramneg3, aes(x = cleav_site, y = charge_min))+ 
  geom_point(col="blue", alpha=.2)+
  xlab("Signal peptide length, aminoacids")+
  ylab("Window with minimal charge, aminoacids")+
  facet_wrap(~genus)
#график корреляции, чтобы посмотреть примерную зависимость переменных(сайта отщепления и charge_ , где заряд максимальный)
ggplot(new_min_max_gramneg3, aes(x = cleav_site, y = charge_max))+ 
  geom_point(col="blue", alpha=.2)+
  xlab("Signal peptide length, aminoacids")+
  ylab("Window with maximum charge, aminoacids")+
  facet_wrap(~genus)

#4 DATASET
new_min_max_gramneg4<- rbind(new_min_max_gramneg[new_min_max_gramneg$genus=="Rhizorhapis",], new_min_max_gramneg[new_min_max_gramneg$genus=="Rubricella",], 
                      new_min_max_gramneg[new_min_max_gramneg$genus=="Thermithiobacillus",],
                      new_min_max_gramneg[new_min_max_gramneg$genus=="Woodsholea",])

#график корреляции, чтобы посмотреть примерную зависимость переменных(сайта отщепления и charge_ , где заряд минимальный)
ggplot(new_min_max_gramneg4, aes(x = cleav_site, y = charge_min))+ 
  geom_point(col="blue", alpha=.2)+
  xlab("Signal peptide length, aminoacids")+
  ylab("Window with minimal charge, aminoacids")+
  facet_wrap(~genus)
#график корреляции, чтобы посмотреть примерную зависимость переменных(сайта отщепления и charge_ , где заряд максимальный)
ggplot(new_min_max_gramneg4, aes(x = cleav_site, y = charge_max))+ 
  geom_point(col="blue", alpha=.2)+
  xlab("Signal peptide length, aminoacids")+
  ylab("Window with maximum charge, aminoacids")+
  facet_wrap(~genus)

#E.coli reference
name <- new_data_filtered_ecoli$name
new_min_max_ecoli <- as.data.frame(name)
new_min_max_ecoli$ID <- new_data_filtered_ecoli$ID
#new_min_max_ecoli$genus <-new_data_filtered_ecoli$genus
new_min_max_ecoli$cleav_site <- new_data_filtered_ecoli$cleav_site#сайт отщепления сигнального пептида
new_min_max_ecoli$n_reg <- new_data_filtered_ecoli$n_end
new_min_max_ecoli$h_reg <- as.numeric(new_data_filtered_ecoli$h_end - new_data_filtered_ecoli$h_start +1 )
new_min_max_ecoli$c_reg <- as.numeric(new_data_filtered_ecoli$cleav_site - new_data_filtered_ecoli$h_end)

new_min_max_ecoli$min <- names(new_data_filtered_ecoli[,11:55])[apply(new_data_filtered_ecoli[,11:55], MARGIN = 1, FUN = which.min)] #количество АК, при котором наблюдается минимальных заряд для конкретного белка
new_min_max_ecoli$max <- names(new_data_filtered_ecoli[,11:55])[apply(new_data_filtered_ecoli[,11:55], MARGIN = 1, FUN = which.max)] #количество АК, при котором наблюдается максимальный заряд для конкретного белка
new_min_max_ecoli$charge_min <- as.numeric(sapply(new_min_max_ecoli$min, function(x) strsplit(x, "_")[[1]][2]))#отделяю номер charge_ (без этого получается неверный график)
new_min_max_ecoli$charge_max <- as.numeric(sapply(new_min_max_ecoli$max, function(x) strsplit(x, "_")[[1]][2]))
new_min_max_ecoli$min_charge_value <-apply(new_data_filtered_ecoli[,11:55], MARGIN = 1, FUN = min)#поиск минимального заряда
new_min_max_ecoli$max_charge_value <-apply(new_data_filtered_ecoli[,11:55], MARGIN = 1, FUN = max)#поиск максимального заряда


##корреляция между макс/мин и сайтом отщепления
#график корреляции, чтобы посмотреть примерную зависимость переменных(сайта отщепления и charge_ , где заряд минимальный)
ggplot(new_min_max_ecoli, aes(x = cleav_site, y = charge_min))+ 
  geom_point(col="red",alpha=.2, position = "jitter")+
  xlab("N-region length, aminoacids")+
  ylab("Window with minimal charge, aminoacids")
#график корреляции, чтобы посмотреть примерную зависимость переменных(сайта отщепления и charge_ , где заряд максимальный)
ggplot(new_min_max_ecoli, aes(x = cleav_site, y = charge_max))+ 
  geom_point(col="red",alpha=.2, position="jitter")+
  xlab("N-region length, aminoacids")+
  ylab("Window with maximum charge, aminoacids")


