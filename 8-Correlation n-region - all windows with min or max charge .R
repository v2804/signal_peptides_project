library(dplyr)
library(ggplot2)
###########################Gram negative
new_all_min_max_gramneg <- new_min_max_gramneg[,3:11]
new_all_min_max_gramneg$min_value <- apply(new_data_filtered_gramneg[,12:56], 1, min)#find min value in a row
new_all_min_max_gramneg$max_value <- apply(new_data_filtered_gramneg[,12:56], 1, max)#find max value in a row

#make a list of variable names that correspond to min charge
list_gramneg_min <- list()
for(i in 1:nrow(new_data_filtered_gramneg[,12:56])){
  new_element <- colnames(new_data_filtered_gramneg[i, 12:56])[which(apply(new_data_filtered_gramneg[i,12:56],1, 
                                                               function(x) x==new_all_min_max_gramneg$min_value[i]))]
  list_gramneg_min[[length(list_gramneg_min) + 1]] <- new_element
}

#make a list of variable names that correspond to max charge
list_gramneg_max <- list()
for(i in 1:nrow(new_data_filtered_gramneg[,12:56])){
  new_element1 <- colnames(new_data_filtered_gramneg[i, 12:56])[which(apply(new_data_filtered_gramneg[i,12:56],1, 
                                                               function(x) x==new_all_min_max_gramneg$max_value[i]))]
  list_gramneg_max[[length(list_gramneg_max) + 1]] <- new_element1
}

#append lists of vectors to a dataframe
new_all_min_max_gramneg$all_min <- list_gramneg_min
new_all_min_max_gramneg$all_max <- list_gramneg_max
new_all_min_max_gramneg$length_min <- sapply(new_all_min_max_gramneg$all_min, function(x) length(x))
new_all_min_max_gramneg$length_max <- sapply(new_all_min_max_gramneg$all_max, function(x) length(x))

#create a list of dataframes with two columns: cleav site and window with minimum charge
rm(df_list_gramneg_min)
df_list_gramneg_min <- list()
for(i in 1:nrow(new_all_min_max_gramneg)){
  new_element3 <- data.frame(cleav_site = rep(new_all_min_max_gramneg$cleav_site[i], new_all_min_max_gramneg$length_min[i]), 
                             charge_min = new_all_min_max_gramneg$all_min[i], genus= rep(new_data_filtered_gramneg$genus[i], new_all_min_max_gramneg$length_min[i]),
                             n_reg= rep(new_all_min_max_gramneg$n_reg[i], new_all_min_max_gramneg$length_min[i]), 
                             h_reg= rep(new_all_min_max_gramneg$h_reg[i], new_all_min_max_gramneg$length_min[i]),
                             c_reg= rep(new_all_min_max_gramneg$c_reg[i], new_all_min_max_gramneg$length_min[i]),
                             min_value= rep(new_all_min_max_gramneg$min_value[i], new_all_min_max_gramneg$length_min[i]) )
  colnames(new_element3) <- c("cleav_site", "charge_min", "genus", "n_reg", "h_reg", "c_reg", "min_charge_value")
  df_list_gramneg_min[[length(df_list_gramneg_min) + 1]] <- new_element3
}

rm(dataframe_min_gramneg)
dataframe_min_gramneg <- bind_rows(df_list_gramneg_min, .id= "column label")
#create a list of dataframes with two columns: cleav site and window with maximum charge
df_list_gramneg_max <- list()
for(i in 1:nrow(new_all_min_max_gramneg)){
  new_element4 <- data.frame(cleav_site = rep(new_all_min_max_gramneg$cleav_site[i], new_all_min_max_gramneg$length_max[i]), 
                             charge_max = new_all_min_max_gramneg$all_max[i], genus=rep(new_data_filtered_gramneg$genus[i], new_all_min_max_gramneg$length_max[i]),
                             n_reg= rep(new_all_min_max_gramneg$n_reg[i], new_all_min_max_gramneg$length_max[i]), 
                             h_reg= rep(new_all_min_max_gramneg$h_reg[i], new_all_min_max_gramneg$length_max[i]),
                             c_reg= rep(new_all_min_max_gramneg$c_reg[i], new_all_min_max_gramneg$length_max[i]),
                             max_value= rep(new_all_min_max_gramneg$max_value[i], new_all_min_max_gramneg$length_max[i]))
  colnames(new_element4) <- c("cleav_site", "charge_max", "genus", "n_reg", "h_reg", "c_reg", "max_charge_value")
  df_list_gramneg_max[[length(df_list_gramneg_max) + 1]] <- new_element4
}


dataframe_max_gramneg <- bind_rows(df_list_gramneg_max, .id= "column label")

#mutate dataframes
dataframe_min_gramneg$min <- as.numeric(sapply(dataframe_min_gramneg$charge_min, function(x) strsplit(x, "_")[[1]][2]))#отделяю номер charge_ (без этого получается неверный график)
dataframe_max_gramneg$max <- as.numeric(sapply(dataframe_max_gramneg$charge_max, function(x) strsplit(x, "_")[[1]][2]))

#1 DATASET
dataframe_min_gramneg1<- rbind(dataframe_min_gramneg[dataframe_min_gramneg$genus=="Alkalilacustris",], dataframe_min_gramneg[dataframe_min_gramneg$genus=="Ameyamaea",], 
                        dataframe_min_gramneg[dataframe_min_gramneg$genus=="Aquidulcibacter",],
                      dataframe_min_gramneg[dataframe_min_gramneg$genus=="Cucumibacter",])
dataframe_max_gramneg1<- rbind(dataframe_max_gramneg[dataframe_max_gramneg$genus=="Alkalilacustris",], dataframe_max_gramneg[dataframe_max_gramneg$genus=="Ameyamaea",], dataframe_max_gramneg[dataframe_max_gramneg$genus=="Aquidulcibacter",],
                      dataframe_max_gramneg[dataframe_max_gramneg$genus=="Cucumibacter",])


#график корреляции, чтобы посмотреть примерную зависимость переменных(сайта отщепления и charge_ , где заряд минимальный)
ggplot(dataframe_min_gramneg1, aes(x = n_reg, y = min))+ 
  geom_point(col="blue", alpha=.2)+
  xlab("Длина N-региона сигнального пептида, АК остатки")+
  ylab("Длина окна АК последовательности с минимальным зарядом, АК остатки")+
  facet_wrap(~genus)+
  scale_x_continuous(breaks = seq(0,70,5))
#график корреляции, чтобы посмотреть примерную зависимость переменных(сайта отщепления и charge_ , где заряд максимальный)
ggplot(dataframe_max_gramneg1, aes(x = n_reg, y = max))+ 
  geom_point(col="blue", alpha=.2)+
  xlab("Длина N-региона сигнального пептида, АК остатки")+
  ylab("Длина окна АК последовательности с макисмальным зарядом, АК остатки")+
  facet_wrap(~genus)+
  scale_x_continuous(breaks = seq(0,70,5))
#2 DATASET
dataframe_min_gramneg2<- rbind(dataframe_min_gramneg[dataframe_min_gramneg$genus=="Halovulum",], dataframe_min_gramneg[dataframe_min_gramneg$genus=="Hellea",], 
                          dataframe_min_gramneg[dataframe_min_gramneg$genus=="Limibacillus",],
                          dataframe_min_gramneg[dataframe_min_gramneg$genus=="Luteithermobacter",])
dataframe_max_gramneg2<- rbind(dataframe_max_gramneg[dataframe_max_gramneg$genus=="Halovulum",], dataframe_max_gramneg[dataframe_max_gramneg$genus=="Hellea",], 
                          dataframe_max_gramneg[dataframe_max_gramneg$genus=="Limibacillus",],
                          dataframe_max_gramneg[dataframe_max_gramneg$genus=="Luteithermobacter",])


#график корреляции, чтобы посмотреть примерную зависимость переменных(сайта отщепления и charge_ , где заряд минимальный)
ggplot(dataframe_min_gramneg2, aes(x = n_reg, y = min))+ 
  geom_point(col="blue", alpha=.2)+
  xlab("Длина N-региона сигнального пептида, АК остатки")+
  ylab("Длина окна АК последовательности с минимальным зарядом, АК остатки")+
  facet_wrap(~genus)+
  scale_x_continuous(breaks = seq(0,70,5))
#график корреляции, чтобы посмотреть примерную зависимость переменных(сайта отщепления и charge_ , где заряд максимальный)
ggplot(dataframe_max_gramneg2, aes(x = n_reg, y = max))+ 
  geom_point(col="blue", alpha=.2)+
  xlab("Длина N-региона сигнального пептида, АК остатки")+
  ylab("Длина окна АК последовательности с макисмальным зарядом, АК остатки")+
  facet_wrap(~genus)+
  scale_x_continuous(breaks = seq(0,70,5))

#3 DATASET
dataframe_min_gramneg3<- rbind(dataframe_min_gramneg[dataframe_min_gramneg$genus=="Neptunicoccus",],
                               dataframe_min_gramneg[dataframe_min_gramneg$genus=="Nguyenibacter",],
                      dataframe_min_gramneg[dataframe_min_gramneg$genus=="Parapontixanthobacter",],
                      dataframe_min_gramneg[dataframe_min_gramneg$genus=="Phaeovibrio",])
                    
dataframe_max_gramneg3<- rbind(dataframe_max_gramneg[dataframe_max_gramneg$genus=="Neptunicoccus",],
                               dataframe_max_gramneg[dataframe_max_gramneg$genus=="Nguyenibacter",],
                               dataframe_max_gramneg[dataframe_max_gramneg$genus=="Parapontixanthobacter",],
                               dataframe_max_gramneg[dataframe_max_gramneg$genus=="Phaeovibrio",])


#график корреляции, чтобы посмотреть примерную зависимость переменных(сайта отщепления и charge_ , где заряд минимальный)
ggplot(dataframe_min_gramneg3, aes(x = n_reg, y = min))+ 
  geom_point(col="blue", alpha=.2)+
  xlab("Длина N-региона сигнального пептида, АК остатки")+
  ylab("Длина окна АК последовательности с минимальным зарядом, АК остатки")+
  facet_wrap(~genus)+
  scale_x_continuous(breaks = seq(0,70,5))
#график корреляции, чтобы посмотреть примерную зависимость переменных(сайта отщепления и charge_ , где заряд максимальный)
ggplot(dataframe_max_gramneg3, aes(x = n_reg, y = max))+ 
  geom_point(col="blue", alpha=.2)+
  xlab("Длина N-региона сигнального пептида, АК остатки")+
  ylab("Длина окна АК последовательности с макисмальным зарядом, АК остатки")+
  facet_wrap(~genus)+
  scale_x_continuous(breaks = seq(0,70,5))

#4 DATASET
dataframe_min_gramneg4<- rbind(dataframe_min_gramneg[dataframe_min_gramneg$genus=="Rhizorhapis",], dataframe_min_gramneg[dataframe_min_gramneg$genus=="Rubricella",], 
                      dataframe_min_gramneg[dataframe_min_gramneg$genus=="Thermithiobacillus",],
                      dataframe_min_gramneg[dataframe_min_gramneg$genus=="Woodsholea",])
dataframe_max_gramneg4<- rbind(dataframe_max_gramneg[dataframe_max_gramneg$genus=="Rhizorhapis",], dataframe_max_gramneg[dataframe_max_gramneg$genus=="Rubricella",], 
                      dataframe_max_gramneg[dataframe_max_gramneg$genus=="Thermithiobacillus",],
                      dataframe_max_gramneg[dataframe_max_gramneg$genus=="Woodsholea",])

#график корреляции, чтобы посмотреть примерную зависимость переменных(сайта отщепления и charge_ , где заряд минимальный)
ggplot(dataframe_min_gramneg4, aes(x = n_reg, y =min))+ 
  geom_point(col="blue", alpha=.2)+
  xlab("Длина N-региона сигнального пептида, АК остатки")+
  ylab("Длина окна АК последовательности с минимальным зарядом, АК остатки")+
  facet_wrap(~genus)+
  scale_x_continuous(breaks = seq(0,70,5))
#график корреляции, чтобы посмотреть примерную зависимость переменных(сайта отщепления и charge_ , где заряд максимальный)
ggplot(dataframe_max_gramneg4, aes(x = n_reg, y =max))+ 
  geom_point(col="blue", alpha=.2)+
  xlab("Длина N-региона сигнального пептида, АК остатки")+
  ylab("Длина окна АК последовательности с макисмальным зарядом, АК остатки")+
  facet_wrap(~genus)+
  scale_x_continuous(breaks = seq(0,70,5))

################################ E.coli reference
ecol_table <- select(new_data_filtered_ecoli, 4 )
ecol_table$min_value <- apply(new_data_filtered_ecoli[,11:55], 1, min)#find min value in a row
ecol_table$max_value <- apply(new_data_filtered_ecoli[,11:55], 1, max)#find max value in a row

#make a list of variable names that correspond to min charge
list_min_ecol <- list()
for(i in 1:nrow(new_data_filtered_ecoli[,11:55])){
  new_element6 <- colnames(new_data_filtered_ecoli[i, 11:55])[which(apply(new_data_filtered_ecoli[i,11:55],1, 
                                                               function(x) x==ecol_table$min_value[i]))]
  list_min_ecol[[length(list_min_ecol) + 1]] <- new_element6
}

#make a list of variable names that correspond to max charge
list_max_ecol <- list()
for(i in 1:nrow(new_data_filtered_ecoli[,11:55])){
  new_element7 <- colnames(new_data_filtered_ecoli[i, 11:55])[which(apply(new_data_filtered_ecoli[i,11:55],1, 
                                                                function(x) x==ecol_table$max_value[i]))]
  list_max_ecol[[length(list_max_ecol) + 1]] <- new_element7
}

#append lists of vectors to a dataframe
ecol_table$all_min <- list_min_ecol
ecol_table$all_max <- list_max_ecol
ecol_table$length_min <- sapply(ecol_table$all_min, function(x) length(x))
ecol_table$length_max <- sapply(ecol_table$all_max, function(x) length(x))
ecol_table$n_reg <- new_min_max_ecoli$n_reg
ecol_table$h_reg <- new_min_max_ecoli$h_reg
ecol_table$c_reg <- new_min_max_ecoli$c_reg

#create a list of dataframes with two columns: cleav site and window with minimum charge
df_list_min_ecol <- list()
for(i in 1:nrow(ecol_table)){
  new_element8 <- data.frame(cleav_site = rep(ecol_table$cleav_site[i], ecol_table$length_min[i]), 
                             charge_min = ecol_table$all_min[i],
                             n_reg= rep(ecol_table$n_reg[i], ecol_table$length_min[i]), 
                             h_reg= rep(ecol_table$h_reg[i], ecol_table$length_min[i]),
                             c_reg= rep(ecol_table$c_reg[i], ecol_table$length_min[i]),
                             min_value= rep(ecol_table$min_value[i], ecol_table$length_min[i]))
  colnames(new_element8) <- c("cleav_site", "charge_min", "n_reg", "h_reg", "c_reg", "min_charge_value")
  df_list_min_ecol[[length(df_list_min_ecol) + 1]] <- new_element8
}
df_min_ecol <- bind_rows(df_list_min_ecol, .id= "column label")
#create a list of dataframes with two columns: cleav site and window with maximum charge
df_list_max_ecol <- list()
for(i in 1:nrow(ecol_table)){
  new_element9 <- data.frame(cleav_site = rep(ecol_table$cleav_site[i], ecol_table$length_max[i]), 
                             charge_max = ecol_table$all_max[i],
                             n_reg= rep(ecol_table$n_reg[i], ecol_table$length_max[i]), 
                             h_reg= rep(ecol_table$h_reg[i], ecol_table$length_max[i]),
                             c_reg= rep(ecol_table$c_reg[i], ecol_table$length_max[i]),
                             max_value= rep(ecol_table$max_value[i], ecol_table$length_max[i]))
  colnames(new_element9) <- c("cleav_site", "charge_max", "n_reg", "h_reg", "c_reg", "max_charge_value")
  df_list_max_ecol[[length(df_list_max_ecol) + 1]] <- new_element9
}


df_max_ecol <- bind_rows(df_list_max_ecol, .id= "column label")
#mutate dataframes
df_min_ecol$min <- as.numeric(sapply(df_min_ecol$charge_min, function(x) strsplit(x, "_")[[1]][2]))#отделяю номер charge_ (без этого получается неверный график)
df_max_ecol$max <- as.numeric(sapply(df_max_ecol$charge_max, function(x) strsplit(x, "_")[[1]][2]))
#график корреляции, чтобы посмотреть примерную зависимость переменных(сайта отщепления и charge_ , где заряд минимальный)
ggplot(df_min_ecol, aes(x = n_reg, y = min))+ 
  geom_point(col="red",alpha=.2, position = "jitter")+
  xlab("Длина N-региона сигнального пептида, АК остатки")+
  ylab("Длина окна АК последовательности с минимальным зарядом, АК остатки")+
  scale_x_continuous(breaks = seq(0,70,5))
#график корреляции, чтобы посмотреть примерную зависимость переменных(сайта отщепления и charge_ , где заряд максимальный)
ggplot(df_max_ecol, aes(x = n_reg, y = max))+ 
  geom_point(col="red",alpha=.2, position="jitter")+
  xlab("Длина N-региона сигнального пептида, АК остатки")+
  ylab("Длина окна АК последовательности с макисмальным зарядом, АК остатки")+
  scale_x_continuous(breaks = seq(0,70,5))

