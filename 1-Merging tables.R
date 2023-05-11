library(dplyr)
library(stringi)
library(stringr)

#######E.coli
#import table with singleline fasta to workspace
ecoli$sequence <- sapply(ecoli$V1, function(x) strsplit(x, "] ")[[1]][2])
ecoli$ID <- sapply(ecoli$V1, function(x) strsplit(x, " ")[[1]][1])
ecoli$ID <- sapply(ecoli$ID, function(x) strsplit(x, ">")[[1]][2])
ecoli <- ecoli[,2:3]

#import table "prediction_results" in txt format without headings
prediction_results_ecoli <- filter(prediction_results_ecoli, V2 != "OTHER")
prediction_results_ecoli$ID <- sapply(prediction_results_ecoli$V1, function(x) strsplit(x, " ")[[1]][1])
prediction_results_ecoli$cleav_site <- sapply(prediction_results_ecoli$V9, function(x) strsplit(x, ": ")[[1]][2])
prediction_results_ecoli$cleav_site <- as.numeric(sapply(prediction_results_ecoli$cleav_site, function(x) strsplit(x, "-")[[1]][1]))
#prediction_results_ecoli$genus <- sapply(prediction_results_ecoli$V1, function(x) strsplit(x, "[")[[1]][2])
#prediction_results_ecoli$genus <- sapply(prediction_results_ecoli$V1, function(x) strsplit(x, " ")[[1]][1])

#merging in one table 
new_data_filtered_ecoli <-prediction_results_ecoli[,1:2]
colnames(new_data_filtered_ecoli) <- c("name", "sec_sys")
new_data_filtered_ecoli$ID <- prediction_results_ecoli$ID
new_data_filtered_ecoli$cleav_site <- prediction_results_ecoli$cleav_site
new_data_filtered_ecoli <- merge(new_data_filtered_ecoli, ecoli, by.x= "ID", by.y = "ID")

#import table "region output" 
region_output_ecoli$ID <- sapply(region_output_ecoli$V1, function(x) strsplit(x, " ")[[1]][1])
region_output_ecoli <- select(region_output_ecoli, 3,4,5,10)

#merging with new_data
new_data_filtered_ecoli <- merge(new_data_filtered_ecoli, region_output_ecoli[region_output_ecoli$V3 == "n-region",], 
                                 by.x= "ID", by.y = "ID")
new_data_filtered_ecoli <- merge(new_data_filtered_ecoli, region_output_ecoli[region_output_ecoli$V3 == "h-region",], 
                                 by.x= "ID", by.y = "ID")
new_data_filtered_ecoli <- select(new_data_filtered_ecoli, 1,2,3,4,5,7,8,10,11)
colnames(new_data_filtered_ecoli) <- c("ID","name", "sec_sys", "cleav_site", "sequence", "n_start", "n_end", "h_start", "h_end")


#######Gram negative
#import table with singleline fasta to workspace
gramneg$sequence <- sapply(gramneg$V1, function(x) strsplit(x, "] ")[[1]][2])
gramneg$ID <- sapply(gramneg$V1, function(x) strsplit(x, " ")[[1]][1])
gramneg$ID <- sapply(gramneg$ID, function(x) strsplit(x, ">")[[1]][2])
gramneg <- gramneg[,2:3]

#import table "prediction_results" in txt format without headings
prediction_results_gramneg <- filter(prediction_results_gramneg, V2 != "OTHER")
prediction_results_gramneg$ID <- sapply(prediction_results_gramneg$V1, function(x) strsplit(x, " ")[[1]][1])
prediction_results_gramneg$cleav_site <- sapply(prediction_results_gramneg$V9, function(x) strsplit(x, ": ")[[1]][2])
prediction_results_gramneg$cleav_site <- as.numeric(sapply(prediction_results_gramneg$cleav_site, function(x) strsplit(x, "-")[[1]][1]))
prediction_results_gramneg$genus <- sapply(prediction_results_gramneg$V1, function(x) gsub("[][]", "<", x))
prediction_results_gramneg$genus <- sapply(prediction_results_gramneg$genus, function(x) strsplit(x, "<")[[1]][2])
#delete strings with unknown genera

#merging in one table 
new_data_filtered_gramneg <-prediction_results_gramneg[,1:2]
#colnames(new_data_filtered_gramneg) <- c("name", "sec_sys")
new_data_filtered_gramneg$ID <- prediction_results_gramneg$ID
new_data_filtered_gramneg$cleav_site <- prediction_results_gramneg$cleav_site
new_data_filtered_gramneg$genus <- prediction_results_gramneg$genus
new_data_filtered_gramneg <- merge(new_data_filtered_gramneg, gramneg, by.x= "ID", by.y = "ID")

#import table "region output" 
region_output_gramneg$ID <- sapply(region_output_gramneg$V1, function(x) strsplit(x, " ")[[1]][1])
region_output_gramneg<- select(region_output_gramneg, 3,4,5,10)

#merging with new_data
new_data_filtered_gramneg<- merge(new_data_filtered_gramneg, region_output_gramneg[region_output_gramneg$V3 == "n-region",], 
                                 by.x= "ID", by.y = "ID")
new_data_filtered_gramneg<- merge(new_data_filtered_gramneg, region_output_gramneg[region_output_gramneg$V3 == "h-region",], 
                                 by.x= "ID", by.y = "ID")
new_data_filtered_gramneg<- select(new_data_filtered_gramneg, 1,2,3,4,5,6,8,9,11,12)
colnames(new_data_filtered_gramneg) <- c("ID","name", "sec_sys", "cleav_site","genus","sequence", "n_start", 
                                         "n_end", "h_start", "h_end")






  