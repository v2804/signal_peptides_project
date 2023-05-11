library(dplyr)
#Filterring E.coli data: h region =< 20, only sec pathway
count(new_data_filtered_ecoli, sec_sys)
new_data_filtered_ecoli <- new_data_filtered_ecoli %>%
  filter(sec_sys != "TAT") %>%
  filter(sec_sys != "TATLIPO")
new_data_filtered_ecoli <- filter(new_data_filtered_ecoli, h_end-h_start +1 <= 20)

#filterring gram negative bacteria data: h region <= 20, only sec pathway
#getting rid of unknown genus 
count(new_data_filtered_gramneg, genus)
new_data_filtered_gramneg <- new_data_filtered_gramneg %>%
  filter(genus %in% c("Alkalilacustris brevis", "Ameyamaea chiangmaiensis", "Aquidulcibacter paucihalophilus","Cucumibacter marinus",
                      "Halovulum dunhuangense", "Hellea balneolensis", "Limibacillus halophilus", "Luteithermobacter gelatinilyticus",
                      "Neptunicoccus sediminis", "Nguyenibacter vanlangensis", "Parapontixanthobacter aurantiacus", 
                      "Phaeovibrio sulfidiphilus", "Rhizorhapis suberifaciens", "Rubricella aquisinus", 
                      "Thermithiobacillus tepidarius","Woodsholea maritima"))
new_data_filtered_gramneg$genus <- sapply(new_data_filtered_gramneg$genus, function(x) strsplit(x, " ")[[1]][1])

count(new_data_filtered_gramneg, sec_sys)
new_data_filtered_gramneg <- new_data_filtered_gramneg %>%
  filter(sec_sys != "TAT") %>%
  filter(sec_sys != "TATLIPO")
new_data_filtered_gramneg <- filter(new_data_filtered_gramneg, h_end-h_start +1 <= 20)


