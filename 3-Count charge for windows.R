####Gramnegative 
# making column with sequence of mature proteins
new_data_filtered_gramneg$cleaved_protein <- substr(new_data_filtered_gramneg$sequence, new_data_filtered_gramneg$cleav_site + 1, 
                                              length(new_data_filtered_gramneg$sequence))

#Loop
count_charge <- function(sequence, window){
  sequence <- substr(sequence, 1, window)
  charge <- str_count(sequence, "R") + str_count(sequence, "K") -
    str_count(sequence, "D") - str_count(sequence, "E")
  return (charge)
}

count_charge <- Vectorize(count_charge, SIMPLIFY = T, USE.NAMES = F)

for(i in 6:50) {
  new_data_filtered_gramneg$new <- count_charge(new_data_filtered_gramneg$sequence, i)
  setnames(new_data_filtered_gramneg, "new", paste0("charge_", i))
}

#####E.coli
# making column with sequence of mature proteins
new_data_filtered_ecoli$cleaved_protein <- substr(new_data_filtered_ecoli$sequence, new_data_filtered_ecoli$cleav_site + 1, 
                                                    length(new_data_filtered_ecoli$sequence))

#Loop
count_charge <- function(sequence, window){
  sequence <- substr(sequence, 1, window)
  charge <- str_count(sequence, "R") + str_count(sequence, "K") -
    str_count(sequence, "D") - str_count(sequence, "E")
  return (charge)
}

count_charge <- Vectorize(count_charge, SIMPLIFY = T, USE.NAMES = F)

for(i in 6:50) {
  new_data_filtered_ecoli$new <- count_charge(new_data_filtered_ecoli$sequence, i)
  setnames(new_data_filtered_ecoli, "new", paste0("charge_", i))
}
