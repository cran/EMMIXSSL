## code to prepare `gastro_label_binary` dataset goes here
gastro_label_binary<-read_csv('~/desktop/ssl-function/gasdata/Data/gastro_labels_binary.csv')
usethis::use_data(gastro_label_binary, overwrite = TRUE)
