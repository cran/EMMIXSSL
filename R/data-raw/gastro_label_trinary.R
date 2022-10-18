## code to prepare `gastro_label_trinary` dataset goes here
gastro_label_trinary<- read_csv('~/desktop/ssl-function/gasdata/Data/gastro_labels_trinary.csv')
usethis::use_data(gastro_label_trinary, overwrite = TRUE)
