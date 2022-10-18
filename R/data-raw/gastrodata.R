#' Gastrointestinal dataset
#'
#'The collected dataset is composed of 76 colonoscopic videos (recorded with both White Light (WL) and Narrow Band Imaging (NBI)), the histology (classification ground truth), and  the endoscopist's opinion (including 4 experts and 3 beginners).
#'There are $n=76$ observations, and each observation consists of 698 features extracted from colonoscopic videos on patients with gastrointestinal lesions.
## code to prepare `gastrodata` dataset goes here
library(tidyverse)
gastrodata<-read_csv('~/desktop/ssl-function/gasdata/data.txt', col_names=FALSE)
usethis::use_data(gastrodata, overwrite = TRUE)
