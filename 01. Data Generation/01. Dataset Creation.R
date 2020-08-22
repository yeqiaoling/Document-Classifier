#################################################################################################
# 01. DATASET CREATION

# The aim of this script is to create a dataset with the following information:
#   - Name of the article
#   - Content of the article
#   - Category of the article
#################################################################################################

# Installs
# install.packages("readtext", dependencies=T)

# Imports
library(readtext)
library(dplyr)
# Cleaning environment data
rm(list = ls())

# Working directory
setwd('/Users/yeqiaoling/Desktop/Singularity_take_home/text-document-classifer/01. Data Generation')

# Path of the raw data
TRAIN <- FALSE
if (TRAIN) {
  path <- '/Users/yeqiaoling/Desktop/Singularity_take_home/text-document-classifer/00. Raw Data/train'
  csv_name <- 'train_dataset.csv'
  rda_name <- 'train_dataset.rda'
} else {
  path <- '/Users/yeqiaoling/Desktop/Singularity_take_home/text-document-classifer/00. Raw Data/test'
  csv_name <- 'test_dataset.csv'
  rda_name <- 'test_dataset.rda'
}
# List with the 20 classes
list_categories <- list.files(path = path)

# Check the number of files in each category folder
summary_categories <- data.frame(matrix(ncol = 2, nrow = 0))
colnames(summary_categories) <- c('Category', 'Number_of_Files')

for (category in list_categories){
  category_path <- paste(path, category, sep = '/')
  n_files <- length(list.files(path = category_path))
  summary_categories <- rbind(summary_categories, 
                              data.frame('Category' = category, 
                                         'Number_of_Files' = n_files))
}

summary_categories

# re-sample imbalanced data, i.e., change number of files per category
nof <- summary_categories$Number_of_Files 
boxplot(nof)
IQR <- quantile(nof, prob = 0.75, names = F) - quantile(nof, prob = 0.25, names = F) 
lower_bound <- quantile(nof, prob = 0.5, names = F) - 1.5 * IQR
upper_bound <- quantile(nof, prob = 0.5, names = F) + 1.5 * IQR
under_sample_index <- which(nof < lower_bound)
over_sample_index <- which(nof > upper_bound)
under_sample_size <- ceiling(lower_bound - nof[under_sample_index])
over_sample_size <- ceiling(nof[over_sample_index] - upper_bound)
sample_indices <- list()
for (i in c(under_sample_index, over_sample_index)) {
  sample_indices[[i]] <- sort(sample(1:nof[i], under_sample_size, replace = T))
}

# Read every folder and create the final dataframe in the formaat of (ID, text, category)
df_final <- data.frame(matrix(ncol = 3, nrow = 0))
colnames(df_final) <- c('doc_id', 'text', 'category')

for(category in list_categories){
  category_path <- paste(path, category, sep='/')
  df <- readtext(category_path)
  df["category"] <- category
  df_final <- rbind(df_final, df)
}
colnames(df_final) <- c('File_Name', 'Content', 'Category')

df_final <-
  df_final %>% s
  mutate(Complete_Filename = paste(File_Name, Category, sep='-'))

# Save dataset: .rda
save(df_final, file = rda_name)

# Load dataset
load(file = rda_name)

# Write csv to import to python
write.csv2(df_final,fileEncoding = 'utf8', csv_name, row.names = FALSE)

head(df_final)
