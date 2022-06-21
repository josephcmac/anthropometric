# t-test on height between men and women of the same age

library("readxl")

library("dplyr")


# reading male stature

table_male <- read_excel("/home/caballero/Datasets/Anthropometric/20642-FSMA Anthropometric data.xlsx", sheet = "Male")

stature_male <- unlist(filter(table_male,  `Age (months)` > 200)[3])

remove(table_male)

# reading female stature

table_female <- read_excel("/home/caballero/Datasets/Anthropometric/20642-FSMA Anthropometric data.xlsx", sheet = "Female")

stature_female <- unlist(filter(table_female,  `Age (months)` > 200)[3])

remove(table_female)

# comparison as histograms


hist_male <- hist( stature_male )

hist_female <- hist( stature_female )

plot(hist_male, col = rgb(0,0,1,1/4) )

plot(hist_female, col = rgb(1,0,1,1/4), add = T )

# comparison as boxplots

boxplot(stature_male, stature_female)

# paired sample t.test

t.test(stature_male, stature_female)


