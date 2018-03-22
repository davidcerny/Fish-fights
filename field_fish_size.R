field_fish_size <- read.csv("/Users/David/Documents/College/2017–18/Winter 2018/FBQ_data/FieldFishSize.csv", sep = ",", strip.white = TRUE)
# Calculate the mean and the standard deviation of the number of observations:
counts <- vector()
for(i in colnames(field_fish_size)[-1]) {
  counts <- c(counts, sum(!is.na(field_fish_size[,i])))
}
mean(counts)
sd(counts)
