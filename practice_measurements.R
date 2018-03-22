practice <- read.csv("/Users/David/Documents/College/2017–18/Winter 2018/FBQ_data/practice.csv", strip.white = TRUE, header = TRUE)
# Use paired t-test to see if anyone's estimates were biased:
davids_accuracy <- t.test(practice$Actual_cm, practice$D, alternative = 'two.sided', paired = TRUE)
jocelyns_accuracy <- t.test(practice$Actual_cm, practice$J, alternative = 'two.sided', paired = TRUE)
kristens_accuracy <- t.test(practice$Actual_cm, practice$K, alternative = 'two.sided', paired = TRUE)
# Calculate the average error and its standard deviation
error_across_observers <- c(practice$Actual_cm - practice$D, practice$Actual_cm - practice$J, practice$Actual_cm - practice$K)
mean(abs(error_across_observers))
sd(abs(error_across_observers))
