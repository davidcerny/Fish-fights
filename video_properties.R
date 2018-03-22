# The video file durations were obtained using the following shell command:
# find . -maxdepth 1 -iname '*.MOV' -exec ffprobe -v quiet -of csv=p=0 -show_entries format=duration {} \;
videonames <- read.table("/Users/David/Documents/College/2017–18/Winter 2018/FBQ_data/video_file_names.txt", header = FALSE, stringsAsFactors = FALSE)
videodurations <- read.table("/Users/David/Documents/College/2017–18/Winter 2018/FBQ_data/video_file_lengths.txt", header = FALSE)
videos <- cbind(videonames, videodurations)
colnames(videos) <- c("File", "Duration")
sum(videos$Duration)/3600
# Add a plot column:
for(i in 1:nrow(videos)) {
  # This extracts the last character before the occurrence of the string ".MOV":
  videos[i,3] <- sub('.*(?=.$)', '', strsplit(videos$File[i], ".MOV"), perl = T)
}
colnames(videos)[3] <- "Plot"

# Extract the days of recording:
pattern <- "./(.*?)_"
matches <- regmatches(videos$File, regexec(pattern, videos$File))
days <- vector()
for(i in 1:length(matches)) {
  days <- c(days, matches[[i]][2])
}
days <- levels(as.factor(days))
# Conveniently, the times of the recordings made in the morning begin with 0, while 
# the times of the recordings made in the afternoon begin with 1
sessions <- c(rep(NA, 2*length(days)))
sessions[1:length(days)] <- paste(days, "_0", sep = "")
sessions[(length(days)+1):(2*length(days))] <- paste(days, "_1", sep = "")

library(plyr)
durations <- vector()
for(i in 1:length(sessions)) {
  # All plots, one session
  all_plots_one_session <- videos[which(grepl(sessions[i], videos$File) == TRUE),]
  # Sum of the durations of the videos made at one plot during that session:
  one_plot_one_session <- ddply(all_plots_one_session, "Plot", numcolwise(sum))
  durations <- c(durations, one_plot_one_session$Duration)
}
# Average length of recording made at one plot during one session:
mean(durations)/60

# How many hours of footage were scored?
scored <- read.csv("/Users/David/Documents/College/2017–18/Winter 2018/FBQ_data/ScoredFishInteraction_Feb19.csv", sep = ",", strip.white = TRUE)
scoredfiles <- paste("./", levels(scored$File.Name), ".MOV", sep = "")

# There is a mismatch between the two because of an extra underscore inserted before
# the plot number:
namestofix <- scoredfiles[which(!scoredfiles %in% videos$File)]
fixednames <- gsub('(.*_.*_.*?)_(.*?)','\\1\\2', namestofix)
scoredfiles <- c(scoredfiles[!scoredfiles %in% namestofix], fixednames)

# Now add the names of the videos that Kristen watched but which had no interactions
# in them:
videos$Duration[videos$File %in% scoredfiles]
kristen <- read.table("/Users/David/Documents/College/2017–18/Winter 2018/FBQ_data/kristens_videos.txt", stringsAsFactors = FALSE)

# Verify that there is no mismatch between these file names and the filenames from 
# the backup drive
length(kristen[which(!kristen %in% videos$File)])

# ALL watched files:
watched <- c(scoredfiles, sort(kristen[which(!kristen %in% scoredfiles)]))
sum(videos$Duration[videos$File %in% watched])/3600
