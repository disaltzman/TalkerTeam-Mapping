rm(list=ls()) #clear all variables
setwd("~/Documents/UConn/Research/Talker Team - Mapping/randomizations/")

files <- as.data.frame(list.files())
colnames(files) <- "files"
#

for(n in 1:length(files$files)){
  randomization <- read.csv(as.character(files[n,]))
  subject <- unlist(strsplit(as.character(files[n,]),split = ".", fixed = TRUE))[1]
  mixed <- randomization[1:48,]
  blockedmale <- randomization[49:72,]
  blockedfemale <- randomization[73:96,]
  write.csv(mixed, paste0(subject,"mixed.csv"), row.names = FALSE)
  write.csv(blockedmale, paste0(subject,"blockedmale.csv"), row.names = FALSE)
  write.csv(blockedfemale, paste0(subject,"blockedfemale.csv"), row.names = FALSE)
}

