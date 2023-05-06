library(dplyr)
library(openxlsx)
library(readxl)

score <- read_excel("C:/Users/Пользователь/Desktop/dIpLoM/data/static models/Features for f1h mod.xlsx")
fs1_net <- read_excel("C:/Users/Пользователь/Desktop/dIpLoM/data/static models/W3 full net.xlsx")
View(fs1_net)
View(score)
friends <- fs1_net


full_res <- score
names(full_res)
results <- c(3,4,5,6,7,8)
names(full_res[,results])
fr_full_mean_res <- c()

for (i in unique(friends$Ego)){
  
  fr <- friends$Alter[friends$Ego == i]
  
  mean_i <- c()
  
  for (j in results){
    
    scores <-c(unlist(full_res[,j]))
    
    score_i <- scores[full_res$ID %in% fr]
    
    fr_score_i <- mean(score_i)
    
    mean_i <- c(mean_i,fr_score_i)
    
  }
  
  fr_i_mean_res <- c(i,mean_i)
  
  fr_full_mean_res <- rbind(fr_full_mean_res,fr_i_mean_res)
  
}


colnames(fr_full_mean_res) <- c('student_id',names(full_res[,results]))
View(fr_full_mean_res)
dim(fr_full_mean_res)
FriendScorefull <- data.frame(fr_full_mean_res)
write.xlsx(FriendScorefull , file = "W3 f score.xlsx")
