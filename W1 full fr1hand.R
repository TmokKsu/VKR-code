library(dplyr)
library(readxl)
library(openxlsx)

fs_net <- read_excel("C:/Users/Пользователь/Desktop/dIpLoM/data/static models/W3 full net.xlsx")
View(fs_net)

#пустая таличка со всеми друзьями
friends <- data.frame(rep(0,100))

for (i in unique(fs_net$Ego)) {
  #вектор состоящий из номера студента и перечисления его друзей
  friends_1 <-c(i,fs_net[1][fs_net[2] == i])
  #добавим в исходную таблицу вектор, увеличив его длину за счет "нулей" до 11
  friends <- cbind(friends,c(friends_1, rep(0,100-length(friends_1))))
}

#имена студентов сделаем названиями столбцов 
student_name <- friends[1,] %>% as.matrix() %>% as.vector() 
friends2 <- friends[-1,]
colnames(friends2) <- student_name

#число людей
N <- dim(friends2)[2]
Data_friends_all <- NULL
for(i in 1:N) {
  #столбцы, в которых соедаржатся его друзья
  Col_friends <- colnames(friends2) %in% friends2[,i]
  #вектор друзей его друзей
  a <- friends2[,Col_friends] %>% as.matrix() %>% as.vector() %>% unique()
  #уберем повторы и нули
  a2 <- a[!a %in% friends2[,i] & a != 0]
  #если вектор имеет нулевую длину, то друзей нет; если не нулевую - пишем в столбец
  if(length(a2) > 0) {Data_friends <- data.frame(Name = student_name[i], Friend_name = a2)} else {Data_friends <- NULL}
  Data_friends_all <- rbind(Data_friends_all, Data_friends)
}

View(Data_friends_all)
write.xlsx(Data_friends_all , file = "Friend1hand_W3.xlsx")
