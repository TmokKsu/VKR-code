library("igraph")
library("readxl")
library("openxlsx")
library("dplyr")

#Full Net
W0_full <- read_excel("C:/Users/Пользователь/Desktop/dIpLoM/data/static models/W0 full net.xlsx")
Ego_f <- na.omit(unique(c(W0_full$Ego, W0_full$Alter)))
W0_full_1 <- na.omit(W0_full)
links_full <- data.frame(source=W0_full_1$Ego,target=W0_full_1$Alter)

W0full_dir_gr <- graph_from_data_frame(d=links_full, directed=T, vertices = Ego_f)
#Плотность
edge_density(W0full_dir_gr, loops = FALSE)

#Взаимность - вероятность, что образованная связь будет взаимной.
reciprocity(W0full_dir_gr, ignore.loops = TRUE, mode = c("default"))

#Транзитивность - вероятность что две соседние вершины графа соединены между собой 
transitivity(W0full_dir_gr)


#создадим граф
W0 <- read_excel("Wave_0.xlsx")
View(W0)

length(W0$Alter)

Ego <- na.omit(unique(c(W0$Ego, W0$Alter)))
length(Ego)
W0_1 <- na.omit(W0)
View(W0_1)

head(W0)
links <- data.frame(source=W0_1$Ego,target=W0_1$Alter)

#направленный
#W0_gr <- graph_from_data_frame(d=links, directed=T)
#ненаправленный
W0_undir_gr <- graph_from_data_frame(d=links, directed=F, vertices = Ego)
str(V(W0_undir_gr))
?graph_from_data_frame

length(unique(W0$Ego))

?graph_from_data_frame
View(W0_undir_gr)

W0_dir_gr <- graph_from_data_frame(d=links, directed=T, vertices = Ego)

#Плотность
edge_density(W0_dir_gr, loops = FALSE)

#Взаимность - вероятность, что образованная связь будет взаимной.
reciprocity(W0_dir_gr, ignore.loops = TRUE, mode = c("default"))

#Транзитивность - вероятность что две соседние вершины графа соединены между собой 
transitivity(W0_dir_gr)

#МАТРИЦА СМЕЖНОСТИ 
W0_matrix <- as_adjacency_matrix(W0_dir_gr)
W0_matrix <- as.matrix(W0_matrix)
View(W0_matrix)
dim(W0_matrix) #192x192
sum(diag(W0_matrix)) #1


W0_matrix_data <- as.data.frame(W0_matrix)
View(W0_matrix_data)
write.xlsx(W0_matrix_data , file = "Матрица смежности W0.xlsx")


#посмотрим картинку
#plot(W0_undir_gr, vertex.label = NA, layout = layout_with_fr, vertex.size = 2)
plot(W0_undir_gr, vertex.label = NA, layout = layout_with_fr, vertex.size = 2)


W0d <- as.data.frame(W0)
dplyr::filter(W0d$Alter_hash, Alter_hash %in% W0$Ego_hash)
filter(W0d, !Alter_hash %in% Ego_hash) %>% select(Alter_hash) %>% unique()
