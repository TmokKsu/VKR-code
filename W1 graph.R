library("igraph")
library("readxl")
library("openxlsx")
library("dplyr")
library("visNetwork")

#Full Net
W1_full <- read_excel("C:/Users/Пользователь/Desktop/dIpLoM/data/static models/W1 full net.xlsx")
Ego_f <- na.omit(unique(c(W1_full$Ego, W1_full$Alter)))
W1_full_1 <- na.omit(W1_full)
links_full <- data.frame(source=W1_full_1$Ego,target=W1_full_1$Alter)

W1full_dir_gr <- graph_from_data_frame(d=links_full, directed=T, vertices = Ego_f)

#степень вершины

D <- degree(W1full_dir_gr)
head(D)
#кластеризация
m <- hclust(dist(D))
N <- cutree(m, 4)




#Плотность
edge_density(W1full_dir_gr, loops = FALSE)

#Взаимность - вероятность, что образованная связь будет взаимной.
reciprocity(W1full_dir_gr, ignore.loops = TRUE, mode = c("default"))

#Транзитивность - вероятность что две соседние вершины графа соединены между собой 
transitivity(W1full_dir_gr)

#All from survey net 
W1_surv <-  read_excel("C:/Users/Пользователь/Desktop/dIpLoM/data/static models/W1 surv only.xlsx")
Ego_s <- na.omit(unique(c(W1_surv$Ego, W1_surv$Alter)))
W1_surv_1 <- na.omit(W1_surv)
links_s <- data.frame(source=W1_surv_1$Ego,target=W1_surv_1$Alter)


W1s_dir_gr <- graph_from_data_frame(d=links_s, directed=T, vertices = Ego_s)
#Плотность
edge_density(W1s_dir_gr, loops = FALSE)

#Взаимность - вероятность, что образованная связь будет взаимной.
reciprocity(W1s_dir_gr, ignore.loops = TRUE, mode = c("default"))

#Транзитивность - вероятность что две соседние вершины графа соединены между собой 
transitivity(W1s_dir_gr)

#МАТРИЦА СМЕЖНОСТИ 
W1s_matrix <- as_adjacency_matrix(W1s_dir_gr)
W1s_matrix <- as.matrix(W1s_matrix)
View(W1s_matrix)
dim(W1s_matrix) 
sum(diag(W1s_matrix)) 


W1s_matrix_data <- as.data.frame(W1s_matrix)
View(W1s_matrix_data)
write.xlsx(W1s_matrix_data , file = "МатрицаW1 all from surv.xlsx")



#SAOM
#создадим граф
W1 <- read_excel("C:/Users/Пользователь/Desktop/dIpLoM/data/Wave_1.xlsx")
View(W1)
Ego <- na.omit(unique(c(W1$Ego, W1$Alter)))
length(Ego)
View(Ego)


W1_1 <- na.omit(W1)
View(W1_1)

head(W1)
links <- data.frame(source=W1_1$Ego,target=W1_1$Alter)

#направленный
#W0_gr <- graph_from_data_frame(d=links, directed=T)
#ненаправленный
W1_undir_gr <- graph_from_data_frame(d=links, directed=F, vertices = Ego)
str(V(W1_undir_gr))

length(unique(W1$Ego))

W1_dir_gr <- graph_from_data_frame(d=links, directed=T, vertices = Ego)

D <- degree(W1_dir_gr)
head(D)
#кластеризация
m <- hclust(dist(D))
N <- cutree(m, 5)


#Плотность
edge_density(W1_dir_gr, loops = FALSE)

#Взаимность - вероятность, что образованная связь будет взаимной.
reciprocity(W1_dir_gr, ignore.loops = TRUE, mode = c("default"))

#Транзитивность - вероятность что две соседние вершины графа соединены между собой 
transitivity(W1_dir_gr)


#МАТРИЦА СМЕЖНОСТИ 
W1_matrix <- as_adjacency_matrix(W1_dir_gr)
W1_matrix <- as.matrix(W1_matrix)
View(W0_matrix)
dim(W1_matrix) #162x162
sum(diag(W1_matrix)) #3???


W1_matrix_data <- as.data.frame(W1_matrix)
View(W0_matrix_data)
write.xlsx(W1_matrix_data , file = "Матрица смежности W1.xlsx")

#посмотрим картинку
#plot(W0_undir_gr, vertex.label = NA, layout = layout_with_fr, vertex.size = 2)
plot(W1_undir_gr, vertex.label = NA, layout = layout_with_fr, vertex.size = 2)



D <- degree(W1_undir_gr)
?degree

#кластеризация
m <- hclust(dist(D))
N <- cutree(m, 5)
View(N)

#######
#### Визуализируем ####
#подготовка данны
data <- W1
nodes <- data.frame(id = names(D), group = as.character(N))
names(nodes) <- c("id", "group")
edges <- data.frame(data)
colnames(edges) <- c("to", "from")


#визуализация
visNetwork(nodes, edges, width = "100%", height = "500px") %>%
  visIgraphLayout() %>%
  visNodes(
    color = list(
      background = "#0085AF",
      border = "#013848",
      highlight = "#FF8000"
    ),
    shadow = list(enabled = TRUE, size = 15)
  ) %>%
  visEdges(
    shadow = FALSE,
    color = list(color = "#0085AF", highlight = "#C62F4B")
  ) %>%
  visOptions(highlightNearest = list(hover = T),
             selectedBy = "group") %>% 
  visGroups(groupname = "1",  shape = "square",
            color = list(
              background = "darkblue",
              border = "#013848",
              highlight = "#FF8000"
            )) %>% 
  visGroups(groupname = "2", shape = "triangle",
            color = list(
              background = "red",
              border = "#013848",
              highlight = "#FF8000"
            )) %>% 
  visGroups(groupname = "3", shape = "diamond",
            color = list(
              background = "lightblue",
              border = "#013848",
              highlight = "#FF8000"
            )) %>% 
  visGroups(groupname = "4", shape = "star",
            color = list(
              background = "green",
              border = "#013848",
              highlight = "#FF8000"
            )) %>% 
  visGroups(groupname = "5", shape = "dot",
            color = list(
              background = "yellow",
              border = "#013848",
              highlight = "#FF8000"
            )) %>% 
  visLayout(randomSeed = 11)

#интерпретация
max(D[N == "1"]); min(D[N == "1"]) #square: 5 - 0
max(D[N == "2"]); min(D[N == "2"]) #triangle: 25-22
max(D[N == "3"]); min(D[N == "3"]) #diamond: 14-6
max(D[N == "4"]); min(D[N == "4"]) #star: 20-15
max(D[N == "5"]); min(D[N == "5"]) #dot: 36-34
