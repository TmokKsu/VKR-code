library("igraph")
library("readxl")
library("openxlsx")
library("dplyr")
library("visNetwork")

#Full Net
W3_full <- read_excel("C:/Users/Пользователь/Desktop/dIpLoM/data/static models/W3 full net.xlsx")

Ego_f <- na.omit(unique(c(W3_full$Ego, W3_full$Alter)))
W3_full_1 <- na.omit(W3_full)
links_full <- data.frame(source=W3_full_1$Ego,target=W3_full_1$Alter)

length(unique(W3_full$Ego))
length((W3_full$Ego))


W3full_dir_gr <- graph_from_data_frame(d=links_full, directed=T, vertices = Ego_f)
#Плотность
edge_density(W3full_dir_gr, loops = FALSE)

#Взаимность - вероятность, что образованная связь будет взаимной.
reciprocity(W3full_dir_gr, ignore.loops = TRUE, mode = c("default"))

#Транзитивность - вероятность что две соседние вершины графа соединены между собой 
transitivity(W3full_dir_gr)

#All from survey net 
W3_surv <-  read_excel("C:/Users/Пользователь/Desktop/dIpLoM/data/static models/W3 surv only.xlsx")
Ego_s <- na.omit(unique(c(W3_surv$Ego, W3_surv$Alter)))
length(W3_surv$Ego)
W3_surv_1 <- na.omit(W3_surv)
links_s <- data.frame(source=W3_surv_1$Ego,target=W3_surv_1$Alter)

W3s_dir_gr <- graph_from_data_frame(d=links_s, directed=T, vertices = Ego_s)
#Плотность
edge_density(W3s_dir_gr, loops = FALSE)

#Взаимность - вероятность, что образованная связь будет взаимной.
reciprocity(W3s_dir_gr, ignore.loops = TRUE, mode = c("default"))

#Транзитивность - вероятность что две соседние вершины графа соединены между собой 
transitivity(W3s_dir_gr)

#МАТРИЦА СМЕЖНОСТИ 
W3s_matrix <- as_adjacency_matrix(W3s_dir_gr)
W3s_matrix <- as.matrix(W3s_matrix)
View(W3s_matrix)

dim(W3s_matrix) #165x165
sum(diag(W3s_matrix)) #2???


W3s_matrix_data <- as.data.frame(W3s_matrix)
write.xlsx(W3s_matrix_data , file = "МатрицаW3 all from surv.xlsx")




#SAOM
#создадим граф
W3 <- read_excel("Wave_3.xlsx")
View(W3)
Ego <- na.omit(unique(c(W3$Ego, W3$Alter)))
length(W3$Ego)

W3_1 <- na.omit(W3)
View(W3_1)

links <- data.frame(source=W3_1$Ego,target=W3_1$Alter)

#направленный
#W0_gr <- graph_from_data_frame(d=links, directed=T)
#ненаправленный
W3_undir_gr <- graph_from_data_frame(d=links, directed=F, vertices = Ego)
#str(V(W2_undir_gr))

length((W2$Ego))

?graph_from_data_frame
View(W1_undir_gr)

W3_dir_gr <- graph_from_data_frame(d=links, directed=T, vertices = Ego)

#plot(W3_dir_gr, vertex.label = NA, layout = layout_with_fr, vertex.size = 2)

#Плотность
edge_density(W3_dir_gr, loops = FALSE)

#Взаимность - вероятность, что образованная связь будет взаимной.
reciprocity(W3_dir_gr, ignore.loops = TRUE, mode = c("default"))

#Транзитивность - вероятность что две соседние вершины графа соединены между собой 
transitivity(W3_dir_gr)



#МАТРИЦА СМЕЖНОСТИ 
W3_matrix <- as_adjacency_matrix(W3_dir_gr)
W3_matrix <- as.matrix(W3_matrix)
View(W3_matrix)

dim(W3_matrix) #192x192
sum(diag(W3_matrix)) #2???


W3_matrix_data <- as.data.frame(W3_matrix)
write.xlsx(W3_matrix_data , file = "Матрица смежности W3.xlsx")

#посмотрим картинку
#plot(W0_undir_gr, vertex.label = NA, layout = layout_with_fr, vertex.size = 2)
plot(W2_undir_gr, vertex.label = NA, layout = layout_with_fr, vertex.size = 2)


D <- degree(W3_undir_gr)
?degree

#кластеризация
m <- hclust(dist(D))
N <- cutree(m, 5)
View(N)

#######
#### Визуализируем ####
#подготовка данны
data <- W3
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
max(D[N == "1"]); min(D[N == "1"]) #square: 8 - 0
max(D[N == "2"]); min(D[N == "2"]) #triangle: 22-15
max(D[N == "3"]); min(D[N == "3"]) #diamond: 14-9
max(D[N == "4"]); min(D[N == "4"]) #star: 36-25
max(D[N == "5"]); min(D[N == "5"]) #dot: 44-42


