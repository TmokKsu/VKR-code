library("igraph")
library("readxl")
library("openxlsx")
library("dplyr")
library("visNetwork")

#Full Net
W2_full <- read_excel("C:/Users/Пользователь/Desktop/dIpLoM/data/static models/W2 full net.xlsx")
Ego_f <- na.omit(unique(c(W2_full$Ego, W2_full$Alter)))
W2_full_1 <- na.omit(W2_full)
links_full <- data.frame(source=W2_full_1$Ego,target=W2_full_1$Alter)

W2full_dir_gr <- graph_from_data_frame(d=links_full, directed=T, vertices = Ego_f)
#Плотность
edge_density(W2full_dir_gr, loops = FALSE)

#Взаимность - вероятность, что образованная связь будет взаимной.
reciprocity(W2full_dir_gr, ignore.loops = TRUE, mode = c("default"))

#Транзитивность - вероятность что две соседние вершины графа соединены между собой 
transitivity(W2full_dir_gr)

#All from survey net 
W2_surv <-  read_excel("C:/Users/Пользователь/Desktop/dIpLoM/data/static models/W2 surv only.xlsx")
length(W2_surv$Ego)
Ego_s <- na.omit(unique(c(W2_surv$Ego, W2_surv$Alter)))
W2_surv_1 <- na.omit(W2_surv)
links_s <- data.frame(source=W2_surv_1$Ego,target=W2_surv_1$Alter)
length(Ego_s)

W2s_dir_gr <- graph_from_data_frame(d=links_s, directed=T, vertices = Ego_s)
#Плотность
edge_density(W2s_dir_gr, loops = FALSE)

#Взаимность - вероятность, что образованная связь будет взаимной.
reciprocity(W2s_dir_gr, ignore.loops = TRUE, mode = c("default"))

#Транзитивность - вероятность что две соседние вершины графа соединены между собой 
transitivity(W2s_dir_gr)

W2s_matrix <- as_adjacency_matrix(W2s_dir_gr)
W2s_matrix <- as.matrix(W2s_matrix)

dim(W2s_matrix) #192x192
sum(diag(W2_matrix)) #2???


W2s_matrix_data <- as.data.frame(W2s_matrix)
write.xlsx(W2s_matrix_data , file = "МатрицаW2 all from surv.xlsx")

#SAOM
#создадим граф
W2 <- read_excel("Wave_2.xlsx")
View(W2)
Ego <- na.omit(unique(c(W2$Ego, W2$Alter)))

length(W2$Ego)

W2_1 <- na.omit(W2)
View(W2_1)

links <- data.frame(source=W2_1$Ego,target=W2_1$Alter)

#направленный
#W0_gr <- graph_from_data_frame(d=links, directed=T)
#ненаправленный
W2_undir_gr <- graph_from_data_frame(d=links, directed=F, vertices = Ego)
str(V(W2_undir_gr))

length((W2$Ego))

?graph_from_data_frame
View(W1_undir_gr)

W2_dir_gr <- graph_from_data_frame(d=links, directed=T, vertices = Ego)
#Плотность
edge_density(W2_dir_gr, loops = FALSE)

#Взаимность - вероятность, что образованная связь будет взаимной.
reciprocity(W2_dir_gr, ignore.loops = TRUE, mode = c("default"))

#Транзитивность - вероятность что две соседние вершины графа соединены между собой 
transitivity(W2_dir_gr)



#МАТРИЦА СМЕЖНОСТИ 
W2_matrix <- as_adjacency_matrix(W2_dir_gr)
W2_matrix <- as.matrix(W2_matrix)

dim(W2_matrix) #192x192
sum(diag(W2_matrix)) #2???


W2_matrix_data <- as.data.frame(W2_matrix)
write.xlsx(W2_matrix_data , file = "Матрица смежности W2.xlsx")

#посмотрим картинку
#plot(W0_undir_gr, vertex.label = NA, layout = layout_with_fr, vertex.size = 2)
plot(W2_undir_gr, vertex.label = NA, layout = layout_with_fr, vertex.size = 2)

D <- degree(W2_undir_gr)
?degree

#кластеризация
m <- hclust(dist(D))
N <- cutree(m, 5)
View(N)

#######
#### Визуализируем ####
#подготовка данны
data <- W2
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

