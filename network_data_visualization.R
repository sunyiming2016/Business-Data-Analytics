
## Visualize networked data
#log in data and extract what I want
airbnb<-read.csv("listings.csv")
View(airbnb)
install.packages("sqldf")
library(sqldf)

top <-  sqldf('SELECT *
              FROM
              (SELECT count(*),
              host_id
              FROM airbnb
              GROUP BY 2
              ORDER BY 1 DESC
              LIMIT 5) AS t1
              LEFT JOIN airbnb ON airbnb.host_id=t1.host_id ')
View(top)
#visualize the networked data, see the homes that different hosts have
install.packages("igraph")
library(igraph)
top[,2]<- as.factor(top[,2])
top[,3]<- as.factor(top[,3])

graph.edges <- as.matrix(top[,2:3])
g<-graph.edgelist(graph.edges,directed = FALSE)
hashouse<-V(g)$house_id %in% graph.edges[,2]

plot(g,vertex.label=NA, vertex.color=ifelse(hashouse,"gray","black"),
     vertex.size=ifelse(hashouse,7,10))

