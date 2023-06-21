# script to plot the network map
# need to load the output of scenario 1.1
load("~/scenario_A_1.RData")

library(igraph)
pattern1<-c(2,3,5,8,10)
pattern2<-1:7
pattern3<-c(1,2,4,9,10)
pattern4<-c(1,2,3,5,6,8,10)
pattern5<-c(1,2,3,4,6,7)
pattern6<-2:10
pattern7<-1:10
pattern8<-3:10
patternV<-list(pattern1, pattern2, pattern3, pattern4, pattern5, pattern6, pattern7, pattern8)

count_com<-function(ppp){
m1<-matrix(0, nrow=10, ncol=10)
for(i in 1:length(ppp)){  
  m1[ppp[i],ppp[-i]]<-1 }
return(m1)
}

weight_edge<-Reduce('+', lapply(patternV, count_com))
net=graph.adjacency(weight_edge, mode="undirected",weighted=TRUE,diag=FALSE)
#summary(net)
#E(net)$weight
E(net)[weight==2]$color <- "grey"
E(net)[weight==3]$color <- "green"
E(net)[weight==4]$color <- "red"
E(net)[weight==5]$color <- "blue"
E(net)[weight==6]$color <- "black"


node.size<-setNames(as.vector(scenario_out$overall[3,]/1000 *100), sapply(1:10,function(i)paste0("T",i)))


dev.new()
par(mar=c(0,0,0,0))
set.seed(10)
plot.igraph(net,vertex.label=LETTERS[1:10],#V(net)$name,
            layout=layout.fruchterman.reingold, vertex.size=node.size,
            vertex.color="pink",
            edge.color=E(net)$color, 
            edge.width=E(net)$weight,layout=layout.circle)

legend(1,1,fill = c("grey", "green", "red", "blue", "black"),
       legend=2:6, title="Number of direct comparisons", ncol=3)
