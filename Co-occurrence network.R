# install.packages("igraph")
# install.packages("psych")
library(igraph)
library(psych)
otu <- read.csv("otu.csv", row.names = 1)
otu <- t(otu)
occor <- corr.test(otu,method="spearman",adjust="fdr",alpha=0.05)
occor.r <-  occor$r
occor.p <- occor$p 
p <- p.adjust(occor.p, method = 'BH') 
occor.r[occor.p>0.05|abs(occor.r)<0.8] = 0 
diag(occor.r) <- 0
z <- occor.r * occor.p
diag(z) <- 0   
set.seed(123)
g <- graph.adjacency(z, weighted = TRUE, mode = 'undirected')
g <- delete.vertices(g, names(degree(g)[degree(g) == 0]))
E(g)$correlation <- E(g)$weight
E(g)$weight <- abs(E(g)$weight)
E(g)$cor[E(g)$correlation>0]<- 1
E(g)$cor[E(g)$correlation<0]<- -1
tax <- read.csv('Comments.csv', row.name = 1)
tax <- tax[as.character(V(g)$name), ]
V(g)$Domain <- tax$Domain
V(g)$Phylum <- tax$Phylum
V(g)$Class <- tax$Class
V(g)$Order <- tax$Order
V(g)$Family <- tax$Family
V(g)$Genus <- tax$Genus
V(g)$Species <- tax$Species
plot(g)
write_graph(g, " otu.graphml","graphml")
