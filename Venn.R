rm(list=ls())#clear Global Environment
library(VennDiagram) 
library (venn)
library(ggvenn)
library(gridExtra)
data <- read.table("otu.txt",sep="\t",header=T,check.names=FALSE,row.names = 1)
df <- data.frame(A=rowSums(data[,c(1:3)]),
                 B=rowSums(data[,c(4:6)]),
                 C=rowSums(data[,c(7:9)]),
                 D=rowSums(data[,c(10:12)])) 
head(df)
df1 <- list()
for (i in 1:length(colnames(df))){
  group<- colnames(df)[i]
  df1[[group]] <- rownames(df)[which(df[,i]!= 0)]
}
V1<-ggvenn(data = df1,     
           columns = NULL,
           show_elements = F,
           label_sep = "\n",
           show_percentage = T,
           digits = 1,
           fill_color = c("#377EB8", "#E41A1C", "#4DAF4A", "#984EA3"),
           fill_alpha = 0.5,
           stroke_color = "white",
           stroke_alpha = 0.5,
           stroke_size = 0.5,
           stroke_linetype = "blank",
           set_name_color = "black",
           set_name_size = 6,
           text_color = "black",
           text_size = 4
)+ggtitle("A")
V1
dt <- data.frame(A=rowSums(data[,c(13:15)]),
                 B=rowSums(data[,c(16:18)]),
                 C=rowSums(data[,c(19:21)]),
                 D=rowSums(data[,c(22:24)]))
head(dt)
dt1 <- list()
for (i in 1:length(colnames(dt))){
  group<- colnames(dt)[i]
  dt1[[group]] <- rownames(dt)[which(dt[,i]!= 0)]
}
V2<-ggvenn(data = dt1,
           columns = NULL,
           show_elements = F, 
           label_sep = "\n",
           show_percentage = T,
           digits = 1,
           fill_color = c("#377EB8", "#E41A1C", "#4DAF4A", "#984EA3"),
           fill_alpha = 0.5,
           stroke_color = "white",
           stroke_alpha = 0.5,
           stroke_size = 0.5,
           stroke_linetype = "blank",
           set_name_color = "black",
           set_name_size = 6,
           text_color = "black",
           text_size = 4
)+ggtitle("B")
V2
df3 <- data.frame(E=rowSums(data[,c(1:12)]),
                  F=rowSums(data[,c(13:24)]))
head(df3)
df4 <- list()
for (i in 1:length(colnames(df3))){
  group<- colnames(df)[i]
  df4[[group]] <- rownames(df3)[which(df3[,i]!= 0)]
}
V3<-ggvenn(data = df4,
           columns = NULL,
           show_elements = F,
           label_sep = "\n",
           show_percentage = T,
           digits = 1,
           fill_color = c("darkorange","darkgreen"),
           fill_alpha = 0.5,
           stroke_color = "white",
           stroke_alpha = 0.5,
           stroke_size = 0.5,
           stroke_linetype = "blank",
           set_name_color = "black",
           set_name_size = 6,
           text_color = "black",
           text_size = 4
)+ ggtitle("A")
V3
grid.arrange(V1, V2, V3,ncol = 3)

