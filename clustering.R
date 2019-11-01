#rmuh
library(ggplot2)
require(cluster)
library("factoextra")
library("Hmisc")
require(MASS)
require(C50)


#Read the dataset with the indicators for the socioeconomic stratus
dat = read.csv("indicadores.csv", header = TRUE)
set.seed(123)

#------------------------------Correlation analysis----------------------------------------------
#Check for redundant attributes
cor_1 <- round(cor(dat[,5:26]), 2)
cor_1
cor_2 <- rcorr(as.matrix(dat[,5:26]))
cor_2$r

library(corrplot)
M <- cor(as.matrix(dat[,5:26]))
#Print the correlation matrix
jpeg("cor-anal.jpeg", width = 7, height = 7, units = 'in', res = 300)
corrplot(M)
dev.off()

write.csv(cor_1, file="corr_anal_1.csv", row.names = TRUE)

#---------------------------Principal component analysis-----------------------------------------
#Compute the principal components of the dataset and plot the scatter plot
dat.pca <- prcomp(dat[,5:25], center = TRUE, scale = TRUE)

dat.pcac <- as.data.frame(dat.pca$x)
jpeg("agebs-censo.jpeg", width = 11, height = 7, units = 'in', res = 300)
g1<- ggplot(data = dat.pcac[,1:2],aes(x = PC1, y = PC2)) +
  labs(x = "PC1 (0.54)", y = "PC2 (0.1)",title="Datos originales") +
  geom_point() +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_cartesian(xlim =c(-1.5, 2.5), ylim = c(-1,1.7))
dev.off()


plot(dat.pca$x[,1],dat.pca$x[,2])
summary(dat.pca)
dat1 <- dat.pca$x[,1:11]

#---------------------------- Outlier detection ------------------------------------------------
#Check for outliers with differents approaches
#--Clustering-based
kmeans.result <- kmeans(dat[,5:25], 7, nstart = 50, algorithm="Lloyd", iter.max=160)
centers <- kmeans.result$centers[kmeans.result$cluster, ]
distances <- sqrt(rowSums((dat[,5:25] - centers)^2))
outliers <- order(distances, decreasing=T)[1:856]
print(outliers)
plot(dat.pca$x[,1],dat.pca$x[,2])
points(dat.pca$x[outliers,], col="red")

#--LOF
library(DescTools)
ol <- LOF(dat[,5:25],15)
outl <- order(ol, decreasing=T)[1:856]
plot(dat.pca$x[,1],dat.pca$x[,2])
points(dat.pca$x[outl,], col="red")

#--density
library(OutlierDetection)
md.2 <- dens(dat[,5:25],cutoff = 0.98)
plot(dat.pca$x[,1],dat.pca$x[,2])
points(dat.pca$x[md.2$`Location of Outlier`,], col="red")

plot((prcomp(dat[-md.2$`Location of Outlier`,5:25], center = TRUE))$x[,1],
     (prcomp(dat[-md.2$`Location of Outlier`,5:25], center = TRUE))$x[,2])

#Scatter plot after the outlier detection
dat = dat[-md.2$`Location of Outlier`,]
dat.pca <- prcomp(dat[,5:25], center = TRUE)

dat.pcac <- as.data.frame(dat.pca$x)
jpeg("agebs-censo-wo.jpeg", width = 11, height = 7, units = 'in', res = 300)
g2 <- ggplot(data = dat.pcac[,1:2],aes(x = PC1, y = PC2)) +
  labs(x = "PC1 (0.55)", y = "PC2 (0.1)",title = "Datos sin valores atípicos") +
  geom_point() +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_cartesian(xlim =c(-1.5, 2.5), ylim = c(-1,1.7))
dev.off()

library(gridExtra)
jpeg("agebs-censo-ww.jpeg", width = 9, height = 8, units = 'in', res = 300)
grid.arrange(g1,g2,nrow = 2)
dev.off()


#-------------------------------Number of clusters----------------------------------------------
#Determine the number of clusters

#-------------------- Elbow 1
library(purrr)
wss <- function(k) {
  kmeans(dat[,5:25], k, nstart = 50, algorithm="Lloyd", iter.max=140)$tot.withinss
}

#Compute and plot Within-cluster-sum of Squared Errors (wss) for k = 1 to k = 15
k.values <- 1:10

#Extract wss for 2-15 clusters
wss_values <- map_dbl(k.values, wss)

plot(k.values, wss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

#--------------------Elbow 2
#Create a vector for storing the sse
sse=vector('numeric')
for(i in 2:10){
  #k-means function in R has a feature withinss which stores sse for each cluster group
  sse[i-1]=sum(kmeans(dat[,5:25], i, nstart = 50, algorithm="Lloyd", iter.max=140)$withinss)
}
#Converting the sse to a data frame and storing corresponding value of k
sse=as.data.frame(sse)
sse$k=seq.int(2,10)
#Making the plot
plot(sse$k,sse$sse,type="b",main="Elbow method",xlab="Number of clusters K",ylab="SSE")


#----------------Silhouette Method
#Function to compute average silhouette for k clusters
smp_size <- floor(0.87 * nrow(dat))
train_ind <- sample(nrow(dat), size = smp_size)
train.df <- as.data.frame(dat[train_ind, ])
test.df <- as.data.frame(dat[-train_ind, ])

fviz_nbclust(train.df[,5:25], cluster::clara,  method = "silhouette")

avg_sil <- function(k) {
  km.res <- kmeans(train.df[,5:25], k, nstart = 50, algorithm="Lloyd", iter.max=140)
  ss <- silhouette(km.res$cluster, dist(train.df[,5:25]))
  mean(ss[, 3])
}

# Compute and plot wss for k = 2 to k = 15
k.values <- 2:10

#Extract avg silhouette for 2-15 clusters
avg_sil_values <- map_dbl(k.values, avg_sil)

plot(k.values, avg_sil_values,
     type = "b", pch = 19, frame = FALSE, 
     xlab = "Number of clusters K",
     ylab = "Average Silhouettes")


#-----------------------------------Clustering-----------------------------------------------
#------------------------ kmeans

#Cluster the points with k-means, k = 3
set.seed(123)
agebsClust1 <- kmeans(dat[,5:25], 3, nstart = 50, algorithm="Lloyd", iter.max=160)

print(agebsClust1$size)

#Scatter plot after the clustering
#fviz_cluster(agebsClust1, data = dat[,5:25],show.clust.cent = TRUE,geom="point")

dat.pcac2 <- cbind.data.frame(agebsClust1$cluster,dat.pca$x)
colnames(dat.pcac2)[1] <- "cluster"
dat.pcac2 <- as.data.frame(dat.pcac2)
dat.pcac2$cluster <- as.factor(dat.pcac2$cluster)

jpeg("agebs-kmeans-3.jpeg", width = 11, height = 7, units = 'in', res = 300)
ggplot(data = dat.pcac2[,1:3],aes(x = PC1, y = PC2)) +
  labs(x = "PC1 (0.55)", y = "PC2 (0.1)") +
  geom_point(aes(color = cluster, shape = cluster), size = 1) +
  theme_classic() +
  coord_cartesian(xlim =c(-1.5, 2.5), ylim = c(-1,1.7))
dev.off()

#Cluster validation
library(fpc)
valid1 <- cdbw(dat[,5:25],agebsClust1$cluster)
valid1
#cdbw: the higher the better
#cohesion: the higher the better
#sep: the lower the better

#----------------------- C5.0 for feature selection
fs_dat <- cbind(agebsClust1$cluster,dat[,5:25])
colnames(fs_dat)[1] <- "cluster"

model_c5 = C5.0(fs_dat[,-1],as.factor(fs_dat$cluster),trials = 10)

summary(model_c5)

#-------------clara

#Cluster the points with clara, k = 3
agebsClust4 <- clara(dat[,5:25],3)


#Scatter plot after the clustering
dat.pcac2 <- cbind.data.frame(agebsClust4$clustering,dat.pca$x)
colnames(dat.pcac2)[1] <- "cluster"
dat.pcac2 <- as.data.frame(dat.pcac2)
dat.pcac2$cluster <- as.factor(dat.pcac2$cluster)

jpeg("agebs-clara-3.jpeg", width = 11, height = 7, units = 'in', res = 300)
ggplot(data = dat.pcac2[,1:3],aes(x = PC1, y = PC2)) +
  labs(x = "PC1 (0.55)", y = "PC2 (0.1)") +
  geom_point(aes(color = cluster, shape = cluster), size = 1) +
  theme_classic() +
  coord_cartesian(xlim =c(-1.5, 2.5), ylim = c(-1,1.7))
dev.off()

#Cluster validation
valid2 <- cdbw(dat[,5:25],agebsClust4$clustering )
valid2

print(agebsClust4)
fviz_cluster(agebsClust4,show.clust.cent = TRUE,geom="point")

#-------Model-based clustering

#library(mclust)
#mc <- Mclust(dat[,5:25], G = 3)
#summary(mc)
#d_clust <- Mclust(dat[,5:25], G=1:10, 
#                  modelNames = mclust.options("emModelNames"))
#d_clust$BIC
#plot(d_clust)
#fviz_cluster(mc)

#Compute the distance between each center. With this distances we can order the groups.
clust <- agebsClust1$centers
print(clust)
D <- vector()
for(i in 1:7){
  n <- i+1
  if(n > 7){
    break
  }
  for(j in n:7){
    aux <- 0
    for(k in 1:21){
      z <- (clust[i,k] - clust[j,k])^2
      aux <- aux + z
    }
    print(aux)
    D <- c(D,aux)
  }
}
Ds <- sort(D)
print(Ds)

#Data set with cluster information
a <- dat[[1]]
b <- dat[[2]]
c <- dat[[3]]
d <- dat[[4]]
x <- agebsClust1$cluster
y <- agebsClust4$clustering
df <- do.call(rbind, Map(data.frame, ENT=a, MUN=b, LOC=c, ageb=d, cluster1=x, cluster2=y))
write.csv(df, file="kmeans_clust_ageb_3.csv", row.names = FALSE)

#------------------------------ Distribution bar charts -----------------------------

dat1 = read.csv("ageb_clustered_order_3.csv", header = TRUE)
colnames(dat1)[1] <- "ENT"
dat1 <- transform(dat1, ENT = as.factor(ENT), 
                  cluster1 = as.factor(cluster1),
                  cluster2 = as.factor(cluster2))

#Plot the distribution of agebs by socioeconomic status

jpeg("hist-kmeans7.jpeg", width = 11, height = 7, units = 'in', res = 300)
plot(x = as.factor(dat1$cluster1),main="Distribución de agebs (kmeans k=7)",xlab = "Nivel Socioeconómico", ylab = "Frecuencia")
dev.off()

jpeg("hist-clara7.jpeg", width = 11, height = 7, units = 'in', res = 300)
plot(x = as.factor(dat1$cluster2),main="Distribución de agebs (clara k=7)",xlab = "Nivel Socioeconómico", ylab = "Frecuencia")
dev.off()

dat1$ENT <- replace(as.character(dat1$ENT), dat1$ENT == "1", "Aguascalientes")
dat1$ENT <- replace(as.character(dat1$ENT), dat1$ENT == "2", "Baja California")
dat1$ENT <- replace(as.character(dat1$ENT), dat1$ENT == "3", "Baja California Sur")
dat1$ENT <- replace(as.character(dat1$ENT), dat1$ENT == "4", "Campeche")
dat1$ENT <- replace(as.character(dat1$ENT), dat1$ENT == "5", "Coahuila")
dat1$ENT <- replace(as.character(dat1$ENT), dat1$ENT == "6", "Colima")
dat1$ENT <- replace(as.character(dat1$ENT), dat1$ENT == "7", "Chiapas")
dat1$ENT <- replace(as.character(dat1$ENT), dat1$ENT == "8", "Chihuahua")
dat1$ENT <- replace(as.character(dat1$ENT), dat1$ENT == "9", "Ciudad de México")
dat1$ENT <- replace(as.character(dat1$ENT), dat1$ENT == "10", "Durango")
dat1$ENT <- replace(as.character(dat1$ENT), dat1$ENT == "11", "Guanajuato")
dat1$ENT <- replace(as.character(dat1$ENT), dat1$ENT == "12", "Guerrero")
dat1$ENT <- replace(as.character(dat1$ENT), dat1$ENT == "13", "Hidalgo")
dat1$ENT <- replace(as.character(dat1$ENT), dat1$ENT == "14", "Jalisco")
dat1$ENT <- replace(as.character(dat1$ENT), dat1$ENT == "15", "México")
dat1$ENT <- replace(as.character(dat1$ENT), dat1$ENT == "16", "Michoacán")
dat1$ENT <- replace(as.character(dat1$ENT), dat1$ENT == "17", "Morelos")
dat1$ENT <- replace(as.character(dat1$ENT), dat1$ENT == "18", "Nayarit")
dat1$ENT <- replace(as.character(dat1$ENT), dat1$ENT == "19", "Nuevo León")
dat1$ENT <- replace(as.character(dat1$ENT), dat1$ENT == "20", "Oaxaca")
dat1$ENT <- replace(as.character(dat1$ENT), dat1$ENT == "21", "Puebla")
dat1$ENT <- replace(as.character(dat1$ENT), dat1$ENT == "22", "Querétaro")
dat1$ENT <- replace(as.character(dat1$ENT), dat1$ENT == "23", "Quintana Roo")
dat1$ENT <- replace(as.character(dat1$ENT), dat1$ENT == "24", "San Luis Potosí")
dat1$ENT <- replace(as.character(dat1$ENT), dat1$ENT == "25", "Sinaloa")
dat1$ENT <- replace(as.character(dat1$ENT), dat1$ENT == "26", "Sonora")
dat1$ENT <- replace(as.character(dat1$ENT), dat1$ENT == "27", "Tabasco")
dat1$ENT <- replace(as.character(dat1$ENT), dat1$ENT == "28", "Tamaulipas")
dat1$ENT <- replace(as.character(dat1$ENT), dat1$ENT == "29", "Tlaxcala")
dat1$ENT <- replace(as.character(dat1$ENT), dat1$ENT == "30", "Veracruz")
dat1$ENT <- replace(as.character(dat1$ENT), dat1$ENT == "31", "Yucatán")
dat1$ENT <- replace(as.character(dat1$ENT), dat1$ENT == "32", "Zacatecas")

#Plot the distribution of agebs by socioeconomic status and by states

jpeg("dist-est-clara3.jpeg", width = 10, height = 7, units = 'in', res = 300)
g <- ggplot(dat1, aes(ENT))
g + geom_bar(aes(fill=cluster2), width = 0.8) + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) + 
  theme_classic() +
  coord_flip() +
  labs(title="Distribución de agebs por estado (clara k=3)",x = "", y = "",fill = "clase") 
dev.off()
