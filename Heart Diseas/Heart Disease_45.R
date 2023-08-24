#steps for preprocessing 
#-Read data
#-drop unwanted features 
#-missig value
#-check duplication
#-handle outlier
#-data scaling 

################################################################################
# age - age in years
# sex - sex (1 = male; 0 = female)
# cp - chest pain type (1 = typical angina; 2 = atypical angina; 3 = non-anginal pain; 4 = asymptomatic)
# trestbps - resting blood pressure (in mm Hg on admission to the hospital)
# chol - serum cholestoral in mg/dl
# fbs - fasting blood sugar > 120 mg/dl (1 = true; 0 = false)
# restecg - resting electrocardiographic results (0 = normal; 1 = having ST-T; 2 = hypertrophy)
# thalach - maximum heart rate achieved
# exang - exercise induced angina (1 = yes; 0 = no)
# oldpeak - ST depression induced by exercise relative to rest
# slope - the slope of the peak exercise ST segment (1 = upsloping; 2 = flat; 3 = downsloping)

################################################################################
#first we read data from txt file
dataset=read.table(file.choose(),header=TRUE,sep=",");
dataset;

#print first 5 Rows
head(dataset, n =5);

################################################################################
#drop unwanted features (coloumn ID)
newdata<-dataset[-1];
newdata;

#print first 5 Rows for new dataset
head(newdata, n =5);

################################################################################
#check missig values
is.na(newdata);
sum(is.null(newdata));
sum(is.null(newdata$age));
sum(is.null(newdata$sex));
sum(is.null(newdata$cp));
sum(is.null(newdata$trestbps));
sum(is.null(newdata$chol));
sum(is.null(newdata$fbs));
sum(is.null(newdata$restecg));
sum(is.null(newdata$thalach));
sum(is.null(newdata$exang));
sum(is.null(newdata$oldpeak));
sum(is.null(newdata$oldpeak));
sum(is.null(newdata$slope));
#print Rows with missing values
newdata[!complete.cases(newdata),];
#conc: no missing values 
################################################################################


#check duplicated rows
sum(newdata[duplicated(newdata),]);
#conc: no duplicated rows 

################################################################################
#str(newdata)
#handle outlier 
boxplot(newdata$age,col ="cyan")$out;#no
boxplot(newdata$sex,col ="cyan")$out;#no
boxplot(newdata$restecg,col ="cyan")$out;#no
boxplot(newdata$exang,col ="cyan")$out;#no
boxplot(newdata$slope,col ="cyan")$out;#no
#########################################

boxplot(newdata$cp,col ="cyan")$out;#yes but it is large part from data
boxplot(newdata$fbs,col ="cyan")$out;#yes but it is large part from data 

#########################################

boxplot(newdata$trestbps,col ="cyan")$out;#yes ...... it will be removed
#display rows with outlier
which(newdata$trestbps %in% boxplot(newdata$trestbps,col ="cyan")$out)
# ROWS outlier now  :15,84,127,173,184,189,202,214,232

boxplot(newdata$chol,col ="cyan")$out;#yes...... it will be removed
#display rows with outlier
which(newdata$chol %in% boxplot(newdata$chol,col ="cyan")$out)
# ROWS outlier now  : 15  84 127 173 184 189 202 214 232 ...49 122 153 174 182



boxplot(newdata$thalach,col ="cyan")$out;#yes...... it will be removed
#display rows with outlier
which(newdata$thalach %in% boxplot(newdata$thalach,col ="cyan")$out)
# ROWS outlier now  : 15  84 127 173 184 189 202 214 232 ...49 122 153 174 182...246


boxplot(newdata$oldpeak,col ="cyan")$out;#yes...... it will be removed
#display rows with outlier
which(newdata$oldpeak %in% boxplot(newdata$oldpeak,col ="cyan")$out)
# ROWS outlier now  : 15  84 127 173 184 189 202 214 232 ...49 122 153 174 182...246.... 92 124 184 192 286

#then remove rows
newdata2<-newdata[-c(15,84,127,173,184,189,202,214,232,49,122,153,174,182,246,92,124,184,192,286),]

################################################################################

#data scaling 

Dataset<-scale(newdata2[,1:11])
head(Dataset, n =5);
# ################################################################################
# SEX<-table(newdata2$sex)
# pie(SEX,
#     col=c("cyan","red"),
#     labels = c),
#     rainbow(length(newdata2)))






#display some statistics
#mean,median,max,min,Q1,Q3
print("Age");
summary(newdata$age);
print("sex");
summary(newdata$sex);
print("cp");
summary(newdata$cp);
print("trestbps");
summary(newdata$trestbps);
print("chol");
summary(newdata$chol);
print("fbs");
summary(newdata$fbs);
print("restecg");
summary(newdata$restecg);
print("thalach");
summary(newdata$thalach);
print("exang");
summary(newdata$exang);
print("oldpeak");
summary(newdata$oldpeak);
print("slope");
summary(newdata$slope);
################################################################################
#display Correlation between columns
cor(newdata);
#install.packages("metan")
library(metan)
cor=corr_coef(newdata)
plot(cor)

################################################################################
AGE<-table(newdata2$age)
barplot( AGE,xlab = "AGE",
         ylab = "Frequecy",
         col = c("red"));

TRESTBPS<-table(newdata2$trestbps)
plot(TRESTBPS,type="o",col=c("purple","red"))


##display correlation after preprocessing
cor(newdata2);
#install.packages("metan")
library(metan)
cor2=corr_coef(newdata2)
plot(cor2)

#install.packages("GGally")
library(GGally)
##install.packages("ggplot2")
#newdata2$slope,newdata2$thalach,newdata2$exang,newdata2$age
GGally::ggpairs(newdata2,columns =c(8,1));


hist<-ggplot(data = newdata2,aes(x = chol))
hist + geom_histogram(binwidth = 15,
                      color="purple",
                      fill="purple",
                      alpha=0.5) + ggtitle("HIST for chol col")+
  labs(x="chol",y="Frequency")

## convert chol column to categorial values 
converted_chol<-cut(newdata2$chol,breaks = c(120,160,200,240,280,320,360),labels = c("120<160","160<200","200<240","240<280","280<320","320<360"))
converted_chol

converted_chol<-table(converted_chol)
barplot(converted_chol,col = "cyan")
pie(converted_chol,col = c("Red","cyan","purple","yellow","Green","blue"))
mean(newdata2$thalach)



converted_thalach<-cut(newdata2$thalach,breaks = c(80,106,132,158,184,210),labels = c("80<106","106<132","132<158","158<184","184<210"))
converted_thalach

converted_thalach<-table(converted_thalach)
barplot(converted_thalach,col = "Yellow")
pie(converted_thalach,col = c("Red","cyan","purple","yellow","Green"))






# BAR<-ggplot(data = newdata2,aes(x = newdata2$fbs,fill=newdata2$slope))
# BAR + geom_bar(binwidth = 15,,color="purple",
#                       
#                       alpha=0.5) + ggtitle("HIST for chol col")+
#   labs(x="chol",y="Frequency")
# 

newdata3<-newdata2

library(dplyr)
newdata3$sex<- case_when(
  newdata2$sex == 0 ~ "Female",
  newdata2$sex == 1 ~ "Male"
)


newdata3$fbs<- case_when(
  newdata3$fbs == 0 ~ "Less than 120",
  newdata3$fbs == 1 ~ "Greater than 120"
)
newdata3$slope<- case_when(
  newdata3$slope == 1~ "upsloping",
  newdata3$slope == 2 ~ "flat",
  newdata3$slope == 3 ~ "downsloping",
  
)


#Converted_Sex<-table(Converted_Sex)
#barplot(Converted_Sex,col = c("pink","light blue"))
#Converted_Sex


#############################################
# hist3<-ggplot(data = newdata3,aes(x = age))
# hist3
# hist3 + geom_histogram(binwidth = 5,,
#                      
#                       fill=sex,
#                       alpha=0.5) + ggtitle("HIST for chol col")




#######################################
#newdata2[order(newdata2$age)]
#library(dplyr)
#newdata2 %>%
# ggplot(aes(x=newdata2$age,y=newdata2$thalach,color=exang))+
#geom_line()


library(lattice)
barchart(newdata2$trestbps~newdata2$chol,data = newdata2)








library(ggplot2)
##install.packages("ggsci")
library(ggsci)
ggplot(data = newdata3,mapping = aes(x=thalach, fill = newdata3$sex))+geom_histogram(bins = 30,color="black")+
  theme_light()+scale_fill_manual(values = pal_igv()(2))

ggplot(data = newdata3,mapping = aes(x = sex, fill=fbs))+geom_bar()+
  theme_light()+scale_fill_manual(values = pal_igv()(2))

scatter1<-ggplot(data = newdata3,mapping = aes(x = trestbps,y = thalach,color=slope))+geom_point()+
  theme_bw()
scatter1
scatter2<-ggplot(data = newdata3,mapping = aes(x = trestbps,y = thalach,color=slope))+geom_point()+
  theme_bw()+facet_grid(~slope)
scatter2
scatter1+scatter2


ggplot(data = newdata3,mapping = aes(slope, chol,color=slope))+
  geom_violin(fill="purple")+geom_boxplot(width=0.2)

#install.packages("pheatmap")
library(pheatmap)


######################################################################################################3333

################################################################################


#k_means algorthim
#install.packages("factoextra")
library(factoextra)
#install.packages("NbClust")
library(NbClust)
#######
#distance
Dataset1 <- dist(Dataset)
Dataset1
#how many clusters needed 
#within sum squares
res<-NbClust(Dataset1, distance = "euclidean", min.nc=2, max.nc=8, 
             method = "complete", index = "ch")
res
# Compute and plot wss for k = 2 to k = 15 ,using elbow method
k.max <- 15
wss <- sapply(1:k.max, 
              function(k){kmeans(Dataset, k, nstart=50,iter.max = 15 )$tot.withinss})
wss
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")




# Kmeans
km.out <- kmeans(Dataset1, centers=4,nstart=40)
km.out$centers
print(km.out)
#plot results of final k-means model
fviz_cluster(km.out, data = Dataset1)


#########
#hierarchical-clustering
#for me
# Hierarchical clustering begins by treating every data point as a separate cluster. Then, it repeatedly executes the subsequent steps: Identify the 2 clusters which can be closest together, and. Merge the 2 maximum comparable clusters

#install.packages("dplyr")
library(dplyr)
head(Dataset)
# Finding distance matrix
distance_mat <- dist(Dataset, method = 'euclidean')
distance_mat
Hierar_cl <- hclust(distance_mat, method = "average")
Hierar_cl
plot(Hierar_cl)
# Choosing no. of clusters
# Cutting tree by height
abline(h = 100, col = "purple")
# Cutting tree by no. of clusters
fit <- cutree(Hierar_cl, k = 6 )
table(fit)
rect.hclust(Hierar_cl, k = 6, border = "purple")



####################################################################################
#DBSCAN



#install.packages("fpc")
#install.packages("dbscan")
#library(dbscan)
#library(fpc)
#library(factoextra)
#install.packages("NbClust")
#library(NbClust)
#data("multishapes")
#df <- multishapes[, 1:2]
#db <- fpc::dbscan(df, eps = 0.12, MinPts = 4)
#fviz_cluster(db, data = df, stand = FALSE,
#             ellipse = FALSE, show.clust.cent = FALSE,
#             geom = "point",palette = "jco", ggtheme = theme_classic())


############
#DBSCAN
#install.packages("fpc")
#install.packages("dbscan")
library(dbscan)
library(fpc)
library(factoextra)
#install.packages("NbClust")
library(NbClust)
#Data containing clusters of any shapes
data("multishapes")
multishapes <- multishapes[, 1:2]
kNNdistplot(Dataset,k = 3)
# creation of an object km which store the output of the function kmeans
#minPts: The minimum number of points (a threshold) clustered together for a region to be considered dense.
#eps (ε): A distance measure that will be used to locate the points in the neighborhood of any point.
db_scan <- fpc::dbscan(Dataset, eps =2.85, MinPts = 5)
db_scan

plot(db_scan, multishapes, main = "DBSCAN", frame = TRUE)
fviz_cluster(db_scan, data = Dataset, stand = TRUE,
             ellipse = TRUE, show.clust.cent = TRUE,
             geom = "point",palette = "jco")






#For each point xi, compute the distance between xi and the other points. Finds all neighbor points within distance eps of the starting point (xi). Each point, with a neighbor count greater than or equal to MinPts, is marked as core point or visited.
#For each core point, if it’s not already assigned to a cluster, create a new cluster. Find recursively all its density connected points and assign them to the same cluster as the core point.
#Iterate through the remaining unvisited points in the data set.
#Those points that do not belong to any cluster are treated as outliers or noise.

##########################
#Gusian_Model:Probabilistic model-based clustering techniques/a probabilistic model that assumes all the data points are generated from a mixture of a finite number of Gaussian distributions with unknown parameters
#install.packages("mclust")
library(mclust)
# perform model-based clustering ,specify num of clusters 7
mb = Mclust (Dataset)
summary(mb)
#OptimalSelectedModel
#mb$modelName
#optimal Num of clusters
#mb$G
#choose the best number of models and component using bayesian info(probability)
summary(mb)
#plot(mb, what=c("classification"))
#plot(mb,what = 'BIC')
#plot(mb,what = 'density')

##############Optics
#applay optics on data
#install dbscan package
# Its basic idea is similar to DBSCAN,[3] but it addresses one of DBSCAN's major weaknesses: the problem of detecting meaningful clusters in data of varying density. To do so, the points of the database are (linearly) ordered such that spatially closest points become neighbors in the ordering. Additionally, a special distance is stored for each point that represents the density that must be accepted for a cluster so that both points belong to the same cluster.
opt <- optics(Dataset, eps=10, minPts = 15)
opt
### plot produces a reachability plot
#We can identify the clusters with a threshold, say 3.3, on the reachability distance:
opt1 <- extractDBSCAN(opt, eps_cl = 3.3)
plot(opt1)
#black lines are noise
#######################Affinity Algorthim One of the drawbacks of KMeans is that it is sensitive to the initial random selection of exemplars. Affinity propagation overcomes this problem and we do not need to specify the number of clusters in advance. It compute the optimal number of clusters for us. ​
#install.packages("apcluster")
library(apcluster)
#Determine optimal number of clusters
a <- apcluster(negDistMat(r=2), Dataset)
a
#cat("affinity propogation optimal number of clusters:", length(a@clusters), "\n")
plot(a, Dataset)




###############
#K-Mediods
library(factoextra)
library(cluster)
head(Dataset)
#to find optimal number of clusters
#To perform k-medoids clustering in R we can use the pam() function, which stands for “partitioning around medians”
fviz_nbclust(Dataset, pam, method = "wss")
#Another way to determine the optimal number of clusters is to use a metric known as the gap statistic, which compares the total intra-cluster variation for different values of k with their expected values for a distribution with no clustering.
gap_stat <- clusGap(Dataset,
                    FUN = pam,
                    K.max = 10, #max clusters to consider
                    B = 30) #number of iterations
fviz_gap_stat(gap_stat)
#Perform K-Medoids Clustering with Optimal K
kmed <- pam(Dataset, k = 2)
kmed
fviz_cluster(kmed, data = Dataset)

#Kmedoid(PAM)
#install.packages("cluster")
#install.packages("factextra")
library(cluster)
library(factoextra)

tmp_Set <- Dataset
#optimal number of clusters by avg silhouette method
fviz_nbclust(tmp_Set, pam, method = "silhouette")

pam.res <- pam(tmp_Set, 2)
print(pam.res)

fviz_cluster(pam.res, geom = "point", ellipse.type = "norm")
#####################################################################################
#Clara Algo.

tmp_Set2 <- Dataset

#optimal number of clusters by avg silhouette method
fviz_nbclust(tmp_Set2, clara, method = "silhouette")

#clara clustering 

clara.res <- clara(tmp_Set2, 2, metric ='euclidean')

fviz_cluster(clara.res, geom = "point", ellipse.type = "norm")

#Calculating SSE(sum of squared error) for cluster

#Evaluate clustering results with SSE and SST
#clusterSSE<- SSE(tmp_Set2,clara.res)
#clusterSST<- SST(tmp_Set2)
#calculate the r-squared for the cluster sol.
#rsq <- 1-(cluster.SSE/cluster.SST)


