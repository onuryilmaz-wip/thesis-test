setwd("C:/ONUR/THESIS/ECLIPSE/Thesis-V1/test/coselog/filtered")
require(data.table)

data1=read.csv("CoSeLoG WABO 1_KPI.txt", header=FALSE)
data1raw=data1$V2
data1raw=t(data1raw)

data2=read.csv("CoSeLoG WABO 2_KPI.txt", header=FALSE)
data2raw=data2$V2
data2raw=t(data2raw)

data3=read.csv("CoSeLoG WABO 3_KPI.txt", header=FALSE)
data3raw=data3$V2
data3raw=t(data3raw)

data4=read.csv("CoSeLoG WABO 4_KPI.txt", header=FALSE)
data4raw=data4$V2
data4raw=t(data4raw)

data5=read.csv("CoSeLoG WABO 5_KPI.txt", header=FALSE)
data5raw=data5$V2
data5raw=t(data5raw)
 

 

test = data.table(Property= data1[,1], Municipality_1=c(data1raw), Municipality_3=c(data3raw), Municipality_4=c(data4raw), Municipality_5=c(data5raw))


completeData = test;
completeData [completeData ==0] <-NaN
completeDataClean <- completeData [complete.cases(completeData ), ]
completeDataClean = t(completeDataClean )
colnames(completeDataClean ) = (completeDataClean [1,])
completeDataClean = completeDataClean [2:5,]

wss <- (nrow(completeDataClean )-1)*sum(apply(completeDataClean ,2,var))
for (i in 1:3) wss[i] <- sum(kmeans(completeDataClean ,	centers=i)$withinss)
plot(1:3, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")

kmeansResultcompleteData = kmeans(completeDataClean ,2)

write.table(kmeansResultcompleteData$centers, "C:/ONUR/THESIS/ECLIPSE/Thesis-V1/test/coselog/filtered/cluster-means.txt", sep="\t")


