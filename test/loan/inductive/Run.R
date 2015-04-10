setwd("C:/ONUR/THESIS/ECLIPSE/Thesis-V1/test/loan/inductive")
require(data.table)

data1=read.csv("ETM_Configuration1_KPI.txt", header=FALSE)
data1raw=data1$V2
data1raw=t(data1raw)

data2=read.csv("ETM_Configuration2_KPI.txt", header=FALSE)
data2raw=data2$V2
data2raw=t(data2raw)

data3=read.csv("ETM_Configuration3_KPI.txt", header=FALSE)
data3raw=data3$V2
data3raw=t(data3raw)

data4=read.csv("ETM_Configuration4_KPI.txt", header=FALSE)
data4raw=data4$V2
data4raw=t(data4raw)

test = data.table(Property= data1[,1], Log1=c(data1raw), Log2=c(data2raw), Log3=c(data3raw), Log4=c(data4raw))

avgValues = test[1:144,]
avgValues [avgValues ==0] <-NaN
avgValuesClean <- avgValues [complete.cases(avgValues ), ]
avgValuesClean = t(avgValuesClean )
colnames(avgValuesClean ) = (avgValuesClean [1,])
avgValuesClean = avgValuesClean [2:5,]

wss <- (nrow(avgValuesClean)-1)*sum(apply(avgValuesClean,2,var))
for (i in 1:3) wss[i] <- sum(kmeans(avgValuesClean,	centers=i)$withinss)
plot(1:3, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")
kmeansResult = kmeans(avgValuesClean,2)

stdValues = test[433:576,]
stdValues [stdValues ==0] <-NaN
stdValuesClean <- stdValues [complete.cases(stdValues ), ]
stdValuesClean = t(stdValuesClean )
colnames(stdValuesClean ) = (stdValuesClean [1,])
stdValuesClean = stdValuesClean [2:5,]
kmeansResultStd = kmeans(stdValuesClean ,2)

wss <- (nrow(stdValuesClean )-1)*sum(apply(stdValuesClean ,2,var))
for (i in 1:3) wss[i] <- sum(kmeans(stdValuesClean ,	centers=i)$withinss)
plot(1:3, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")

