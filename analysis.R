library(tidyverse)
dataMat <- read.csv("student-mat.csv")

xfamrel <- dataMat$famrel
yabsences <- dataMat$absences
M = tapply(dataMat$absences, INDEX = dataMat$famrel, FUN= mean)

relevantdata <- spread(dataMat,famrel,absences)
names(relevantdata)[32]<-"Very bad"
names(relevantdata)[33]<-"Bad"
names(relevantdata)[34]<-"Average"
names(relevantdata)[35]<-"Good"
names(relevantdata)[36]<-"Excellent"

png("absencesboxplot.png", width = 700, height = 700)
boxplot(relevantdata[,c(32:36)], pch = 19, main = "Boxplot to Show Distribution In Recorded Absences (Y axis) Between Students Who Report 
        Different Quality of Family Relationships (1 - very bad to 5 - excellent) (X - axis)",
        xlab = "Self Reported Family Relationships (Very bad, Bad, Average, Good, and Excellent)", ylab = "Number of Recorded Absences", frame = T)
points(yabsences~xfamrel, col="cyan")
points(M,col="red",pch="+",cex=2)
legend(x="topright", legend=c("Data points", "Mean", "Outliers"),
       col=c("cyan", "red", "black"), pch = c(1, 3, 19))
dev.off()

write.table(file="descriptivestats.txt",summary(relevantdata[,c(32:36)],digits = 3))
summary(relevantdata[,c(32:36)],digits = 3)

png("verybadhist.png")
hist(relevantdata$`Very bad`)
dev.off()

png("badhist.png")
hist(relevantdata$Bad)
dev.off()

png("averagehist.png")
hist(relevantdata$Average)
dev.off()

png("goodhist.png")
hist(relevantdata$Good)
dev.off()

png("excellenthist.png")
hist(relevantdata$Excellent)
dev.off()


#compare famrel 1 and 2
datatotest <- subset(dataMat, famrel<3)
wilcox.test(absences ~famrel, data = datatotest)


#compare famrel 1 and 3
datatotest2 <- subset(dataMat, famrel == 1 | famrel == 3)
wilcox.test(absences ~famrel, data = datatotest2)


#compare famrel 1 and 4
datatotest3 <- subset(dataMat, famrel == 1 | famrel == 4)
wilcox.test(absences ~famrel, data = datatotest3)

#compare famrel 1 and 5
datatotest4 <- subset(dataMat, famrel == 1 | famrel == 5)
wilcox.test(absences ~famrel, data = datatotest4)


#compare famrel 2 and 3
datatotest5 <- subset(dataMat, famrel == 2 | famrel == 3)
wilcox.test(absences ~famrel, data = datatotest5)


#compare famrel 2 and 4
datatotest6 <- subset(dataMat, famrel == 2 | famrel == 4)
wilcox.test(absences ~famrel, data = datatotest6)

#compare famrel 2 and 5
datatotest7 <- subset(dataMat, famrel == 2 | famrel == 5)
wilcox.test(absences ~famrel, data = datatotest7)


#compare famrel 3 and 4
datatotest8 <- subset(dataMat, famrel == 3 | famrel == 4)
wilcox.test(absences ~famrel, data = datatotest8)


#compare famrel 3 and 5
datatotest9 <- subset(dataMat, famrel == 3 | famrel == 5)
wilcox.test(absences ~famrel, data = datatotest9)

#compare famrel 4 and 5
datatotest10 <- subset(dataMat, famrel == 4 | famrel == 5)
wilcox.test(absences ~famrel, data = datatotest10)
