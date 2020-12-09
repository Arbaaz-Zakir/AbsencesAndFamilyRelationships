install.packages("tidyverse")
library(tidyverse)
pdf("visualization.pdf")

d <- read.csv("student-mat.csv")
relevantdata <- d %>% select(famrel, absences)

#x <- newdf$famrel
#y <- newdf$absences
x <- relevantdata$famrel
y <- relevantdata$absences

M = tapply(relevantdata$absences, INDEX = relevantdata$famrel, FUN= mean)


boxplot(y ~ x, main = "family relationships v absences", xlab = "family relationships", ylab = "absences", pch = 19, frame = T)
points(y~x, col="cyan")


points(M,col="red",pch="+",cex=2)
dev.off()

