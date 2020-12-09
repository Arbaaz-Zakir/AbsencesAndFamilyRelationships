install.packages("tidyverse")
library(tidyverse)
pdf("visualization.pdf", width = 10, height = 10)

d <- read.csv("student-mat.csv")
relevantdata <- d %>% select(famrel, absences)

x <- relevantdata$famrel
y <- relevantdata$absences

M = tapply(relevantdata$absences, INDEX = relevantdata$famrel, FUN= mean)


boxplot(y ~ x, main = "Boxplot to Show Distribution In Recorded Absences (Y axis) Between Students Who Report 
        Different Quality of Family Relationships (1 - very bad to 5 - excellent) (X - axis)",
        xlab = "Self Reported Family Relationships (1 - very bad to 5 - excellent)", ylab = "Number of Recorded Absences", pch = 19, frame = T)

points(y~x, col="cyan")
points(M,col="red",pch="+",cex=2)

legend(x="topright", legend=c("Data points", "Mean", "Outliers"), col=c("cyan", "red", "black"), pch = c(1, 3, 19))

dev.off()

