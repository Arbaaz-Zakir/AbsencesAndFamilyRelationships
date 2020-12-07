d <- read.csv("student-mat.csv")
relevantdata <- d %>% select(famrel, absences)

x <- newdf$famrel
y <- newdf$absences
x <- relevantdata$famrel
y <- relevantdata$absences

pdf("visualization.pdf")

boxplot(y ~ x, main = "family relationships v absences", xlab = "family relationships", ylab = "absences", pch = 19, frame = T)
points(y~x, col="red")


dev.off()

