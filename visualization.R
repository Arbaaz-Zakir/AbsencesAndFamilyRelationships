#install 'tidyverse' if computer does not have it, and call the library
install.packages("tidyverse")
library(tidyverse)

#create a pdf to which visualisation will be placed in file called 'visualization.pdf'
pdf("visualization.pdf", width = 10, height = 10)

#read the dataset and create a data frame with only the two relevant columns used, 'famrel' and 'absences'
d <- read.csv("student-mat.csv")
relevantdata <- d %>% select(famrel, absences)

xfamrel <- relevantdata$famrel
yabsences <- relevantdata$absences

#finding the means of the absences for the different reported family relationships
M = tapply(relevantdata$absences, INDEX = relevantdata$famrel, FUN= mean)

#plotting boxplot
boxplot(yabsences ~ xfamrel, main = "Boxplot to Show Distribution In Recorded Absences (Y axis) Between Students Who Report 
        Different Quality of Family Relationships (1 - very bad to 5 - excellent) (X - axis)",
        xlab = "Self Reported Family Relationships (1 - very bad to 5 - excellent)", ylab = "Number of Recorded Absences", pch = 19, frame = T)

#overlaying all the data points on the boxplot in cyan
points(yabsences~xfamrel, col="cyan")

#overlaying means on boxplot as a red '+'
points(M,col="red",pch="+",cex=2)

#create a key
legend(x="topright", legend=c("Data points", "Mean", "Outliers"), col=c("cyan", "red", "black"), pch = c(1, 3, 19))

dev.off()

