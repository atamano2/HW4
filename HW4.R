bodymass = read.csv("./bodymass.csv")

plot(bodymass$height, bodymass$bodymass, xlab="Height" , ylab="Bodymass" , main="Bodymass vs. heights")
abline(lm(bodymass~height, data=bodymass))

plot(lm(bodymass~height, data=bodymass))

body.fit = lm(bodymass~height+I(height^2), data=bodymass)

library(car)
vif(body.fit)

bodymass$leverage = hatvalues(body.fit)

head(bodymass[order(bodymass$leverage, decreasing=TRUE),])

summary(bodymass)

physical = read.table("./PhysicalData.txt" , header=TRUE)

cor(physical)

fit.physical = lm(HeadCirc~RtFoot+Male, data=physical)
physical$hat = hatvalues(fit.physical)

head(physical[order(physical$hat, decreasing=TRUE),])

summary(physical[physical$Male==1,])

plot(physical$RtFoot, physical$HeadCirc, xlab="Right Foot size" , ylab="Head Circumference")

points(head(physical[order(physical$hat, decreasing=TRUE),])[1,"RtFoot"], head(physical[order(physical$hat, decreasing=TRUE),])[1,"HeadCirc"], col="red", cex=1.5 , pch=19)

points(head(physical[order(physical$hat, decreasing=TRUE),])[2,"RtFoot"], head(physical[order(physical$hat, decreasing=TRUE),])[2,"HeadCirc"], col="red", cex=1.5 , pch=19)

fit.physical2 = lm(HeadCirc~LeftFoot+RtFoot+Male, data=physical)

physical$hat2 = hatvalues(fit.physical2)

head(physical[order(physical$hat2, decreasing=TRUE),])

summary(physical[physical$Female==1,])

library(leaps)

full = lm(HeadCirc~.-Female, data=physical)

forward = step(lm(HeadCirc~1, data=physical), scope=list(upper=full), direction="forward")

summary(forward)

subsets = regsubsets(HeadCirc~.-Female, data=physical)
