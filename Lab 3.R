amid <- c(2.2, 1.6, 0.8, 1.8, 1.4, 0.4, 0.6,1.5, 0.5)
> vehicle <- c(0.3, 0.0, 0.6, 0.0, -0.3, 0.2)
> saline <- c(0.1, 0.1, 0.2, -0.4, 0.3, 0.1, 0.1,-0.5)
> trt <- c(rep("amid", each=9), rep("vehicle", each=6), rep("saline", each=8))
> tempdiff <- c(amid, vehicle, saline)
> rabbits <- data.frame(trt, tempdiff)
> trt <- factor(trt)
> attach(rabbits)
means <- tapply(tempdiff,trt, mean)
tvar <- tapply(tempdiff, trt, var)
tsdv <- tapply(tempdiff, trt, var)

boxplot(tempdiff~trt)
stripchart(tempdiff~trt, vertical=TRUE, pch=12)
autot.lm <- lm(tempdiff~trt)

summary.aov(autot.lm)
pairwise.t.test(tempdiff, trt, data=rabbits)

##tried both for better understanding
rabbits.aov <- aov(tempdiff~trt, data=rabbits)
> summary.aov(rabbits.aov)

tukey <- TukeyHSD(rabbits.aov)
> plot(TukeyHSD(rabbits.aov))






