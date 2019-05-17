Lab 2 Adewuya Gbemisola
## 1. 
package<- rep(c("plastic", "vacuum", "1%CO", "100%CO2"), each = 3)
> bacteria <- data.frame(package, logcount)
> attach(bacteria)

 tapply(logcount,package,mean)
 1%CO 100%CO2 plastic  vacuum 
   7.26    3.36    7.48    5.50 

> tapply(logcount,package,sd)
     1%CO   100%CO2   plastic    vacuum 
0.1946792 0.3968627 0.4386342 0.2749545

>  bacteria <- data.frame(cbind(package, logcount))
> stripchart(logcount~package, vertical=TRUE, pch=16)

> all_mean <- tapply(logcount,package,sd)
> all_mean
     1%CO   100%CO2   plastic    vacuum 
0.1946792 0.3968627 0.4386342 0.2749545 


all_mean <- tapply(logcount,package,mean)
> lines(all_mean)
> bact.lm <- aov(logcount~package)
>  summary.aov(bact.lm)
            Df Sum Sq Mean Sq F value   Pr(>F)    
package      3  32.87  10.958   94.58 1.38e-06 ***
Residuals    8   0.93   0.116                     
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


> anova(bact.lm)
Analysis of Variance Table

Response: logcount
          Df Sum Sq Mean Sq F value    Pr(>F)    
package    3 32.873 10.9576  94.584 1.376e-06 ***
Residuals  8  0.927  0.1159                      
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

> summary.lm(bact.lm)
> bartlett.test(logcount~package)
> TukeyHSD(bact.lm)


## 2

> strength <- c(3129, 3000, 2865, 2890, 3200, 3300, 2975, 3150, 2800, 2900, 2985, 3050, 2600, 2700, 2600, 2765)
> mixing <- rep(c(1:4),each = 4)
> 
> cement <- data.frame(cbind(mixing, strength))
> attach(cement)
> mixing <- as.factor(mixing)
> cement.aov <- aov(strength ~ mixing)
> summary.aov(cement.aov)
> qt(.975,12)
>  2.178813* sqrt(12826/2)

> y.means <- tapply (strength, mixing, mean)
> TukeyHSD(cement.aov)

> TukeyHSD(cement.aov)
  Tukey multiple comparisons of means
    95% family-wise confidence level

Fit: aov(formula = strength ~ mixing)

$mixing
       diff        lwr        upr     p adj
2-1  185.25  -52.50029  423.00029 0.1493561
3-1  -37.25 -275.00029  200.50029 0.9652776
4-1 -304.75 -542.50029  -66.99971 0.0115923
3-2 -222.50 -460.25029   15.25029 0.0693027
4-2 -490.00 -727.75029 -252.24971 0.0002622
4-3 -267.50 -505.25029  -29.74971 0.0261838

> tukey <- TukeyHSD(cement.aov)
> plot(tukey)


## 3

> coating_type <- rep(c(1:4),each = 4)
> conductivity <- rep(c(143, 141, 150, 146, 152, 149, 137, 143, 134, 136, 132, 127, 129, 127, 132, 129))
> tv <- data.frame(cbind(coating_type,conductivity))
> attach(tv)

> coating_type <- factor(coating_type)
> tv.aov <- aov(conductivity ~ coating_type)
> summary.aov(tv.aov)

> mean(conductivity)
> means <- tapply(conductivity, coating_type, mean)
> grand.means <- rep(137.9375, time=4)

> coating.4 <- conductivity[c(13:6)]
> sd(coating.4)
> summary.aov(tv.aov)



> LSD.test(tv.aov, "coating_type", 19.69)
> result <- LSD.test(tv.aov,"coating_type", 19.69, console=T)

> stripchart(conductivity~coating_type, vertical=TRUE, pch=16)

> qtukey(.05,4,12)
> qt(.975,4,12)




