No. 1
 chem <- read.csv(file.choose(), header = TRUE)
> chem
    X Chemical Bolt Strength
1   1        1    1       73
2   2        1    2       68
3   3        1    3       74
4   4        1    4       71
5   5        1    5       67
6   6        2    1       73
7   7        2    2       67
8   8        2    3       75
9   9        2    4       72
10 10        2    5       70
11 11        3    1       75
12 12        3    2       68
13 13        3    3       78
14 14        3    4       73
15 15        3    5       68
16 16        4    1       73
17 17        4    2       71
18 18        4    3       75
19 19        4    4       75
20 20        4    5       69
> attach(chem)

> Bolt <- factor(Bolt)
> Chemical <- factor(Chemical)
> chem.aov <- aov(Strength ~ Bolt +Chemical)
> summary.aov(chem.aov)
          Df Sum Sq Mean Sq F value   Pr(>F)    
Bolt         4 157.00   39.25  21.606 2.06e-05 ***
Chemical     3  12.95    4.32   2.376    0.121    
Residuals   12  21.80    1.82                     
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

 ## Comparing the treatment means
> chem.lm <- lm(Strength~Bolt)
> summary.lm(chem.lm)

Call:
lm(formula = Strength ~ Bolt)

Residuals:
    Min      1Q  Median      3Q     Max 
-1.7500 -0.5625 -0.5000  0.7500  2.5000 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept)   73.500      0.761  96.580  < 2e-16 ***
Bolt2         -5.000      1.076  -4.646 0.000317 ***
Bolt3          2.000      1.076   1.858 0.082864 .  
Bolt4         -0.750      1.076  -0.697 0.496550    
Bolt5         -5.000      1.076  -4.646 0.000317 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 1.522 on 15 degrees of freedom
Multiple R-squared:  0.8188,	Adjusted R-squared:  0.7704 
F-statistic: 16.94 on 4 and 15 DF,  p-value: 1.952e-05

 bartlett.test(Strength~Bolt)

	Bartlett test of homogeneity of variances

data:  Strength by Bolt
Bartlett's K-squared = 1.1175, df = 4, p-value = 0.8915


> TukeyHSD(chem.aov)
> tk <- TukeyHSD(chem.aov)
> plot(tk)
  Tukey multiple comparisons of means
    95% family-wise confidence level

Fit: aov(formula = Strength ~ Bolt + Chemical)

$Bolt
     diff        lwr        upr     p adj
2-1 -5.00  -8.037831 -1.9621691 0.0015656
3-1  2.00  -1.037831  5.0378309 0.2814173
4-1 -0.75  -3.787831  2.2878309 0.9295872
5-1 -5.00  -8.037831 -1.9621691 0.0015656
3-2  7.00   3.962169 10.0378309 0.0000717
4-2  4.25   1.212169  7.2878309 0.0056966
5-2  0.00  -3.037831  3.0378309 1.0000000
4-3 -2.75  -5.787831  0.2878309 0.0830636
5-3 -7.00 -10.037831 -3.9621691 0.0000717
5-4 -4.25  -7.287831 -1.2121691 0.0056966

$Chemical
    diff        lwr      upr     p adj
2-1  0.8 -1.7308322 3.330832 0.7852734
3-1  1.8 -0.7308322 4.330832 0.2042593
4-1  2.0 -0.5308322 4.530832 0.1417326
3-2  1.0 -1.5308322 3.530832 0.6540138
4-2  1.2 -1.3308322 3.730832 0.5182726
4-3  0.2 -2.3308322 2.730832 0.9952030


### checking the residual assumptions

> res <- residuals(chem.aov)
> res
    1     2     3     4     5     6     7     8     9    10    11    12    13    14    15    16    17    18 
 0.65  0.65 -0.35 -0.60 -0.35 -0.15 -1.15 -0.15 -0.40  1.85  0.85 -1.15  1.85 -0.40 -1.15 -1.35  1.65 -1.35 
   19    20 
 1.40 -0.35 
> plot(res)
> lines(res)
> abline(h=0)
> qqnorm(res)


## No. 2

> Solution <- read.csv(file.choose(), header = TRUE)
> Solution
    X Solutions Days Growth
1   1         1    1     13
2   2         1    2     22
3   3         1    3     18
4   4         1    4     39
5   5         2    1     16
6   6         2    2     24
7   7         2    3     17
8   8         2    4     44
9   9         3    1      5
10 10         3    2      4
11 11         3    3      1
12 12         3    4     22

 attach(Solution)
> Days <- factor(Days)
> Solutions <- factor(Solutions)
> washing.aov <- aov(Growth ~ Days +Solutions)
> summary(washing.aov)
            Df Sum Sq Mean Sq F value   Pr(>F)    
Days         3 1106.9   369.0   42.71 0.000192 ***
Solutions    2  703.5   351.8   40.72 0.000323 ***
Residuals    6   51.8     8.6                     
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

tuk <- TukeyHSD(washing.aov)
> plot(tuk)
> tuk
  Tukey multiple comparisons of means
    95% family-wise confidence level

Fit: aov(formula = Growth ~ Days + Solutions)

$Days
          diff        lwr       upr     p adj
2-1  5.3333333  -2.974240 13.640906 0.2193500
3-1  0.6666667  -7.640906  8.974240 0.9917442
4-1 23.6666667  15.359094 31.974240 0.0002622
3-2 -4.6666667 -12.974240  3.640906 0.3037891
4-2 18.3333333  10.025760 26.640906 0.0010843
4-3 23.0000000  14.692427 31.307573 0.0003081

$Solutions
      diff        lwr        upr     p adj
2-1   2.25  -4.126879   8.626879 0.5577862
3-1 -15.00 -21.376879  -8.623121 0.0008758
3-2 -17.25 -23.626879 -10.873121 0.0004067

> resi <- residuals(washing.aov)
> resi
         1          2          3          4          5          6          7          8          9         10 
-2.5833333  1.0833333  1.7500000 -0.2500000 -1.8333333  0.8333333 -1.5000000  2.5000000  4.4166667 -1.9166667 
        11         12 
-0.2500000 -2.2500000 
> qqnorm(resi)
> plot(resi)
> abline(h=0)



## No.3 Aluminum master alloy

 Aluminum <- read.csv(file.choose(), header = TRUE)
> Aluminum
> attach(Aluminum)

Rate <- factor(Rate)
> Furnance <- factor(Furnance)
 aluminum.aov <- aov(Grain ~ Furnance +Rate)
> summary(aluminum.aov)
LSD.test(aluminum.aov, "Furnance", MSerror)
> evidence <- LSD.test(aluminum.aov, "Furnance", MSerror, console=T)
least Significant Difference: 4.710942
 tukeyevi <- TukeyHSD(aluminum.aov)
plot(tukeyevi)

> qqnorm(residu)
> qqline(residu)
 plot(residu, Rate)
>plot(residu, Furnance)


> fitted <- aluminum.aov$fitted.values
> residua <- aluminum.aov$residuals
> plot(fitted,residua)
> abline(h=0)


