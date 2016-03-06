# Env_val
Sarah Klain  
March 6, 2016  


![alt text](Pandora8.jpg)

#### Setup


```r
library(Hmisc) #to run correlations with sig levels
```

```
## Loading required package: grid
## Loading required package: lattice
## Loading required package: survival
## Loading required package: Formula
## Loading required package: ggplot2
## 
## Attaching package: 'Hmisc'
## 
## The following objects are masked from 'package:base':
## 
##     format.pval, round.POSIXt, trunc.POSIXt, units
```

```r
library(ggplot2) # for great charts
library(ggthemes) # for pretty themes in ggplot
library(viridis) # for pretty colors
suppressMessages(library(dplyr))
library(knitr) # tool for making nice tables
library(tidyr) # data table wrangling tool
library(broom)
library(stargazer) # mekes pretty tables
```

```
## 
## Please cite as: 
## 
##  Hlavac, Marek (2015). stargazer: Well-Formatted Regression and Summary Statistics Tables.
##  R package version 5.2. http://CRAN.R-project.org/package=stargazer
```

```r
library(stats) # for PCA & FA
library(psych) # for PCA
```

```
## 
## Attaching package: 'psych'
## 
## The following object is masked from 'package:Hmisc':
## 
##     describe
## 
## The following object is masked from 'package:ggplot2':
## 
##     %+%
```

#### Input data

```r
setwd("/Users/sarahklain/Documents/R_2015/navi") #set working directory
ev <- read.csv("Env_Val_02_29_2016.csv")
```

### Cronbach's alpha

```r
# Apply to NEP scores
nep <- read.csv("nep.csv")
alpha(nep)
```

```
## 
## Reliability analysis   
## Call: alpha(x = nep)
## 
##   raw_alpha std.alpha G6(smc) average_r S/N   ase mean   sd
##       0.74      0.76    0.75      0.39 3.1 0.023    4 0.75
## 
##  lower alpha upper     95% confidence boundaries
## 0.69 0.74 0.78 
## 
##  Reliability if an item is dropped:
##               raw_alpha std.alpha G6(smc) average_r S/N alpha se
## abuse_nep          0.65      0.67    0.66      0.34 2.1    0.030
## bal_r_nep          0.68      0.72    0.71      0.39 2.5    0.028
## crisis_r_nep       0.81      0.81    0.78      0.52 4.3    0.023
## spaceship_nep      0.67      0.70    0.67      0.37 2.3    0.029
## bau_nep            0.62      0.65    0.62      0.32 1.8    0.031
## 
##  Item statistics 
##                 n raw.r std.r r.cor r.drop mean   sd
## abuse_nep     918  0.76  0.79  0.74   0.62  4.3 0.91
## bal_r_nep     916  0.71  0.71  0.59   0.52  4.0 1.05
## crisis_r_nep  914  0.54  0.49  0.26   0.23  3.6 1.31
## spaceship_nep 914  0.73  0.75  0.68   0.55  4.0 1.06
## bau_nep       917  0.82  0.83  0.82   0.68  4.0 1.05
## 
## Non missing response frequency for each item
##                  1    2 2.5    3 3.5    4 4.5    5 miss
## abuse_nep     0.02 0.03   0 0.12   0 0.32   0 0.51 0.03
## bal_r_nep     0.03 0.08   0 0.16   0 0.36   0 0.37 0.03
## crisis_r_nep  0.10 0.12   0 0.18   0 0.29   0 0.31 0.03
## spaceship_nep 0.03 0.08   0 0.16   0 0.33   0 0.41 0.03
## bau_nep       0.02 0.08   0 0.17   0 0.29   0 0.44 0.03
```

```r
# Apply to M-Turk, all variables
mt <- read.csv("env_val_MT.csv")
alpha(mt)
```

```
## Warning in var(if (is.vector(x)) x else as.double(x), na.rm = na.rm): NAs
## introduced by coercion
```

```
## Warning in var(if (is.vector(x)) x else as.double(x), na.rm = na.rm): NAs
## introduced by coercion
```

```
## Warning in alpha(mt): Item = sub_pop had no variance and was deleted
```

```
## Warning in alpha(mt): Item = id_num had no variance and was deleted
```

```
## Warning in alpha(mt): Some items were negatively correlated with the total
## scale and probably should be reversed. To do this, run the function again
## with the 'check.keys=TRUE' option
```

```
## Some items ( bal_nep crisis_nep extract_ins loss_instr decade_mor tech right univ_degr income self.emp pol_dem coast_rec ) were negatively correlated with the total scale and probably should be reversed.  To do this, run the function again with the 'check.keys=TRUE' option
```

```
## 
## Reliability analysis   
## Call: alpha(x = mt)
## 
##   raw_alpha std.alpha G6(smc) average_r  S/N   ase mean   sd
##       0.14      0.49    0.69     0.029 0.95 0.045  3.6 0.41
## 
##  lower alpha upper     95% confidence boundaries
## 0.05 0.14 0.23 
## 
##  Reliability if an item is dropped:
##               raw_alpha std.alpha G6(smc) average_r  S/N alpha se
## abuse_nep         0.132      0.44    0.66     0.025 0.80    0.045
## bal_nep           0.157      0.53    0.71     0.034 1.11    0.045
## crisis_nep        0.163      0.55    0.72     0.038 1.21    0.045
## spaceship_nep     0.125      0.45    0.67     0.026 0.83    0.044
## bau_nep           0.131      0.45    0.66     0.025 0.81    0.044
## extract_ins       0.153      0.52    0.71     0.033 1.07    0.045
## loss_instr        0.158      0.53    0.71     0.035 1.11    0.045
## decade_mor        0.168      0.54    0.72     0.037 1.18    0.045
## comm_rel          0.127      0.44    0.66     0.024 0.77    0.045
## wild_rel          0.116      0.42    0.65     0.023 0.73    0.044
## clean_inst        0.135      0.45    0.66     0.025 0.80    0.045
## tech              0.145      0.48    0.69     0.029 0.91    0.045
## iden_rel          0.100      0.41    0.64     0.022 0.71    0.044
## kin_rel           0.104      0.42    0.64     0.022 0.71    0.044
## right             0.170      0.54    0.71     0.037 1.19    0.045
## health_rel        0.124      0.46    0.67     0.027 0.86    0.044
## other_rel         0.130      0.44    0.66     0.024 0.77    0.045
## kin_met           0.092      0.41    0.63     0.022 0.68    0.044
## resp_met          0.102      0.41    0.64     0.022 0.68    0.044
## iden_met          0.085      0.41    0.64     0.022 0.69    0.043
## other_met         0.125      0.43    0.65     0.024 0.76    0.044
## white             0.138      0.49    0.69     0.030 0.95    0.045
## female            0.140      0.50    0.70     0.032 1.01    0.045
## age               0.477      0.49    0.70     0.031 0.98    0.038
## univ_degr         0.135      0.49    0.70     0.031 0.98    0.045
## income            0.157      0.51    0.70     0.032 1.03    0.043
## wages             0.144      0.50    0.70     0.032 1.01    0.045
## self.emp          0.141      0.50    0.70     0.031 1.00    0.045
## pol_dem           0.147      0.52    0.68     0.033 1.06    0.045
## pol_ind           0.145      0.50    0.68     0.032 1.02    0.045
## pol_rep           0.141      0.50    0.69     0.032 1.02    0.045
## coast_rec         0.145      0.51    0.71     0.033 1.04    0.045
## 
##  Item statistics 
##                 n   raw.r  std.r   r.cor  r.drop   mean    sd
## abuse_nep     400  0.1546  0.461  0.4925  0.0951  4.253  0.80
## bal_nep       400 -0.0348 -0.091 -0.1856 -0.1082  2.188  0.97
## crisis_nep    400 -0.0680 -0.276 -0.3944 -0.1448  2.033  1.03
## spaceship_nep 398  0.2107  0.406  0.3920  0.1401  3.877  0.95
## bau_nep       400  0.1677  0.453  0.4858  0.0933  3.895  0.99
## extract_ins   400 -0.0053 -0.033 -0.1556 -0.0780  3.192  0.95
## loss_instr    398 -0.0160 -0.100 -0.2213 -0.0990  2.226  1.09
## decade_mor    400 -0.1057 -0.232 -0.3476 -0.1816  2.140  1.02
## comm_rel      399  0.2113  0.518  0.5268  0.1583  4.068  0.73
## wild_rel      400  0.2744  0.584  0.6236  0.2060  3.697  0.96
## clean_inst    400  0.1409  0.460  0.4668  0.1018  4.685  0.52
## tech          400  0.0559  0.262  0.1883 -0.0129  3.498  0.90
## iden_rel      400  0.3925  0.632  0.6965  0.3307  3.822  0.94
## kin_rel       398  0.3815  0.625  0.6836  0.3231  4.003  0.87
## right         398 -0.1326 -0.244 -0.3536 -0.2052  1.995  0.99
## health_rel    398  0.2108  0.359  0.3290  0.1347  3.719  1.05
## other_rel     400  0.1812  0.514  0.5320  0.1310  4.343  0.70
## kin_met       398  0.4036  0.678  0.7670  0.3226  3.382  1.14
## resp_met      399  0.3793  0.676  0.7561  0.3157  3.985  0.95
## iden_met      400  0.4149  0.662  0.7371  0.3325  2.987  1.25
## other_met     400  0.2107  0.541  0.5660  0.1417  4.088  0.93
## white         400  0.1163  0.192  0.0919  0.0885  0.825  0.38
## female        400  0.0680  0.085 -0.0234  0.0315  0.590  0.49
## age           400  0.8431  0.140  0.0322  0.0046 32.383 11.07
## univ_degr     400  0.1524  0.138  0.0292  0.1168  0.662  0.47
## income        400  0.1641  0.052 -0.0751 -0.0273  5.362  2.51
## wages         400  0.0216  0.082 -0.0247 -0.0161  0.560  0.50
## self.emp      400  0.0472  0.094 -0.0074  0.0239  0.107  0.31
## pol_dem       400 -0.0332 -0.012 -0.0595 -0.0706  0.415  0.49
## pol_ind       400  0.0022  0.071  0.0399 -0.0344  0.407  0.49
## pol_rep       400  0.0608  0.064 -0.0082  0.0386  0.085  0.28
## coast_rec     400  0.0024  0.025 -0.1159 -0.0329  0.360  0.48
```

```r
# alpha(met[3:7])

mt_nep <- (mt[3:7]) # only NEP values
# alpha(mt_nep)
alpha(mt_nep, check.keys=TRUE)
```

```
## Warning in alpha(mt_nep, check.keys = TRUE): Some items were negatively correlated with total scale and were automatically reversed.
##  This is indicated by a negative sign for the variable name.
```

```
## 
## Reliability analysis   
## Call: alpha(x = mt_nep, check.keys = TRUE)
## 
##   raw_alpha std.alpha G6(smc) average_r S/N   ase mean   sd
##       0.84      0.84    0.82      0.52 5.3 0.028    4 0.74
## 
##  lower alpha upper     95% confidence boundaries
## 0.79 0.84 0.89 
## 
##  Reliability if an item is dropped:
##               raw_alpha std.alpha G6(smc) average_r S/N alpha se
## abuse_nep          0.80      0.80    0.77      0.50 4.0    0.036
## bal_nep-           0.82      0.83    0.79      0.54 4.8    0.034
## crisis_nep-        0.79      0.80    0.75      0.49 3.9    0.036
## spaceship_nep      0.84      0.84    0.81      0.57 5.2    0.033
## bau_nep            0.78      0.79    0.75      0.48 3.7    0.037
## 
##  Item statistics 
##                 n raw.r std.r r.cor r.drop mean   sd
## abuse_nep     400  0.79  0.81  0.75   0.69  4.3 0.80
## bal_nep-      400  0.75  0.74  0.64   0.59  3.8 0.97
## crisis_nep-   400  0.83  0.82  0.77   0.70  4.0 1.03
## spaceship_nep 398  0.70  0.71  0.59   0.53  3.9 0.95
## bau_nep       400  0.84  0.84  0.80   0.73  3.9 0.99
## 
## Non missing response frequency for each item
##                  1    2    3    4    5 miss
## abuse_nep     0.00 0.03 0.10 0.43 0.43 0.00
## bal_nep       0.24 0.48 0.16 0.11 0.02 0.00
## crisis_nep    0.37 0.36 0.16 0.10 0.01 0.00
## spaceship_nep 0.01 0.11 0.15 0.47 0.26 0.01
## bau_nep       0.02 0.09 0.18 0.41 0.30 0.00
```

```r
mt_val <- (mt[3:21]) # includes all value prompts
str(mt_val)
```

```
## 'data.frame':	400 obs. of  19 variables:
##  $ abuse_nep    : int  5 5 3 5 4 5 5 5 5 5 ...
##  $ bal_nep      : int  3 2 4 1 3 2 1 2 2 1 ...
##  $ crisis_nep   : int  1 1 3 1 2 1 1 1 1 1 ...
##  $ spaceship_nep: int  5 4 2 4 3 4 4 5 4 5 ...
##  $ bau_nep      : int  5 4 2 5 4 5 5 4 4 5 ...
##  $ extract_ins  : int  4 2 4 3 3 1 2 4 2 2 ...
##  $ loss_instr   : int  4 1 3 1 2 3 2 2 1 1 ...
##  $ decade_mor   : int  4 2 3 1 2 5 2 2 2 1 ...
##  $ comm_rel     : int  5 4 4 4 4 5 4 4 4 5 ...
##  $ wild_rel     : int  5 4 1 5 4 5 3 4 4 5 ...
##  $ clean_inst   : int  5 5 4 5 4 5 5 5 5 5 ...
##  $ tech         : int  5 3 3 4 4 5 4 4 3 4 ...
##  $ iden_rel     : int  3 4 2 5 4 5 5 5 3 5 ...
##  $ kin_rel      : int  3 4 4 5 4 5 4 5 3 5 ...
##  $ right        : int  4 1 4 1 2 1 1 2 1 1 ...
##  $ health_rel   : int  2 4 2 5 4 1 5 4 3 5 ...
##  $ other_rel    : int  5 4 4 5 5 5 5 5 4 5 ...
##  $ kin_met      : int  3 4 3 5 5 5 2 4 3 4 ...
##  $ resp_met     : int  4 4 3 5 5 5 4 4 3 5 ...
```

```r
alpha(mt_val, check.keys = TRUE)
```

```
## Warning in alpha(mt_val, check.keys = TRUE): Some items were negatively correlated with total scale and were automatically reversed.
##  This is indicated by a negative sign for the variable name.
```

```
## 
## Reliability analysis   
## Call: alpha(x = mt_val, check.keys = TRUE)
## 
##   raw_alpha std.alpha G6(smc) average_r S/N   ase mean   sd
##        0.9       0.9    0.92      0.32 9.1 0.011  3.8 0.55
## 
##  lower alpha upper     95% confidence boundaries
## 0.88 0.9 0.92 
## 
##  Reliability if an item is dropped:
##               raw_alpha std.alpha G6(smc) average_r  S/N alpha se
## abuse_nep          0.89      0.89    0.91      0.32  8.3    0.012
## bal_nep-           0.89      0.90    0.91      0.32  8.6    0.012
## crisis_nep-        0.89      0.89    0.91      0.32  8.4    0.012
## spaceship_nep      0.89      0.90    0.92      0.33  8.8    0.012
## bau_nep            0.89      0.89    0.91      0.32  8.3    0.012
## extract_ins-       0.90      0.90    0.92      0.34  9.1    0.011
## loss_instr-        0.89      0.90    0.92      0.33  8.8    0.012
## decade_mor-        0.89      0.89    0.91      0.32  8.4    0.012
## comm_rel           0.89      0.90    0.92      0.33  8.8    0.012
## wild_rel           0.89      0.90    0.91      0.32  8.6    0.012
## clean_inst         0.89      0.90    0.91      0.32  8.6    0.012
## tech-              0.91      0.91    0.92      0.36 10.1    0.011
## iden_rel           0.89      0.89    0.91      0.32  8.4    0.012
## kin_rel            0.89      0.90    0.91      0.32  8.5    0.012
## right-             0.89      0.89    0.91      0.32  8.3    0.012
## health_rel         0.90      0.90    0.92      0.33  8.8    0.012
## other_rel          0.89      0.90    0.91      0.32  8.6    0.012
## kin_met            0.89      0.90    0.91      0.33  8.7    0.012
## resp_met           0.89      0.89    0.91      0.32  8.4    0.012
## 
##  Item statistics 
##                 n raw.r std.r r.cor r.drop mean   sd
## abuse_nep     400  0.71  0.72 0.710  0.668  4.3 0.80
## bal_nep-      400  0.63  0.63 0.607  0.573  3.8 0.97
## crisis_nep-   400  0.70  0.70 0.689  0.643  4.0 1.03
## spaceship_nep 398  0.56  0.56 0.527  0.497  3.9 0.95
## bau_nep       400  0.72  0.72 0.713  0.670  3.9 0.99
## extract_ins-  400  0.44  0.44 0.382  0.363  2.8 0.95
## loss_instr-   398  0.56  0.55 0.513  0.486  3.8 1.09
## decade_mor-   400  0.70  0.70 0.685  0.649  3.9 1.02
## comm_rel      399  0.53  0.54 0.504  0.473  4.1 0.73
## wild_rel      400  0.63  0.63 0.604  0.567  3.7 0.96
## clean_inst    400  0.58  0.61 0.584  0.550  4.7 0.52
## tech-         400  0.14  0.13 0.059  0.054  2.5 0.90
## iden_rel      400  0.69  0.69 0.674  0.634  3.8 0.94
## kin_rel       398  0.64  0.64 0.625  0.584  4.0 0.87
## right-        398  0.73  0.72 0.719  0.680  4.0 0.99
## health_rel    398  0.55  0.54 0.501  0.473  3.7 1.05
## other_rel     400  0.61  0.63 0.603  0.566  4.3 0.70
## kin_met       398  0.58  0.57 0.554  0.501  3.4 1.14
## resp_met      399  0.69  0.69 0.680  0.639  4.0 0.95
## 
## Non missing response frequency for each item
##                  1    2    3    4    5 miss
## abuse_nep     0.00 0.03 0.10 0.43 0.43 0.00
## bal_nep       0.24 0.48 0.16 0.11 0.02 0.00
## crisis_nep    0.37 0.36 0.16 0.10 0.01 0.00
## spaceship_nep 0.01 0.11 0.15 0.47 0.26 0.01
## bau_nep       0.02 0.09 0.18 0.41 0.30 0.00
## extract_ins   0.05 0.20 0.31 0.42 0.03 0.00
## loss_instr    0.28 0.40 0.15 0.13 0.03 0.01
## decade_mor    0.30 0.39 0.18 0.12 0.01 0.00
## comm_rel      0.00 0.03 0.12 0.58 0.26 0.00
## wild_rel      0.01 0.12 0.21 0.46 0.19 0.00
## clean_inst    0.00 0.00 0.01 0.28 0.71 0.00
## tech          0.02 0.12 0.34 0.41 0.12 0.00
## iden_rel      0.01 0.08 0.22 0.44 0.25 0.00
## kin_rel       0.01 0.05 0.17 0.46 0.30 0.01
## right         0.36 0.39 0.14 0.09 0.01 0.01
## health_rel    0.02 0.14 0.18 0.42 0.24 0.01
## other_rel     0.00 0.02 0.05 0.48 0.44 0.00
## kin_met       0.06 0.17 0.29 0.29 0.19 0.01
## resp_met      0.02 0.03 0.24 0.36 0.35 0.00
```

### Correlation Matrix


```r
# corr1 <- rcorr(mt_val, type = "pearson")
# corr1
```

### PCA


```r
fit <- prcomp(~ ., data=mt_val, na.action=na.omit, scale=TRUE)

summary(fit) # print variance accounted for 
```

```
## Importance of components:
##                           PC1    PC2     PC3     PC4     PC5     PC6
## Standard deviation     2.6909 1.3999 1.10845 0.96710 0.89897 0.87153
## Proportion of Variance 0.3811 0.1032 0.06467 0.04923 0.04253 0.03998
## Cumulative Proportion  0.3811 0.4843 0.54892 0.59814 0.64068 0.68065
##                            PC7     PC8     PC9    PC10    PC11    PC12
## Standard deviation     0.86213 0.79574 0.79146 0.77923 0.75166 0.70741
## Proportion of Variance 0.03912 0.03333 0.03297 0.03196 0.02974 0.02634
## Cumulative Proportion  0.71977 0.75310 0.78607 0.81803 0.84776 0.87410
##                           PC13    PC14    PC15    PC16    PC17    PC18
## Standard deviation     0.68726 0.65277 0.60954 0.57093 0.55504 0.50657
## Proportion of Variance 0.02486 0.02243 0.01955 0.01716 0.01621 0.01351
## Cumulative Proportion  0.89896 0.92139 0.94094 0.95810 0.97431 0.98782
##                           PC19
## Standard deviation     0.48109
## Proportion of Variance 0.01218
## Cumulative Proportion  1.00000
```

```r
loadings(fit) # pc loadings 
```

```
## NULL
```

```r
plot(fit,type="lines") # scree plot 
```

![](Mother_Tree_files/figure-html/unnamed-chunk-5-1.png) 

```r
fit$scores # the principal components
```

```
## NULL
```

```r
biplot(fit)
```

![](Mother_Tree_files/figure-html/unnamed-chunk-5-2.png) 

```r
# what is this?
# how do I account for demographic features?
# next steps: Factor Analysis
```
 
 ### Factor Analysis

 # factanal(x, factors, data = NULL, covmat = NULL, n.obs = NA,
 #        subset, na.action, start = NULL,
 #        scores = c("none", "regression", "Bartlett"),
 #        rotation = "varimax", control = NULL, ...)
 


```r
fit_mt <- factanal(~ ., factors = 5, data = mt_val)
print(fit_mt, digits=2, cutoff=.3, sort=TRUE)
```

```
## 
## Call:
## factanal(x = ~., factors = 5, data = mt_val)
## 
## Uniquenesses:
##     abuse_nep       bal_nep    crisis_nep spaceship_nep       bau_nep 
##          0.28          0.36          0.35          0.63          0.31 
##   extract_ins    loss_instr    decade_mor      comm_rel      wild_rel 
##          0.83          0.67          0.40          0.66          0.53 
##    clean_inst          tech      iden_rel       kin_rel         right 
##          0.62          0.85          0.43          0.45          0.16 
##    health_rel     other_rel       kin_met      resp_met 
##          0.67          0.46          0.27          0.37 
## 
## Loadings:
##               Factor1 Factor2 Factor3 Factor4 Factor5
## wild_rel       0.61                                  
## iden_rel       0.67                                  
## kin_rel        0.67                                  
## kin_met        0.84                                  
## resp_met       0.73                                  
## abuse_nep              0.73                          
## crisis_nep            -0.63    0.35   -0.32          
## spaceship_nep          0.53                          
## bau_nep                0.73                          
## bal_nep               -0.46    0.62                  
## other_rel      0.31    0.35            0.56          
## right                 -0.33    0.53           -0.59  
## extract_ins                                          
## loss_instr                     0.45                  
## decade_mor                     0.48   -0.31   -0.36  
## comm_rel       0.49                                  
## clean_inst             0.38            0.35          
## tech                           0.37                  
## health_rel                             0.39          
## 
##                Factor1 Factor2 Factor3 Factor4 Factor5
## SS loadings       3.41    2.78    1.63    1.19    0.68
## Proportion Var    0.18    0.15    0.09    0.06    0.04
## Cumulative Var    0.18    0.33    0.41    0.47    0.51
## 
## Test of the hypothesis that 5 factors are sufficient.
## The chi square statistic is 149.33 on 86 degrees of freedom.
## The p-value is 2.76e-05
```

```r
# plot factor 1 by factor 2 
load <- fit_mt$loadings[,1:2] 
plot(load,type="n") # set up plot 
text(load,labels=names(mt_val),cex=.7) # add variable names
```

![](Mother_Tree_files/figure-html/unnamed-chunk-6-1.png) 

