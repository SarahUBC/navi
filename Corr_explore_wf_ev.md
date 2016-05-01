# Correlation exploration WF EV
Sarah Klain  
May 1, 2016  



Setup


```r
library(Hmisc) #to run correlations with sig levels
```

```
## Loading required package: grid
## Loading required package: lattice
## Loading required package: survival
## Loading required package: Formula
## Loading required package: ggplot2
```

```
## Warning: package 'ggplot2' was built under R version 3.2.4
```

```
## Warning: replacing previous import by 'ggplot2::unit' when loading 'Hmisc'
```

```
## Warning: replacing previous import by 'ggplot2::arrow' when loading 'Hmisc'
```

```
## Warning: replacing previous import by 'scales::alpha' when loading 'Hmisc'
```

```
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
```

```
## Warning: replacing previous import by 'grid::arrow' when loading 'ggthemes'
```

```
## Warning: replacing previous import by 'grid::unit' when loading 'ggthemes'
```

```
## Warning: replacing previous import by 'scales::alpha' when loading
## 'ggthemes'
```

```r
library(viridis) # for pretty colors
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
## 
## The following objects are masked from 'package:Hmisc':
## 
##     combine, src, summarize
## 
## The following objects are masked from 'package:stats':
## 
##     filter, lag
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(knitr) # tool for making nice tables
library(tidyr) # data table wrangling tool
library(broom)
library(stargazer) # makes pretty tables
```

```
## 
## Please cite as: 
## 
##  Hlavac, Marek (2015). stargazer: Well-Formatted Regression and Summary Statistics Tables.
##  R package version 5.2. http://CRAN.R-project.org/package=stargazer
```

```r
# library(stats) # for PCA
# library(psych)
```

Input data

```r
setwd("/Users/sarahklain/Documents/R_2015/navi") #set working directory
ev_wf <- read.csv("cer_2016_01_08_dem3.csv")
str(ev_wf)
```

```
## 'data.frame':	400 obs. of  56 variables:
##  $ white        : int  1 0 1 0 1 0 1 1 1 1 ...
##  $ female       : int  1 1 1 1 1 1 1 1 1 0 ...
##  $ age          : int  28 28 25 38 28 63 33 22 37 28 ...
##  $ univ_degr    : int  1 1 1 1 1 1 1 0 1 1 ...
##  $ income       : int  6 7 8 8 6 8 11 2 6 5 ...
##  $ wages        : int  0 0 0 1 0 0 0 0 0 1 ...
##  $ self.emp     : int  0 0 0 0 0 0 0 0 1 0 ...
##  $ pol_dem      : int  0 0 1 0 0 1 0 1 0 0 ...
##  $ pol_ind      : int  0 1 0 1 1 0 1 0 0 0 ...
##  $ pol_rep      : int  0 0 0 0 0 0 0 0 0 1 ...
##  $ coast_rec    : int  0 1 0 1 1 1 0 0 0 0 ...
##  $ mean_nep     : num  4.6 4 5 4.8 4.2 5 3.6 3.8 4 4.4 ...
##  $ mean_rel     : num  4.17 3.33 4 3.67 3.33 4.83 3.17 3.6 3.83 2.83 ...
##  $ mean_met     : num  1.75 2.5 1.75 2 3.5 1 2.75 3 2.25 3.75 ...
##  $ mean_inst    : num  3 3 3.5 3.67 4 3.33 3.33 3 3 3 ...
##  $ mean_mor     : num  3 2.5 3 3 2.5 3 2 4 3 2.5 ...
##  $ att_w_US     : int  1 1 1 1 1 1 2 1 1 1 ...
##  $ oper         : int  1 2 2 1 1 1 2 2 1 1 ...
##  $ const_st     : int  1 2 1 1 1 1 4 1 1 1 ...
##  $ wf_rec       : int  3 3 3 3 3 3 3 3 3 3 ...
##  $ abuse_nep    : int  5 4 5 5 4 5 4 4 5 5 ...
##  $ bal_r_nep    : int  4 4 5 5 4 5 4 4 4 4 ...
##  $ crisis_r_nep : int  5 4 5 5 5 5 2 4 4 5 ...
##  $ spaceship_nep: int  5 5 5 5 5 5 4 3 4 4 ...
##  $ bau_nep      : int  4 3 5 4 3 5 4 4 3 4 ...
##  $ extract_r_ins: int  2 2 2 4 4 4 3 3 2 2 ...
##  $ loss_r_ins   : int  4 4 NA 4 3 5 3 4 4 4 ...
##  $ decade_r_mor : int  4 4 5 4 3 5 2 4 4 3 ...
##  $ comm_rel     : int  4 5 4 4 3 5 4 4 4 4 ...
##  $ wild_rel     : int  4 3 4 4 4 4 2 4 4 2 ...
##  $ clean_inst   : int  5 5 5 5 5 5 4 4 5 5 ...
##  $ tech         : int  4 4 3 4 4 3 2 3 4 2 ...
##  $ tech_r       : int  2 2 3 2 2 3 4 3 2 4 ...
##  $ iden_rel     : int  5 3 4 4 2 5 3 4 4 2 ...
##  $ kin_rel      : int  5 3 5 4 3 5 4 NA 5 2 ...
##  $ right_r_mor  : int  4 5 5 4 4 5 4 NA 4 4 ...
##  $ health_rel   : int  2 1 2 2 4 5 4 2 2 3 ...
##  $ other_rel    : int  5 5 5 4 4 5 2 4 4 4 ...
##  $ kin_met      : int  2 3 2 2 3 1 4 3 2 5 ...
##  $ resp_met     : int  2 2 2 2 3 1 2 3 2 3 ...
##  $ iden_met     : int  2 4 2 2 5 1 2 4 3 5 ...
##  $ other_met    : int  1 1 1 2 3 1 3 2 2 2 ...
##  $ feedback     : Factor w/ 97 levels ""," "," No but I enjoyed this survey very much!",..: 1 1 1 11 1 1 40 1 1 1 ...
##  $ lat          : num  42.2 42.6 42.1 43.3 42.4 ...
##  $ long         : num  -71.2 -70.9 -72.3 -70.6 -71.2 ...
##  $ zip_code     : int  3820 1801 2171 2919 6779 6716 2108 2062 1905 2215 ...
##  $ ResponseID   : Factor w/ 400 levels "R_02Il4uscqNJuWBT",..: 8 23 27 32 34 37 39 40 44 45 ...
##  $ Block        : int  1 1 1 1 1 1 1 1 1 1 ...
##  $ Q1           : int  0 0 0 0 0 0 1 0 0 0 ...
##  $ Q2           : int  1 1 1 0 1 1 1 1 0 0 ...
##  $ Q3           : int  1 0 1 2 2 1 1 1 1 2 ...
##  $ Q4           : int  1 1 1 2 1 1 1 1 1 2 ...
##  $ Q5           : int  0 0 0 0 0 0 1 0 0 0 ...
##  $ Q6           : int  0 0 0 0 0 0 0 0 0 0 ...
##  $ Q7           : int  0 0 0 0 0 0 1 0 0 2 ...
##  $ Q8           : int  1 1 1 1 1 1 0 1 1 1 ...
```

### Correlations


```r
corr1 <- rcorr(as.matrix(ev_wf[11:42]), type = "pearson")
corr1
```

```
##               coast_rec mean_nep mean_rel mean_met mean_inst mean_mor
## coast_rec          1.00    -0.10    -0.05     0.08      0.00     0.05
## mean_nep          -0.10     1.00     0.45    -0.45      0.10    -0.02
## mean_rel          -0.05     0.45     1.00    -0.70      0.09     0.00
## mean_met           0.08    -0.45    -0.70     1.00     -0.05     0.02
## mean_inst          0.00     0.10     0.09    -0.05      1.00    -0.07
## mean_mor           0.05    -0.02     0.00     0.02     -0.07     1.00
## att_w_US           0.05    -0.05    -0.01     0.02     -0.02     0.00
## oper               0.04    -0.36    -0.24     0.26     -0.08     0.02
## const_st           0.09    -0.22    -0.11     0.16     -0.02    -0.02
## wf_rec            -0.05     0.09     0.03    -0.06      0.01    -0.04
## abuse_nep         -0.08     0.79     0.43    -0.42      0.10    -0.10
## bal_r_nep         -0.04     0.75     0.22    -0.22     -0.03    -0.03
## crisis_r_nep      -0.07     0.83     0.35    -0.34      0.08     0.01
## spaceship_nep     -0.11     0.70     0.33    -0.36      0.10    -0.03
## bau_nep           -0.12     0.84     0.44    -0.45      0.13     0.04
## extract_r_ins      0.00     0.32     0.22    -0.17      0.61    -0.03
## loss_r_ins        -0.01     0.41     0.28    -0.29     -0.52     0.04
## decade_r_mor      -0.01     0.56     0.37    -0.36      0.02     0.43
## comm_rel          -0.08     0.35     0.67    -0.46     -0.01    -0.01
## wild_rel          -0.04     0.42     0.74    -0.60      0.09     0.01
## clean_inst         0.00     0.50     0.39    -0.38      0.29    -0.05
## tech              -0.07    -0.05     0.10    -0.11      0.04     0.02
## tech_r             0.07     0.05    -0.10     0.11     -0.04    -0.02
## iden_rel          -0.04     0.44     0.76    -0.63      0.09     0.01
## kin_rel           -0.06     0.37     0.75    -0.61      0.02    -0.04
## right_r_mor       -0.06     0.60     0.38    -0.38      0.08    -0.37
## health_rel         0.05    -0.43    -0.06     0.28      0.00    -0.04
## other_rel         -0.03     0.51     0.56    -0.40      0.11     0.09
## kin_met            0.02    -0.32    -0.63     0.88     -0.02     0.04
## resp_met           0.10    -0.45    -0.63     0.84     -0.04     0.01
## iden_met           0.05    -0.29    -0.55     0.85     -0.07     0.06
## other_met          0.09    -0.46    -0.47     0.66     -0.01    -0.06
##               att_w_US  oper const_st wf_rec abuse_nep bal_r_nep
## coast_rec         0.05  0.04     0.09  -0.05     -0.08     -0.04
## mean_nep         -0.05 -0.36    -0.22   0.09      0.79      0.75
## mean_rel         -0.01 -0.24    -0.11   0.03      0.43      0.22
## mean_met          0.02  0.26     0.16  -0.06     -0.42     -0.22
## mean_inst        -0.02 -0.08    -0.02   0.01      0.10     -0.03
## mean_mor          0.00  0.02    -0.02  -0.04     -0.10     -0.03
## att_w_US          1.00  0.11     0.16  -0.05     -0.02     -0.03
## oper              0.11  1.00     0.49  -0.21     -0.32     -0.17
## const_st          0.16  0.49     1.00  -0.28     -0.15     -0.11
## wf_rec           -0.05 -0.21    -0.28   1.00      0.08      0.03
## abuse_nep        -0.02 -0.32    -0.15   0.08      1.00      0.44
## bal_r_nep        -0.03 -0.17    -0.11   0.03      0.44      1.00
## crisis_r_nep     -0.07 -0.35    -0.28   0.12      0.58      0.59
## spaceship_nep    -0.02 -0.25    -0.13   0.03      0.50      0.38
## bau_nep          -0.03 -0.31    -0.19   0.10      0.65      0.49
## extract_r_ins     0.03 -0.03     0.05   0.02      0.24      0.25
## loss_r_ins        0.01 -0.13    -0.02   0.07      0.32      0.43
## decade_r_mor      0.02 -0.15    -0.04  -0.02      0.47      0.48
## comm_rel         -0.04 -0.14    -0.06   0.07      0.31      0.22
## wild_rel          0.00 -0.21    -0.16  -0.01      0.38      0.25
## clean_inst       -0.08 -0.40    -0.19   0.13      0.47      0.34
## tech             -0.09 -0.13    -0.20   0.13     -0.01     -0.18
## tech_r            0.09  0.13     0.20  -0.13      0.01      0.18
## iden_rel         -0.04 -0.22    -0.13   0.07      0.38      0.26
## kin_rel           0.00 -0.13    -0.04   0.03      0.37      0.20
## right_r_mor       0.02 -0.17    -0.02   0.01      0.56      0.53
## health_rel        0.08  0.13     0.12  -0.06     -0.33     -0.38
## other_rel        -0.07 -0.27    -0.14   0.01      0.44      0.31
## kin_met          -0.02  0.18     0.11  -0.07     -0.30     -0.13
## resp_met          0.02  0.27     0.20  -0.04     -0.41     -0.26
## iden_met          0.06  0.18     0.07  -0.02     -0.28     -0.08
## other_met         0.01  0.24     0.16  -0.05     -0.39     -0.30
##               crisis_r_nep spaceship_nep bau_nep extract_r_ins loss_r_ins
## coast_rec            -0.07         -0.11   -0.12          0.00      -0.01
## mean_nep              0.83          0.70    0.84          0.32       0.41
## mean_rel              0.35          0.33    0.44          0.22       0.28
## mean_met             -0.34         -0.36   -0.45         -0.17      -0.29
## mean_inst             0.08          0.10    0.13          0.61      -0.52
## mean_mor              0.01         -0.03    0.04         -0.03       0.04
## att_w_US             -0.07         -0.02   -0.03          0.03       0.01
## oper                 -0.35         -0.25   -0.31         -0.03      -0.13
## const_st             -0.28         -0.13   -0.19          0.05      -0.02
## wf_rec                0.12          0.03    0.10          0.02       0.07
## abuse_nep             0.58          0.50    0.65          0.24       0.32
## bal_r_nep             0.59          0.38    0.49          0.25       0.43
## crisis_r_nep          1.00          0.39    0.65          0.26       0.35
## spaceship_nep         0.39          1.00    0.49          0.22       0.21
## bau_nep               0.65          0.49    1.00          0.30       0.30
## extract_r_ins         0.26          0.22    0.30          1.00       0.26
## loss_r_ins            0.35          0.21    0.30          0.26       1.00
## decade_r_mor          0.47          0.33    0.45          0.31       0.43
## comm_rel              0.29          0.22    0.33          0.10       0.23
## wild_rel              0.33          0.30    0.37          0.19       0.20
## clean_inst            0.46          0.30    0.41          0.23       0.34
## tech                 -0.05          0.05   -0.01         -0.17      -0.17
## tech_r                0.05         -0.05    0.01          0.17       0.17
## iden_rel              0.38          0.30    0.41          0.23       0.29
## kin_rel               0.27          0.24    0.38          0.19       0.30
## right_r_mor           0.48          0.36    0.43          0.35       0.41
## health_rel           -0.40         -0.23   -0.33         -0.12      -0.25
## other_rel             0.45          0.35    0.44          0.21       0.26
## kin_met              -0.21         -0.29   -0.34         -0.16      -0.23
## resp_met             -0.34         -0.34   -0.42         -0.19      -0.32
## iden_met             -0.19         -0.25   -0.33         -0.10      -0.11
## other_met            -0.41         -0.32   -0.38         -0.14      -0.32
##               decade_r_mor comm_rel wild_rel clean_inst  tech tech_r
## coast_rec            -0.01    -0.08    -0.04       0.00 -0.07   0.07
## mean_nep              0.56     0.35     0.42       0.50 -0.05   0.05
## mean_rel              0.37     0.67     0.74       0.39  0.10  -0.10
## mean_met             -0.36    -0.46    -0.60      -0.38 -0.11   0.11
## mean_inst             0.02    -0.01     0.09       0.29  0.04  -0.04
## mean_mor              0.43    -0.01     0.01      -0.05  0.02  -0.02
## att_w_US              0.02    -0.04     0.00      -0.08 -0.09   0.09
## oper                 -0.15    -0.14    -0.21      -0.40 -0.13   0.13
## const_st             -0.04    -0.06    -0.16      -0.19 -0.20   0.20
## wf_rec               -0.02     0.07    -0.01       0.13  0.13  -0.13
## abuse_nep             0.47     0.31     0.38       0.47 -0.01   0.01
## bal_r_nep             0.48     0.22     0.25       0.34 -0.18   0.18
## crisis_r_nep          0.47     0.29     0.33       0.46 -0.05   0.05
## spaceship_nep         0.33     0.22     0.30       0.30  0.05  -0.05
## bau_nep               0.45     0.33     0.37       0.41 -0.01   0.01
## extract_r_ins         0.31     0.10     0.19       0.23 -0.17   0.17
## loss_r_ins            0.43     0.23     0.20       0.34 -0.17   0.17
## decade_r_mor          1.00     0.24     0.36       0.38 -0.13   0.13
## comm_rel              0.24     1.00     0.44       0.29  0.03  -0.03
## wild_rel              0.36     0.44     1.00       0.28  0.06  -0.06
## clean_inst            0.38     0.29     0.28       1.00  0.05  -0.05
## tech                 -0.13     0.03     0.06       0.05  1.00  -1.00
## tech_r                0.13    -0.03    -0.06      -0.05 -1.00   1.00
## iden_rel              0.41     0.38     0.55       0.42  0.04  -0.04
## kin_rel               0.34     0.48     0.43       0.30  0.02  -0.02
## right_r_mor           0.68     0.26     0.37       0.43 -0.16   0.16
## health_rel           -0.42    -0.22    -0.26      -0.29  0.11  -0.11
## other_rel             0.41     0.36     0.38       0.45  0.07  -0.07
## kin_met              -0.22    -0.42    -0.52      -0.24 -0.10   0.10
## resp_met             -0.40    -0.40    -0.56      -0.41 -0.06   0.06
## iden_met             -0.21    -0.34    -0.45      -0.22 -0.11   0.11
## other_met            -0.37    -0.36    -0.42      -0.44 -0.06   0.06
##               iden_rel kin_rel right_r_mor health_rel other_rel kin_met
## coast_rec        -0.04   -0.06       -0.06       0.05     -0.03    0.02
## mean_nep          0.44    0.37        0.60      -0.43      0.51   -0.32
## mean_rel          0.76    0.75        0.38      -0.06      0.56   -0.63
## mean_met         -0.63   -0.61       -0.38       0.28     -0.40    0.88
## mean_inst         0.09    0.02        0.08       0.00      0.11   -0.02
## mean_mor          0.01   -0.04       -0.37      -0.04      0.09    0.04
## att_w_US         -0.04    0.00        0.02       0.08     -0.07   -0.02
## oper             -0.22   -0.13       -0.17       0.13     -0.27    0.18
## const_st         -0.13   -0.04       -0.02       0.12     -0.14    0.11
## wf_rec            0.07    0.03        0.01      -0.06      0.01   -0.07
## abuse_nep         0.38    0.37        0.56      -0.33      0.44   -0.30
## bal_r_nep         0.26    0.20        0.53      -0.38      0.31   -0.13
## crisis_r_nep      0.38    0.27        0.48      -0.40      0.45   -0.21
## spaceship_nep     0.30    0.24        0.36      -0.23      0.35   -0.29
## bau_nep           0.41    0.38        0.43      -0.33      0.44   -0.34
## extract_r_ins     0.23    0.19        0.35      -0.12      0.21   -0.16
## loss_r_ins        0.29    0.30        0.41      -0.25      0.26   -0.23
## decade_r_mor      0.41    0.34        0.68      -0.42      0.41   -0.22
## comm_rel          0.38    0.48        0.26      -0.22      0.36   -0.42
## wild_rel          0.55    0.43        0.37      -0.26      0.38   -0.52
## clean_inst        0.42    0.30        0.43      -0.29      0.45   -0.24
## tech              0.04    0.02       -0.16       0.11      0.07   -0.10
## tech_r           -0.04   -0.02        0.16      -0.11     -0.07    0.10
## iden_rel          1.00    0.61        0.42      -0.31      0.38   -0.57
## kin_rel           0.61    1.00        0.39      -0.29      0.40   -0.57
## right_r_mor       0.42    0.39        1.00      -0.40      0.36   -0.26
## health_rel       -0.31   -0.29       -0.40       1.00     -0.39    0.18
## other_rel         0.38    0.40        0.36      -0.39      1.00   -0.28
## kin_met          -0.57   -0.57       -0.26       0.18     -0.28    1.00
## resp_met         -0.56   -0.56       -0.42       0.28     -0.38    0.66
## iden_met         -0.54   -0.48       -0.26       0.16     -0.21    0.74
## other_met        -0.39   -0.40       -0.34       0.34     -0.48    0.43
##               resp_met iden_met other_met
## coast_rec         0.10     0.05      0.09
## mean_nep         -0.45    -0.29     -0.46
## mean_rel         -0.63    -0.55     -0.47
## mean_met          0.84     0.85      0.66
## mean_inst        -0.04    -0.07     -0.01
## mean_mor          0.01     0.06     -0.06
## att_w_US          0.02     0.06      0.01
## oper              0.27     0.18      0.24
## const_st          0.20     0.07      0.16
## wf_rec           -0.04    -0.02     -0.05
## abuse_nep        -0.41    -0.28     -0.39
## bal_r_nep        -0.26    -0.08     -0.30
## crisis_r_nep     -0.34    -0.19     -0.41
## spaceship_nep    -0.34    -0.25     -0.32
## bau_nep          -0.42    -0.33     -0.38
## extract_r_ins    -0.19    -0.10     -0.14
## loss_r_ins       -0.32    -0.11     -0.32
## decade_r_mor     -0.40    -0.21     -0.37
## comm_rel         -0.40    -0.34     -0.36
## wild_rel         -0.56    -0.45     -0.42
## clean_inst       -0.41    -0.22     -0.44
## tech             -0.06    -0.11     -0.06
## tech_r            0.06     0.11      0.06
## iden_rel         -0.56    -0.54     -0.39
## kin_rel          -0.56    -0.48     -0.40
## right_r_mor      -0.42    -0.26     -0.34
## health_rel        0.28     0.16      0.34
## other_rel        -0.38    -0.21     -0.48
## kin_met           0.66     0.74      0.43
## resp_met          1.00     0.61      0.50
## iden_met          0.61     1.00      0.33
## other_met         0.50     0.33      1.00
## 
## n
##               coast_rec mean_nep mean_rel mean_met mean_inst mean_mor
## coast_rec           400      400      400      400       400      400
## mean_nep            400      400      400      400       400      400
## mean_rel            400      400      400      400       400      400
## mean_met            400      400      400      400       400      400
## mean_inst           400      400      400      400       400      400
## mean_mor            400      400      400      400       400      400
## att_w_US            400      400      400      400       400      400
## oper                400      400      400      400       400      400
## const_st            398      398      398      398       398      398
## wf_rec              400      400      400      400       400      400
## abuse_nep           400      400      400      400       400      400
## bal_r_nep           400      400      400      400       400      400
## crisis_r_nep        400      400      400      400       400      400
## spaceship_nep       398      398      398      398       398      398
## bau_nep             400      400      400      400       400      400
## extract_r_ins       400      400      400      400       400      400
## loss_r_ins          398      398      398      398       398      398
## decade_r_mor        400      400      400      400       400      400
## comm_rel            399      399      399      399       399      399
## wild_rel            400      400      400      400       400      400
## clean_inst          400      400      400      400       400      400
## tech                400      400      400      400       400      400
## tech_r              400      400      400      400       400      400
## iden_rel            400      400      400      400       400      400
## kin_rel             398      398      398      398       398      398
## right_r_mor         398      398      398      398       398      398
## health_rel          398      398      398      398       398      398
## other_rel           400      400      400      400       400      400
## kin_met             398      398      398      398       398      398
## resp_met            399      399      399      399       399      399
## iden_met            400      400      400      400       400      400
## other_met           400      400      400      400       400      400
##               att_w_US oper const_st wf_rec abuse_nep bal_r_nep
## coast_rec          400  400      398    400       400       400
## mean_nep           400  400      398    400       400       400
## mean_rel           400  400      398    400       400       400
## mean_met           400  400      398    400       400       400
## mean_inst          400  400      398    400       400       400
## mean_mor           400  400      398    400       400       400
## att_w_US           400  400      398    400       400       400
## oper               400  400      398    400       400       400
## const_st           398  398      398    398       398       398
## wf_rec             400  400      398    400       400       400
## abuse_nep          400  400      398    400       400       400
## bal_r_nep          400  400      398    400       400       400
## crisis_r_nep       400  400      398    400       400       400
## spaceship_nep      398  398      396    398       398       398
## bau_nep            400  400      398    400       400       400
## extract_r_ins      400  400      398    400       400       400
## loss_r_ins         398  398      396    398       398       398
## decade_r_mor       400  400      398    400       400       400
## comm_rel           399  399      397    399       399       399
## wild_rel           400  400      398    400       400       400
## clean_inst         400  400      398    400       400       400
## tech               400  400      398    400       400       400
## tech_r             400  400      398    400       400       400
## iden_rel           400  400      398    400       400       400
## kin_rel            398  398      396    398       398       398
## right_r_mor        398  398      396    398       398       398
## health_rel         398  398      396    398       398       398
## other_rel          400  400      398    400       400       400
## kin_met            398  398      397    398       398       398
## resp_met           399  399      397    399       399       399
## iden_met           400  400      398    400       400       400
## other_met          400  400      398    400       400       400
##               crisis_r_nep spaceship_nep bau_nep extract_r_ins loss_r_ins
## coast_rec              400           398     400           400        398
## mean_nep               400           398     400           400        398
## mean_rel               400           398     400           400        398
## mean_met               400           398     400           400        398
## mean_inst              400           398     400           400        398
## mean_mor               400           398     400           400        398
## att_w_US               400           398     400           400        398
## oper                   400           398     400           400        398
## const_st               398           396     398           398        396
## wf_rec                 400           398     400           400        398
## abuse_nep              400           398     400           400        398
## bal_r_nep              400           398     400           400        398
## crisis_r_nep           400           398     400           400        398
## spaceship_nep          398           398     398           398        396
## bau_nep                400           398     400           400        398
## extract_r_ins          400           398     400           400        398
## loss_r_ins             398           396     398           398        398
## decade_r_mor           400           398     400           400        398
## comm_rel               399           397     399           399        397
## wild_rel               400           398     400           400        398
## clean_inst             400           398     400           400        398
## tech                   400           398     400           400        398
## tech_r                 400           398     400           400        398
## iden_rel               400           398     400           400        398
## kin_rel                398           396     398           398        396
## right_r_mor            398           396     398           398        396
## health_rel             398           396     398           398        396
## other_rel              400           398     400           400        398
## kin_met                398           396     398           398        396
## resp_met               399           397     399           399        397
## iden_met               400           398     400           400        398
## other_met              400           398     400           400        398
##               decade_r_mor comm_rel wild_rel clean_inst tech tech_r
## coast_rec              400      399      400        400  400    400
## mean_nep               400      399      400        400  400    400
## mean_rel               400      399      400        400  400    400
## mean_met               400      399      400        400  400    400
## mean_inst              400      399      400        400  400    400
## mean_mor               400      399      400        400  400    400
## att_w_US               400      399      400        400  400    400
## oper                   400      399      400        400  400    400
## const_st               398      397      398        398  398    398
## wf_rec                 400      399      400        400  400    400
## abuse_nep              400      399      400        400  400    400
## bal_r_nep              400      399      400        400  400    400
## crisis_r_nep           400      399      400        400  400    400
## spaceship_nep          398      397      398        398  398    398
## bau_nep                400      399      400        400  400    400
## extract_r_ins          400      399      400        400  400    400
## loss_r_ins             398      397      398        398  398    398
## decade_r_mor           400      399      400        400  400    400
## comm_rel               399      399      399        399  399    399
## wild_rel               400      399      400        400  400    400
## clean_inst             400      399      400        400  400    400
## tech                   400      399      400        400  400    400
## tech_r                 400      399      400        400  400    400
## iden_rel               400      399      400        400  400    400
## kin_rel                398      397      398        398  398    398
## right_r_mor            398      397      398        398  398    398
## health_rel             398      397      398        398  398    398
## other_rel              400      399      400        400  400    400
## kin_met                398      397      398        398  398    398
## resp_met               399      398      399        399  399    399
## iden_met               400      399      400        400  400    400
## other_met              400      399      400        400  400    400
##               iden_rel kin_rel right_r_mor health_rel other_rel kin_met
## coast_rec          400     398         398        398       400     398
## mean_nep           400     398         398        398       400     398
## mean_rel           400     398         398        398       400     398
## mean_met           400     398         398        398       400     398
## mean_inst          400     398         398        398       400     398
## mean_mor           400     398         398        398       400     398
## att_w_US           400     398         398        398       400     398
## oper               400     398         398        398       400     398
## const_st           398     396         396        396       398     397
## wf_rec             400     398         398        398       400     398
## abuse_nep          400     398         398        398       400     398
## bal_r_nep          400     398         398        398       400     398
## crisis_r_nep       400     398         398        398       400     398
## spaceship_nep      398     396         396        396       398     396
## bau_nep            400     398         398        398       400     398
## extract_r_ins      400     398         398        398       400     398
## loss_r_ins         398     396         396        396       398     396
## decade_r_mor       400     398         398        398       400     398
## comm_rel           399     397         397        397       399     397
## wild_rel           400     398         398        398       400     398
## clean_inst         400     398         398        398       400     398
## tech               400     398         398        398       400     398
## tech_r             400     398         398        398       400     398
## iden_rel           400     398         398        398       400     398
## kin_rel            398     398         397        396       398     396
## right_r_mor        398     397         398        396       398     396
## health_rel         398     396         396        398       398     396
## other_rel          400     398         398        398       400     398
## kin_met            398     396         396        396       398     398
## resp_met           399     397         397        397       399     397
## iden_met           400     398         398        398       400     398
## other_met          400     398         398        398       400     398
##               resp_met iden_met other_met
## coast_rec          399      400       400
## mean_nep           399      400       400
## mean_rel           399      400       400
## mean_met           399      400       400
## mean_inst          399      400       400
## mean_mor           399      400       400
## att_w_US           399      400       400
## oper               399      400       400
## const_st           397      398       398
## wf_rec             399      400       400
## abuse_nep          399      400       400
## bal_r_nep          399      400       400
## crisis_r_nep       399      400       400
## spaceship_nep      397      398       398
## bau_nep            399      400       400
## extract_r_ins      399      400       400
## loss_r_ins         397      398       398
## decade_r_mor       399      400       400
## comm_rel           398      399       399
## wild_rel           399      400       400
## clean_inst         399      400       400
## tech               399      400       400
## tech_r             399      400       400
## iden_rel           399      400       400
## kin_rel            397      398       398
## right_r_mor        397      398       398
## health_rel         397      398       398
## other_rel          399      400       400
## kin_met            397      398       398
## resp_met           399      399       399
## iden_met           399      400       400
## other_met          399      400       400
## 
## P
##               coast_rec mean_nep mean_rel mean_met mean_inst mean_mor
## coast_rec               0.0370   0.3056   0.1159   0.9670    0.3582  
## mean_nep      0.0370             0.0000   0.0000   0.0501    0.6649  
## mean_rel      0.3056    0.0000            0.0000   0.0738    0.9942  
## mean_met      0.1159    0.0000   0.0000            0.3360    0.6622  
## mean_inst     0.9670    0.0501   0.0738   0.3360             0.1612  
## mean_mor      0.3582    0.6649   0.9942   0.6622   0.1612            
## att_w_US      0.3375    0.3680   0.8285   0.6389   0.6364    0.9711  
## oper          0.3828    0.0000   0.0000   0.0000   0.1245    0.6709  
## const_st      0.0771    0.0000   0.0302   0.0018   0.6825    0.6493  
## wf_rec        0.3204    0.0602   0.5783   0.2714   0.8405    0.4345  
## abuse_nep     0.1091    0.0000   0.0000   0.0000   0.0445    0.0559  
## bal_r_nep     0.4526    0.0000   0.0000   0.0000   0.4905    0.5457  
## crisis_r_nep  0.1779    0.0000   0.0000   0.0000   0.0950    0.8961  
## spaceship_nep 0.0334    0.0000   0.0000   0.0000   0.0389    0.6148  
## bau_nep       0.0212    0.0000   0.0000   0.0000   0.0075    0.4141  
## extract_r_ins 0.9755    0.0000   0.0000   0.0004   0.0000    0.5245  
## loss_r_ins    0.8908    0.0000   0.0000   0.0000   0.0000    0.4280  
## decade_r_mor  0.7719    0.0000   0.0000   0.0000   0.6356    0.0000  
## comm_rel      0.1261    0.0000   0.0000   0.0000   0.8992    0.9184  
## wild_rel      0.4206    0.0000   0.0000   0.0000   0.0761    0.8444  
## clean_inst    0.9427    0.0000   0.0000   0.0000   0.0000    0.3063  
## tech          0.1430    0.2986   0.0391   0.0336   0.4444    0.7387  
## tech_r        0.1430    0.2986   0.0391   0.0336   0.4444    0.7387  
## iden_rel      0.4760    0.0000   0.0000   0.0000   0.0590    0.8409  
## kin_rel       0.2620    0.0000   0.0000   0.0000   0.7543    0.4380  
## right_r_mor   0.2571    0.0000   0.0000   0.0000   0.1126    0.0000  
## health_rel    0.2967    0.0000   0.2106   0.0000   0.9753    0.4011  
## other_rel     0.5195    0.0000   0.0000   0.0000   0.0217    0.0739  
## kin_met       0.6495    0.0000   0.0000   0.0000   0.6285    0.4501  
## resp_met      0.0378    0.0000   0.0000   0.0000   0.4337    0.8534  
## iden_met      0.3105    0.0000   0.0000   0.0000   0.1419    0.2426  
## other_met     0.0818    0.0000   0.0000   0.0000   0.7761    0.2642  
##               att_w_US oper   const_st wf_rec abuse_nep bal_r_nep
## coast_rec     0.3375   0.3828 0.0771   0.3204 0.1091    0.4526   
## mean_nep      0.3680   0.0000 0.0000   0.0602 0.0000    0.0000   
## mean_rel      0.8285   0.0000 0.0302   0.5783 0.0000    0.0000   
## mean_met      0.6389   0.0000 0.0018   0.2714 0.0000    0.0000   
## mean_inst     0.6364   0.1245 0.6825   0.8405 0.0445    0.4905   
## mean_mor      0.9711   0.6709 0.6493   0.4345 0.0559    0.5457   
## att_w_US               0.0259 0.0013   0.3087 0.6244    0.5068   
## oper          0.0259          0.0000   0.0000 0.0000    0.0007   
## const_st      0.0013   0.0000          0.0000 0.0029    0.0219   
## wf_rec        0.3087   0.0000 0.0000          0.0979    0.6010   
## abuse_nep     0.6244   0.0000 0.0029   0.0979           0.0000   
## bal_r_nep     0.5068   0.0007 0.0219   0.6010 0.0000             
## crisis_r_nep  0.1792   0.0000 0.0000   0.0128 0.0000    0.0000   
## spaceship_nep 0.7325   0.0000 0.0086   0.6042 0.0000    0.0000   
## bau_nep       0.5216   0.0000 0.0002   0.0471 0.0000    0.0000   
## extract_r_ins 0.5896   0.5573 0.3202   0.7135 0.0000    0.0000   
## loss_r_ins    0.8224   0.0123 0.6300   0.1514 0.0000    0.0000   
## decade_r_mor  0.6511   0.0028 0.4318   0.6394 0.0000    0.0000   
## comm_rel      0.4304   0.0040 0.2380   0.1789 0.0000    0.0000   
## wild_rel      0.9583   0.0000 0.0018   0.9083 0.0000    0.0000   
## clean_inst    0.1235   0.0000 0.0002   0.0086 0.0000    0.0000   
## tech          0.0594   0.0106 0.0000   0.0115 0.7687    0.0004   
## tech_r        0.0594   0.0106 0.0000   0.0115 0.7687    0.0004   
## iden_rel      0.4270   0.0000 0.0124   0.1441 0.0000    0.0000   
## kin_rel       0.9763   0.0084 0.4601   0.5897 0.0000    0.0000   
## right_r_mor   0.6545   0.0008 0.6266   0.8711 0.0000    0.0000   
## health_rel    0.1168   0.0106 0.0172   0.2400 0.0000    0.0000   
## other_rel     0.1717   0.0000 0.0061   0.8457 0.0000    0.0000   
## kin_met       0.6745   0.0003 0.0308   0.1509 0.0000    0.0091   
## resp_met      0.6291   0.0000 0.0000   0.4631 0.0000    0.0000   
## iden_met      0.2416   0.0003 0.1894   0.7131 0.0000    0.0902   
## other_met     0.8594   0.0000 0.0013   0.2886 0.0000    0.0000   
##               crisis_r_nep spaceship_nep bau_nep extract_r_ins loss_r_ins
## coast_rec     0.1779       0.0334        0.0212  0.9755        0.8908    
## mean_nep      0.0000       0.0000        0.0000  0.0000        0.0000    
## mean_rel      0.0000       0.0000        0.0000  0.0000        0.0000    
## mean_met      0.0000       0.0000        0.0000  0.0004        0.0000    
## mean_inst     0.0950       0.0389        0.0075  0.0000        0.0000    
## mean_mor      0.8961       0.6148        0.4141  0.5245        0.4280    
## att_w_US      0.1792       0.7325        0.5216  0.5896        0.8224    
## oper          0.0000       0.0000        0.0000  0.5573        0.0123    
## const_st      0.0000       0.0086        0.0002  0.3202        0.6300    
## wf_rec        0.0128       0.6042        0.0471  0.7135        0.1514    
## abuse_nep     0.0000       0.0000        0.0000  0.0000        0.0000    
## bal_r_nep     0.0000       0.0000        0.0000  0.0000        0.0000    
## crisis_r_nep               0.0000        0.0000  0.0000        0.0000    
## spaceship_nep 0.0000                     0.0000  0.0000        0.0000    
## bau_nep       0.0000       0.0000                0.0000        0.0000    
## extract_r_ins 0.0000       0.0000        0.0000                0.0000    
## loss_r_ins    0.0000       0.0000        0.0000  0.0000                  
## decade_r_mor  0.0000       0.0000        0.0000  0.0000        0.0000    
## comm_rel      0.0000       0.0000        0.0000  0.0572        0.0000    
## wild_rel      0.0000       0.0000        0.0000  0.0001        0.0000    
## clean_inst    0.0000       0.0000        0.0000  0.0000        0.0000    
## tech          0.3431       0.3653        0.9059  0.0006        0.0007    
## tech_r        0.3431       0.3653        0.9059  0.0006        0.0007    
## iden_rel      0.0000       0.0000        0.0000  0.0000        0.0000    
## kin_rel       0.0000       0.0000        0.0000  0.0001        0.0000    
## right_r_mor   0.0000       0.0000        0.0000  0.0000        0.0000    
## health_rel    0.0000       0.0000        0.0000  0.0138        0.0000    
## other_rel     0.0000       0.0000        0.0000  0.0000        0.0000    
## kin_met       0.0000       0.0000        0.0000  0.0014        0.0000    
## resp_met      0.0000       0.0000        0.0000  0.0001        0.0000    
## iden_met      0.0001       0.0000        0.0000  0.0424        0.0283    
## other_met     0.0000       0.0000        0.0000  0.0068        0.0000    
##               decade_r_mor comm_rel wild_rel clean_inst tech   tech_r
## coast_rec     0.7719       0.1261   0.4206   0.9427     0.1430 0.1430
## mean_nep      0.0000       0.0000   0.0000   0.0000     0.2986 0.2986
## mean_rel      0.0000       0.0000   0.0000   0.0000     0.0391 0.0391
## mean_met      0.0000       0.0000   0.0000   0.0000     0.0336 0.0336
## mean_inst     0.6356       0.8992   0.0761   0.0000     0.4444 0.4444
## mean_mor      0.0000       0.9184   0.8444   0.3063     0.7387 0.7387
## att_w_US      0.6511       0.4304   0.9583   0.1235     0.0594 0.0594
## oper          0.0028       0.0040   0.0000   0.0000     0.0106 0.0106
## const_st      0.4318       0.2380   0.0018   0.0002     0.0000 0.0000
## wf_rec        0.6394       0.1789   0.9083   0.0086     0.0115 0.0115
## abuse_nep     0.0000       0.0000   0.0000   0.0000     0.7687 0.7687
## bal_r_nep     0.0000       0.0000   0.0000   0.0000     0.0004 0.0004
## crisis_r_nep  0.0000       0.0000   0.0000   0.0000     0.3431 0.3431
## spaceship_nep 0.0000       0.0000   0.0000   0.0000     0.3653 0.3653
## bau_nep       0.0000       0.0000   0.0000   0.0000     0.9059 0.9059
## extract_r_ins 0.0000       0.0572   0.0001   0.0000     0.0006 0.0006
## loss_r_ins    0.0000       0.0000   0.0000   0.0000     0.0007 0.0007
## decade_r_mor               0.0000   0.0000   0.0000     0.0070 0.0070
## comm_rel      0.0000                0.0000   0.0000     0.5614 0.5614
## wild_rel      0.0000       0.0000            0.0000     0.2419 0.2419
## clean_inst    0.0000       0.0000   0.0000              0.3536 0.3536
## tech          0.0070       0.5614   0.2419   0.3536            0.0000
## tech_r        0.0070       0.5614   0.2419   0.3536     0.0000       
## iden_rel      0.0000       0.0000   0.0000   0.0000     0.4302 0.4302
## kin_rel       0.0000       0.0000   0.0000   0.0000     0.7250 0.7250
## right_r_mor   0.0000       0.0000   0.0000   0.0000     0.0018 0.0018
## health_rel    0.0000       0.0000   0.0000   0.0000     0.0299 0.0299
## other_rel     0.0000       0.0000   0.0000   0.0000     0.1542 0.1542
## kin_met       0.0000       0.0000   0.0000   0.0000     0.0449 0.0449
## resp_met      0.0000       0.0000   0.0000   0.0000     0.1964 0.1964
## iden_met      0.0000       0.0000   0.0000   0.0000     0.0307 0.0307
## other_met     0.0000       0.0000   0.0000   0.0000     0.1980 0.1980
##               iden_rel kin_rel right_r_mor health_rel other_rel kin_met
## coast_rec     0.4760   0.2620  0.2571      0.2967     0.5195    0.6495 
## mean_nep      0.0000   0.0000  0.0000      0.0000     0.0000    0.0000 
## mean_rel      0.0000   0.0000  0.0000      0.2106     0.0000    0.0000 
## mean_met      0.0000   0.0000  0.0000      0.0000     0.0000    0.0000 
## mean_inst     0.0590   0.7543  0.1126      0.9753     0.0217    0.6285 
## mean_mor      0.8409   0.4380  0.0000      0.4011     0.0739    0.4501 
## att_w_US      0.4270   0.9763  0.6545      0.1168     0.1717    0.6745 
## oper          0.0000   0.0084  0.0008      0.0106     0.0000    0.0003 
## const_st      0.0124   0.4601  0.6266      0.0172     0.0061    0.0308 
## wf_rec        0.1441   0.5897  0.8711      0.2400     0.8457    0.1509 
## abuse_nep     0.0000   0.0000  0.0000      0.0000     0.0000    0.0000 
## bal_r_nep     0.0000   0.0000  0.0000      0.0000     0.0000    0.0091 
## crisis_r_nep  0.0000   0.0000  0.0000      0.0000     0.0000    0.0000 
## spaceship_nep 0.0000   0.0000  0.0000      0.0000     0.0000    0.0000 
## bau_nep       0.0000   0.0000  0.0000      0.0000     0.0000    0.0000 
## extract_r_ins 0.0000   0.0001  0.0000      0.0138     0.0000    0.0014 
## loss_r_ins    0.0000   0.0000  0.0000      0.0000     0.0000    0.0000 
## decade_r_mor  0.0000   0.0000  0.0000      0.0000     0.0000    0.0000 
## comm_rel      0.0000   0.0000  0.0000      0.0000     0.0000    0.0000 
## wild_rel      0.0000   0.0000  0.0000      0.0000     0.0000    0.0000 
## clean_inst    0.0000   0.0000  0.0000      0.0000     0.0000    0.0000 
## tech          0.4302   0.7250  0.0018      0.0299     0.1542    0.0449 
## tech_r        0.4302   0.7250  0.0018      0.0299     0.1542    0.0449 
## iden_rel               0.0000  0.0000      0.0000     0.0000    0.0000 
## kin_rel       0.0000           0.0000      0.0000     0.0000    0.0000 
## right_r_mor   0.0000   0.0000              0.0000     0.0000    0.0000 
## health_rel    0.0000   0.0000  0.0000                 0.0000    0.0004 
## other_rel     0.0000   0.0000  0.0000      0.0000               0.0000 
## kin_met       0.0000   0.0000  0.0000      0.0004     0.0000           
## resp_met      0.0000   0.0000  0.0000      0.0000     0.0000    0.0000 
## iden_met      0.0000   0.0000  0.0000      0.0010     0.0000    0.0000 
## other_met     0.0000   0.0000  0.0000      0.0000     0.0000    0.0000 
##               resp_met iden_met other_met
## coast_rec     0.0378   0.3105   0.0818   
## mean_nep      0.0000   0.0000   0.0000   
## mean_rel      0.0000   0.0000   0.0000   
## mean_met      0.0000   0.0000   0.0000   
## mean_inst     0.4337   0.1419   0.7761   
## mean_mor      0.8534   0.2426   0.2642   
## att_w_US      0.6291   0.2416   0.8594   
## oper          0.0000   0.0003   0.0000   
## const_st      0.0000   0.1894   0.0013   
## wf_rec        0.4631   0.7131   0.2886   
## abuse_nep     0.0000   0.0000   0.0000   
## bal_r_nep     0.0000   0.0902   0.0000   
## crisis_r_nep  0.0000   0.0001   0.0000   
## spaceship_nep 0.0000   0.0000   0.0000   
## bau_nep       0.0000   0.0000   0.0000   
## extract_r_ins 0.0001   0.0424   0.0068   
## loss_r_ins    0.0000   0.0283   0.0000   
## decade_r_mor  0.0000   0.0000   0.0000   
## comm_rel      0.0000   0.0000   0.0000   
## wild_rel      0.0000   0.0000   0.0000   
## clean_inst    0.0000   0.0000   0.0000   
## tech          0.1964   0.0307   0.1980   
## tech_r        0.1964   0.0307   0.1980   
## iden_rel      0.0000   0.0000   0.0000   
## kin_rel       0.0000   0.0000   0.0000   
## right_r_mor   0.0000   0.0000   0.0000   
## health_rel    0.0000   0.0010   0.0000   
## other_rel     0.0000   0.0000   0.0000   
## kin_met       0.0000   0.0000   0.0000   
## resp_met               0.0000   0.0000   
## iden_met      0.0000            0.0000   
## other_met     0.0000   0.0000
```

Make it visual


```r
library(corrgram)

corrgram(ev_wf[11:42], order=TRUE, lower.panel=panel.shade,
  upper.panel=panel.pie, text.panel=panel.txt,
  main="Environmental Values and Wind Farm PC2/PC1 Order")
```

![](Corr_explore_wf_ev_files/figure-html/unnamed-chunk-4-1.png) 

