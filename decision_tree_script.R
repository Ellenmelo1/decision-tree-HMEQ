#ELLEN DE MELO ALVES
>  #TRINE UNIVERSITY 
>  
>  library(rpart)
>  library(rpart.plot)
>  library(ROCR)
>  
>  PATH <- "/Users/ellenalves/Desktop/master"
>  FILE_NAME <- "HMEQ_Scrubbed.csv"
>  INFILE = paste( PATH, FILE_NAME, sep="/" )
> 
> setwd( PATH )
>  df = read.csv( INFILE )
> 
> head(df)
  TARGET_BAD_FLAG TARGET_LOSS_AMT LOAN IMP_MORTDUE M_MORTDUE IMP_VALUE M_VALUE IMP_YOJ M_YOJ IMP_DEROG
1               1             641 1100       25860         0     39025       0    10.5     0         0
2               1            1109 1300       70053         0     68400       0     7.0     0         0
3               1             767 1500       13500         0     16700       0     4.0     0         0
4               1            1425 1500       65000         1     89000       1     7.0     1         1
5               0               0 1700       97800         0    112000       0     3.0     0         0
6               1             335 1700       30548         0     40320       0     9.0     0         0
  M_DEROG IMP_DELINQ M_DELINQ IMP_CLAGE M_CLAGE IMP_NINQ M_NINQ IMP_CLNO M_CLNO IMP_DEBTINC M_DEBTINC
1       0          0        0  94.36667       0        1      0        9      0    35.00000         1
2       0          2        0 121.83333       0        0      0       14      0    35.00000         1
3       0          0        0 149.46667       0        1      0       10      0    35.00000         1
4       1          1        1 174.00000       1        1      1       20      1    35.00000         1
5       0          0        0  93.33333       0        0      0       14      0    35.00000         1
6       0          0        0 101.46600       0        1      0        8      0    37.11361         0
  FLAG.Job.Mgr FLAG.Job.Office FLAG.Job.Other FLAG.Job.ProfExe FLAG.Job.Sales FLAG.Job.Self
1            0               0              1                0              0             0
2            0               0              1                0              0             0
3            0               0              1                0              0             0
4            0               0              0                0              0             0
5            0               1              0                0              0             0
6            0               0              1                0              0             0
  FLAG.Reason.DebtCon FLAG.Reason.HomeImp
1                   0                   1
2                   0                   1
3                   0                   1
4                   0                   0
5                   0                   1
6                   0                   1
> 
> str(df)
'data.frame':	5960 obs. of  29 variables:
 $ TARGET_BAD_FLAG    : int  1 1 1 1 0 1 1 1 1 1 ...
 $ TARGET_LOSS_AMT    : int  641 1109 767 1425 0 335 1841 373 1217 1523 ...
 $ LOAN               : int  1100 1300 1500 1500 1700 1700 1800 1800 2000 2000 ...
 $ IMP_MORTDUE        : num  25860 70053 13500 65000 97800 ...
 $ M_MORTDUE          : int  0 0 0 1 0 0 0 0 0 1 ...
 $ IMP_VALUE          : num  39025 68400 16700 89000 112000 ...
 $ M_VALUE            : int  0 0 0 1 0 0 0 0 0 0 ...
 $ IMP_YOJ            : num  10.5 7 4 7 3 9 5 11 3 16 ...
 $ M_YOJ              : int  0 0 0 1 0 0 0 0 0 0 ...
 $ IMP_DEROG          : int  0 0 0 1 0 0 3 0 0 0 ...
 $ M_DEROG            : int  0 0 0 1 0 0 0 0 0 0 ...
 $ IMP_DELINQ         : int  0 2 0 1 0 0 2 0 2 0 ...
 $ M_DELINQ           : int  0 0 0 1 0 0 0 0 0 0 ...
 $ IMP_CLAGE          : num  94.4 121.8 149.5 174 93.3 ...
 $ M_CLAGE            : int  0 0 0 1 0 0 0 0 0 0 ...
 $ IMP_NINQ           : int  1 0 1 1 0 1 1 0 1 0 ...
 $ M_NINQ             : int  0 0 0 1 0 0 0 0 0 0 ...
 $ IMP_CLNO           : int  9 14 10 20 14 8 17 8 12 13 ...
 $ M_CLNO             : int  0 0 0 1 0 0 0 0 0 0 ...
 $ IMP_DEBTINC        : num  35 35 35 35 35 ...
 $ M_DEBTINC          : int  1 1 1 1 1 0 1 0 1 1 ...
 $ FLAG.Job.Mgr       : int  0 0 0 0 0 0 0 0 0 0 ...
 $ FLAG.Job.Office    : int  0 0 0 0 1 0 0 0 0 0 ...
 $ FLAG.Job.Other     : int  1 1 1 0 0 1 1 1 1 0 ...
 $ FLAG.Job.ProfExe   : int  0 0 0 0 0 0 0 0 0 0 ...
 $ FLAG.Job.Sales     : int  0 0 0 0 0 0 0 0 0 1 ...
 $ FLAG.Job.Self      : int  0 0 0 0 0 0 0 0 0 0 ...
 $ FLAG.Reason.DebtCon: int  0 0 0 0 0 0 0 0 0 0 ...
 $ FLAG.Reason.HomeImp: int  1 1 1 0 1 1 1 1 1 1 ...
> 
> summary(df)
 TARGET_BAD_FLAG  TARGET_LOSS_AMT      LOAN        IMP_MORTDUE       M_MORTDUE         IMP_VALUE     
 Min.   :0.0000   Min.   :    0   Min.   : 1100   Min.   :  2063   Min.   :0.00000   Min.   :  8000  
 1st Qu.:0.0000   1st Qu.:    0   1st Qu.:11100   1st Qu.: 48139   1st Qu.:0.00000   1st Qu.: 66490  
 Median :0.0000   Median :    0   Median :16300   Median : 65000   Median :0.00000   Median : 89000  
 Mean   :0.1995   Mean   : 2676   Mean   :18608   Mean   : 72999   Mean   :0.08691   Mean   :101536  
 3rd Qu.:0.0000   3rd Qu.:    0   3rd Qu.:23300   3rd Qu.: 88200   3rd Qu.:0.00000   3rd Qu.:119005  
 Max.   :1.0000   Max.   :78987   Max.   :89900   Max.   :399550   Max.   :1.00000   Max.   :855909  
    M_VALUE           IMP_YOJ           M_YOJ           IMP_DEROG          M_DEROG      
 Min.   :0.00000   Min.   : 0.000   Min.   :0.00000   Min.   : 0.0000   Min.   :0.0000  
 1st Qu.:0.00000   1st Qu.: 3.000   1st Qu.:0.00000   1st Qu.: 0.0000   1st Qu.:0.0000  
 Median :0.00000   Median : 7.000   Median :0.00000   Median : 0.0000   Median :0.0000  
 Mean   :0.01879   Mean   : 8.756   Mean   :0.08641   Mean   : 0.3431   Mean   :0.1188  
 3rd Qu.:0.00000   3rd Qu.:12.000   3rd Qu.:0.00000   3rd Qu.: 0.0000   3rd Qu.:0.0000  
 Max.   :1.00000   Max.   :41.000   Max.   :1.00000   Max.   :10.0000   Max.   :1.0000  
   IMP_DELINQ        M_DELINQ         IMP_CLAGE         M_CLAGE           IMP_NINQ    
 Min.   : 0.000   Min.   :0.00000   Min.   :   0.0   Min.   :0.00000   Min.   : 0.00  
 1st Qu.: 0.000   1st Qu.:0.00000   1st Qu.: 117.4   1st Qu.:0.00000   1st Qu.: 0.00  
 Median : 0.000   Median :0.00000   Median : 174.0   Median :0.00000   Median : 1.00  
 Mean   : 0.503   Mean   :0.09732   Mean   : 179.5   Mean   :0.05168   Mean   : 1.17  
 3rd Qu.: 1.000   3rd Qu.:0.00000   3rd Qu.: 227.1   3rd Qu.:0.00000   3rd Qu.: 2.00  
 Max.   :15.000   Max.   :1.00000   Max.   :1168.2   Max.   :1.00000   Max.   :17.00  
     M_NINQ           IMP_CLNO         M_CLNO         IMP_DEBTINC         M_DEBTINC     
 Min.   :0.00000   Min.   : 0.00   Min.   :0.00000   Min.   :  0.5245   Min.   :0.0000  
 1st Qu.:0.00000   1st Qu.:15.00   1st Qu.:0.00000   1st Qu.: 30.7632   1st Qu.:0.0000  
 Median :0.00000   Median :20.00   Median :0.00000   Median : 35.0000   Median :0.0000  
 Mean   :0.08557   Mean   :21.25   Mean   :0.03725   Mean   : 34.0393   Mean   :0.2126  
 3rd Qu.:0.00000   3rd Qu.:26.00   3rd Qu.:0.00000   3rd Qu.: 37.9499   3rd Qu.:0.0000  
 Max.   :1.00000   Max.   :71.00   Max.   :1.00000   Max.   :203.3122   Max.   :1.0000  
  FLAG.Job.Mgr    FLAG.Job.Office  FLAG.Job.Other   FLAG.Job.ProfExe FLAG.Job.Sales   
 Min.   :0.0000   Min.   :0.0000   Min.   :0.0000   Min.   :0.0000   Min.   :0.00000  
 1st Qu.:0.0000   1st Qu.:0.0000   1st Qu.:0.0000   1st Qu.:0.0000   1st Qu.:0.00000  
 Median :0.0000   Median :0.0000   Median :0.0000   Median :0.0000   Median :0.00000  
 Mean   :0.1287   Mean   :0.1591   Mean   :0.4007   Mean   :0.2141   Mean   :0.01829  
 3rd Qu.:0.0000   3rd Qu.:0.0000   3rd Qu.:1.0000   3rd Qu.:0.0000   3rd Qu.:0.00000  
 Max.   :1.0000   Max.   :1.0000   Max.   :1.0000   Max.   :1.0000   Max.   :1.00000  
 FLAG.Job.Self     FLAG.Reason.DebtCon FLAG.Reason.HomeImp
 Min.   :0.00000   Min.   :0.0000      Min.   :0.0000     
 1st Qu.:0.00000   1st Qu.:0.0000      1st Qu.:0.0000     
 Median :0.00000   Median :1.0000      Median :0.0000     
 Mean   :0.03238   Mean   :0.6591      Mean   :0.2987     
 3rd Qu.:0.00000   3rd Qu.:1.0000      3rd Qu.:1.0000     
 Max.   :1.00000   Max.   :1.0000      Max.   :1.0000     
> 
> df_class <- df
> df_class$TARGET_LOSS_AMT <- NULL
> 
> head(df_class)
  TARGET_BAD_FLAG LOAN IMP_MORTDUE M_MORTDUE IMP_VALUE M_VALUE IMP_YOJ M_YOJ IMP_DEROG M_DEROG
1               1 1100       25860         0     39025       0    10.5     0         0       0
2               1 1300       70053         0     68400       0     7.0     0         0       0
3               1 1500       13500         0     16700       0     4.0     0         0       0
4               1 1500       65000         1     89000       1     7.0     1         1       1
5               0 1700       97800         0    112000       0     3.0     0         0       0
6               1 1700       30548         0     40320       0     9.0     0         0       0
  IMP_DELINQ M_DELINQ IMP_CLAGE M_CLAGE IMP_NINQ M_NINQ IMP_CLNO M_CLNO IMP_DEBTINC M_DEBTINC
1          0        0  94.36667       0        1      0        9      0    35.00000         1
2          2        0 121.83333       0        0      0       14      0    35.00000         1
3          0        0 149.46667       0        1      0       10      0    35.00000         1
4          1        1 174.00000       1        1      1       20      1    35.00000         1
5          0        0  93.33333       0        0      0       14      0    35.00000         1
6          0        0 101.46600       0        1      0        8      0    37.11361         0
  FLAG.Job.Mgr FLAG.Job.Office FLAG.Job.Other FLAG.Job.ProfExe FLAG.Job.Sales FLAG.Job.Self
1            0               0              1                0              0             0
2            0               0              1                0              0             0
3            0               0              1                0              0             0
4            0               0              0                0              0             0
5            0               1              0                0              0             0
6            0               0              1                0              0             0
  FLAG.Reason.DebtCon FLAG.Reason.HomeImp
1                   0                   1
2                   0                   1
3                   0                   1
4                   0                   0
5                   0                   1
6                   0                   1
> 
> #GINI TREE
> 
> tree_gini <- rpart(TARGET_BAD_FLAG ~ ., data=df_class, method="class", parms=list(split="gini"))
> rpart.plot(tree_gini)
> tree_gini$variable.importance
  M_DEBTINC IMP_DEBTINC  IMP_DELINQ     M_VALUE   IMP_CLAGE        LOAN   IMP_DEROG     M_DEROG 
 570.021010  128.539072   77.371518   51.334486   36.076295   25.645675   22.501563    9.540586 
  IMP_VALUE    M_DELINQ      M_NINQ     IMP_YOJ      M_CLNO    IMP_CLNO IMP_MORTDUE 
   8.551021    7.632469    6.311465    4.323751    4.256569    2.837461    1.621407 
> 
> 
> #ENTROPY TREE
> 
> tree_entropy <- rpart(TARGET_BAD_FLAG ~ ., data=df_class, method="class", parms=list(split="information"))
> rpart.plot(tree_entropy) 
> 
> tree_entropy$variable.importance
  M_DEBTINC IMP_DEBTINC  IMP_DELINQ   IMP_CLAGE        LOAN     M_VALUE   IMP_DEROG   IMP_VALUE 
 762.591210  188.922871   68.152477   40.125205   34.053718   30.094365   12.037746   10.263083 
    IMP_YOJ    IMP_CLNO IMP_MORTDUE 
   3.436136    3.075170    1.219274 
> 
> #ROC 
> 
> prob_gini <- predict(tree_gini, df_class, type="prob")[,2]
> pred_gini <- prediction(prob_gini, df_class$TARGET_BAD_FLAG)
> perf_gini <- performance(pred_gini, "tpr", "fpr")
> plot(perf_gini, main="ROC Curve - Gini")
> 
> prob_entropy <- predict(tree_entropy, df_class, type="prob")[,2]
> pred_entropy <- prediction(prob_entropy, df_class$TARGET_BAD_FLAG)
> perf_entropy <- performance(pred_entropy, "tpr", "fpr")
> plot(perf_entropy, main="ROC Curve - Entropy")
> 
> plot( perf_gini, col="red" )
> plot( perf_entropy , col="green", add=TRUE )
> abline(0,1,lty=2)
> 
> legend("bottomright",c("GINI","ENTROPY"),col=c("red","green"), bty="y", lty=1 )
> 
> aucG = performance(  pred_gini, "auc" )@y.values
> aucE = performance( pred_entropy, "auc" )@y.values
> 
> print( aucG )
[[1]]
[1] 0.8433084

> print( aucE )
[[1]]
[1] 0.8293732

> 
> 
> fG = predict( tree_gini, df, type="class" )
> fE = predict( tree_entropy, df, type="class" )
> 
> table( fG, df$TARGET_BAD_FLAG)
   
fG     0    1
  0 4468  370
  1  303  819
> 
> table( fE, df$TARGET_BAD_FLAG)
   
fE     0    1
  0 4515  458
  1  256  731
> 
> 
> #REGRESSION DECISION TEE
> 
> df_reg <- df
> df_reg$TARGET_BAD_FLAG <- NULL
> 
> head(df_reg)
  TARGET_LOSS_AMT LOAN IMP_MORTDUE M_MORTDUE IMP_VALUE M_VALUE IMP_YOJ M_YOJ IMP_DEROG M_DEROG
1             641 1100       25860         0     39025       0    10.5     0         0       0
2            1109 1300       70053         0     68400       0     7.0     0         0       0
3             767 1500       13500         0     16700       0     4.0     0         0       0
4            1425 1500       65000         1     89000       1     7.0     1         1       1
5               0 1700       97800         0    112000       0     3.0     0         0       0
6             335 1700       30548         0     40320       0     9.0     0         0       0
  IMP_DELINQ M_DELINQ IMP_CLAGE M_CLAGE IMP_NINQ M_NINQ IMP_CLNO M_CLNO IMP_DEBTINC M_DEBTINC
1          0        0  94.36667       0        1      0        9      0    35.00000         1
2          2        0 121.83333       0        0      0       14      0    35.00000         1
3          0        0 149.46667       0        1      0       10      0    35.00000         1
4          1        1 174.00000       1        1      1       20      1    35.00000         1
5          0        0  93.33333       0        0      0       14      0    35.00000         1
6          0        0 101.46600       0        1      0        8      0    37.11361         0
  FLAG.Job.Mgr FLAG.Job.Office FLAG.Job.Other FLAG.Job.ProfExe FLAG.Job.Sales FLAG.Job.Self
1            0               0              1                0              0             0
2            0               0              1                0              0             0
3            0               0              1                0              0             0
4            0               0              0                0              0             0
5            0               1              0                0              0             0
6            0               0              1                0              0             0
  FLAG.Reason.DebtCon FLAG.Reason.HomeImp
1                   0                   1
2                   0                   1
3                   0                   1
4                   0                   0
5                   0                   1
6                   0                   1
> 
> #ANOVA TREE
> 
> tree_anova <- rpart(TARGET_LOSS_AMT ~ ., data=df_reg, method="anova")
> rpart.plot(tree_anova)
> tree_anova$variable.importance
          M_DEBTINC                LOAN         IMP_DEBTINC          IMP_DELINQ 
        64758513590         64443856477         19307937442         18468415581 
          IMP_VALUE            IMP_CLNO         IMP_MORTDUE           IMP_CLAGE 
         9985413830          8640006256          7345104792          5561821234 
            M_VALUE           IMP_DEROG FLAG.Reason.HomeImp FLAG.Reason.DebtCon 
         3812596217          3423606021          2487025698          2376139202 
            M_DEROG            M_DELINQ              M_NINQ             IMP_YOJ 
         1695086247          1384320435          1101806061           803802835 
              M_YOJ      FLAG.Job.Other           M_MORTDUE       FLAG.Job.Self 
          727900700           569633461           363950350           269034105 
> 
> pred_anova <- predict(tree_anova, df_reg)
> RMSE_anova <- sqrt(mean((df$TARGET_LOSS_AMT - pred_anova)^2))
> 
> print( RMSE_anova)
[1] 4848.417
> 
> pred_anova <- predict(tree_anova, df_reg)
> 
> #POISON TREE
> 
> tree_poisson <- rpart(TARGET_LOSS_AMT ~ ., data=df_reg, method="poisson")
> rpart.plot(tree_poisson)
> tree_poisson$variable.importance
          M_DEBTINC         IMP_DEBTINC                LOAN          IMP_DELINQ 
        18534649.01          6636788.15          5093017.45          1989199.88 
          IMP_VALUE             M_VALUE         IMP_MORTDUE           IMP_DEROG 
          765775.84           731438.40           390250.40           292575.36 
FLAG.Reason.HomeImp FLAG.Reason.DebtCon            IMP_CLNO             IMP_YOJ 
          214334.43           197111.13            82289.11            24796.57 
      FLAG.Job.Self 
           12398.29 
> pred_poisson <- predict(tree_poisson, df_reg)
> 
> RMSE_poisson <- sqrt(mean((df$TARGET_LOSS_AMT - pred_poisson)^2))
> 
> print(RMSE_poisson)
[1] 5558.973
> 
> #PROBABILITY X SEVERITY MODEL 
> 
> tree_prob <- rpart(TARGET_BAD_FLAG ~ ., data=df_class, method="class")
> prob_default <- predict(tree_prob, df_class, type="prob")[,2]
> 
> rpart.plot(tree_prob)
> 
> df_severity <- subset(df, TARGET_BAD_FLAG == 1)
> head(df_severity)
  TARGET_BAD_FLAG TARGET_LOSS_AMT LOAN IMP_MORTDUE M_MORTDUE IMP_VALUE M_VALUE IMP_YOJ M_YOJ
1               1             641 1100       25860         0     39025       0    10.5     0
2               1            1109 1300       70053         0     68400       0     7.0     0
3               1             767 1500       13500         0     16700       0     4.0     0
4               1            1425 1500       65000         1     89000       1     7.0     1
6               1             335 1700       30548         0     40320       0     9.0     0
7               1            1841 1800       48649         0     57037       0     5.0     0
  IMP_DEROG M_DEROG IMP_DELINQ M_DELINQ IMP_CLAGE M_CLAGE IMP_NINQ M_NINQ IMP_CLNO M_CLNO
1         0       0          0        0  94.36667       0        1      0        9      0
2         0       0          2        0 121.83333       0        0      0       14      0
3         0       0          0        0 149.46667       0        1      0       10      0
4         1       1          1        1 174.00000       1        1      1       20      1
6         0       0          0        0 101.46600       0        1      0        8      0
7         3       0          2        0  77.10000       0        1      0       17      0
  IMP_DEBTINC M_DEBTINC FLAG.Job.Mgr FLAG.Job.Office FLAG.Job.Other FLAG.Job.ProfExe
1    35.00000         1            0               0              1                0
2    35.00000         1            0               0              1                0
3    35.00000         1            0               0              1                0
4    35.00000         1            0               0              0                0
6    37.11361         0            0               0              1                0
7    35.00000         1            0               0              1                0
  FLAG.Job.Sales FLAG.Job.Self FLAG.Reason.DebtCon FLAG.Reason.HomeImp
1              0             0                   0                   1
2              0             0                   0                   1
3              0             0                   0                   1
4              0             0                   0                   0
6              0             0                   0                   1
7              0             0                   0                   1
> 
> tree_severity <- rpart(TARGET_LOSS_AMT ~ ., data=df_severity, method="anova")
> rpart.plot(tree_prob)
> rpart.plot(tree_severity)
Warning message:
labs do not fit even at cex 0.15, there may be some overplotting 
> tr_set <- rpart.control(maxdepth = 10)
> tree_severity <- rpart(TARGET_LOSS_AMT ~ ., data = df_severity, method = "anova", control = tr_set)
> rpart.plot(tree_severity)
Warning message:
labs do not fit even at cex 0.15, there may be some overplotting 
> tr_set <- rpart.control(maxdepth = 4)
> tree_severity <- rpart(TARGET_LOSS_AMT ~ ., data = df_severity, method = "anova", control = tr_set)
>  rpart.plot(tree_severity)
Warning message:
labs do not fit even at cex 0.15, there may be some overplotting 
> tr_set <- rpart.control(maxdepth = 3)
> tree_severity <- rpart(TARGET_LOSS_AMT ~ ., data = df_severity, method = "anova", control = tr_set)
> rpart.plot(tree_severity)
Warning message:
labs do not fit even at cex 0.15, there may be some overplotting 
> tr_set <- rpart.control(maxdepth = 3)
> tree_severity <- rpart(TARGET_LOSS_AMT ~ ., data = df_severity, method = "anova", control = tr_set)
> rpart.plot(tree_severity, type = 2, extra = 101, fallen.leaves = TRUE, cex = 0.6)
Warning message:
labs do not fit even at cex 0.15, there may be some overplotting 
> 
> rpart.plot(tree_severity,
+            type = 2,
+            extra = 101,
+            fallen.leaves = TRUE,
+            cex = 0.6,
+            compress = TRUE,
+            space = 0)
> 
> tree_prob$variable.importance
  M_DEBTINC IMP_DEBTINC  IMP_DELINQ     M_VALUE   IMP_CLAGE        LOAN   IMP_DEROG     M_DEROG 
 570.021010  128.539072   77.371518   51.334486   36.076295   25.645675   22.501563    9.540586 
  IMP_VALUE    M_DELINQ      M_NINQ     IMP_YOJ      M_CLNO    IMP_CLNO IMP_MORTDUE 
   8.551021    7.632469    6.311465    4.323751    4.256569    2.837461    1.621407 
> tree_severity$variable.importance
               LOAN           IMP_VALUE         IMP_MORTDUE         IMP_DEBTINC 
        95490401040         12784086193          9235762508          8190794036 
           IMP_CLNO FLAG.Reason.HomeImp FLAG.Reason.DebtCon            IMP_NINQ 
         5197172526          4166142650          3945210843          1284908173 
            IMP_YOJ           M_MORTDUE           IMP_CLAGE          IMP_DELINQ 
          742530790           620243894           444203863           243986582 
          IMP_DEROG 
           88840773 
> 
> prob_default <- predict(tree_prob, df_class, type="prob")[,2]
> loss_pred <- predict(tree_severity, df)
> 
> expected_loss <- prob_default * loss_pred
> 
> RMSE_combined <- sqrt(mean((df$TARGET_LOSS_AMT - expected_loss)^2))
> 
> print(RMSE_combined)
[1] 4997.231
> 
> print(paste("RMSE - ANOVA:", RMSE_anova))
[1] "RMSE - ANOVA: 4848.41731538758"
> print(paste("RMSE - POISSON:", RMSE_poisson))
[1] "RMSE - POISSON: 5558.97326812049"
> print(paste("RMSE - Probability x Severity:", RMSE_combined))
[1] "RMSE - Probability x Severity: 4997.23076464949"
