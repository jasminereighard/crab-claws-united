data_analysis
================
Jasmine Reighard
2024-11-03

``` r
#load data saved from data org RMD 

load("mbio621_data.RData")

#load metadata csv
metadata <- read.csv("/Users/jasminereighard/Desktop/mbio621/data/meta_data.csv", stringsAsFactors = FALSE)

head(metadata)
```

    ##       date treatment fish_id weight_g new_mass temp arm_status when_arm_lost
    ## 1 11/19/24   Control  TC1_C1    11.70 2.557035 25.5          0          good
    ## 2 11/19/24   Control  TC2_C1    15.02 3.282621 25.5          0          good
    ## 3 11/19/24   Control  TC3_C1    15.35 3.354742 25.5          1        before
    ## 4 11/19/24   Control  TC4_C1    11.64 2.543922 25.5          1        during
    ## 5 11/21/24   Control  TC1_C2     9.62 2.102451 25.5          0          good
    ## 6 11/21/24   Control  TC2_C2     9.40 2.054370 25.5          1        during
    ##   limb_type
    ## 1      none
    ## 2      none
    ## 3      both
    ## 4      claw
    ## 5      none
    ## 6      claw

``` r
print(metadata)
```

    ##        date treatment fish_id weight_g new_mass temp arm_status when_arm_lost
    ## 1  11/19/24   Control  TC1_C1    11.70 2.557035 25.5          0          good
    ## 2  11/19/24   Control  TC2_C1    15.02 3.282621 25.5          0          good
    ## 3  11/19/24   Control  TC3_C1    15.35 3.354742 25.5          1        before
    ## 4  11/19/24   Control  TC4_C1    11.64 2.543922 25.5          1        during
    ## 5  11/21/24   Control  TC1_C2     9.62 2.102451 25.5          0          good
    ## 6  11/21/24   Control  TC2_C2     9.40 2.054370 25.5          1        during
    ## 7  11/21/24   Control  TC3_C2     9.34 2.041257 25.5          1        before
    ## 8  11/21/24   Control  TC4_C2     9.73 2.126492 25.5          1        before
    ## 9  11/26/24       MHW  TC1_T2    16.34 3.571107 28.5          0          good
    ## 10 11/26/24       MHW  TC2_T2    17.06 3.728463 28.5          1        before
    ## 11 11/26/24       MHW  TC3_T2    17.39 3.800584 28.5          1        before
    ## 12 11/26/24       MHW  TC4_T2    18.52 4.047546 28.5          0          good
    ## 13 11/17/24   Control  TC1_C3     6.00 1.311300 26.0          0          good
    ## 14 11/17/24   Control  TC2_C3    13.87 3.031289 26.0          1        during
    ## 15 11/17/24   Control  TC3_C3     9.82 2.146161 26.0          0          good
    ##    limb_type
    ## 1       none
    ## 2       none
    ## 3       both
    ## 4       claw
    ## 5       none
    ## 6       claw
    ## 7        leg
    ## 8       claw
    ## 9       none
    ## 10       leg
    ## 11       leg
    ## 12      none
    ## 13      none
    ## 14      claw
    ## 15      none

``` r
# Ensure the lists are named with fish_id as keys
# all_MMR, all_q20, and aerobic_scope_values should have named elements where names match the fish_id values.

# Convert the lists to dataframes for merging
MMR_dfs <- data.frame(fish_id = names(all_MMR), MMR = unlist(all_MMR), stringsAsFactors = FALSE)
SMR_dfs <- data.frame(fish_id = names(all_q20), SMR = unlist(all_q20), stringsAsFactors = FALSE)
AS_dfs <- data.frame(fish_id = names(aerobic_scope_values), AS = unlist(aerobic_scope_values), stringsAsFactors = FALSE)
pcrit_dfs <- data.frame(fish_id = names(pcrit_values), Pcrit = unlist(pcrit_values), stringsAsFactors = FALSE)

# Merge the metadata dataframe with the newly created dataframes
respdat <- metadata %>%
  left_join(MMR_dfs, by = "fish_id") %>%
  left_join(SMR_dfs, by = "fish_id") %>%
  left_join(AS_dfs, by = "fish_id") %>%
  left_join(pcrit_dfs, by = "fish_id")

# Clean the Pcrit column to retain only numeric values
respdat$Pcrit <- as.numeric(gsub("Pcrit = ", "", respdat$Pcrit))

# View the updated dataframe
respdat
```

    ##        date treatment fish_id weight_g new_mass temp arm_status when_arm_lost
    ## 1  11/19/24   Control  TC1_C1    11.70 2.557035 25.5          0          good
    ## 2  11/19/24   Control  TC2_C1    15.02 3.282621 25.5          0          good
    ## 3  11/19/24   Control  TC3_C1    15.35 3.354742 25.5          1        before
    ## 4  11/19/24   Control  TC4_C1    11.64 2.543922 25.5          1        during
    ## 5  11/21/24   Control  TC1_C2     9.62 2.102451 25.5          0          good
    ## 6  11/21/24   Control  TC2_C2     9.40 2.054370 25.5          1        during
    ## 7  11/21/24   Control  TC3_C2     9.34 2.041257 25.5          1        before
    ## 8  11/21/24   Control  TC4_C2     9.73 2.126492 25.5          1        before
    ## 9  11/26/24       MHW  TC1_T2    16.34 3.571107 28.5          0          good
    ## 10 11/26/24       MHW  TC2_T2    17.06 3.728463 28.5          1        before
    ## 11 11/26/24       MHW  TC3_T2    17.39 3.800584 28.5          1        before
    ## 12 11/26/24       MHW  TC4_T2    18.52 4.047546 28.5          0          good
    ## 13 11/17/24   Control  TC1_C3     6.00 1.311300 26.0          0          good
    ## 14 11/17/24   Control  TC2_C3    13.87 3.031289 26.0          1        during
    ## 15 11/17/24   Control  TC3_C3     9.82 2.146161 26.0          0          good
    ##    limb_type      MMR      SMR        AS     Pcrit
    ## 1       none 1427.324 219.2513 1208.0723  45.09828
    ## 2       none 1564.327 266.0357 1298.2909  96.18695
    ## 3       both 1493.537 384.4170 1109.1204  96.00625
    ## 4       claw 1925.103 225.2951 1699.8075  33.64721
    ## 5       none 1995.983 743.2031 1252.7802  86.03582
    ## 6       claw 3507.073 272.0468 3235.0262  40.98869
    ## 7        leg 1862.931 308.3890 1554.5423  56.62216
    ## 8       claw 1840.884 447.6744 1393.2095 100.67538
    ## 9       none 1995.983 274.6359 1721.3474  38.45168
    ## 10       leg 3507.073 338.4395 3168.6335  51.53860
    ## 11       leg 1862.931 339.7384 1523.1929  51.69869
    ## 12      none 1840.884 361.1510 1479.7329  67.00543
    ## 13      none 2434.134 406.6570 2027.4769  68.25830
    ## 14      claw 1437.630 851.2558  586.3737 138.83393
    ## 15      none 2338.524 255.3019 2083.2220  38.71340

``` r
longrespdat <- pivot_longer(respdat, cols = c(SMR, MMR, AS, Pcrit), names_to = "metric", values_to = "value")
str(longrespdat)
```

    ## tibble [60 × 11] (S3: tbl_df/tbl/data.frame)
    ##  $ date         : chr [1:60] "11/19/24" "11/19/24" "11/19/24" "11/19/24" ...
    ##  $ treatment    : chr [1:60] "Control" "Control" "Control" "Control" ...
    ##  $ fish_id      : chr [1:60] "TC1_C1" "TC1_C1" "TC1_C1" "TC1_C1" ...
    ##  $ weight_g     : num [1:60] 11.7 11.7 11.7 11.7 15 ...
    ##  $ new_mass     : num [1:60] 2.56 2.56 2.56 2.56 3.28 ...
    ##  $ temp         : num [1:60] 25.5 25.5 25.5 25.5 25.5 25.5 25.5 25.5 25.5 25.5 ...
    ##  $ arm_status   : int [1:60] 0 0 0 0 0 0 0 0 1 1 ...
    ##  $ when_arm_lost: chr [1:60] "good" "good" "good" "good" ...
    ##  $ limb_type    : chr [1:60] "none" "none" "none" "none" ...
    ##  $ metric       : chr [1:60] "SMR" "MMR" "AS" "Pcrit" ...
    ##  $ value        : num [1:60] 219.3 1427.3 1208.1 45.1 266 ...

``` r
longrespdat$treatment <- as.factor(longrespdat$treatment)
longrespdat$id <- as.factor(longrespdat$fish_id)
longrespdat$when_arm_lost <- as.factor(longrespdat$when_arm_lost)
longrespdat$limb_type <- as.factor(longrespdat$limb_type)
longrespdat$metric <- as.factor(longrespdat$metric)
longrespdat$mass <- longrespdat$new_mass
longrespdat$arm_status <- as.factor(longrespdat$arm_status)


str(longrespdat)
```

    ## tibble [60 × 13] (S3: tbl_df/tbl/data.frame)
    ##  $ date         : chr [1:60] "11/19/24" "11/19/24" "11/19/24" "11/19/24" ...
    ##  $ treatment    : Factor w/ 2 levels "Control","MHW": 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ fish_id      : chr [1:60] "TC1_C1" "TC1_C1" "TC1_C1" "TC1_C1" ...
    ##  $ weight_g     : num [1:60] 11.7 11.7 11.7 11.7 15 ...
    ##  $ new_mass     : num [1:60] 2.56 2.56 2.56 2.56 3.28 ...
    ##  $ temp         : num [1:60] 25.5 25.5 25.5 25.5 25.5 25.5 25.5 25.5 25.5 25.5 ...
    ##  $ arm_status   : Factor w/ 2 levels "0","1": 1 1 1 1 1 1 1 1 2 2 ...
    ##  $ when_arm_lost: Factor w/ 3 levels "before","during",..: 3 3 3 3 3 3 3 3 1 1 ...
    ##  $ limb_type    : Factor w/ 4 levels "both","claw",..: 4 4 4 4 4 4 4 4 1 1 ...
    ##  $ metric       : Factor w/ 4 levels "AS","MMR","Pcrit",..: 4 2 1 3 4 2 1 3 4 2 ...
    ##  $ value        : num [1:60] 219.3 1427.3 1208.1 45.1 266 ...
    ##  $ id           : Factor w/ 15 levels "TC1_C1","TC1_C2",..: 1 1 1 1 5 5 5 5 9 9 ...
    ##  $ mass         : num [1:60] 2.56 2.56 2.56 2.56 3.28 ...

``` r
#exclude small fish !!!
longrespdat <- longrespdat %>% 
  filter(!fish_id %in% c("TC1_C3", "TC2_C2"))

# Verify the changes
table(longrespdat$fish_id)
```

    ## 
    ## TC1_C1 TC1_C2 TC1_T2 TC2_C1 TC2_C3 TC2_T2 TC3_C1 TC3_C2 TC3_C3 TC3_T2 TC4_C1 
    ##      4      4      4      4      4      4      4      4      4      4      4 
    ## TC4_C2 TC4_T2 
    ##      4      4

``` r
#linear mixed model mod1
values_mod <- lmer(value ~ metric + metric:treatment + metric:mass + metric:treatment:mass + (1|id), data = longrespdat)

summary(values_mod)
```

    ## Linear mixed model fit by REML. t-tests use Satterthwaite's method [
    ## lmerModLmerTest]
    ## Formula: 
    ## value ~ metric + metric:treatment + metric:mass + metric:treatment:mass +  
    ##     (1 | id)
    ##    Data: longrespdat
    ## 
    ## REML criterion at convergence: 537.5
    ## 
    ## Scaled residuals: 
    ##      Min       1Q   Median       3Q      Max 
    ## -1.57739 -0.38081 -0.03338  0.26536  2.48393 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  id       (Intercept)  30742   175.3   
    ##  Residual             116361   341.1   
    ## Number of obs: 52, groups:  id, 13
    ## 
    ## Fixed effects:
    ##                               Estimate Std. Error       df t value Pr(>|t|)    
    ## (Intercept)                    2493.56     676.63    31.83   3.685 0.000845 ***
    ## metricMMR                       370.55     851.05    27.00   0.435 0.666731    
    ## metricPcrit                   -2499.33     851.05    27.00  -2.937 0.006706 ** 
    ## metricSMR                     -2123.00     851.05    27.00  -2.495 0.019029 *  
    ## metricAS:treatmentMHW          4298.51    4284.44    31.83   1.003 0.323291    
    ## metricMMR:treatmentMHW         3624.57    4284.44    31.83   0.846 0.403877    
    ## metricPcrit:treatmentMHW       -161.93    4284.44    31.83  -0.038 0.970086    
    ## metricSMR:treatmentMHW         -673.94    4284.44    31.83  -0.157 0.876003    
    ## metricAS:mass                  -442.36     257.91    31.83  -1.715 0.096037 .  
    ## metricMMR:mass                 -426.58     257.91    31.83  -1.654 0.107962    
    ## metricPcrit:mass                 32.08     257.91    31.83   0.124 0.901799    
    ## metricSMR:mass                   15.78     257.91    31.83   0.061 0.951596    
    ## metricAS:treatmentMHW:mass     -830.13    1145.44    31.83  -0.725 0.473916    
    ## metricMMR:treatmentMHW:mass    -679.05    1145.44    31.83  -0.593 0.557485    
    ## metricPcrit:treatmentMHW:mass    25.98    1145.44    31.83   0.023 0.982043    
    ## metricSMR:treatmentMHW:mass     151.08    1145.44    31.83   0.132 0.895898    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    ## 
    ## Correlation matrix not shown by default, as p = 16 > 12.
    ## Use print(x, correlation=TRUE)  or
    ##     vcov(x)        if you need it

``` r
Anova(values_mod)
```

    ## Analysis of Deviance Table (Type II Wald chisquare tests)
    ## 
    ## Response: value
    ##                          Chisq Df Pr(>Chisq)    
    ## metric                268.5937  3  < 2.2e-16 ***
    ## metric:treatment       18.1309  4   0.001163 ** 
    ## metric:mass             6.9560  4   0.138230    
    ## metric:treatment:mass   0.9113  4   0.922935    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
#anova of random effects
#there is an effect of treatment 
#no effect of mass across treatment groups
#there is an effect of mass within groups 


#just treatment! no mass 
values_mod2 <- lmer(value ~ metric + metric:treatment + (1|id), data = longrespdat)

summary(values_mod2)
```

    ## Linear mixed model fit by REML. t-tests use Satterthwaite's method [
    ## lmerModLmerTest]
    ## Formula: value ~ metric + metric:treatment + (1 | id)
    ##    Data: longrespdat
    ## 
    ## REML criterion at convergence: 660.2
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -2.0308 -0.5538 -0.0070  0.3660  2.6197 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  id       (Intercept)  31677   178.0   
    ##  Residual             115437   339.8   
    ## Number of obs: 52, groups:  id, 13
    ## 
    ## Fixed effects:
    ##                          Estimate Std. Error       df t value Pr(>|t|)    
    ## (Intercept)               1353.94     127.85    38.63  10.590 5.54e-13 ***
    ## metricMMR                  411.20     160.16    33.00   2.567   0.0150 *  
    ## metricPcrit              -1277.07     160.16    33.00  -7.973 3.38e-09 ***
    ## metricSMR                 -942.73     160.16    33.00  -5.886 1.35e-06 ***
    ## metricAS:treatmentMHW      619.29     230.49    38.63   2.687   0.0106 *  
    ## metricMMR:treatmentMHW     536.58     230.49    38.63   2.328   0.0252 *  
    ## metricPcrit:treatmentMHW   -24.70     230.49    38.63  -0.107   0.9152    
    ## metricSMR:treatmentMHW     -82.71     230.49    38.63  -0.359   0.7217    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) mtrMMR mtrcPc mtrSMR mAS:MH mMMR:M mP:MHW
    ## metricMMR   -0.626                                          
    ## metricPcrit -0.626  0.500                                   
    ## metricSMR   -0.626  0.500  0.500                            
    ## mtrcAS:tMHW -0.555  0.347  0.347  0.347                     
    ## mtrcMMR:MHW -0.119 -0.347  0.000  0.000  0.215              
    ## mtrcPcr:MHW -0.119  0.000 -0.347  0.000  0.215  0.215       
    ## mtrcSMR:MHW -0.119  0.000  0.000 -0.347  0.215  0.215  0.215

``` r
Anova(values_mod2)
```

    ## Analysis of Deviance Table (Type II Wald chisquare tests)
    ## 
    ## Response: value
    ##                    Chisq Df Pr(>Chisq)    
    ## metric           270.744  3     <2e-16 ***
    ## metric:treatment  12.836  4     0.0121 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
#slopes
mass_slopes_mod <- emtrends(values_mod, ~metric:treatment, "mass")
```

    ## NOTE: A nesting structure was detected in the fitted model:
    ##     treatment %in% metric

``` r
summary(mass_slopes_mod, infer = TRUE)
```

    ##  treatment metric mass.trend   SE   df lower.CL upper.CL t.ratio p.value
    ##  Control   AS         -442.4  258 31.8     -968     83.1  -1.715  0.0960
    ##  MHW       AS        -1272.5 1116 31.8    -3546   1001.3  -1.140  0.2627
    ##  Control   MMR        -426.6  258 31.8     -952     98.9  -1.654  0.1080
    ##  MHW       MMR       -1105.6 1116 31.8    -3379   1168.1  -0.991  0.3293
    ##  Control   Pcrit        32.1  258 31.8     -493    557.5   0.124  0.9018
    ##  MHW       Pcrit        58.1 1116 31.8    -2216   2331.8   0.052  0.9588
    ##  Control   SMR          15.8  258 31.8     -510    541.2   0.061  0.9516
    ##  MHW       SMR         166.9 1116 31.8    -2107   2440.6   0.150  0.8821
    ## 
    ## Degrees-of-freedom method: kenward-roger 
    ## Confidence level used: 0.95

``` r
#mass predicts MMR & AS in control fish 
```

``` r
#descriptive stats
#--------Model 1-------------
longrespdat %>%
  group_by(treatment, metric) %>%
  summarise(mean=mean(value))
```

    ## `summarise()` has grouped output by 'treatment'. You can override using the
    ## `.groups` argument.

    ## # A tibble: 8 × 3
    ## # Groups:   treatment [2]
    ##   treatment metric   mean
    ##   <fct>     <fct>   <dbl>
    ## 1 Control   AS     1354. 
    ## 2 Control   MMR    1765. 
    ## 3 Control   Pcrit    76.9
    ## 4 Control   SMR     411. 
    ## 5 MHW       AS     1973. 
    ## 6 MHW       MMR    2302. 
    ## 7 MHW       Pcrit    52.2
    ## 8 MHW       SMR     328.

``` r
#pull out mean values

emmeans_mod <- emmeans(values_mod, specs = c("metric", "treatment"))
```

    ## NOTE: A nesting structure was detected in the fitted model:
    ##     treatment %in% metric
    ## NOTE: Results may be misleading due to involvement in interactions

``` r
#model slays
summary(emmeans_mod)
```

    ##  treatment metric  emmean  SE   df lower.CL upper.CL
    ##  Control   AS     1189.14 160 31.8    863.3     1515
    ##  MHW       AS     3039.81 955 31.8   1094.3     4985
    ##  Control   MMR    1606.23 160 31.8   1280.4     1932
    ##  MHW       MMR    3228.44 955 31.8   1283.0     5174
    ##  Control   Pcrit    88.82 160 31.8   -237.0      415
    ##  MHW       Pcrit     3.51 955 31.8  -1942.0     1949
    ##  Control   SMR     417.08 160 31.8     91.3      743
    ##  MHW       SMR     188.63 955 31.8  -1756.8     2134
    ## 
    ## Degrees-of-freedom method: kenward-roger 
    ## Confidence level used: 0.95

``` r
ttest_mod <- emmeans(values_mod, pairwise ~ treatment|metric, adjust = "fdr")
```

    ## NOTE: A nesting structure was detected in the fitted model:
    ##     treatment %in% metric
    ## NOTE: Results may be misleading due to involvement in interactions

``` r
ttest_mod
```

    ## $emmeans
    ## metric = AS:
    ##  treatment  emmean  SE   df lower.CL upper.CL
    ##  Control   1189.14 160 31.8    863.3     1515
    ##  MHW       3039.81 955 31.8   1094.3     4985
    ## 
    ## metric = MMR:
    ##  treatment  emmean  SE   df lower.CL upper.CL
    ##  Control   1606.23 160 31.8   1280.4     1932
    ##  MHW       3228.44 955 31.8   1283.0     5174
    ## 
    ## metric = Pcrit:
    ##  treatment  emmean  SE   df lower.CL upper.CL
    ##  Control     88.82 160 31.8   -237.0      415
    ##  MHW          3.51 955 31.8  -1942.0     1949
    ## 
    ## metric = SMR:
    ##  treatment  emmean  SE   df lower.CL upper.CL
    ##  Control    417.08 160 31.8     91.3      743
    ##  MHW        188.63 955 31.8  -1756.8     2134
    ## 
    ## Degrees-of-freedom method: kenward-roger 
    ## Confidence level used: 0.95 
    ## 
    ## $contrasts
    ## metric = AS:
    ##  contrast      estimate  SE   df t.ratio p.value
    ##  Control - MHW  -1850.7 968 31.8  -1.911  0.0650
    ## 
    ## metric = MMR:
    ##  contrast      estimate  SE   df t.ratio p.value
    ##  Control - MHW  -1622.2 968 31.8  -1.676  0.1036
    ## 
    ## metric = Pcrit:
    ##  contrast      estimate  SE   df t.ratio p.value
    ##  Control - MHW     85.3 968 31.8   0.088  0.9303
    ## 
    ## metric = SMR:
    ##  contrast      estimate  SE   df t.ratio p.value
    ##  Control - MHW    228.4 968 31.8   0.236  0.8150
    ## 
    ## Degrees-of-freedom method: kenward-roger

``` r
#--------Model 2-------------
longrespdat %>%
  group_by(treatment, metric) %>%
  summarise(mean=mean(value))
```

    ## `summarise()` has grouped output by 'treatment'. You can override using the
    ## `.groups` argument.

    ## # A tibble: 8 × 3
    ## # Groups:   treatment [2]
    ##   treatment metric   mean
    ##   <fct>     <fct>   <dbl>
    ## 1 Control   AS     1354. 
    ## 2 Control   MMR    1765. 
    ## 3 Control   Pcrit    76.9
    ## 4 Control   SMR     411. 
    ## 5 MHW       AS     1973. 
    ## 6 MHW       MMR    2302. 
    ## 7 MHW       Pcrit    52.2
    ## 8 MHW       SMR     328.

``` r
#pull out mean values

emmeans_mod2 <- emmeans(values_mod2, specs = c("metric", "treatment"))
```

    ## NOTE: A nesting structure was detected in the fitted model:
    ##     treatment %in% metric

``` r
#model slays
summary(emmeans_mod2)
```

    ##  treatment metric emmean  SE   df lower.CL upper.CL
    ##  Control   AS     1353.9 128 38.6   1095.3     1613
    ##  MHW       AS     1973.2 192 38.6   1585.2     2361
    ##  Control   MMR    1765.1 128 38.6   1506.5     2024
    ##  MHW       MMR    2301.7 192 38.6   1913.7     2690
    ##  Control   Pcrit    76.9 128 38.6   -181.8      336
    ##  MHW       Pcrit    52.2 192 38.6   -335.9      440
    ##  Control   SMR     411.2 128 38.6    152.5      670
    ##  MHW       SMR     328.5 192 38.6    -59.5      717
    ## 
    ## Degrees-of-freedom method: kenward-roger 
    ## Confidence level used: 0.95

``` r
ttest_mod2 <- emmeans(values_mod2, pairwise ~ treatment|metric, adjust = "fdr")
```

    ## NOTE: A nesting structure was detected in the fitted model:
    ##     treatment %in% metric

``` r
ttest_mod2
```

    ## $emmeans
    ## metric = AS:
    ##  treatment emmean  SE   df lower.CL upper.CL
    ##  Control   1353.9 128 38.6   1095.3     1613
    ##  MHW       1973.2 192 38.6   1585.2     2361
    ## 
    ## metric = MMR:
    ##  treatment emmean  SE   df lower.CL upper.CL
    ##  Control   1765.1 128 38.6   1506.5     2024
    ##  MHW       2301.7 192 38.6   1913.7     2690
    ## 
    ## metric = Pcrit:
    ##  treatment emmean  SE   df lower.CL upper.CL
    ##  Control     76.9 128 38.6   -181.8      336
    ##  MHW         52.2 192 38.6   -335.9      440
    ## 
    ## metric = SMR:
    ##  treatment emmean  SE   df lower.CL upper.CL
    ##  Control    411.2 128 38.6    152.5      670
    ##  MHW        328.5 192 38.6    -59.5      717
    ## 
    ## Degrees-of-freedom method: kenward-roger 
    ## Confidence level used: 0.95 
    ## 
    ## $contrasts
    ## metric = AS:
    ##  contrast      estimate  SE   df t.ratio p.value
    ##  Control - MHW   -619.3 230 38.6  -2.687  0.0106
    ## 
    ## metric = MMR:
    ##  contrast      estimate  SE   df t.ratio p.value
    ##  Control - MHW   -536.6 230 38.6  -2.328  0.0252
    ## 
    ## metric = Pcrit:
    ##  contrast      estimate  SE   df t.ratio p.value
    ##  Control - MHW     24.7 230 38.6   0.107  0.9152
    ## 
    ## metric = SMR:
    ##  contrast      estimate  SE   df t.ratio p.value
    ##  Control - MHW     82.7 230 38.6   0.359  0.7217
    ## 
    ## Degrees-of-freedom method: kenward-roger

## R Markdown

This is an R Markdown document. Markdown is a simple formatting syntax
for authoring HTML, PDF, and MS Word documents. For more details on
using R Markdown see <http://rmarkdown.rstudio.com>.

When you click the **Knit** button a document will be generated that
includes both content as well as the output of any embedded R code
chunks within the document. You can embed an R code chunk like this:

``` r
summary(cars)
```

    ##      speed           dist       
    ##  Min.   : 4.0   Min.   :  2.00  
    ##  1st Qu.:12.0   1st Qu.: 26.00  
    ##  Median :15.0   Median : 36.00  
    ##  Mean   :15.4   Mean   : 42.98  
    ##  3rd Qu.:19.0   3rd Qu.: 56.00  
    ##  Max.   :25.0   Max.   :120.00

## Including Plots

You can also embed plots, for example:

![](data_analysis_files/figure-gfm/pressure-1.png)<!-- -->

Note that the `echo = FALSE` parameter was added to the code chunk to
prevent printing of the R code that generated the plot.
