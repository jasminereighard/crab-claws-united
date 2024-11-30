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


# Merge the metadata dataframe with the newly created dataframes
respdat <- metadata %>%
  left_join(MMR_dfs, by = "fish_id") %>%
  left_join(SMR_dfs, by = "fish_id") %>%
  left_join(AS_dfs, by = "fish_id")

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
    ##    limb_type      MMR      SMR        AS
    ## 1       none 1427.324 219.2513 1208.0723
    ## 2       none 1564.327 266.0357 1298.2909
    ## 3       both 1493.537 384.4170 1109.1204
    ## 4       claw 1925.103 225.2951 1699.8075
    ## 5       none 1995.983 743.2031 1252.7802
    ## 6       claw 3507.073 272.0468 3235.0262
    ## 7        leg 1862.931 308.3890 1554.5423
    ## 8       claw 1840.884 447.6744 1393.2095
    ## 9       none 1995.983 274.6359 1721.3474
    ## 10       leg 3507.073 338.4395 3168.6335
    ## 11       leg 1862.931 339.7384 1523.1929
    ## 12      none 1840.884 361.1510 1479.7329
    ## 13      none 2434.134 406.6570 2027.4769
    ## 14      claw 1437.630 851.2558  586.3737
    ## 15      none 2338.524 255.3019 2083.2220

``` r
longrespdat <- pivot_longer(respdat, cols = c(SMR, MMR, AS), names_to = "metric", values_to = "value")
str(longrespdat)
```

    ## tibble [45 × 11] (S3: tbl_df/tbl/data.frame)
    ##  $ date         : chr [1:45] "11/19/24" "11/19/24" "11/19/24" "11/19/24" ...
    ##  $ treatment    : chr [1:45] "Control" "Control" "Control" "Control" ...
    ##  $ fish_id      : chr [1:45] "TC1_C1" "TC1_C1" "TC1_C1" "TC2_C1" ...
    ##  $ weight_g     : num [1:45] 11.7 11.7 11.7 15 15 ...
    ##  $ new_mass     : num [1:45] 2.56 2.56 2.56 3.28 3.28 ...
    ##  $ temp         : num [1:45] 25.5 25.5 25.5 25.5 25.5 25.5 25.5 25.5 25.5 25.5 ...
    ##  $ arm_status   : int [1:45] 0 0 0 0 0 0 1 1 1 1 ...
    ##  $ when_arm_lost: chr [1:45] "good" "good" "good" "good" ...
    ##  $ limb_type    : chr [1:45] "none" "none" "none" "none" ...
    ##  $ metric       : chr [1:45] "SMR" "MMR" "AS" "SMR" ...
    ##  $ value        : num [1:45] 219 1427 1208 266 1564 ...

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

    ## tibble [45 × 13] (S3: tbl_df/tbl/data.frame)
    ##  $ date         : chr [1:45] "11/19/24" "11/19/24" "11/19/24" "11/19/24" ...
    ##  $ treatment    : Factor w/ 2 levels "Control","MHW": 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ fish_id      : chr [1:45] "TC1_C1" "TC1_C1" "TC1_C1" "TC2_C1" ...
    ##  $ weight_g     : num [1:45] 11.7 11.7 11.7 15 15 ...
    ##  $ new_mass     : num [1:45] 2.56 2.56 2.56 3.28 3.28 ...
    ##  $ temp         : num [1:45] 25.5 25.5 25.5 25.5 25.5 25.5 25.5 25.5 25.5 25.5 ...
    ##  $ arm_status   : Factor w/ 2 levels "0","1": 1 1 1 1 1 1 2 2 2 2 ...
    ##  $ when_arm_lost: Factor w/ 3 levels "before","during",..: 3 3 3 3 3 3 1 1 1 2 ...
    ##  $ limb_type    : Factor w/ 4 levels "both","claw",..: 4 4 4 4 4 4 1 1 1 2 ...
    ##  $ metric       : Factor w/ 3 levels "AS","MMR","SMR": 3 2 1 3 2 1 3 2 1 3 ...
    ##  $ value        : num [1:45] 219 1427 1208 266 1564 ...
    ##  $ id           : Factor w/ 15 levels "TC1_C1","TC1_C2",..: 1 1 1 5 5 5 9 9 9 13 ...
    ##  $ mass         : num [1:45] 2.56 2.56 2.56 3.28 3.28 ...

``` r
#exclude small fish !!!
longrespdat <- longrespdat %>% 
  filter(!fish_id %in% c("TC1_C3", "TC2_C2"))

# Verify the changes
table(longrespdat$fish_id)
```

    ## 
    ## TC1_C1 TC1_C2 TC1_T2 TC2_C1 TC2_C3 TC2_T2 TC3_C1 TC3_C2 TC3_C3 TC3_T2 TC4_C1 
    ##      3      3      3      3      3      3      3      3      3      3      3 
    ## TC4_C2 TC4_T2 
    ##      3      3

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
    ## REML criterion at convergence: 409.9
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -1.4075 -0.5567 -0.1195  0.3599  1.9078 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  id       (Intercept)  62288   249.6   
    ##  Residual             133564   365.5   
    ## Number of obs: 39, groups:  id, 13
    ## 
    ## Fixed effects:
    ##                             Estimate Std. Error       df t value Pr(>|t|)   
    ## (Intercept)                  2493.56     780.73    22.46   3.194  0.00412 **
    ## metricMMR                     370.55     911.80    18.00   0.406  0.68924   
    ## metricSMR                   -2123.00     911.80    18.00  -2.328  0.03175 * 
    ## metricAS:treatmentMHW        4298.51    4943.64    22.46   0.870  0.39377   
    ## metricMMR:treatmentMHW       3624.57    4943.64    22.46   0.733  0.47104   
    ## metricSMR:treatmentMHW       -673.94    4943.64    22.46  -0.136  0.89278   
    ## metricAS:mass                -442.36     297.59    22.46  -1.486  0.15106   
    ## metricMMR:mass               -426.58     297.59    22.46  -1.433  0.16551   
    ## metricSMR:mass                 15.78     297.59    22.46   0.053  0.95818   
    ## metricAS:treatmentMHW:mass   -830.13    1321.68    22.46  -0.628  0.53628   
    ## metricMMR:treatmentMHW:mass  -679.05    1321.68    22.46  -0.514  0.61243   
    ## metricSMR:treatmentMHW:mass   151.08    1321.68    22.46   0.114  0.91001   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) mtrMMR mtrSMR mtAS:MHW mtMMR:MHW mtSMR:MHW mtrAS: mtMMR:
    ## metricMMR   -0.584                                                         
    ## metricSMR   -0.584  0.500                                                  
    ## mtrcAS:tMHW -0.158  0.092  0.092                                           
    ## mtrcMMR:MHW -0.050 -0.092  0.000  0.318                                    
    ## mtrcSMR:MHW -0.050  0.000 -0.092  0.318    0.318                           
    ## metrcAS:mss -0.982  0.573  0.573  0.155    0.049     0.049                 
    ## mtrcMMR:mss -0.312 -0.573  0.000  0.049    0.155     0.049     0.318       
    ## mtrcSMR:mss -0.312  0.000 -0.573  0.049    0.049     0.155     0.318  0.318
    ## mtrcAS:MHW:  0.221 -0.129 -0.129 -0.996   -0.317    -0.317    -0.225 -0.072
    ## mtrMMR:MHW:  0.070  0.129  0.000 -0.317   -0.996    -0.317    -0.072 -0.225
    ## mtrSMR:MHW:  0.070  0.000  0.129 -0.317   -0.317    -0.996    -0.072 -0.072
    ##             mtSMR: mAS:MHW: mMMR:MHW:
    ## metricMMR                            
    ## metricSMR                            
    ## mtrcAS:tMHW                          
    ## mtrcMMR:MHW                          
    ## mtrcSMR:MHW                          
    ## metrcAS:mss                          
    ## mtrcMMR:mss                          
    ## mtrcSMR:mss                          
    ## mtrcAS:MHW: -0.072                   
    ## mtrMMR:MHW: -0.072  0.318            
    ## mtrSMR:MHW: -0.225  0.318    0.318

``` r
Anova(values_mod)
```

    ## Analysis of Deviance Table (Type II Wald chisquare tests)
    ## 
    ## Response: value
    ##                          Chisq Df Pr(>Chisq)    
    ## metric                125.7819  2  < 2.2e-16 ***
    ## metric:treatment       13.1010  3   0.004423 ** 
    ## metric:mass             4.9275  3   0.177186    
    ## metric:treatment:mass   0.6837  3   0.877025    
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
    ## REML criterion at convergence: 503.1
    ## 
    ## Scaled residuals: 
    ##      Min       1Q   Median       3Q      Max 
    ## -1.76527 -0.54130 -0.02632  0.41506  1.99690 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  id       (Intercept)  66305   257.5   
    ##  Residual             129532   359.9   
    ## Number of obs: 39, groups:  id, 13
    ## 
    ## Fixed effects:
    ##                        Estimate Std. Error      df t value Pr(>|t|)    
    ## (Intercept)             1353.94     147.51   26.85   9.179 9.11e-10 ***
    ## metricMMR                411.20     169.66   22.00   2.424   0.0240 *  
    ## metricSMR               -942.73     169.66   22.00  -5.557 1.38e-05 ***
    ## metricAS:treatmentMHW    619.29     265.93   26.85   2.329   0.0276 *  
    ## metricMMR:treatmentMHW   536.58     265.93   26.85   2.018   0.0537 .  
    ## metricSMR:treatmentMHW   -82.71     265.93   26.85  -0.311   0.7582    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) mtrMMR mtrSMR mAS:MH mMMR:M
    ## metricMMR   -0.575                            
    ## metricSMR   -0.575  0.500                     
    ## mtrcAS:tMHW -0.555  0.319  0.319              
    ## mtrcMMR:MHW -0.188 -0.319  0.000  0.339       
    ## mtrcSMR:MHW -0.188  0.000 -0.319  0.339  0.339

``` r
Anova(values_mod2)
```

    ## Analysis of Deviance Table (Type II Wald chisquare tests)
    ## 
    ## Response: value
    ##                     Chisq Df Pr(>Chisq)    
    ## metric           129.6969  2    < 2e-16 ***
    ## metric:treatment   9.5304  3    0.02301 *  
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
    ##  Control   AS         -442.4  298 22.5    -1059      174  -1.486  0.1511
    ##  MHW       AS        -1272.5 1288 22.5    -3940     1395  -0.988  0.3336
    ##  Control   MMR        -426.6  298 22.5    -1043      190  -1.433  0.1655
    ##  MHW       MMR       -1105.6 1288 22.5    -3773     1562  -0.859  0.3996
    ##  Control   SMR          15.8  298 22.5     -601      632   0.053  0.9582
    ##  MHW       SMR         166.9 1288 22.5    -2501     2834   0.130  0.8981
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

    ## # A tibble: 6 × 3
    ## # Groups:   treatment [2]
    ##   treatment metric  mean
    ##   <fct>     <fct>  <dbl>
    ## 1 Control   AS     1354.
    ## 2 Control   MMR    1765.
    ## 3 Control   SMR     411.
    ## 4 MHW       AS     1973.
    ## 5 MHW       MMR    2302.
    ## 6 MHW       SMR     328.

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

    ##  treatment metric emmean   SE   df lower.CL upper.CL
    ##  Control   AS       1189  185 22.5    806.9     1571
    ##  MHW       AS       3040 1102 22.5    757.5     5322
    ##  Control   MMR      1606  185 22.5   1224.0     1988
    ##  MHW       MMR      3228 1102 22.5    946.1     5511
    ##  Control   SMR       417  185 22.5     34.8      799
    ##  MHW       SMR       189 1102 22.5  -2093.7     2471
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
    ##  treatment emmean   SE   df lower.CL upper.CL
    ##  Control     1189  185 22.5    806.9     1571
    ##  MHW         3040 1102 22.5    757.5     5322
    ## 
    ## metric = MMR:
    ##  treatment emmean   SE   df lower.CL upper.CL
    ##  Control     1606  185 22.5   1224.0     1988
    ##  MHW         3228 1102 22.5    946.1     5511
    ## 
    ## metric = SMR:
    ##  treatment emmean   SE   df lower.CL upper.CL
    ##  Control      417  185 22.5     34.8      799
    ##  MHW          189 1102 22.5  -2093.7     2471
    ## 
    ## Degrees-of-freedom method: kenward-roger 
    ## Confidence level used: 0.95 
    ## 
    ## $contrasts
    ## metric = AS:
    ##  contrast      estimate   SE   df t.ratio p.value
    ##  Control - MHW    -1851 1117 22.5  -1.657  0.1115
    ## 
    ## metric = MMR:
    ##  contrast      estimate   SE   df t.ratio p.value
    ##  Control - MHW    -1622 1117 22.5  -1.452  0.1603
    ## 
    ## metric = SMR:
    ##  contrast      estimate   SE   df t.ratio p.value
    ##  Control - MHW      228 1117 22.5   0.204  0.8398
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

    ## # A tibble: 6 × 3
    ## # Groups:   treatment [2]
    ##   treatment metric  mean
    ##   <fct>     <fct>  <dbl>
    ## 1 Control   AS     1354.
    ## 2 Control   MMR    1765.
    ## 3 Control   SMR     411.
    ## 4 MHW       AS     1973.
    ## 5 MHW       MMR    2302.
    ## 6 MHW       SMR     328.

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
    ##  Control   AS       1354 148 26.9     1051     1657
    ##  MHW       AS       1973 221 26.9     1519     2427
    ##  Control   MMR      1765 148 26.9     1462     2068
    ##  MHW       MMR      2302 221 26.9     1848     2756
    ##  Control   SMR       411 148 26.9      108      714
    ##  MHW       SMR       328 221 26.9     -126      783
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
    ##  Control     1354 148 26.9     1051     1657
    ##  MHW         1973 221 26.9     1519     2427
    ## 
    ## metric = MMR:
    ##  treatment emmean  SE   df lower.CL upper.CL
    ##  Control     1765 148 26.9     1462     2068
    ##  MHW         2302 221 26.9     1848     2756
    ## 
    ## metric = SMR:
    ##  treatment emmean  SE   df lower.CL upper.CL
    ##  Control      411 148 26.9      108      714
    ##  MHW          328 221 26.9     -126      783
    ## 
    ## Degrees-of-freedom method: kenward-roger 
    ## Confidence level used: 0.95 
    ## 
    ## $contrasts
    ## metric = AS:
    ##  contrast      estimate  SE   df t.ratio p.value
    ##  Control - MHW   -619.3 266 26.9  -2.329  0.0276
    ## 
    ## metric = MMR:
    ##  contrast      estimate  SE   df t.ratio p.value
    ##  Control - MHW   -536.6 266 26.9  -2.018  0.0537
    ## 
    ## metric = SMR:
    ##  contrast      estimate  SE   df t.ratio p.value
    ##  Control - MHW     82.7 266 26.9   0.311  0.7582
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
