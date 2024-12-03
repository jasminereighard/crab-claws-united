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
    ## 1 11/19/24   Control  TC1_C1    11.70 2.557035 25.5       good          good
    ## 2 11/19/24   Control  TC2_C1    15.02 3.282621 25.5       good          good
    ## 3 11/19/24   Control  TC3_C1    15.35 3.354742 25.5    missing        before
    ## 4 11/19/24   Control  TC4_C1    11.64 2.543922 25.5    missing        during
    ## 5 11/21/24   Control  TC1_C2     9.62 2.102451 25.5       good          good
    ## 6 11/21/24   Control  TC2_C2     9.40 2.054370 25.5    missing        during
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
    ## 1  11/19/24   Control  TC1_C1    11.70 2.557035 25.5       good          good
    ## 2  11/19/24   Control  TC2_C1    15.02 3.282621 25.5       good          good
    ## 3  11/19/24   Control  TC3_C1    15.35 3.354742 25.5    missing        before
    ## 4  11/19/24   Control  TC4_C1    11.64 2.543922 25.5    missing        during
    ## 5  11/21/24   Control  TC1_C2     9.62 2.102451 25.5       good          good
    ## 6  11/21/24   Control  TC2_C2     9.40 2.054370 25.5    missing        during
    ## 7  11/21/24   Control  TC3_C2     9.34 2.041257 25.5    missing        before
    ## 8  11/21/24   Control  TC4_C2     9.73 2.126492 25.5    missing        before
    ## 9  11/26/24       MHW  TC1_T2    16.34 3.571107 28.5       good          good
    ## 10 11/26/24       MHW  TC2_T2    17.06 3.728463 28.5    missing        before
    ## 11 11/26/24       MHW  TC3_T2    17.39 3.800584 28.5    missing        before
    ## 12 11/26/24       MHW  TC4_T2    18.52 4.047546 28.5       good          good
    ## 13 11/17/24   Control  TC1_C3     6.00 1.311300 26.0       good          good
    ## 14 11/17/24   Control  TC2_C3    13.87 3.031289 26.0    missing        during
    ## 15 11/17/24   Control  TC3_C3     9.82 2.146161 26.0       good          good
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
    ## 1  11/19/24   Control  TC1_C1    11.70 2.557035 25.5       good          good
    ## 2  11/19/24   Control  TC2_C1    15.02 3.282621 25.5       good          good
    ## 3  11/19/24   Control  TC3_C1    15.35 3.354742 25.5    missing        before
    ## 4  11/19/24   Control  TC4_C1    11.64 2.543922 25.5    missing        during
    ## 5  11/21/24   Control  TC1_C2     9.62 2.102451 25.5       good          good
    ## 6  11/21/24   Control  TC2_C2     9.40 2.054370 25.5    missing        during
    ## 7  11/21/24   Control  TC3_C2     9.34 2.041257 25.5    missing        before
    ## 8  11/21/24   Control  TC4_C2     9.73 2.126492 25.5    missing        before
    ## 9  11/26/24       MHW  TC1_T2    16.34 3.571107 28.5       good          good
    ## 10 11/26/24       MHW  TC2_T2    17.06 3.728463 28.5    missing        before
    ## 11 11/26/24       MHW  TC3_T2    17.39 3.800584 28.5    missing        before
    ## 12 11/26/24       MHW  TC4_T2    18.52 4.047546 28.5       good          good
    ## 13 11/17/24   Control  TC1_C3     6.00 1.311300 26.0       good          good
    ## 14 11/17/24   Control  TC2_C3    13.87 3.031289 26.0    missing        during
    ## 15 11/17/24   Control  TC3_C3     9.82 2.146161 26.0       good          good
    ##    limb_type      MMR       SMR       AS     Pcrit
    ## 1       none 316.7053  48.64913 268.0561  6.480427
    ## 2       none 348.6152  59.28690 289.3283 14.191032
    ## 3       both 332.9836  85.70562 247.2780 15.024477
    ## 4       claw 427.1228  49.98625 377.1365  8.846849
    ## 5       none 440.7338 163.97920 276.7546 21.277992
    ## 6       claw 775.8250  60.17435 715.6507  7.919439
    ## 7        leg 410.7455  67.54003 343.2055 14.821830
    ## 8       claw 406.4257  98.40724 308.0184 15.492286
    ## 9       none 440.7338  60.83094 379.9028 11.241514
    ## 10       leg 775.8250  75.62005 700.2050 12.425405
    ## 11       leg 410.7455  75.37746 335.3681 11.271264
    ## 12      none 406.4257  80.85463 325.5711 31.707309
    ## 13      none 524.7147  83.62180 441.0929 12.377826
    ## 14      claw 314.6415 187.46516 127.1763 32.793832
    ## 15      none 509.9121  52.43102 457.4810  9.746129

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
    ##  $ arm_status   : chr [1:60] "good" "good" "good" "good" ...
    ##  $ when_arm_lost: chr [1:60] "good" "good" "good" "good" ...
    ##  $ limb_type    : chr [1:60] "none" "none" "none" "none" ...
    ##  $ metric       : chr [1:60] "SMR" "MMR" "AS" "Pcrit" ...
    ##  $ value        : num [1:60] 48.65 316.71 268.06 6.48 59.29 ...

``` r
longrespdat$treatment <- as.factor(longrespdat$treatment)
longrespdat$id <- as.factor(longrespdat$fish_id)
longrespdat$when_arm_lost <- as.factor(longrespdat$when_arm_lost)
longrespdat$limb_type <- as.factor(longrespdat$limb_type)
longrespdat$metric <- as.factor(longrespdat$metric)
longrespdat$mass <- longrespdat$weight_g #NOT USING NEW MASS, USE ACTUAL WEIGHT
longrespdat$arm_status <- as.factor(longrespdat$arm_status)

longrespdat$mass
```

    ##  [1] 11.70 11.70 11.70 11.70 15.02 15.02 15.02 15.02 15.35 15.35 15.35 15.35
    ## [13] 11.64 11.64 11.64 11.64  9.62  9.62  9.62  9.62  9.40  9.40  9.40  9.40
    ## [25]  9.34  9.34  9.34  9.34  9.73  9.73  9.73  9.73 16.34 16.34 16.34 16.34
    ## [37] 17.06 17.06 17.06 17.06 17.39 17.39 17.39 17.39 18.52 18.52 18.52 18.52
    ## [49]  6.00  6.00  6.00  6.00 13.87 13.87 13.87 13.87  9.82  9.82  9.82  9.82

``` r
str(longrespdat)
```

    ## tibble [60 × 13] (S3: tbl_df/tbl/data.frame)
    ##  $ date         : chr [1:60] "11/19/24" "11/19/24" "11/19/24" "11/19/24" ...
    ##  $ treatment    : Factor w/ 2 levels "Control","MHW": 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ fish_id      : chr [1:60] "TC1_C1" "TC1_C1" "TC1_C1" "TC1_C1" ...
    ##  $ weight_g     : num [1:60] 11.7 11.7 11.7 11.7 15 ...
    ##  $ new_mass     : num [1:60] 2.56 2.56 2.56 2.56 3.28 ...
    ##  $ temp         : num [1:60] 25.5 25.5 25.5 25.5 25.5 25.5 25.5 25.5 25.5 25.5 ...
    ##  $ arm_status   : Factor w/ 2 levels "good","missing": 1 1 1 1 1 1 1 1 2 2 ...
    ##  $ when_arm_lost: Factor w/ 3 levels "before","during",..: 3 3 3 3 3 3 3 3 1 1 ...
    ##  $ limb_type    : Factor w/ 4 levels "both","claw",..: 4 4 4 4 4 4 4 4 1 1 ...
    ##  $ metric       : Factor w/ 4 levels "AS","MMR","Pcrit",..: 4 2 1 3 4 2 1 3 4 2 ...
    ##  $ value        : num [1:60] 48.65 316.71 268.06 6.48 59.29 ...
    ##  $ id           : Factor w/ 15 levels "TC1_C1","TC1_C2",..: 1 1 1 1 5 5 5 5 9 9 ...
    ##  $ mass         : num [1:60] 11.7 11.7 11.7 11.7 15 ...

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

#this model includes the treatmeant and mass and interaction 
values_mod <- lmer(value ~ metric + metric:treatment + metric:mass + metric:treatment:mass + (1|id), data = longrespdat)

summary(values_mod) #dont bother trying to decode this just look at the next line of code which is the anova
```

    ## Linear mixed model fit by REML. t-tests use Satterthwaite's method [
    ## lmerModLmerTest]
    ## Formula: 
    ## value ~ metric + metric:treatment + metric:mass + metric:treatment:mass +  
    ##     (1 | id)
    ##    Data: longrespdat
    ## 
    ## REML criterion at convergence: 453.4
    ## 
    ## Scaled residuals: 
    ##      Min       1Q   Median       3Q      Max 
    ## -1.60978 -0.37859 -0.04491  0.26671  2.49719 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  id       (Intercept) 1471     38.36   
    ##  Residual             5742     75.78   
    ## Number of obs: 52, groups:  id, 13
    ## 
    ## Fixed effects:
    ##                                Estimate Std. Error        df t value Pr(>|t|)
    ## (Intercept)                    546.9813   149.8318   32.0051   3.651 0.000924
    ## metricMMR                       77.8695   189.0523   27.0000   0.412 0.683669
    ## metricPcrit                   -539.8048   189.0523   27.0000  -2.855 0.008166
    ## metricSMR                     -469.1118   189.0523   27.0000  -2.481 0.019608
    ## metricAS:treatmentMHW          962.2284   948.7440   32.0051   1.014 0.318087
    ## metricMMR:treatmentMHW         811.5523   948.7440   32.0051   0.855 0.398690
    ## metricPcrit:treatmentMHW      -159.3324   948.7440   32.0051  -0.168 0.867687
    ## metricSMR:treatmentMHW        -150.6761   948.7440   32.0051  -0.159 0.874811
    ## metricAS:mass                  -21.0048    12.4818   32.0051  -1.683 0.102140
    ## metricMMR:mass                 -19.9432    12.4818   32.0051  -1.598 0.119920
    ## metricPcrit:mass                 0.6983    12.4818   32.0051   0.056 0.955731
    ## metricSMR:mass                   1.0616    12.4818   32.0051   0.085 0.932750
    ## metricAS:treatmentMHW:mass     -40.9746    55.4345   32.0051  -0.739 0.465200
    ## metricMMR:treatmentMHW:mass    -33.6116    55.4345   32.0051  -0.606 0.548572
    ## metricPcrit:treatmentMHW:mass    9.0444    55.4345   32.0051   0.163 0.871422
    ## metricSMR:treatmentMHW:mass      7.3630    55.4345   32.0051   0.133 0.895164
    ##                                  
    ## (Intercept)                   ***
    ## metricMMR                        
    ## metricPcrit                   ** 
    ## metricSMR                     *  
    ## metricAS:treatmentMHW            
    ## metricMMR:treatmentMHW           
    ## metricPcrit:treatmentMHW         
    ## metricSMR:treatmentMHW           
    ## metricAS:mass                    
    ## metricMMR:mass                   
    ## metricPcrit:mass                 
    ## metricSMR:mass                   
    ## metricAS:treatmentMHW:mass       
    ## metricMMR:treatmentMHW:mass      
    ## metricPcrit:treatmentMHW:mass    
    ## metricSMR:treatmentMHW:mass      
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
    ## metric                265.1332  3  < 2.2e-16 ***
    ## metric:treatment       17.4017  4   0.001615 ** 
    ## metric:mass             6.6262  4   0.157010    
    ## metric:treatment:mass   1.0287  4   0.905414    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
#anova of random effects
#there is an effect of treatment 
#no effect of mass within or across treatment groups


#USING THIS MODEL !!!! 
#Model 2 just treatment as a random effect! no mass 
values_mod2 <- lmer(value ~ metric + metric:treatment + (1|id), data = longrespdat)

summary(values_mod2)
```

    ## Linear mixed model fit by REML. t-tests use Satterthwaite's method [
    ## lmerModLmerTest]
    ## Formula: value ~ metric + metric:treatment + (1 | id)
    ##    Data: longrespdat
    ## 
    ## REML criterion at convergence: 527.5
    ## 
    ## Scaled residuals: 
    ##      Min       1Q   Median       3Q      Max 
    ## -2.05941 -0.51303 -0.02349  0.37336  2.64551 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  id       (Intercept) 1496     38.68   
    ##  Residual             5676     75.34   
    ## Number of obs: 52, groups:  id, 13
    ## 
    ## Fixed effects:
    ##                          Estimate Std. Error       df t value Pr(>|t|)    
    ## (Intercept)               299.382     28.230   38.918  10.605 4.85e-13 ***
    ## metricMMR                  90.383     35.516   33.000   2.545   0.0158 *  
    ## metricPcrit              -283.973     35.516   33.000  -7.996 3.18e-09 ***
    ## metricSMR                -208.998     35.516   33.000  -5.885 1.35e-06 ***
    ## metricAS:treatmentMHW     135.880     50.893   38.918   2.670   0.0110 *  
    ## metricMMR:treatmentMHW    118.667     50.893   38.918   2.332   0.0250 *  
    ## metricPcrit:treatmentMHW    1.253     50.893   38.918   0.025   0.9805    
    ## metricSMR:treatmentMHW    -17.213     50.893   38.918  -0.338   0.7370    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) mtrMMR mtrcPc mtrSMR mAS:MH mMMR:M mP:MHW
    ## metricMMR   -0.629                                          
    ## metricPcrit -0.629  0.500                                   
    ## metricSMR   -0.629  0.500  0.500                            
    ## mtrcAS:tMHW -0.555  0.349  0.349  0.349                     
    ## mtrcMMR:MHW -0.116 -0.349  0.000  0.000  0.209              
    ## mtrcPcr:MHW -0.116  0.000 -0.349  0.000  0.209  0.209       
    ## mtrcSMR:MHW -0.116  0.000  0.000 -0.349  0.209  0.209  0.209

``` r
Anova(values_mod2)
```

    ## Analysis of Deviance Table (Type II Wald chisquare tests)
    ## 
    ## Response: value
    ##                   Chisq Df Pr(>Chisq)    
    ## metric           268.20  3    < 2e-16 ***
    ## metric:treatment  12.46  4    0.01424 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
#slopes for model 1 to look at mass 
mass_slopes_mod <- emtrends(values_mod, ~metric:treatment, "mass")
```

    ## NOTE: A nesting structure was detected in the fitted model:
    ##     treatment %in% metric

``` r
summary(mass_slopes_mod, infer = TRUE)
```

    ##  treatment metric mass.trend   SE df lower.CL upper.CL t.ratio p.value
    ##  Control   AS        -21.005 12.5 32    -46.4     4.42  -1.683  0.1021
    ##  MHW       AS        -61.979 54.0 32   -172.0    48.04  -1.148  0.2597
    ##  Control   MMR       -19.943 12.5 32    -45.4     5.48  -1.598  0.1199
    ##  MHW       MMR       -53.555 54.0 32   -163.6    56.46  -0.992  0.3289
    ##  Control   Pcrit       0.698 12.5 32    -24.7    26.12   0.056  0.9557
    ##  MHW       Pcrit       9.743 54.0 32   -100.3   119.76   0.180  0.8580
    ##  Control   SMR         1.062 12.5 32    -24.4    26.49   0.085  0.9327
    ##  MHW       SMR         8.425 54.0 32   -101.6   118.44   0.156  0.8770
    ## 
    ## Degrees-of-freedom method: kenward-roger 
    ## Confidence level used: 0.95

``` r
#mass predicts nothing
```

``` r
#descriptive stats
#--------Model 1-------------Treatment and mass, though mass did not have an effect 
longrespdat %>%
  group_by(treatment, metric) %>%
  summarise(mean=mean(value))
```

    ## `summarise()` has grouped output by 'treatment'. You can override using the
    ## `.groups` argument.

    ## # A tibble: 8 × 3
    ## # Groups:   treatment [2]
    ##   treatment metric  mean
    ##   <fct>     <fct>  <dbl>
    ## 1 Control   AS     299. 
    ## 2 Control   MMR    390. 
    ## 3 Control   Pcrit   15.4
    ## 4 Control   SMR     90.4
    ## 5 MHW       AS     435. 
    ## 6 MHW       MMR    508. 
    ## 7 MHW       Pcrit   16.7
    ## 8 MHW       SMR     73.2

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

    ##  treatment metric emmean    SE df lower.CL upper.CL
    ##  Control   AS      263.6  35.4 32    191.4    335.7
    ##  MHW       AS      673.0 211.5 32    242.3   1103.7
    ##  Control   MMR     355.8  35.4 32    283.6    427.9
    ##  MHW       MMR     713.8 211.5 32    283.1   1144.5
    ##  Control   Pcrit    16.6  35.4 32    -55.5     88.7
    ##  MHW       Pcrit   -20.7 211.5 32   -451.4    410.0
    ##  Control   SMR      92.2  35.4 32     20.1    164.3
    ##  MHW       SMR      40.9 211.5 32   -389.8    471.6
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
    ##  treatment emmean    SE df lower.CL upper.CL
    ##  Control    263.6  35.4 32    191.4    335.7
    ##  MHW        673.0 211.5 32    242.3   1103.7
    ## 
    ## metric = MMR:
    ##  treatment emmean    SE df lower.CL upper.CL
    ##  Control    355.8  35.4 32    283.6    427.9
    ##  MHW        713.8 211.5 32    283.1   1144.5
    ## 
    ## metric = Pcrit:
    ##  treatment emmean    SE df lower.CL upper.CL
    ##  Control     16.6  35.4 32    -55.5     88.7
    ##  MHW        -20.7 211.5 32   -451.4    410.0
    ## 
    ## metric = SMR:
    ##  treatment emmean    SE df lower.CL upper.CL
    ##  Control     92.2  35.4 32     20.1    164.3
    ##  MHW         40.9 211.5 32   -389.8    471.6
    ## 
    ## Degrees-of-freedom method: kenward-roger 
    ## Confidence level used: 0.95 
    ## 
    ## $contrasts
    ## metric = AS:
    ##  contrast      estimate  SE df t.ratio p.value
    ##  Control - MHW   -409.4 214 32  -1.909  0.0652
    ## 
    ## metric = MMR:
    ##  contrast      estimate  SE df t.ratio p.value
    ##  Control - MHW   -358.1 214 32  -1.670  0.1047
    ## 
    ## metric = Pcrit:
    ##  contrast      estimate  SE df t.ratio p.value
    ##  Control - MHW     37.3 214 32   0.174  0.8630
    ## 
    ## metric = SMR:
    ##  contrast      estimate  SE df t.ratio p.value
    ##  Control - MHW     51.3 214 32   0.239  0.8123
    ## 
    ## Degrees-of-freedom method: kenward-roger

``` r
#MHW only had a significant effect on AS


#--------Model 2-------------#Model 2 just treatment as a factor! no mass 
longrespdat %>%
  group_by(treatment, metric) %>%
  summarise(mean=mean(value))
```

    ## `summarise()` has grouped output by 'treatment'. You can override using the
    ## `.groups` argument.

    ## # A tibble: 8 × 3
    ## # Groups:   treatment [2]
    ##   treatment metric  mean
    ##   <fct>     <fct>  <dbl>
    ## 1 Control   AS     299. 
    ## 2 Control   MMR    390. 
    ## 3 Control   Pcrit   15.4
    ## 4 Control   SMR     90.4
    ## 5 MHW       AS     435. 
    ## 6 MHW       MMR    508. 
    ## 7 MHW       Pcrit   16.7
    ## 8 MHW       SMR     73.2

``` r
#pull out mean values
#USE THIS MODEL 

emmeans_mod2 <- emmeans(values_mod2, specs = c("metric", "treatment"))
```

    ## NOTE: A nesting structure was detected in the fitted model:
    ##     treatment %in% metric

``` r
#model slays
summary(emmeans_mod2)
```

    ##  treatment metric emmean   SE   df lower.CL upper.CL
    ##  Control   AS      299.4 28.2 38.9    242.3    356.5
    ##  MHW       AS      435.3 42.3 38.9    349.6    520.9
    ##  Control   MMR     389.8 28.2 38.9    332.7    446.9
    ##  MHW       MMR     508.4 42.3 38.9    422.8    594.1
    ##  Control   Pcrit    15.4 28.2 38.9    -41.7     72.5
    ##  MHW       Pcrit    16.7 42.3 38.9    -69.0    102.3
    ##  Control   SMR      90.4 28.2 38.9     33.3    147.5
    ##  MHW       SMR      73.2 42.3 38.9    -12.5    158.8
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
    ##  treatment emmean   SE   df lower.CL upper.CL
    ##  Control    299.4 28.2 38.9    242.3    356.5
    ##  MHW        435.3 42.3 38.9    349.6    520.9
    ## 
    ## metric = MMR:
    ##  treatment emmean   SE   df lower.CL upper.CL
    ##  Control    389.8 28.2 38.9    332.7    446.9
    ##  MHW        508.4 42.3 38.9    422.8    594.1
    ## 
    ## metric = Pcrit:
    ##  treatment emmean   SE   df lower.CL upper.CL
    ##  Control     15.4 28.2 38.9    -41.7     72.5
    ##  MHW         16.7 42.3 38.9    -69.0    102.3
    ## 
    ## metric = SMR:
    ##  treatment emmean   SE   df lower.CL upper.CL
    ##  Control     90.4 28.2 38.9     33.3    147.5
    ##  MHW         73.2 42.3 38.9    -12.5    158.8
    ## 
    ## Degrees-of-freedom method: kenward-roger 
    ## Confidence level used: 0.95 
    ## 
    ## $contrasts
    ## metric = AS:
    ##  contrast      estimate   SE   df t.ratio p.value
    ##  Control - MHW  -135.88 50.9 38.9  -2.670  0.0110
    ## 
    ## metric = MMR:
    ##  contrast      estimate   SE   df t.ratio p.value
    ##  Control - MHW  -118.67 50.9 38.9  -2.332  0.0250
    ## 
    ## metric = Pcrit:
    ##  contrast      estimate   SE   df t.ratio p.value
    ##  Control - MHW    -1.25 50.9 38.9  -0.025  0.9805
    ## 
    ## metric = SMR:
    ##  contrast      estimate   SE   df t.ratio p.value
    ##  Control - MHW    17.21 50.9 38.9   0.338  0.7370
    ## 
    ## Degrees-of-freedom method: kenward-roger

``` r
#When you take out mass, (bc its insignificant), AS and MMR significantly goes down <3 
```

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
