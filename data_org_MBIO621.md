data_org_MBIO621
================
Jasmine Reighard
2024-11-03

``` r
knitr::opts_chunk$set(echo = TRUE)


#Step 1: Set working directory 
#> Click "Session" tab at the very top of your screen
#> Set working directory
#> Choose directory...
#> mbio621 folder
#> Open

#Load all necessary libraries
#if you dont have any of them, go to packages tab, then click install

## Libraries
require(ggplot2)
```

    ## Loading required package: ggplot2

``` r
require(dplyr)
```

    ## Loading required package: dplyr

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
require(performance)
```

    ## Loading required package: performance

``` r
require(mclust) #SMR
```

    ## Loading required package: mclust

    ## Package 'mclust' version 6.0.0
    ## Type 'citation("mclust")' for citing this R package in publications.

``` r
require(tidyr)
```

    ## Loading required package: tidyr

``` r
#Loading calculate SMR function from (Chabot et al 2016)
#y is column for mo2
#single file for each fish, one summary file for publication
#set G to = 2, double norm

calcSMR = function(Y, q=c(0.1,0.15,0.2,0.25,0.3), G=1:4){
    u = sort(Y)
    the.Mclust <- Mclust(Y,  G=G)
    cl <- the.Mclust$classification
    # sometimes, the class containing SMR is not called 1
    # the following presumes that when class 1 contains > 10% of cases, 
    # it contains SMR, otherwise we take class 2
    cl2 <- as.data.frame(table(cl))
    cl2$cl <- as.numeric(levels(cl2$cl))
    valid <- cl2$Freq>=0.1*length(time)  
    the.cl <- min(cl2$cl[valid])
    left.distr <- Y[the.Mclust$classification==the.cl]
    mlnd = the.Mclust$parameters$mean[the.cl]
    CVmlnd = sd(left.distr)/mlnd * 100
    quant=quantile(Y, q)
    low10=mean(u[1:10])
    low10pc = mean(u[6:(5 + round(0.1*(length(u)-5)))])
    # remove 5 outliers, keep lowest 10% of the rest, average
    # Herrmann & Enders 2000
    return(list(mlnd=mlnd, quant=quant, low10=low10, low10pc=low10pc,
              cl=cl, CVmlnd=CVmlnd))}
```

``` r
#Import csv files and create dataframes

#e.g.

#Control group smr .csv, sometimes you have to write the entire file path to get it to load
#CHANGE TO YOUR OWN FILE PATH :)

resp_SMR_C1 <- read.csv("/Users/jasminereighard/Desktop/mbio621/data/resp_SMR_C1.csv", stringsAsFactors = FALSE)
resp_SMR_C2 <- read.csv("/Users/jasminereighard/Desktop/mbio621/data/resp_SMR_C2.csv", stringsAsFactors = FALSE)
resp_SMR_C3 <- read.csv("/Users/jasminereighard/Desktop/mbio621/data/resp_SMR_C3.csv", stringsAsFactors = FALSE)

#Load MMR
```

``` r
#Convert date to date-time format: Control

resp_SMR_C1$Clock.TIME <- as.POSIXct(resp_SMR_C1$Clock.TIME, format = "%m/%d/%y %H:%M", tz = "GMT")
resp_SMR_C2$Clock.TIME <- as.POSIXct(resp_SMR_C2$Clock.TIME, format = "%m/%d/%y %H:%M", tz = "GMT")
resp_SMR_C3$Clock.TIME <- as.POSIXct(resp_SMR_C3$Clock.TIME, format = "%m/%d/%y %H:%M", tz = "GMT")

#str(resp_SMR_C1)
```

``` r
#create individual DFs based on unique fish_id values and filter for R.2 values above 0.95.

# Define the function to process a dataset
process_dataset <- function(dataset) {
  # Get unique fish IDs
  id <- unique(dataset$fish_id)
  
  # Loop through each unique fish_id and create individual filtered dataframes
  for (i in 1:length(id)) {
    assign(id[i], subset(dataset, fish_id == id[i] & R.2 >= 0.95), envir = .GlobalEnv)
  }
}

#Write out all datasets you want this applied to 
# Apply the function to each dataset

#smr_datasets <- list(resp_SMR_t1)
#lapply(smr_datasets, process_dataset)


#How you would write it with multiple datasets

smr_datasets <- list(resp_SMR_C1, resp_SMR_C2, resp_SMR_C3)
lapply(smr_datasets, process_dataset)
```

    ## [[1]]
    ## NULL
    ## 
    ## [[2]]
    ## NULL
    ## 
    ## [[3]]
    ## NULL

``` r
#Load MMR files
#MMR c1
MMR_TC1_C1 <- read.csv("/Users/jasminereighard/Desktop/mbio621/data/MMR_TC1_C1.csv", stringsAsFactors = FALSE)
MMR_TC2_C1 <- read.csv("/Users/jasminereighard/Desktop/mbio621/data/MMR_TC2_C1.csv", stringsAsFactors = FALSE)
```

    ## Warning in read.table(file = file, header = header, sep = sep, quote = quote, :
    ## incomplete final line found by readTableHeader on
    ## '/Users/jasminereighard/Desktop/mbio621/data/MMR_TC2_C1.csv'

``` r
MMR_TC3_C1 <- read.csv("/Users/jasminereighard/Desktop/mbio621/data/MMR_TC3_C1.csv", stringsAsFactors = FALSE)
```

    ## Warning in read.table(file = file, header = header, sep = sep, quote = quote, :
    ## incomplete final line found by readTableHeader on
    ## '/Users/jasminereighard/Desktop/mbio621/data/MMR_TC3_C1.csv'

``` r
MMR_TC4_C1 <- read.csv("/Users/jasminereighard/Desktop/mbio621/data/MMR_TC4_C1.csv", stringsAsFactors = FALSE)
```

    ## Warning in read.table(file = file, header = header, sep = sep, quote = quote, :
    ## incomplete final line found by readTableHeader on
    ## '/Users/jasminereighard/Desktop/mbio621/data/MMR_TC4_C1.csv'

``` r
#MMR C2
MMR_TC1_C2 <- read.csv("/Users/jasminereighard/Desktop/mbio621/data/MMR_TC1_C2.csv", stringsAsFactors = FALSE)
```

    ## Warning in read.table(file = file, header = header, sep = sep, quote = quote, :
    ## incomplete final line found by readTableHeader on
    ## '/Users/jasminereighard/Desktop/mbio621/data/MMR_TC1_C2.csv'

``` r
MMR_TC2_C2 <- read.csv("/Users/jasminereighard/Desktop/mbio621/data/MMR_TC2_C2.csv", stringsAsFactors = FALSE)
```

    ## Warning in read.table(file = file, header = header, sep = sep, quote = quote, :
    ## incomplete final line found by readTableHeader on
    ## '/Users/jasminereighard/Desktop/mbio621/data/MMR_TC2_C2.csv'

``` r
MMR_TC3_C2 <- read.csv("/Users/jasminereighard/Desktop/mbio621/data/MMR_TC3_C2.csv", stringsAsFactors = FALSE)
```

    ## Warning in read.table(file = file, header = header, sep = sep, quote = quote, :
    ## incomplete final line found by readTableHeader on
    ## '/Users/jasminereighard/Desktop/mbio621/data/MMR_TC3_C2.csv'

``` r
MMR_TC4_C2 <- read.csv("/Users/jasminereighard/Desktop/mbio621/data/MMR_TC4_C2.csv", stringsAsFactors = FALSE)
```

    ## Warning in read.table(file = file, header = header, sep = sep, quote = quote, :
    ## incomplete final line found by readTableHeader on
    ## '/Users/jasminereighard/Desktop/mbio621/data/MMR_TC4_C2.csv'

``` r
#MMR C3
MMR_TC1_C3 <- read.csv("/Users/jasminereighard/Desktop/mbio621/data/MMR_TC1_C3.csv", stringsAsFactors = FALSE)
MMR_TC2_C3 <- read.csv("/Users/jasminereighard/Desktop/mbio621/data/MMR_TC2_C3.csv", stringsAsFactors = FALSE)
```

    ## Warning in read.table(file = file, header = header, sep = sep, quote = quote, :
    ## incomplete final line found by readTableHeader on
    ## '/Users/jasminereighard/Desktop/mbio621/data/MMR_TC2_C3.csv'

``` r
MMR_TC3_C3 <- read.csv("/Users/jasminereighard/Desktop/mbio621/data/MMR_TC3_C3.csv", stringsAsFactors = FALSE)
```

    ## Warning in read.table(file = file, header = header, sep = sep, quote = quote, :
    ## incomplete final line found by readTableHeader on
    ## '/Users/jasminereighard/Desktop/mbio621/data/MMR_TC3_C3.csv'

``` r
#convert date to date-time format for MMR dfs
#trial 1 pre
MMR_TC1_C1$Clock.TIME <- as.POSIXct(MMR_TC1_C1$Clock.TIME, format = "%m/%d/%y %H:%M", tz = "GMT")
MMR_TC2_C1$Clock.TIME <- as.POSIXct(MMR_TC2_C1$Clock.TIME, format = "%m/%d/%y %H:%M", tz = "GMT")
MMR_TC3_C1$Clock.TIME <- as.POSIXct(MMR_TC3_C1$Clock.TIME, format = "%m/%d/%y %H:%M", tz = "GMT")
MMR_TC4_C1$Clock.TIME <- as.POSIXct(MMR_TC4_C1$Clock.TIME, format = "%m/%d/%y %H:%M", tz = "GMT")

#trial 2 pre
MMR_TC1_C2$Clock.TIME <- as.POSIXct(MMR_TC1_C2$Clock.TIME, format = "%m/%d/%y %H:%M", tz = "GMT")
MMR_TC2_C2$Clock.TIME <- as.POSIXct(MMR_TC2_C2$Clock.TIME, format = "%m/%d/%y %H:%M", tz = "GMT")
MMR_TC3_C2$Clock.TIME <- as.POSIXct(MMR_TC3_C2$Clock.TIME, format = "%m/%d/%y %H:%M", tz = "GMT")
MMR_TC4_C2$Clock.TIME <- as.POSIXct(MMR_TC4_C2$Clock.TIME, format = "%m/%d/%y %H:%M", tz = "GMT")

#trail 3 (pilot)
MMR_TC1_C3$Clock.TIME <- as.POSIXct(MMR_TC1_C3$Clock.TIME, format = "%m/%d/%y %H:%M", tz = "GMT")
MMR_TC2_C3$Clock.TIME <- as.POSIXct(MMR_TC2_C3$Clock.TIME, format = "%m/%d/%y %H:%M", tz = "GMT")
MMR_TC3_C3$Clock.TIME <- as.POSIXct(MMR_TC3_C3$Clock.TIME, format = "%m/%d/%y %H:%M", tz = "GMT")
```

``` r
head(MMR_TC1_C1)
```

    ##            Clock.TIME TIME.HOURS  TIME.UNIX      MO2       SLOPE Intercept
    ## 1 2024-11-19 13:31:00  0.0750000 1732059136 317.3727 -0.09714177  96.38982
    ## 2 2024-11-19 13:34:00  0.1291667 1732059331 240.9030 -0.07373584  97.06570
    ## 3 2024-11-19 13:38:00  0.1833333 1732059526 187.5243 -0.05739763  97.86621
    ## 4 2024-11-19 13:41:00  0.2375000 1732059721 164.7585 -0.05042946  98.24875
    ## 5 2024-11-19 13:44:00  0.2916667 1732059916 147.0216 -0.04500053  98.55515
    ##    Pearson.R       R.2        P     Std.Err Measurement.duration.seconds
    ## 1 -0.9989543 0.9979097 2.94e-59 0.000678008                           45
    ## 2 -0.9985469 0.9970960 3.45e-56 0.000606842                           45
    ## 3 -0.9979527 0.9959095 5.45e-53 0.000560965                           45
    ## 4 -0.9973317 0.9946706 2.23e-49 0.000569588                           44
    ## 5 -0.9954825 0.9909853 1.31e-45 0.000654523                           45
    ##    avg.po2 median.po2 minimum.po2 max.po2 delta.po2 oxygen.solubility
    ## 1 94.15556     94.234      92.133  96.330     4.197          6.707583
    ## 2 95.36978     95.362      93.753  97.011     3.258          6.707583
    ## 3 96.54607     96.514      95.344  97.810     2.466          6.707583
    ## 4 97.11409     97.146      95.984  98.260     2.276          6.707583
    ## 5 97.52013     97.562      96.571  98.539     1.968          6.707583
    ##   ratio.vreal.fish total.experiment.duration.hours minutes seconds        days
    ## 1         13.52991                       0.0750000    4.50     270 0.003125000
    ## 2         13.52991                       0.1291667    7.75     465 0.005381944
    ## 3         13.52991                       0.1833333   11.00     660 0.007638889
    ## 4         13.52991                       0.2375000   14.25     855 0.009895833
    ## 5         13.52991                       0.2916667   17.50    1050 0.012152778

``` r
#Add fish_id column and take out R.2 less than 95 to MMR dfs

# List of dataframes
df_names <- c("MMR_TC1_C1", "MMR_TC2_C1", "MMR_TC3_C1", "MMR_TC4_C1",
              "MMR_TC1_C2", "MMR_TC2_C2", "MMR_TC3_C2", "MMR_TC4_C2",
              "MMR_TC1_C3", "MMR_TC2_C3", "MMR_TC3_C3")

# Loop through the dataframes
for (df_name in df_names) {
  # Extract the identifier by removing "MMR_" from the dataframe name
  fish_id <- sub("MMR_", "", df_name)
  
  # Dynamically add the fish_id column and filter out rows with R.2 >= 0.95
  filtered_df <- get(df_name) %>%
    filter(R.2 >= 0.95) %>%  # Exclude rows with R.2 < 0.95
    mutate(fish_id = fish_id)  # Add the fish_id column
  
  # Assign the modified dataframe back to the original name
  assign(df_name, filtered_df)
}

head(MMR_TC1_C1)
```

    ##            Clock.TIME TIME.HOURS  TIME.UNIX      MO2       SLOPE Intercept
    ## 1 2024-11-19 13:31:00  0.0750000 1732059136 317.3727 -0.09714177  96.38982
    ## 2 2024-11-19 13:34:00  0.1291667 1732059331 240.9030 -0.07373584  97.06570
    ## 3 2024-11-19 13:38:00  0.1833333 1732059526 187.5243 -0.05739763  97.86621
    ## 4 2024-11-19 13:41:00  0.2375000 1732059721 164.7585 -0.05042946  98.24875
    ## 5 2024-11-19 13:44:00  0.2916667 1732059916 147.0216 -0.04500053  98.55515
    ##    Pearson.R       R.2        P     Std.Err Measurement.duration.seconds
    ## 1 -0.9989543 0.9979097 2.94e-59 0.000678008                           45
    ## 2 -0.9985469 0.9970960 3.45e-56 0.000606842                           45
    ## 3 -0.9979527 0.9959095 5.45e-53 0.000560965                           45
    ## 4 -0.9973317 0.9946706 2.23e-49 0.000569588                           44
    ## 5 -0.9954825 0.9909853 1.31e-45 0.000654523                           45
    ##    avg.po2 median.po2 minimum.po2 max.po2 delta.po2 oxygen.solubility
    ## 1 94.15556     94.234      92.133  96.330     4.197          6.707583
    ## 2 95.36978     95.362      93.753  97.011     3.258          6.707583
    ## 3 96.54607     96.514      95.344  97.810     2.466          6.707583
    ## 4 97.11409     97.146      95.984  98.260     2.276          6.707583
    ## 5 97.52013     97.562      96.571  98.539     1.968          6.707583
    ##   ratio.vreal.fish total.experiment.duration.hours minutes seconds        days
    ## 1         13.52991                       0.0750000    4.50     270 0.003125000
    ## 2         13.52991                       0.1291667    7.75     465 0.005381944
    ## 3         13.52991                       0.1833333   11.00     660 0.007638889
    ## 4         13.52991                       0.2375000   14.25     855 0.009895833
    ## 5         13.52991                       0.2916667   17.50    1050 0.012152778
    ##   fish_id
    ## 1  TC1_C1
    ## 2  TC1_C1
    ## 3  TC1_C1
    ## 4  TC1_C1
    ## 5  TC1_C1

``` r
#LOAD PCRIT DFS, THIS WILL REMOVE R2 >95, ADD FISH_ID COLUMN, AND REMOVES ALL DATA WHEN MO2 HITS THE NEGATIVE (WHEN WE OPENED THE CHAMBER) 

# Base path for raw data files
base_path <- "/Users/jasminereighard/Desktop/mbio621/raw_data"

# Define control groups and experiment numbers
control_groups <- c("C1", "C2", "C3")
experiment_numbers <- 1:4

# Create a list to store the data
pcrit_data <- list()

# Loop through control groups and experiment numbers
for (control in control_groups) {
  # Get the rawdata directory for the control group
  rawdata_dir <- file.path(base_path, paste0(control, "_rawdata"))
  
  # Get the subdirectories in the rawdata directory
  experiment_dirs <- list.dirs(rawdata_dir, recursive = FALSE, full.names = TRUE)
  
  # Find the directory that starts with "Experiment_PCRIT_TC_"
  experiment_dir <- grep("Experiment_PCRIT_TC_", experiment_dirs, value = TRUE)
  
  if (length(experiment_dir) == 1) {
    for (exp_num in experiment_numbers) {
      # Construct the file path
      file_path <- file.path(experiment_dir, paste0("Summary data resp ", exp_num, ".txt"))
      
      # Check if the file exists
      if (file.exists(file_path)) {
        # Read the data
        data <- read.table(file = file_path,
                           skip = 15,
                           header = TRUE,
                           fill = TRUE,
                           sep = ";")
        
        # Convert Clock.TIME to POSIXct if the column exists
        if ("Clock.TIME" %in% colnames(data)) {
          data$Clock.TIME <- as.POSIXct(data$Clock.TIME)
        }
        
        # Add the fish_id column
        fish_id <- paste0("TC", exp_num, "_", control)
        data$fish_id <- fish_id
        
        # Filter out data points after the first negative MO2 value
        if ("MO2" %in% colnames(data)) {
          first_negative_index <- which(data$MO2 < 0)[1]
          if (!is.na(first_negative_index)) {
            data <- data[1:(first_negative_index - 1), ]
          }
        }
        
        # Filter out rows where R2 is less than 0.95
        if ("R2" %in% colnames(data)) {
          data <- data[data$R2 >= 0.95, ]
        }
        
        # Assign the data to the list with the desired naming convention
        list_name <- paste0("pcrit_TC", exp_num, "_", control)
        pcrit_data[[list_name]] <- data
      } else {
        message("File not found: ", file_path)
      }
    }
  } else {
    message("Experiment directory not found or multiple matches for: ", rawdata_dir)
  }
}

# Access the data using the list
print(names(pcrit_data))  # List all the data frames loaded
```

    ##  [1] "pcrit_TC1_C1" "pcrit_TC2_C1" "pcrit_TC3_C1" "pcrit_TC4_C1" "pcrit_TC1_C2"
    ##  [6] "pcrit_TC2_C2" "pcrit_TC3_C2" "pcrit_TC4_C2" "pcrit_TC1_C3" "pcrit_TC2_C3"
    ## [11] "pcrit_TC3_C3" "pcrit_TC4_C3"

``` r
# Remove pcrit_TC4_C3 from the list
pcrit_data$pcrit_TC4_C3 <- NULL

# Move the dataframes in the pcrit_data list into the environment
list2env(pcrit_data, envir = .GlobalEnv)
```

    ## <environment: R_GlobalEnv>

``` r
head(pcrit_TC1_C1)
```

    ##            Clock.TIME TIME.HOURS  TIME.UNIX       MO2       SLOPE Intercept
    ## 1 2024-11-20 09:58:13 0.01722222 1732132753 198.19331 -0.06066321  93.86238
    ## 2 2024-11-20 09:59:15 0.03472222 1732132815 163.75782 -0.05012316  90.34090
    ## 3 2024-11-20 10:00:17 0.05166667 1732132877 166.92574 -0.05109280  86.95273
    ## 4 2024-11-20 10:01:20 0.06916667 1732132939  73.52465 -0.02250450  83.92497
    ## 5 2024-11-20 10:02:22 0.08638889 1732133001 164.89086 -0.05046996  82.96280
    ## 6 2024-11-20 10:03:24 0.10361111 1732133063 176.25929 -0.05394962  79.52485
    ##    Pearson.R       R.2            P      Std.Err Measurement.duration.seconds
    ## 1 -0.9971558 0.9943198 7.874095e-67 0.0006020492                           60
    ## 2 -0.9954924 0.9910051 4.565753e-62 0.0006216895                           61
    ## 3 -0.9938343 0.9877066 4.182991e-57 0.0007484600                           60
    ## 4 -0.9919787 0.9840217 8.396721e-54 0.0003765461                           60
    ## 5 -0.9824633 0.9652341 5.238797e-44 0.0012577070                           60
    ## 6 -0.9919342 0.9839334 9.852722e-54 0.0009052191                           60
    ##    avg.po2 median.po2 minimum.po2 max.po2 delta.po2 oxygen.solubility
    ## 1 92.01215    91.9900      90.331  93.699     3.368          6.707583
    ## 2 88.78708    88.8300      87.130  90.218     3.088          6.707583
    ## 3 85.39440    85.2835      84.087  86.992     2.905          6.707583
    ## 4 83.23858    83.2075      82.546  83.974     1.428          6.707583
    ## 5 81.42347    81.6005      79.769  82.545     2.776          6.707583
    ## 6 77.87938    77.7500      76.535  79.556     3.021          6.707583
    ##   ratio.vreal.fish total.experiment.duration.hours  minutes seconds
    ## 1         13.52991                      0.01722222 1.033333      62
    ## 2         13.52991                      0.03472222 2.083333     125
    ## 3         13.52991                      0.05166667 3.100000     186
    ## 4         13.52991                      0.06916667 4.150000     249
    ## 5         13.52991                      0.08638889 5.183333     311
    ## 6         13.52991                      0.10361111 6.216667     373
    ##           days  X fish_id
    ## 1 0.0007175926 NA  TC1_C1
    ## 2 0.0014467593 NA  TC1_C1
    ## 3 0.0021527778 NA  TC1_C1
    ## 4 0.0028819444 NA  TC1_C1
    ## 5 0.0035995370 NA  TC1_C1
    ## 6 0.0043171296 NA  TC1_C1

``` r
# List of pcrit dataframes
pcrit_names <- names(pcrit_data)

# Create a list to store the new pcrit subsets
pcrit_subsets <- list()

# Loop through the pcrit dataframes
for (name in pcrit_names) {
  # Get the dataframe from the list
  df <- pcrit_data[[name]]
  
  # Filter rows where avg.po2 >= 30
  subset_df <- df %>% filter(avg.po2 >= 60)
  
  # Check if any rows remain after filtering
  if (nrow(subset_df) == 0) {
    message(name, " has no measurements with avg.po2 >= 50.")
  }
  
  # Add the subset to the list with a new name
  new_name <- paste0("po50_", name)
  pcrit_subsets[[new_name]] <- subset_df
}

# Access the new subset dataframes
print(names(pcrit_subsets))  # List the names of the new subset dataframes
```

    ##  [1] "po50_pcrit_TC1_C1" "po50_pcrit_TC2_C1" "po50_pcrit_TC3_C1"
    ##  [4] "po50_pcrit_TC4_C1" "po50_pcrit_TC1_C2" "po50_pcrit_TC2_C2"
    ##  [7] "po50_pcrit_TC3_C2" "po50_pcrit_TC4_C2" "po50_pcrit_TC1_C3"
    ## [10] "po50_pcrit_TC2_C3" "po50_pcrit_TC3_C3"

``` r
# Move the dataframes in the pcrit_subsets list into the environment
list2env(pcrit_subsets, envir = .GlobalEnv)
```

    ## <environment: R_GlobalEnv>

``` r
# List of MMR and SMR dataframe names
MMR_names <- c("MMR_TC1_C1", "MMR_TC2_C1", "MMR_TC3_C1", "MMR_TC4_C1",
               "MMR_TC1_C2", "MMR_TC2_C2", "MMR_TC3_C2", "MMR_TC4_C2",
               "MMR_TC1_C3", "MMR_TC2_C3", "MMR_TC3_C3")

SMR_names <- c("TC1_C1", "TC2_C1", "TC3_C1", "TC4_C1",
               "TC1_C2", "TC2_C2", "TC3_C2", "TC4_C2",
               "TC1_C3", "TC2_C3", "TC3_C3")

po50_pcrit_names <- c("po50_pcrit_TC1_C1", "po50_pcrit_TC2_C1", "po50_pcrit_TC3_C1", "po50_pcrit_TC4_C1",
               "po50_pcrit_TC1_C2", "po50_pcrit_TC2_C2", "po50_pcrit_TC3_C2", "po50_pcrit_TC4_C2",
               "po50_pcrit_TC1_C3", "po50_pcrit_TC2_C3", "po50_pcrit_TC3_C3")

# Loop through both lists to perform a left join
for (i in seq_along(MMR_names)) {
  # Get the MMR and SMR dataframes
  MMR_df <- get(MMR_names[i])
  SMR_df <- get(SMR_names[i])
  po50_PCRIT_df <- get(po50_pcrit_names[i])
  
  # Add a column to prioritize the order
  MMR_df <- MMR_df %>% mutate(priority = 1)
  SMR_df <- SMR_df %>% mutate(priority = 2)
  po50_PCRIT_df <- po50_PCRIT_df %>% mutate(priority = 3)
  
  # Combine the dataframes with left join
  combined_df <- bind_rows(MMR_df, SMR_df, po50_PCRIT_df) %>%
    arrange(priority) %>%  # Ensure MMR comes first
    select(-priority)      # Remove the priority column
  
  # Assign the combined dataframe to the SMR name
  assign(SMR_names[i], combined_df)
}
```

``` r
#BACKGROUND
#import pre and post background and create dataframes
#The orange warning message u get is okay!! 

#CHANGE TO YOUR OWN FILE PATH :)
#>> cmd + F, 
#>> paste /Users/jasminereighard/Desktop/mbio621/data/ into "Find"
#>> type your own file path into "Replace" 
#>> highlight the rest of the chunk
#>> Select box next to "In selection"
#>> Press "All" 

#Control 25.5
#preback c1
preback_TC1_C1 <- read.csv("/Users/jasminereighard/Desktop/mbio621/data/preback_TC1_C1.csv", stringsAsFactors = FALSE)
```

    ## Warning in read.table(file = file, header = header, sep = sep, quote = quote, :
    ## incomplete final line found by readTableHeader on
    ## '/Users/jasminereighard/Desktop/mbio621/data/preback_TC1_C1.csv'

``` r
preback_TC2_C1 <- read.csv("/Users/jasminereighard/Desktop/mbio621/data/preback_TC2_C1.csv", stringsAsFactors = FALSE)
```

    ## Warning in read.table(file = file, header = header, sep = sep, quote = quote, :
    ## incomplete final line found by readTableHeader on
    ## '/Users/jasminereighard/Desktop/mbio621/data/preback_TC2_C1.csv'

``` r
preback_TC3_C1 <- read.csv("/Users/jasminereighard/Desktop/mbio621/data/preback_TC3_C1.csv", stringsAsFactors = FALSE)
```

    ## Warning in read.table(file = file, header = header, sep = sep, quote = quote, :
    ## incomplete final line found by readTableHeader on
    ## '/Users/jasminereighard/Desktop/mbio621/data/preback_TC3_C1.csv'

``` r
preback_TC4_C1 <- read.csv("/Users/jasminereighard/Desktop/mbio621/data/preback_TC4_C1.csv", stringsAsFactors = FALSE)
```

    ## Warning in read.table(file = file, header = header, sep = sep, quote = quote, :
    ## incomplete final line found by readTableHeader on
    ## '/Users/jasminereighard/Desktop/mbio621/data/preback_TC4_C1.csv'

``` r
#postback C1

postback_TC1_C1 <- read.csv("/Users/jasminereighard/Desktop/mbio621/data/postback_TC1_C1.csv", stringsAsFactors = FALSE)
```

    ## Warning in read.table(file = file, header = header, sep = sep, quote = quote, :
    ## incomplete final line found by readTableHeader on
    ## '/Users/jasminereighard/Desktop/mbio621/data/postback_TC1_C1.csv'

``` r
postback_TC2_C1 <- read.csv("/Users/jasminereighard/Desktop/mbio621/data/postback_TC2_C1.csv", stringsAsFactors = FALSE)
```

    ## Warning in read.table(file = file, header = header, sep = sep, quote = quote, :
    ## incomplete final line found by readTableHeader on
    ## '/Users/jasminereighard/Desktop/mbio621/data/postback_TC2_C1.csv'

``` r
postback_TC3_C1 <- read.csv("/Users/jasminereighard/Desktop/mbio621/data/postback_TC3_C1.csv", stringsAsFactors = FALSE)
```

    ## Warning in read.table(file = file, header = header, sep = sep, quote = quote, :
    ## incomplete final line found by readTableHeader on
    ## '/Users/jasminereighard/Desktop/mbio621/data/postback_TC3_C1.csv'

``` r
postback_TC4_C1 <- read.csv("/Users/jasminereighard/Desktop/mbio621/data/postback_TC4_C1.csv", stringsAsFactors = FALSE)
```

    ## Warning in read.table(file = file, header = header, sep = sep, quote = quote, :
    ## incomplete final line found by readTableHeader on
    ## '/Users/jasminereighard/Desktop/mbio621/data/postback_TC4_C1.csv'

``` r
#preback C2
preback_TC1_C2 <- read.csv("/Users/jasminereighard/Desktop/mbio621/data/preback_TC1_C2.csv", stringsAsFactors = FALSE)
```

    ## Warning in read.table(file = file, header = header, sep = sep, quote = quote, :
    ## incomplete final line found by readTableHeader on
    ## '/Users/jasminereighard/Desktop/mbio621/data/preback_TC1_C2.csv'

``` r
preback_TC2_C2 <- read.csv("/Users/jasminereighard/Desktop/mbio621/data/preback_TC2_C2.csv", stringsAsFactors = FALSE)
```

    ## Warning in read.table(file = file, header = header, sep = sep, quote = quote, :
    ## incomplete final line found by readTableHeader on
    ## '/Users/jasminereighard/Desktop/mbio621/data/preback_TC2_C2.csv'

``` r
preback_TC3_C2 <- read.csv("/Users/jasminereighard/Desktop/mbio621/data/preback_TC3_C2.csv", stringsAsFactors = FALSE)
```

    ## Warning in read.table(file = file, header = header, sep = sep, quote = quote, :
    ## incomplete final line found by readTableHeader on
    ## '/Users/jasminereighard/Desktop/mbio621/data/preback_TC3_C2.csv'

``` r
preback_TC4_C2 <- read.csv("/Users/jasminereighard/Desktop/mbio621/data/preback_TC4_C2.csv", stringsAsFactors = FALSE)
```

    ## Warning in read.table(file = file, header = header, sep = sep, quote = quote, :
    ## incomplete final line found by readTableHeader on
    ## '/Users/jasminereighard/Desktop/mbio621/data/preback_TC4_C2.csv'

``` r
#postback C2

postback_TC1_C2 <- read.csv("/Users/jasminereighard/Desktop/mbio621/data/postback_TC1_C2.csv", stringsAsFactors = FALSE)
```

    ## Warning in read.table(file = file, header = header, sep = sep, quote = quote, :
    ## incomplete final line found by readTableHeader on
    ## '/Users/jasminereighard/Desktop/mbio621/data/postback_TC1_C2.csv'

``` r
postback_TC2_C2 <- read.csv("/Users/jasminereighard/Desktop/mbio621/data/postback_TC2_C2.csv", stringsAsFactors = FALSE)
```

    ## Warning in read.table(file = file, header = header, sep = sep, quote = quote, :
    ## incomplete final line found by readTableHeader on
    ## '/Users/jasminereighard/Desktop/mbio621/data/postback_TC2_C2.csv'

``` r
postback_TC3_C2 <- read.csv("/Users/jasminereighard/Desktop/mbio621/data/postback_TC3_C2.csv", stringsAsFactors = FALSE)
```

    ## Warning in read.table(file = file, header = header, sep = sep, quote = quote, :
    ## incomplete final line found by readTableHeader on
    ## '/Users/jasminereighard/Desktop/mbio621/data/postback_TC3_C2.csv'

``` r
postback_TC4_C2 <- read.csv("/Users/jasminereighard/Desktop/mbio621/data/postback_TC4_C2.csv", stringsAsFactors = FALSE)
```

    ## Warning in read.table(file = file, header = header, sep = sep, quote = quote, :
    ## incomplete final line found by readTableHeader on
    ## '/Users/jasminereighard/Desktop/mbio621/data/postback_TC4_C2.csv'

``` r
#preback C3
preback_TC1_C3 <- read.csv("/Users/jasminereighard/Desktop/mbio621/data/preback_TC1_C3.csv", stringsAsFactors = FALSE)
```

    ## Warning in read.table(file = file, header = header, sep = sep, quote = quote, :
    ## incomplete final line found by readTableHeader on
    ## '/Users/jasminereighard/Desktop/mbio621/data/preback_TC1_C3.csv'

``` r
preback_TC2_C3 <- read.csv("/Users/jasminereighard/Desktop/mbio621/data/preback_TC2_C3.csv", stringsAsFactors = FALSE)
```

    ## Warning in read.table(file = file, header = header, sep = sep, quote = quote, :
    ## incomplete final line found by readTableHeader on
    ## '/Users/jasminereighard/Desktop/mbio621/data/preback_TC2_C3.csv'

``` r
preback_TC3_C3 <- read.csv("/Users/jasminereighard/Desktop/mbio621/data/preback_TC3_C3.csv", stringsAsFactors = FALSE)
```

    ## Warning in read.table(file = file, header = header, sep = sep, quote = quote, :
    ## incomplete final line found by readTableHeader on
    ## '/Users/jasminereighard/Desktop/mbio621/data/preback_TC3_C3.csv'

``` r
#postback C3

postback_TC1_C3 <- read.csv("/Users/jasminereighard/Desktop/mbio621/data/postback_TC1_C3.csv", stringsAsFactors = FALSE)
```

    ## Warning in read.table(file = file, header = header, sep = sep, quote = quote, :
    ## incomplete final line found by readTableHeader on
    ## '/Users/jasminereighard/Desktop/mbio621/data/postback_TC1_C3.csv'

``` r
postback_TC2_C3 <- read.csv("/Users/jasminereighard/Desktop/mbio621/data/postback_TC2_C3.csv", stringsAsFactors = FALSE)
```

    ## Warning in read.table(file = file, header = header, sep = sep, quote = quote, :
    ## incomplete final line found by readTableHeader on
    ## '/Users/jasminereighard/Desktop/mbio621/data/postback_TC2_C3.csv'

``` r
postback_TC3_C3 <- read.csv("/Users/jasminereighard/Desktop/mbio621/data/postback_TC3_C3.csv", stringsAsFactors = FALSE)
```

    ## Warning in read.table(file = file, header = header, sep = sep, quote = quote, :
    ## incomplete final line found by readTableHeader on
    ## '/Users/jasminereighard/Desktop/mbio621/data/postback_TC3_C3.csv'

``` r
#convert date to date-time format for background dfs

#Control 25.5
#trial 1 pre
preback_TC1_C1$Clock.TIME <- as.POSIXct(preback_TC1_C1$Clock.TIME, format = "%m/%d/%y %H:%M", tz = "GMT")
preback_TC2_C1$Clock.TIME <- as.POSIXct(preback_TC2_C1$Clock.TIME, format = "%m/%d/%y %H:%M", tz = "GMT")
preback_TC3_C1$Clock.TIME <- as.POSIXct(preback_TC3_C1$Clock.TIME, format = "%m/%d/%y %H:%M", tz = "GMT")
preback_TC4_C1$Clock.TIME <- as.POSIXct(preback_TC4_C1$Clock.TIME, format = "%m/%d/%y %H:%M", tz = "GMT")

#trial 1 post
postback_TC1_C1$Clock.TIME <- as.POSIXct(postback_TC1_C1$Clock.TIME, format = "%m/%d/%y %H:%M", tz = "GMT")
postback_TC2_C1$Clock.TIME <- as.POSIXct(postback_TC2_C1$Clock.TIME, format = "%m/%d/%y %H:%M", tz = "GMT")
postback_TC3_C1$Clock.TIME <- as.POSIXct(postback_TC3_C1$Clock.TIME, format = "%m/%d/%y %H:%M", tz = "GMT")
postback_TC4_C1$Clock.TIME <- as.POSIXct(postback_TC4_C1$Clock.TIME, format = "%m/%d/%y %H:%M", tz = "GMT")


#Control 25.5
#trial 2 pre
preback_TC1_C2$Clock.TIME <- as.POSIXct(preback_TC1_C2$Clock.TIME, format = "%m/%d/%y %H:%M", tz = "GMT")
preback_TC2_C2$Clock.TIME <- as.POSIXct(preback_TC2_C2$Clock.TIME, format = "%m/%d/%y %H:%M", tz = "GMT")
preback_TC3_C2$Clock.TIME <- as.POSIXct(preback_TC3_C2$Clock.TIME, format = "%m/%d/%y %H:%M", tz = "GMT")
preback_TC4_C2$Clock.TIME <- as.POSIXct(preback_TC4_C2$Clock.TIME, format = "%m/%d/%y %H:%M", tz = "GMT")

#trial 2 post
postback_TC1_C2$Clock.TIME <- as.POSIXct(postback_TC1_C2$Clock.TIME, format = "%m/%d/%y %H:%M", tz = "GMT")
postback_TC2_C2$Clock.TIME <- as.POSIXct(postback_TC2_C2$Clock.TIME, format = "%m/%d/%y %H:%M", tz = "GMT")
postback_TC3_C2$Clock.TIME <- as.POSIXct(postback_TC3_C2$Clock.TIME, format = "%m/%d/%y %H:%M", tz = "GMT")
postback_TC4_C2$Clock.TIME <- as.POSIXct(postback_TC4_C2$Clock.TIME, format = "%m/%d/%y %H:%M", tz = "GMT")



#Control 26
#trial 3 pilot!!
preback_TC1_C3$Clock.TIME <- as.POSIXct(preback_TC1_C3$Clock.TIME, format = "%m/%d/%y %H:%M", tz = "GMT")
preback_TC2_C3$Clock.TIME <- as.POSIXct(preback_TC2_C3$Clock.TIME, format = "%m/%d/%y %H:%M", tz = "GMT")
preback_TC3_C3$Clock.TIME <- as.POSIXct(preback_TC3_C3$Clock.TIME, format = "%m/%d/%y %H:%M", tz = "GMT")


#trial 3 post
postback_TC1_C3$Clock.TIME <- as.POSIXct(postback_TC1_C3$Clock.TIME, format = "%m/%d/%y %H:%M", tz = "GMT")
postback_TC2_C3$Clock.TIME <- as.POSIXct(postback_TC2_C3$Clock.TIME, format = "%m/%d/%y %H:%M", tz = "GMT")
postback_TC3_C3$Clock.TIME <- as.POSIXct(postback_TC3_C3$Clock.TIME, format = "%m/%d/%y %H:%M", tz = "GMT")
```

``` r
#If you need to correct the weight of the fish, here is the code
#Eg, Fish weighs 25.46 g, you input .40 kg in Aquaresp and prebackground MO2 measurements was above a negative value

##correct weight of fish
#C2
true_Vre_TC1_C2 <- (0.170*1000)-9.62 #TC1_C2, corrected fish weight goes at the end
true_Vre_TC3_C2 <- (0.170*1000)-9.34 #TC3_C2, 
true_Vre_TC4_C2 <- (0.170*1000)-9.73 #TC4_C2, 


#C3
true_Vre_TC2_C3 <- (0.170*1000)-13.87 #TC2_C3, 
true_Vre_TC3_C3 <- (0.170*1000)-9.82 #TC3_C3, 

##replace Mo2 w/ initial fish weight set to 0.01 C2

preback_TC1_C2$MO2 <- ((preback_TC1_C2$MO2/(0.170*1000))*10)*true_Vre_TC1_C2/9.62
preback_TC3_C2$MO2 <- ((preback_TC3_C2$MO2/(0.170*1000))*10)*true_Vre_TC3_C2/9.34
preback_TC4_C2$MO2 <- ((preback_TC4_C2$MO2/(0.170*1000))*10)*true_Vre_TC4_C2/9.73

##replace Mo2 w/ initial fish weight set to 0.06 C3

preback_TC2_C3$MO2 <- ((preback_TC2_C3$MO2/(0.170*1000))*6)*true_Vre_TC2_C3/13.87
preback_TC3_C3$MO2 <- ((preback_TC3_C3$MO2/(0.170*1000))*6)*true_Vre_TC3_C3/9.82
```

``` r
#replace -Mo2 with 0 
#no negative values!

#make a function that makes preback 0 if >0 
adjust_MO2 <- function(df) {
  df$MO2 <- ifelse(df$MO2 <= 0, 0, df$MO2)
  return(df)
}


#Make a list of dfs for prebackground & named list
preback_list <- list(preback_TC1_C1, preback_TC2_C1, preback_TC3_C1, preback_TC4_C1, 
                     preback_TC1_C2, preback_TC2_C2, preback_TC3_C2, preback_TC4_C2,
                     preback_TC1_C3, preback_TC2_C3, preback_TC3_C3)

#Apply adjust_MO2 to each preback df
preback_list <- lapply(preback_list, adjust_MO2)


#assign back to individual dfs
names(preback_list) <- c("preback_TC1_C1", "preback_TC2_C1", "preback_TC3_C1", "preback_TC4_C1",
                         "preback_TC1_C2", "preback_TC2_C2", "preback_TC3_C2", "preback_TC4_C2",
                         "preback_TC1_C3", "preback_TC2_C3", "preback_TC3_C3")

# Assign the results back to their respective variables in the global environment
list2env(preback_list, envir = .GlobalEnv) 
```

    ## <environment: R_GlobalEnv>

``` r
#Note:
#list2env: This function takes the elements of a list and assigns them to the global environment (or any specified environment) as individual variables.
#preback_list: This is the list containing the results of the adjust_MO2 function applied to each dataset.
#.GlobalEnv: This is the global environment in R, where most user-created variables and functions are stored and accessed.
```

``` r
#Now weʻre going to combine pre and post background dfs together

#make a list for postbackground dfs
postback_list <- list(postback_TC1_C1, postback_TC2_C1, postback_TC3_C1, postback_TC4_C1, 
                     postback_TC1_C2, postback_TC2_C2, postback_TC3_C2, postback_TC4_C2,
                     postback_TC1_C3, postback_TC2_C3, postback_TC3_C3)


# Combine pre and post trial data frames
combinedbg_list <- Map(rbind, preback_list, postback_list)

# Assign names to the combined data frames
names(combinedbg_list) <- c("TC1_C1_bg", "TC2_C1_bg", "TC3_C1_bg", "TC4_C1_bg", 
"TC1_C2_bg", "TC2_C2_bg", "TC3_C2_bg", "TC4_C2_bg",
"TC1_C3_bg", "TC2_C3_bg", "TC3_C3_bg")

# Assign the results back to their respective variables in the global environment
list2env(combinedbg_list, envir = .GlobalEnv)
```

    ## <environment: R_GlobalEnv>

``` r
#We assume bacterial respiration increases linearly over time so,
#A linear model based on pre- and post- background MO2 measurements over time is used to subtract background respiration during experiments (Roche et al., 2013). 


#make a list of combined background dfs
background_list <- list(TC1_C1_bg, TC2_C1_bg, TC3_C1_bg, TC4_C1_bg, 
   TC1_C2_bg, TC2_C2_bg, TC3_C2_bg, TC4_C2_bg,
   TC1_C3_bg, TC2_C3_bg, TC3_C3_bg)

# Apply the lm function to each data frame
bg_regressions <- lapply(background_list, function(df) lm(MO2 ~ Clock.TIME, data = df))

#regression1_t1 <- lm(MO2 ~ Clock.TIME, data = cs1_t1_bg) ^this is what is happening to each individual df
#Performs a linear regression on MO2 over Time for each bg dataset



# Naming the elements of the background list
names(bg_regressions) <- c(
  "regression_TC1_C1", "regression_TC2_C1", "regression_TC3_C1", "regression_TC4_C1",
  "regression_TC1_C2", "regression_TC2_C2", "regression_TC3_C2", "regression_TC4_C2",
  "regression_TC1_C3", "regression_TC2_C3", "regression_TC3_C3") 

# Assign the results back to their respective variables in the global environment
list2env(bg_regressions, envir = .GlobalEnv)
```

    ## <environment: R_GlobalEnv>

``` r
#Now weʻre going manual mode because the lists kept bugging out for me 


#Predict background MO2 
#C1
TC1_C1_bg_estimates = predict(regression_TC1_C1, TC1_C1)
TC2_C1_bg_estimates = predict(regression_TC2_C1, TC2_C1)
TC3_C1_bg_estimates = predict(regression_TC3_C1, TC3_C1)
TC4_C1_bg_estimates = predict(regression_TC4_C1, TC4_C1)

#C2
TC1_C2_bg_estimates = predict(regression_TC1_C2, TC1_C2)
TC2_C2_bg_estimates = predict(regression_TC2_C2, TC2_C2)
TC3_C2_bg_estimates = predict(regression_TC3_C2, TC3_C2)
TC4_C2_bg_estimates = predict(regression_TC4_C2, TC4_C2)

#C3 (pilot)
TC1_C3_bg_estimates = predict(regression_TC1_C3, TC1_C3)
TC2_C3_bg_estimates = predict(regression_TC2_C3, TC2_C3)
TC3_C3_bg_estimates = predict(regression_TC3_C3, TC3_C3)



#This line uses the predict() function to make predictions based on the linear model (regression1_t1) and a new dataset, cs1_t1.
#cs1_t1_bg_estimates will contain the predicted values of MO2 in cs1_t1 based on the relationship found in r1_t1_bg.
```

``` r
#subtract background from SMR

#C2
TC1_C1$MO2 <- TC1_C1$MO2 - TC1_C1_bg_estimates
TC2_C1$MO2 <- TC2_C1$MO2 - TC2_C1_bg_estimates
TC3_C1$MO2 <- TC3_C1$MO2 - TC3_C1_bg_estimates
TC4_C1$MO2 <- TC4_C1$MO2 - TC4_C1_bg_estimates

#C2
TC1_C2$MO2 <- TC1_C2$MO2 - TC1_C2_bg_estimates
TC2_C2$MO2 <- TC2_C2$MO2 - TC2_C2_bg_estimates
TC3_C2$MO2 <- TC3_C2$MO2 - TC3_C2_bg_estimates
TC4_C2$MO2 <- TC4_C2$MO2 - TC4_C2_bg_estimates

#C3
TC1_C3$MO2 <- TC1_C3$MO2 - TC1_C3_bg_estimates
TC2_C3$MO2 <- TC2_C3$MO2 - TC2_C3_bg_estimates
TC3_C3$MO2 <- TC3_C3$MO2 - TC3_C3_bg_estimates
```

``` r
#predict background MO2 on pcrit dfs

#C1
pcrit_TC1_C1_bg_estimates = predict(regression_TC1_C1, pcrit_TC1_C1)
pcrit_TC2_C1_bg_estimates = predict(regression_TC2_C1, pcrit_TC2_C1)
pcrit_TC3_C1_bg_estimates = predict(regression_TC3_C1, pcrit_TC3_C1)
pcrit_TC4_C1_bg_estimates = predict(regression_TC4_C1, pcrit_TC4_C1)

#C2
pcrit_TC1_C2_bg_estimates = predict(regression_TC1_C2, pcrit_TC1_C2)
pcrit_TC2_C2_bg_estimates = predict(regression_TC2_C2, pcrit_TC2_C2)
pcrit_TC3_C2_bg_estimates = predict(regression_TC3_C2, pcrit_TC3_C2)
pcrit_TC4_C2_bg_estimates = predict(regression_TC4_C2, pcrit_TC4_C2)

#C3 (pilot)
pcrit_TC1_C3_bg_estimates = predict(regression_TC1_C3, pcrit_TC1_C3)
pcrit_TC2_C3_bg_estimates = predict(regression_TC2_C3, pcrit_TC2_C3)
pcrit_TC3_C3_bg_estimates = predict(regression_TC3_C3, pcrit_TC3_C3)
```

``` r
#subtract background from PCRIT

#C1
pcrit_TC1_C1$MO2 <- pcrit_TC1_C1$MO2 - pcrit_TC1_C1_bg_estimates
pcrit_TC2_C1$MO2 <- pcrit_TC2_C1$MO2 - pcrit_TC2_C1_bg_estimates
pcrit_TC3_C1$MO2 <- pcrit_TC3_C1$MO2 - pcrit_TC3_C1_bg_estimates
pcrit_TC4_C1$MO2 <- pcrit_TC4_C1$MO2 - pcrit_TC4_C1_bg_estimates

#C2
pcrit_TC1_C2$MO2 <- pcrit_TC1_C2$MO2 - pcrit_TC1_C2_bg_estimates
pcrit_TC2_C2$MO2 <- pcrit_TC2_C2$MO2 - pcrit_TC2_C2_bg_estimates
pcrit_TC3_C2$MO2 <- pcrit_TC3_C2$MO2 - pcrit_TC3_C2_bg_estimates
pcrit_TC4_C2$MO2 <- pcrit_TC4_C2$MO2 - pcrit_TC4_C2_bg_estimates

#C3
pcrit_TC1_C3$MO2 <- pcrit_TC1_C3$MO2 - pcrit_TC1_C3_bg_estimates
pcrit_TC2_C3$MO2 <- pcrit_TC2_C3$MO2 - pcrit_TC2_C3_bg_estimates
pcrit_TC3_C3$MO2 <- pcrit_TC3_C3$MO2 - pcrit_TC3_C3_bg_estimates
```

``` r
#apply calcSMR function to dfs


#Control 1 test SMR
TC1_C1_calcSMR <- calcSMR(TC1_C1$MO2, q=c(0.1,0.15,0.2,0.25,0.3), G=2) 
TC2_C1_calcSMR <- calcSMR(TC2_C1$MO2, q=c(0.1,0.15,0.2,0.25,0.3), G=2) 
TC3_C1_calcSMR <- calcSMR(TC3_C1$MO2, q=c(0.1,0.15,0.2,0.25,0.3), G=2) 
TC4_C1_calcSMR <- calcSMR(TC4_C1$MO2, q=c(0.1,0.15,0.2,0.25,0.3), G=2) 

#Control 2 test SMR
TC1_C2_calcSMR <- calcSMR(TC1_C2$MO2, q=c(0.1,0.15,0.2,0.25,0.3), G=2) 
TC2_C2_calcSMR <- calcSMR(TC2_C2$MO2, q=c(0.1,0.15,0.2,0.25,0.3), G=2) 
TC3_C2_calcSMR <- calcSMR(TC3_C2$MO2, q=c(0.1,0.15,0.2,0.25,0.3), G=2) 
TC4_C2_calcSMR <- calcSMR(TC4_C2$MO2, q=c(0.1,0.15,0.2,0.25,0.3), G=2) 

#Control 3 (pilot) test SMR
TC1_C3_calcSMR <- calcSMR(TC1_C3$MO2, q=c(0.1,0.15,0.2,0.25,0.3), G=2) 
TC2_C3_calcSMR <- calcSMR(TC2_C3$MO2, q=c(0.1,0.15,0.2,0.25,0.3), G=2) 
TC3_C3_calcSMR <- calcSMR(TC3_C3$MO2, q=c(0.1,0.15,0.2,0.25,0.3), G=2) 
```

``` r
#Now weʻre going to plot our corrected SMR data to see which distribution fits best 

#define custom line colors for distribution visualization on graphs

line_colors <- c("Double Normal" = "#2e2b28", "Q10" = "#800080", "Q15" = "#40AD5a",  "Q20" = "#009fff", "Q25" = "#ffb55a")

# Function to create line data
create_line_data <- function(calcSMR_data) {
  yintercept_values <- c(calcSMR_data[[1]][1], calcSMR_data[[2]][1:4])
  labels <- c("Double Normal", "Q10", "Q15", "Q20", "Q25")
  
  data.frame(
    yintercept = yintercept_values,
    line_label = factor(labels)
  )
}

#Apply line colors function to individual fish data frames

# List of SMR data
#calcSMR_datasets <- list(
 # cs1_t1_calcSMR, cs2_t1_calcSMR, cs3_t1_calcSMR, cs4_t1_calcSMR)

calcSMR_datasets <- list(
  TC1_C1_calcSMR, TC2_C1_calcSMR, TC3_C1_calcSMR, TC4_C1_calcSMR,
  TC1_C2_calcSMR, TC2_C2_calcSMR, TC3_C2_calcSMR, TC4_C2_calcSMR,
  TC1_C3_calcSMR, TC2_C3_calcSMR, TC3_C3_calcSMR)


# Assign names to the SMR data
#names(calcSMR_datasets) <- c(
#  "line_data_cs1_t1", "line_data_cs2_t1", "line_data_cs3_t1", "line_data_cs4_t1")

names(calcSMR_datasets) <- c(
  "line_data_TC1_C1", "line_data_TC2_C1", "line_data_TC3_C1", "line_data_TC4_C1",
  "line_data_TC1_C2", "line_data_TC2_C2", "line_data_TC3_C2", "line_data_TC4_C2",
  "line_data_TC1_C3", "line_data_TC2_C3", "line_data_TC3_C3")
# Apply the create_line_data function to each dataset
line_data_results <- lapply(calcSMR_datasets, create_line_data)

# Assign names to the results
names(line_data_results) <- names(calcSMR_datasets)

# Assign the results back to their respective variables in the global environment
list2env(line_data_results, .GlobalEnv)
```

    ## <environment: R_GlobalEnv>

``` r
#Create a list of SMR dfs to streamline creating pdf of smr graphs
#smr_dataframes <- list(cs1_t1, cs2_t1, cs3_t1, cs4_t1)
#names(smr_dataframes) <- c("cs1_t1", "cs2_t1", "cs3_t1", "cs4_t1")


smr_dataframes <- list(TC1_C1, TC2_C1, TC3_C1, TC4_C1, 
                       TC1_C2, TC2_C2, TC3_C2, TC4_C2,
                       TC1_C3, TC2_C3, TC3_C3)

names(smr_dataframes) <- c("TC1_C1", "TC2_C1", "TC3_C1", "TC4_C1",
                           "TC1_C2", "TC2_C2", "TC3_C2", "TC4_C2",
                           "TC1_C3", "TC2_C3", "TC3_C3")
```

``` r
#Here is the code to print all the graphs onto one pdf saved in your figs folder

# Set directory for saving the PDF
figs_dir <- "/Users/jasminereighard/Desktop/mbio621/figs" #change file path

# Function to plot SMR data with adaptive y-axis
plot_smr_data <- function(fish_id, smr_data_list, line_data_list) {
  data <- smr_data_list[[fish_id]]
  line_data <- line_data_list[[paste0("line_data_", fish_id)]]
  
  # Calculate the lower and upper limits for the y-axis
  y_min <- min(data$MO2) - 10
  y_max <- max(data$MO2) + 10
  
  
  ggplot(data = data, aes(x = Clock.TIME, y = MO2)) +
    geom_point() +
    geom_hline(data = line_data, aes(yintercept = yintercept, color = line_label), linetype = "dashed") +
    scale_color_manual(values = line_colors) +
    theme_bw() +
    labs(
      title = paste("SMR", fish_id),
      color = "Legend"
    ) +
    scale_y_continuous(
      limits = c(y_min, y_max),  # Set y-axis limits
      breaks = seq(y_min, y_max, by = 10),  # Set y-axis breaks
      labels = scales::number_format(accuracy = 1)  # Round y-axis labels to whole numbers
    ) +
    scale_x_datetime(date_labels = "%H:%M", date_breaks = "2 hour") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    xlab("Time (s)") +
    ylab("MO2 (mg O2/kg*h)") +
    theme(
      legend.position = c(.95, .95),
      legend.justification = c("right", "top"),
      legend.box.just = "right",
      legend.margin = margin(4, 4, 4, 4)
    )
}

# Generate a PDF with all SMR plots
pdf(file = file.path(figs_dir, "SMR_plots.pdf"))
for (fish_id in names(smr_dataframes)) {
  try(print(plot_smr_data(fish_id, smr_dataframes, line_data_results)))
}
```

    ## Warning: A numeric `legend.position` argument in `theme()` was deprecated in ggplot2
    ## 3.5.0.
    ## ℹ Please use the `legend.position.inside` argument of `theme()` instead.
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
    ## generated.

``` r
dev.off()
```

    ## quartz_off_screen 
    ##                 2

``` r
#I like to see plots in the console as well, go to the gear symbol next to Knit
#Choose chunk output in console


# Loop through each fish_id and display each plot in the console
for (fish_id in names(smr_dataframes)) {
  plot <- plot_smr_data(fish_id, smr_dataframes, line_data_results)
  
  # Display plot in the console
  print(plot)
}
```

![](data_org_MBIO621_files/figure-gfm/unnamed-chunk-26-1.png)<!-- -->![](data_org_MBIO621_files/figure-gfm/unnamed-chunk-26-2.png)<!-- -->![](data_org_MBIO621_files/figure-gfm/unnamed-chunk-26-3.png)<!-- -->![](data_org_MBIO621_files/figure-gfm/unnamed-chunk-26-4.png)<!-- -->![](data_org_MBIO621_files/figure-gfm/unnamed-chunk-26-5.png)<!-- -->![](data_org_MBIO621_files/figure-gfm/unnamed-chunk-26-6.png)<!-- -->![](data_org_MBIO621_files/figure-gfm/unnamed-chunk-26-7.png)<!-- -->![](data_org_MBIO621_files/figure-gfm/unnamed-chunk-26-8.png)<!-- -->![](data_org_MBIO621_files/figure-gfm/unnamed-chunk-26-9.png)<!-- -->![](data_org_MBIO621_files/figure-gfm/unnamed-chunk-26-10.png)<!-- -->![](data_org_MBIO621_files/figure-gfm/unnamed-chunk-26-11.png)<!-- -->

``` r
# Function to calculate the mean distances of all corr_MO2 values +2 above double normal & -2 below q20
calculate_distances <- function(data, calcSMR_data) {
  filtered_data <- data %>%
    filter(MO2 <= calcSMR_data[[1]][1] + 2 & MO2 >= calcSMR_data[[2]][1] - 2)

  filtered_data %>%
    mutate(
      dist_Double_Normal = abs(MO2 - calcSMR_data[[1]][1]),
      dist_q20 = abs(MO2 - calcSMR_data[[2]][1]),
      dist_Q15 = abs(MO2 - calcSMR_data[[2]][2]),
      dist_Q20 = abs(MO2 - calcSMR_data[[2]][3]),
      dist_Q25 = abs(MO2 - calcSMR_data[[2]][4])
    ) %>%
    summarise(
      mean_dist_Double_Normal = mean(dist_Double_Normal, na.rm = TRUE),
      mean_dist_q20 = mean(dist_q20, na.rm = TRUE),
      mean_dist_Q15 = mean(dist_Q15, na.rm = TRUE),
      mean_dist_Q20 = mean(dist_Q20, na.rm = TRUE),
      mean_dist_Q25 = mean(dist_Q25, na.rm = TRUE)
    )
}

# Apply the function to each dataset using mapply
distance_summaries <- mapply(
  calculate_distances,
  smr_dataframes,
  calcSMR_datasets,
  SIMPLIFY = FALSE
)

# Combine results into a data frame
distance_summaries_df <- do.call(rbind, lapply(seq_along(distance_summaries), function(i) {
  cbind(Fish = names(smr_dataframes)[i], distance_summaries[[i]])
}))

# Print the resulting data frame
print(distance_summaries_df)
```

    ##      Fish mean_dist_Double_Normal mean_dist_q20 mean_dist_Q15 mean_dist_Q20
    ## 1  TC1_C1                5.159885      5.247325      3.705302      3.138112
    ## 2  TC2_C1                6.823702      6.693545      5.593977      4.947758
    ## 3  TC3_C1                8.886003     10.722710      8.719599      7.419704
    ## 4  TC4_C1               11.743532      9.321742      7.997187      6.898344
    ## 5  TC1_C2                6.884188      7.537890      5.010773      4.037305
    ## 6  TC2_C2                8.462110      8.710895      7.315227      6.140893
    ## 7  TC3_C2               11.432293     12.033472      7.245037      6.383469
    ## 8  TC4_C2                5.784668      5.667677      4.416115      3.594688
    ## 9  TC1_C3                7.937765      7.250761      4.885553      4.093238
    ## 10 TC2_C3                9.971735     12.963568      8.144816      6.531512
    ## 11 TC3_C3               11.293310     12.601892      7.666237      6.540112
    ##    mean_dist_Q25
    ## 1       3.349638
    ## 2       4.612652
    ## 3       5.950926
    ## 4       6.312327
    ## 5       4.520011
    ## 6       5.441774
    ## 7       8.148361
    ## 8       4.458441
    ## 9       4.899480
    ## 10      5.683829
    ## 11      7.861603

``` r
#Now weʻre going to extract our values for data analyses! 


#Calculate Q20 and extract SMR values 
extract_q20 <- function(calcSMR_data) {
  return(as.numeric(calcSMR_data$quant["20%"]))  # Explicitly get the 20% quantile as a numeric value
}

# Function to apply extract_q20 to all elements in a list with modified names
apply_extract_q20 <- function(smr_results_list) {
  q20_values <- list()
  for (name in names(smr_results_list)) {
    # Extract q20 value as a numeric
    q20 <- extract_q20(smr_results_list[[name]])
    # Modify name by removing "line_data_" prefix
    modified_name <- sub("^line_data_", "", name)
    # Store q20 value with modified name
    q20_values[[modified_name]] <- q20
  }
  return(q20_values)
}

# Extract q20 values for all smr results
control_q20 <- apply_extract_q20(calcSMR_datasets)

#Example code for when adding other treatment groups 
#treatment_q20 <- calculate_MMR(calcSMR_trt_datasets)
#all_q20_values <- c(control_q20, treatment_q20) #combine MMR ctrl and trt
```

``` r
smr_dataframes <- list(TC1_C1, TC2_C1, TC3_C1, TC4_C1, 
                       TC1_C2, TC2_C2, TC3_C2, TC4_C2,
                       TC1_C3, TC2_C3, TC3_C3)

names(smr_dataframes) <- c("TC1_C1", "TC2_C1", "TC3_C1", "TC4_C1",
                           "TC1_C2", "TC2_C2", "TC3_C2", "TC4_C2",
                           "TC1_C3", "TC2_C3", "TC3_C3")

calculate_MMR <- function(smr_data_list) {
  MMR_values <- list()
  for (name in names(smr_data_list)) {
    df <- smr_data_list[[name]]
    MMR <- max(df$MO2, na.rm = TRUE)
    MMR_values[[name]] <- MMR
  }
  return(MMR_values)
}

# Calculate MMR values
control_MMR <- calculate_MMR(smr_dataframes)

# Print the list to verify
print(control_MMR)
```

    ## $TC1_C1
    ## [1] 316.7053
    ## 
    ## $TC2_C1
    ## [1] 348.6152
    ## 
    ## $TC3_C1
    ## [1] 332.9836
    ## 
    ## $TC4_C1
    ## [1] 427.1228
    ## 
    ## $TC1_C2
    ## [1] 440.7338
    ## 
    ## $TC2_C2
    ## [1] 775.825
    ## 
    ## $TC3_C2
    ## [1] 410.7455
    ## 
    ## $TC4_C2
    ## [1] 406.4257
    ## 
    ## $TC1_C3
    ## [1] 524.7147
    ## 
    ## $TC2_C3
    ## [1] 314.6415
    ## 
    ## $TC3_C3
    ## [1] 509.9121

``` r
# Define Function to Calculate Aerobic Scope
calc_aerobic_scope <- function(MMR, SMR) {
  return(MMR - SMR)
}

# Calculate aerobic scope for each dataset
aerobic_scope_values <- mapply(calc_aerobic_scope, control_MMR, control_q20, SIMPLIFY = FALSE)  
# Simplify = FALSES stores as a list
#When adding other treatments, change "control_MMR" and "control_q20" to combined values
```

``` r
# Set directory for saving the PDF
figs_dir <- "/Users/jasminereighard/Desktop/mbio621/figs"  # Change file path if needed

# Function to plot pcrit data with two subplots
plot_pcrit_data <- function(fish_id, pcrit_data_list) {
  data <- pcrit_data_list[[fish_id]]
  
  # First plot: MO2 vs Clock.TIME
  plot1 <- ggplot(data, aes(x = Clock.TIME, y = MO2)) +
    geom_point() +
    theme_bw() +
    labs(
      title = paste("MO2 vs Clock.TIME -", fish_id),
      x = "Clock.TIME",
      y = "MO2"
    )
  
  # Second plot: MO2 vs avg.po2
  plot2 <- ggplot(data, aes(x = avg.po2, y = MO2)) +
    geom_point() +
    theme_bw() +
    labs(
      title = paste("MO2 vs avg.po2 -", fish_id),
      x = "avg.po2",
      y = "MO2"
    )
  
  # Combine the two plots into a grid
  combined_plot <- cowplot::plot_grid(plot1, plot2, ncol = 2, labels = c("A", "B"))
  
  return(combined_plot)
}

# Generate a PDF with all pcrit plots
pdf(file = file.path(figs_dir, "pcrit_quick_viz.pdf"), width = 14, height = 7)
for (fish_id in names(pcrit_data)) {
  try(print(plot_pcrit_data(fish_id, pcrit_data)))
}
dev.off()
```

    ## quartz_off_screen 
    ##                 2

``` r
# Loop through each fish_id in pcrit_data and display each plot in the console
for (fish_id in names(pcrit_data)) {
  plot <- plot_pcrit_data(fish_id, pcrit_data)
  
  # Display plot in the console
  print(plot)
}
```

![](data_org_MBIO621_files/figure-gfm/unnamed-chunk-32-1.png)<!-- -->![](data_org_MBIO621_files/figure-gfm/unnamed-chunk-32-2.png)<!-- -->![](data_org_MBIO621_files/figure-gfm/unnamed-chunk-32-3.png)<!-- -->![](data_org_MBIO621_files/figure-gfm/unnamed-chunk-32-4.png)<!-- -->![](data_org_MBIO621_files/figure-gfm/unnamed-chunk-32-5.png)<!-- -->![](data_org_MBIO621_files/figure-gfm/unnamed-chunk-32-6.png)<!-- -->![](data_org_MBIO621_files/figure-gfm/unnamed-chunk-32-7.png)<!-- -->![](data_org_MBIO621_files/figure-gfm/unnamed-chunk-32-8.png)<!-- -->![](data_org_MBIO621_files/figure-gfm/unnamed-chunk-32-9.png)<!-- -->![](data_org_MBIO621_files/figure-gfm/unnamed-chunk-32-10.png)<!-- -->![](data_org_MBIO621_files/figure-gfm/unnamed-chunk-32-11.png)<!-- -->

``` r
#pcrit function
findPcrit <- function(model, SMR) {
  m <- model$coefficient[2] #slope
  b <- model$coefficient[1] # Intercept
  e <- model$sigma #residual error
  pO2 = (SMR - b - e)/m
  return(paste("Pcrit = ", pO2))
}
```

``` r
# Initialize an empty list to store the results
pcrit_results <- list()

# Loop through each dataframe in the list
for (fish_id in names(pcrit_data)) {
  # Remove the "pcrit_" prefix to match with control_q20 names
  control_fish_id <- sub("pcrit_", "", fish_id)
  
  # Get the dataframe from pcrit_data
  df <- pcrit_data[[fish_id]]
  
  if (nrow(df) >= 6) {
    # Get the last 6 rows
    last_6_df <- tail(df, 6)
    
    # Run the linear model
    pcrit_lm <- lm(MO2 ~ avg.po2, data = last_6_df)
    pcrit_model <- summary(pcrit_lm)
    
    # Retrieve the SMR value from the control_q20 list
    SMR <- control_q20[[control_fish_id]]
    
    if (!is.null(SMR)) {  # Check if SMR exists for this fish_id
      Pcrit <- findPcrit(pcrit_model, SMR)
      
      # Save the results
      pcrit_results[[fish_id]] <- list(
        Pcrit = Pcrit,
        ModelSummary = pcrit_model
      )
    }
  }
}

# Print the results
print(pcrit_results)
```

    ## $pcrit_TC1_C1
    ## $pcrit_TC1_C1$Pcrit
    ## [1] "Pcrit =  1.43485527934779"
    ## 
    ## $pcrit_TC1_C1$ModelSummary
    ## 
    ## Call:
    ## lm(formula = MO2 ~ avg.po2, data = last_6_df)
    ## 
    ## Residuals:
    ##     38     39     40     41     42     43 
    ## -8.968  4.534  5.351  9.900 -5.325 -5.492 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)  
    ## (Intercept)   33.430     16.846   1.984   0.1182  
    ## avg.po2        3.933      1.155   3.406   0.0271 *
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 8.458 on 4 degrees of freedom
    ## Multiple R-squared:  0.7436, Adjusted R-squared:  0.6795 
    ## F-statistic:  11.6 on 1 and 4 DF,  p-value: 0.02713
    ## 
    ## 
    ## 
    ## $pcrit_TC2_C1
    ## $pcrit_TC2_C1$Pcrit
    ## [1] "Pcrit =  14.6834559413903"
    ## 
    ## $pcrit_TC2_C1$ModelSummary
    ## 
    ## Call:
    ## lm(formula = MO2 ~ avg.po2, data = last_6_df)
    ## 
    ## Residuals:
    ##     49     50     51     52     53     54 
    ## -2.333  2.761 -1.191  1.744  1.407 -2.387 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)  
    ## (Intercept)   18.888      5.270   3.584   0.0231 *
    ## avg.po2        2.543      0.557   4.566   0.0103 *
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 2.51 on 4 degrees of freedom
    ## Multiple R-squared:  0.839,  Adjusted R-squared:  0.7988 
    ## F-statistic: 20.85 on 1 and 4 DF,  p-value: 0.01029
    ## 
    ## 
    ## 
    ## $pcrit_TC3_C1
    ## $pcrit_TC3_C1$Pcrit
    ## [1] "Pcrit =  15.9465282241017"
    ## 
    ## $pcrit_TC3_C1$ModelSummary
    ## 
    ## Call:
    ## lm(formula = MO2 ~ avg.po2, data = last_6_df)
    ## 
    ## Residuals:
    ##     24     25     26     27     28     29 
    ## -2.557 -2.677  6.648  1.852  2.207 -5.473 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)   
    ## (Intercept)  21.9531     9.1243   2.406  0.07387 . 
    ## avg.po2       3.7244     0.5914   6.297  0.00325 **
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 4.903 on 4 degrees of freedom
    ## Multiple R-squared:  0.9084, Adjusted R-squared:  0.8855 
    ## F-statistic: 39.65 on 1 and 4 DF,  p-value: 0.00325
    ## 
    ## 
    ## 
    ## $pcrit_TC4_C1
    ## $pcrit_TC4_C1$Pcrit
    ## [1] "Pcrit =  8.20119791229383"
    ## 
    ## $pcrit_TC4_C1$ModelSummary
    ## 
    ## Call:
    ## lm(formula = MO2 ~ avg.po2, data = last_6_df)
    ## 
    ## Residuals:
    ##       85       86       87       88       89       90 
    ##  1.38898 -1.81376  0.04235 -0.05110 -0.15040  0.58393 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -10.7984     3.1405  -3.438 0.026331 *  
    ## avg.po2       6.9816     0.5925  11.783 0.000297 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.182 on 4 degrees of freedom
    ## Multiple R-squared:  0.972,  Adjusted R-squared:  0.965 
    ## F-statistic: 138.8 on 1 and 4 DF,  p-value: 0.0002968
    ## 
    ## 
    ## 
    ## $pcrit_TC1_C2
    ## $pcrit_TC1_C2$Pcrit
    ## [1] "Pcrit =  22.4171692060477"
    ## 
    ## $pcrit_TC1_C2$ModelSummary
    ## 
    ## Call:
    ## lm(formula = MO2 ~ avg.po2, data = last_6_df)
    ## 
    ## Residuals:
    ##     27     28     29     30     31     32 
    ## -1.589  1.067  2.435 -3.240  4.033 -2.706 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -43.4250     9.4533  -4.594 0.010079 *  
    ## avg.po2       9.1046     0.7721  11.792 0.000296 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 3.304 on 4 degrees of freedom
    ## Multiple R-squared:  0.972,  Adjusted R-squared:  0.965 
    ## F-statistic:   139 on 1 and 4 DF,  p-value: 0.000296
    ## 
    ## 
    ## 
    ## $pcrit_TC2_C2
    ## $pcrit_TC2_C2$Pcrit
    ## [1] "Pcrit =  8.13627380161237"
    ## 
    ## $pcrit_TC2_C2$ModelSummary
    ## 
    ## Call:
    ## lm(formula = MO2 ~ avg.po2, data = last_6_df)
    ## 
    ## Residuals:
    ##       74       75       76       77       78       79 
    ## -0.43328  0.08779  2.78596 -2.75745 -1.17981  1.49680 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)    5.816      4.890   1.189 0.300036    
    ## avg.po2        6.442      0.579  11.126 0.000371 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 2.19 on 4 degrees of freedom
    ## Multiple R-squared:  0.9687, Adjusted R-squared:  0.9609 
    ## F-statistic: 123.8 on 1 and 4 DF,  p-value: 0.0003713
    ## 
    ## 
    ## 
    ## $pcrit_TC3_C2
    ## $pcrit_TC3_C2$Pcrit
    ## [1] "Pcrit =  15.4189228730107"
    ## 
    ## $pcrit_TC3_C2$ModelSummary
    ## 
    ## Call:
    ## lm(formula = MO2 ~ avg.po2, data = last_6_df)
    ## 
    ## Residuals:
    ##      63      64      65      66      67      68 
    ##  2.6414 -4.8361  0.3460  2.9453 -0.9842 -0.1123 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)   
    ## (Intercept)  -24.562     13.766  -1.784  0.14895   
    ## avg.po2        5.824      1.178   4.944  0.00779 **
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 3.168 on 4 degrees of freedom
    ## Multiple R-squared:  0.8594, Adjusted R-squared:  0.8242 
    ## F-statistic: 24.45 on 1 and 4 DF,  p-value: 0.007792
    ## 
    ## 
    ## 
    ## $pcrit_TC4_C2
    ## $pcrit_TC4_C2$Pcrit
    ## [1] "Pcrit =  16.0677317408753"
    ## 
    ## $pcrit_TC4_C2$ModelSummary
    ## 
    ## Call:
    ## lm(formula = MO2 ~ avg.po2, data = last_6_df)
    ## 
    ## Residuals:
    ##      43      44      45      46      47      48 
    ## -3.6689  2.4834  1.8784  1.8831  0.4008 -2.9768 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)   
    ## (Intercept)  26.9260     7.2612   3.708  0.02069 * 
    ## avg.po2       4.1496     0.5655   7.338  0.00184 **
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 2.988 on 4 degrees of freedom
    ## Multiple R-squared:  0.9309, Adjusted R-squared:  0.9136 
    ## F-statistic: 53.85 on 1 and 4 DF,  p-value: 0.001836
    ## 
    ## 
    ## 
    ## $pcrit_TC1_C3
    ## $pcrit_TC1_C3$Pcrit
    ## [1] "Pcrit =  11.894681912145"
    ## 
    ## $pcrit_TC1_C3$ModelSummary
    ## 
    ## Call:
    ## lm(formula = MO2 ~ avg.po2, data = last_6_df)
    ## 
    ## Residuals:
    ##      76      77      78      79      80      81 
    ##  0.8455 -0.5267 -2.3664  2.6743 -0.4346 -0.1920 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  10.8101     7.0311   1.537 0.198997    
    ## avg.po2       5.7719     0.6235   9.258 0.000757 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.869 on 4 degrees of freedom
    ## Multiple R-squared:  0.9554, Adjusted R-squared:  0.9443 
    ## F-statistic:  85.7 on 1 and 4 DF,  p-value: 0.0007571
    ## 
    ## 
    ## 
    ## $pcrit_TC2_C3
    ## $pcrit_TC2_C3$Pcrit
    ## [1] "Pcrit =  33.5877629326991"
    ## 
    ## $pcrit_TC2_C3$ModelSummary
    ## 
    ## Call:
    ## lm(formula = MO2 ~ avg.po2, data = last_6_df)
    ## 
    ## Residuals:
    ##      24      25      26      27      28      29 
    ## -1.0240  0.9388  0.6029  0.3847 -0.6075 -0.2950 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -27.6628     2.1438  -12.90 0.000208 ***
    ## avg.po2       6.3246     0.1604   39.43 2.47e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.8511 on 4 degrees of freedom
    ## Multiple R-squared:  0.9974, Adjusted R-squared:  0.9968 
    ## F-statistic:  1554 on 1 and 4 DF,  p-value: 2.472e-06
    ## 
    ## 
    ## 
    ## $pcrit_TC3_C3
    ## $pcrit_TC3_C3$Pcrit
    ## [1] "Pcrit =  8.58924327908573"
    ## 
    ## $pcrit_TC3_C3$ModelSummary
    ## 
    ## Call:
    ## lm(formula = MO2 ~ avg.po2, data = last_6_df)
    ## 
    ## Residuals:
    ##      85      86      87      88      89      90 
    ## -0.8317  1.0343  0.4793 -0.1470 -1.0277  0.4930 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -17.5657     3.0334  -5.791  0.00442 ** 
    ## avg.po2       7.0249     0.4174  16.829 7.31e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.91 on 4 degrees of freedom
    ## Multiple R-squared:  0.9861, Adjusted R-squared:  0.9826 
    ## F-statistic: 283.2 on 1 and 4 DF,  p-value: 7.307e-05