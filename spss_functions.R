#' @title Functions that help with SPSS data management
# ' Functions
get_spss <- function(dir = getwd(), var.pattern = NULL, reverse = FALSE){
  get_variables <- function(x, var.pattern){
    x <- x[grepl(names(x),pattern = var.pattern)]
    x
  }
  if(!dir.exists(dir)){
    stop("Please specify correct directory")
  }
  .files <- list.files(dir, pattern = ".sav")
  if(!length(.files)) {
    stop(sprintf("No .sav files found in %s", dir))
  }
  .files <- lapply(.files, haven::read_sav)
  .files  <- lapply(.files, haven::zap_formats)
  .files <- lapply(.files, haven::zap_labels)
  if(!is.null(var.pattern)) {
    if(reverse) {
      var.pattern <- paste(var.pattern, "[0-9]R", sep = "")
      .files <- lapply(.files, function(X) get_variables(X, var.pattern))
    } else {
      .files <- lapply(.files, function(X) get_variables(X, var.pattern))
    }
  }
  return(.files)
}
recode_variables <- function(x, vars, old_values = NULL, new_values = NULL) {
  if(!all(is.character(vars))){
    stop("vars should be of class character")
  }
  if(!all(vars %in% names(x))){
    stop(sprintf("%s are not all valid columns in x", vars))
  }
  if(length(vars) > 1) {
  message("Old Means:") 
  print(apply(x[,vars], 2, function(x) mean(x, na.rm = TRUE)))
  x[,vars] <- apply(x[,vars], 2, function(x) plyr::mapvalues(x, from = old_values, to = new_values))
  message("New Means:")
  print(apply(x[,vars], 2, function(x) mean(x, na.rm = TRUE)))
  } else {
    message("Old Means:") 
    print(mean(x[,vars], na.rm = TRUE))
    x[,vars] <-  plyr::mapvalues(x[,vars], from = old_values, to = new_values)
    message("New Means:")
    print(mean(x[,vars], na.rm = TRUE))
  }
  return(x)
}
sum_subscale <- function(x, pat, new_col) {
  x[,new_col] <- apply(x[grep(pat, names(x))], 1, function(x) sum(x, na.rm = TRUE))
  return(x)
}
mean_subscale <- function(x, pat, new_col) {
  x[,new_col] <- apply(x[grep(pat, names(x))], 1, function(x) mean(x, na.rm = TRUE))
  return(x)
}
get_vars <- function(x, y, ...){
 .out <- names(x)[grepl(y, names(x), ...)]
 return(.out)
}
# Import Merged Data
pre <- read.csv("Pretestv.csv")
all.data <- read.csv("MergedData.csv")

#Directions
# ' Sum Locus Scores
all.data <- sum_subscale(all.data, "Locus", "Locus_Total")
# ' Reverse code SHS4 c(7,6,5,3,2,1) = c(1,2,3,5,6,7)
all.data <- recode_variables(all.data, "SHS4", c(7,6,5,3,2,1), c(1,2,3,5,6,7))
# ' Compute mean of Happiness (SHS)
all.data <- mean_subscale(all.data, "SHS", "SHS_Mean")
# ' Reverse code LOT_2, LOT_4, LOT_5; sum
all.data <- recode_variables(all.data, c("LOTR_2","LOTR_4","LOTR_5"), c(5,4,2,1), c(1,2,4,5))
all.data <- sum_subscale(all.data , "LOTR", "LOTR_Total")
# ' BSCS: 2,3,4,5,7,9,10,12,13, reverse coded; total;
all.data <- recode_variables(all.data, c("BSCS_2","BSCS_3","BSCS_4","BSCS_5",
                                         "BSCS_7","BSCS_9","BSCS_10","BSCS_12",
                                         "BSCS_13"), c(7,6,5,3,2,1), c(1,2,3,5,6,7))
all.data <- sum_subscale(all.data, "BSCS", "BSCS_Total")
# ' SWLS: sum of 5 items
all.data <- sum_subscale(all.data, "SWLS", "SWLS_Total")
# ' SLS: sum of 5 items
all.data <- sum_subscale(all.data, "SLS", "SLS_Total")
# ' GQ: reverse code items 3 and 6; sum of 6 items
all.data <- recode_variables(all.data, c("Gratitude_3","Gratitude_6"), c(7,6,5,3,2,1), c(1,2,3,5,6,7))
all.data <- sum_subscale(all.data, "Gratitude", "Gratitude_Total")
# ' Flourishing: sum of 8 items
all.data <- sum_subscale(all.data, "Flourishing", "Flourishing_Total")
# ' Stress mindset: should be 0-4 not 1-5; reverse code 1,3,5,7; mean of 8 items
all.data <- recode_variables(all.data, get_vars(all.data, "Str."), c(5,4,3,2,1), c(4,3,2,1,0))
all.data <- recode_variables(all.data, c("Str.Mind_1","Str.Mind_3","Str.Mind_5","Str.Mind_7"), c(4,3,1,0), c(0,1,3,4))
all.data <- mean_subscale(all.data, "Str.Mind", "Str.Mind_Mean")
# ' GSE: recode to 1-4 (was 0-3), reverse score 2,5,6,8,9; sum 10 items 
all.data <- recode_variables(all.data, c("GSE_2","GSE_5","GSE_6","GSE_8","GSE_9"), c(4,3,2,1), c(1,2,3,4))
all.data <- sum_subscale(all.data, "GSE", "GSE_Total")
# ' GAD: sum
all.data <- sum_subscale(all.data, "GAD", "GAD_Total")
# ' CESD: recode to 0-3 (was 1-4) reverse code 5, 8,13; sum
all.data <- recode_variables(all.data, get_vars(all.data, "CESD"), c(4,3,2,1), c(3,2,1,0))
all.data <- recode_variables(all.data, c("CESD_5","CESD_8","CESD_13"), c(0,1,2,3), c(3,2,1,0))
all.data <- sum_subscale(all.data, "CESD", "CESD_Total")
# ' EBDS: sum; higher scores indicate worse health
all.data <- sum_subscale(all.data, "EBDS", "EBDS_Total")
# ' Prosocial Behavior: sum; 
all.data <- sum_subscale(all.data, "Prosocial", "Prosocial_Total")
# ' TEQ: recode to 0-4; sum; 2, 4, 7, 10, 11, 12, 14, 15 are reverse coded
all.data <- recode_variables(all.data, get_vars(all.data, "TEQ"), c(5,4,3,2,1), c(4,3,2,1,0))
all.data <- recode_variables(all.data, c("TEQ_2","TEQ_4","TEQ_7","TEQ_10",
                                         "TEQ_11","TEQ_12","TEQ_14","TEQ_15"), c(4,3,1,0), c(0,1,3,4))
# ' AHS: Items 2, 9, 10, and 12 make up the agency subscale. Items 1, 4, 6, and 8 make up the pathway subscale. 
# ' Researchers can either examine results at the subscale level or combine the two subscales to...
# ' create a total hope score. Other 4 items are fillers.
all.data <- sum_subscale(all.data, "AHS_2|AHS_9|AHS_10|AHS_12", "AHS_Agency_Total")
all.data <- sum_subscale(all.data, "AHS_1|AHS_4|AHS_6|AHS_8", "AHS_Pathway_Total")
# ' SBI: 2 different ways; look in Fred Bryant's email.
# 'I think we should go with the one that doesn't have +/- scores (i.e., the second method.. 
# 'reverse score 12 even items; average of all 24 items)
all.data <- recode_variables(all.data, sprintf("SBI_%s", seq(2,24,2)), c(7,6,5,3,2,1), c(1,2,3,5,6,7))
all.data <- mean_subscale(all.data, "SBI", "SBI_Mean")
# ' MLQ: Item 9 is reverse scored. Items 1, 4, 5, 6, & 9 make up the Presence of Meaning subscale
# ' Items 2, 3, 7, 8, & 10 make up the Search for Meaning subscale; Scoring is kept continuous.
all.data <- recode_variables(all.data, "MLQ_9", c(7,6,5,3,2,1), c(1,2,3,5,6,7))
all.data <- sum_subscale(all.data, "MLQ_1|MLQ_4|MLQ_5|MLQ_6|MLQ_9", "MLQ_Presence_Total")
all.data <- sum_subscale(all.data, "MLQ_2|MLQ_3|MLQ_7|MLQ_8|MLQ_10", "MLQ_Search_Total")
# 'BFI: BFI scale scoring ("R" denotes reverse-scored items):
BFIR <- c("BFI_6","BFI_21","BFI_31","BFI_2","BFI_12",
          "BFI_27","BFI_37","BFI_8","BFI_18","BFI_23",
          "BFI_43","BFI_9","BFI_24","BFI_34","BFI_35", "BFI_41")
pre <- recode_variables(pre, BFIR, c(5,4,2,1), c(1,2,4,5))  
#Extraversion: 1, 6R, 11, 16, 21R, 26, 31R, 36 
pre <- sum_subscale(pre, paste(sprintf("BFI_%s",seq(1,36,5)), collapse = "|"), "BFI_Extraversion_Total")
  #Agreeableness: 2R, 7, 12R, 17, 22, 27R, 32, 37R, 42
pre <- sum_subscale(pre, paste(sprintf("BFI_%s",seq(2,42,5)), collapse = "|"), "BFI_Agreeableness_Total")
  #Conscientiousness: 3, 8R, 13, 18R, 23R, 28, 33, 38, 43R
pre <- sum_subscale(pre, paste(sprintf("BFI_%s",seq(13,43,5)), collapse = "|"), "BFI_Conscientiousness_Total")
  #Neuroticism: 4, 9R, 14, 19, 24R, 29, 34R, 39
pre <- sum_subscale(pre, paste(sprintf("BFI_%s",seq(4,39,5)), collapse = "|"), "BFI_Neuroticism_Total")
  #Openness: 5, 10, 15, 20, 25, 30, 35R, 40, 41R, 44 
pre <- sum_subscale(pre, paste(sprintf("BFI_%s",c(seq(5,40,5),41,44)), collapse = "|"), "BFI_Openness_Total")
#get incrimental data between two files
pre <- pre[c("Email",setdiff(names(pre),names(all.data)))]
#write files
write.csv(all.data, "MergedDataCleaned.csv", row.names = FALSE)
write.csv(pre, "PretestDataCleaned.csv", row.names = FALSE)
