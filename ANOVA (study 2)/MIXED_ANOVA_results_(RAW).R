##### MIXED #####
library(dplyr)
library(tidyverse)

# All ANOVA results (raw data)
###################################################################
########################## ESTIMATES ##############################
###################################################################
########################### WITH CROSSLOADING #####################
########################################################

# Inladen bestanden met ruwe factor ladingen en verander WLS naar DWLS: 
data_withC_raw <- read.csv("MIX_aov_withC_estR1_12.csv")
data_withC_raw$method <- factor(data_withC_raw$method)
levels(data_withC_raw$method) <- c("PML", "DWLS")
write_csv(data_withC_raw, "MIX_aov_withC_estR1_12.csv")

# create factors
data_withC_raw$method <- factor(data_withC_raw$method)
data_withC_raw$factors <- factor(data_withC_raw$factors)
data_withC_raw$nobs <- factor(data_withC_raw$nobs)
data_withC_raw$ncat <- factor(data_withC_raw$ncat)

names(data_withC_raw)
data_withC_raw$method

data_withC_raw_PML <- data_withC_raw[data_withC_raw$method == "PML",]

#change order (row 1 to 12)
data_withC_raw_PML <- data_withC_raw_PML %>% arrange(factors) %>% arrange(nobs)
str(data_withC_raw_PML)
View(data_withC_raw_PML)

# create labels values
data_withC_raw_PML$Row <- rep(1:12, each = 250)

# add labs value
data_withC_raw_PML <- select(data_withC_raw_PML, Row, method, factors, nobs, withC_L2, withC_L6, withC_La, withC_L8, withC_L12, 
                             withC_Ti8.b, withC_Ti12.b, withC_C1, 
                             withC_Errvar2, withC_Errvar6)

data_withC_raw_PML$Row <- as.factor(data_withC_raw_PML$Row)

# calculating raw means
Raw_means_withC_L2 <- data_withC_raw_PML %>% 
  group_by(Row) %>% 
  summarize(mean(withC_L2)) 

Raw_means_withC_L6 <- data_withC_raw_PML %>% 
  group_by(Row) %>% 
  summarize(mean(withC_L6)) 

Raw_means_withC_La <- data_withC_raw_PML %>% 
  group_by(Row) %>% 
  summarize(mean(withC_La))

Raw_means_withC_L8 <- data_withC_raw_PML %>% 
  group_by(Row) %>% 
  summarize(mean(withC_L8))

Raw_means_withC_L12 <- data_withC_raw_PML %>% 
  group_by(Row) %>% 
  summarize(mean(withC_L12))

Raw_means_withC_Ti8.b <- data_withC_raw_PML %>% 
  group_by(Row) %>% 
  summarize(mean(withC_Ti8.b))

Raw_means_withC_Ti12.b <- data_withC_raw_PML %>% 
  group_by(Row) %>% 
  summarize(mean(withC_Ti12.b)) 

Raw_means_withC_C1 <- data_withC_raw_PML %>% 
  group_by(Row) %>% 
  summarize(mean(withC_C1))

Raw_means_withC_Errvar2 <- data_withC_raw_PML %>% 
  group_by(Row) %>% 
  summarize(mean(withC_Errvar2)) 

Raw_means_withC_Errvar6 <- data_withC_raw_PML %>% 
  group_by(Row) %>% 
  summarize(mean(withC_Errvar6))


#combine all means
Raw_means_withC_PML <- cbind(Raw_means_withC_L2, Raw_means_withC_L6, Raw_means_withC_La, Raw_means_withC_L8, Raw_means_withC_L12, Raw_means_withC_Ti8.b, Raw_means_withC_Ti12.b, Raw_means_withC_C1, Raw_means_withC_Errvar2, Raw_means_withC_Errvar6)

# keeping 1 Row column
Raw_means_withC_PML <- Raw_means_withC_PML %>% select(1,'mean(withC_L2)', 'mean(withC_L6)', 'mean(withC_La)', 'mean(withC_L8)', 'mean(withC_L12)', 'mean(withC_Ti8.b)', 'mean(withC_Ti12.b)', 'mean(withC_C1)', 'mean(withC_Errvar2)', 'mean(withC_Errvar6)')

colnames(Raw_means_withC_PML) <- c("Row","est_withC_L2_PML", "est_withC_L6_PML", "est_withC_La_PML", "est_withC_L8_PML", "est_withC_L12_PML", "est_withC_Ti8.b_PML", "est_withC_Ti12.b_PML", "est_withC_C1_PML", "est_withC_Errvar2_PML", "est_withC_Errvar6_PML")

write.csv(Raw_means_withC_PML, file = "Raw_means_withC_PML.csv")

# BIAS #

# calculate bias (Est withC)
data_withC_raw_PML$b_withC_L2 <- (data_withC_raw_PML$withC_L2 - 0.8)
data_withC_raw_PML$b_withC_L6 <- (data_withC_raw_PML$withC_L6 - 0.6)
data_withC_raw_PML$b_withC_La <- (data_withC_raw_PML$withC_La- 0.2)
data_withC_raw_PML$b_withC_L8 <- (data_withC_raw_PML$withC_L8 - 0.8)
data_withC_raw_PML$b_withC_L12 <- (data_withC_raw_PML$withC_L12- 0.6)
data_withC_raw_PML$b_withC_Ti8.b <- (data_withC_raw_PML$withC_Ti8.b - 0)
data_withC_raw_PML$b_withC_Ti12.b <- (data_withC_raw_PML$withC_Ti12.b - 0)
data_withC_raw_PML$b_withC_C1 <- (data_withC_raw_PML$withC_C1 - 0.3)
data_withC_raw_PML$b_withC_Errvar2 <- (data_withC_raw_PML$withC_Errvar2 - 0.36)
data_withC_raw_PML$b_withC_Errvar6 <- (data_withC_raw_PML$withC_Errvar6 - 0.528)

# add Row number 
new_df <- select(data_withC_raw_PML, Row, method, factors, nobs, b_withC_L2, b_withC_L6, b_withC_La, b_withC_L8, b_withC_L12, b_withC_Ti8.b, b_withC_Ti12.b, b_withC_C1, b_withC_Errvar2, b_withC_Errvar6)
new_df$Row <- rep(1:12, each = 250)

# calculating means of bias
Means_withC_L2 <- new_df %>% 
  group_by(Row) %>% 
  summarize(mean(b_withC_L2))

Means_withC_L6 <- new_df %>% 
  group_by(Row) %>% 
  summarize(mean(b_withC_L6)) 

Means_withC_La <- new_df %>% 
  group_by(Row) %>% 
  summarize(mean(b_withC_La)) 

Means_withC_L8 <- new_df %>% 
  group_by(Row) %>% 
  summarize(mean(b_withC_L8)) 

Means_withC_L12 <- new_df %>% 
  group_by(Row) %>% 
  summarize(mean(b_withC_L12)) 

Means_withC_Ti8.b <- new_df %>% 
  group_by(Row) %>% 
  summarize(mean(b_withC_Ti8.b)) 

Means_withC_Ti12.b <- new_df %>% 
  group_by(Row) %>% 
  summarize(mean(b_withC_Ti12.b))

Means_withC_C1 <- new_df %>% 
  group_by(Row) %>% 
  summarize(mean(b_withC_C1))

Means_withC_Errvar2 <- new_df %>% 
  group_by(Row) %>% 
  summarize(mean(b_withC_Errvar2)) 

Means_withC_Errvar6 <- new_df %>% 
  group_by(Row) %>% 
  summarize(mean(b_withC_Errvar6))


#combine all means of bias
Means_withC_PML <- cbind(Means_withC_L2, Means_withC_L6, Means_withC_La, Means_withC_L8, Means_withC_L12, Means_withC_Ti8.b, Means_withC_Ti12.b, Means_withC_C1, Means_withC_Errvar2, Means_withC_Errvar6)

# keeping 1 Row column
Means_withC_PML <- Means_withC_PML %>% select(1,'mean(b_withC_L2)', 'mean(b_withC_L6)', 'mean(b_withC_La)', 'mean(b_withC_L8)', 'mean(b_withC_L12)', 'mean(b_withC_Ti8.b)', 'mean(b_withC_Ti12.b)', 'mean(b_withC_C1)', 'mean(b_withC_Errvar2)', 'mean(b_withC_Errvar6)')

# rename columns
colnames(Means_withC_PML) <- c("Row", "bias_withC_L2_PML", "bias_withC_L6_PML", "bias_withC_La_PML", "bias_withC_L8_PML", "bias_withC_L12_PML", "bias_withC_Ti8.b_PML", "bias_withC_Ti12.b_PML", "bias_withC_C1_PML", "bias_withC_Errvar2_PML", "bias_withC_Errvar6_PML")

write.csv(Means_withC_PML, file = "Means_withC_PML.csv")


######################################## DWLS
# All ANOVA results (raw data)
data_withC_raw <- read.csv("MIX_aov_withC_estR1_12.csv") 

data_withC_raw$method <- factor(data_withC_raw$method)
data_withC_raw$factors <- factor(data_withC_raw$factors)
data_withC_raw$nobs <- factor(data_withC_raw$nobs)
data_withC_raw$ncat <- factor(data_withC_raw$ncat)

names(data_withC_raw)
data_withC_raw$method

data_withC_raw_WLS <- data_withC_raw[data_withC_raw$method == "DWLS",]

#change order (row 1 to 12)
data_withC_raw_WLS <- data_withC_raw_WLS %>% arrange(factors) %>% arrange(nobs)
str(data_withC_raw_WLS)
View(data_withC_raw_WLS)

# create labels values
data_withC_raw_WLS$Row <- rep(1:12, each = 250)

# add labs value
data_withC_raw_WLS <- select(data_withC_raw_WLS, Row, method, factors, nobs, withC_L2, withC_L6, 
                             withC_La, withC_L8, withC_L12, 
                             withC_Ti8.b, withC_Ti12.b, withC_C1, 
                             withC_Errvar2, withC_Errvar6)

data_withC_raw_WLS$Row <- as.factor(data_withC_raw_WLS$Row)

# calculating raw means
Raw_means_withC_L2 <- data_withC_raw_WLS %>% 
  group_by(Row) %>% 
  summarize(mean(withC_L2)) 

Raw_means_withC_L6 <- data_withC_raw_WLS %>% 
  group_by(Row) %>% 
  summarize(mean(withC_L6)) 

Raw_means_withC_La <- data_withC_raw_WLS %>% 
  group_by(Row) %>% 
  summarize(mean(withC_La))

Raw_means_withC_L8 <- data_withC_raw_WLS %>% 
  group_by(Row) %>% 
  summarize(mean(withC_L8))

Raw_means_withC_L12 <- data_withC_raw_WLS %>% 
  group_by(Row) %>% 
  summarize(mean(withC_L12))

Raw_means_withC_Ti8.b <- data_withC_raw_WLS %>% 
  group_by(Row) %>% 
  summarize(mean(withC_Ti8.b))

Raw_means_withC_Ti12.b <- data_withC_raw_WLS %>% 
  group_by(Row) %>% 
  summarize(mean(withC_Ti12.b)) 

Raw_means_withC_C1 <- data_withC_raw_WLS %>% 
  group_by(Row) %>% 
  summarize(mean(withC_C1))

Raw_means_withC_Errvar2 <- data_withC_raw_WLS %>% 
  group_by(Row) %>% 
  summarize(mean(withC_Errvar2)) 

Raw_means_withC_Errvar6 <- data_withC_raw_WLS %>% 
  group_by(Row) %>% 
  summarize(mean(withC_Errvar6))

#combine all means
Raw_means_withC_WLS <- cbind(Raw_means_withC_L2, Raw_means_withC_L6, Raw_means_withC_La, Raw_means_withC_L8, Raw_means_withC_L12, Raw_means_withC_Ti8.b, Raw_means_withC_Ti12.b, Raw_means_withC_C1, Raw_means_withC_Errvar2, Raw_means_withC_Errvar6)

# keeping 1 Row column
Raw_means_withC_WLS <- Raw_means_withC_WLS %>% select(1,'mean(withC_L2)', 'mean(withC_L6)', 'mean(withC_La)', 'mean(withC_L8)', 'mean(withC_L12)', 'mean(withC_Ti8.b)', 'mean(withC_Ti12.b)', 'mean(withC_C1)', 'mean(withC_Errvar2)', 'mean(withC_Errvar6)')

colnames(Raw_means_withC_WLS) <- c("Row","est_withC_L2_WLS", "est_withC_L6_WLS", "est_withC_La_WLS", "est_withC_L8_WLS", "est_withC_L12_WLS", "est_withC_Ti8.b_WLS", "est_withC_Ti12.b_WLS", "est_withC_C1_WLS", "est_withC_Errvar2_WLS", "est_withC_Errvar6_WLS")

write.csv(Raw_means_withC_WLS, file = "Raw_means_withC_WLS.csv")

# BIAS #

# calculate bias (Est withC)
data_withC_raw_WLS$b_withC_L2 <- (data_withC_raw_WLS$withC_L2 - 0.8)
data_withC_raw_WLS$b_withC_L6 <- (data_withC_raw_WLS$withC_L6 - 0.6)
data_withC_raw_WLS$b_withC_La <- (data_withC_raw_WLS$withC_La- 0.2)
data_withC_raw_WLS$b_withC_L8 <- (data_withC_raw_WLS$withC_L8 - 0.8)
data_withC_raw_WLS$b_withC_L12 <- (data_withC_raw_WLS$withC_L12- 0.6)
data_withC_raw_WLS$b_withC_Ti8.b <- (data_withC_raw_WLS$withC_Ti8.b - 0)
data_withC_raw_WLS$b_withC_Ti12.b <- (data_withC_raw_WLS$withC_Ti12.b - 0)
data_withC_raw_WLS$b_withC_C1 <- (data_withC_raw_WLS$withC_C1 - 0.3)
data_withC_raw_WLS$b_withC_Errvar2 <- (data_withC_raw_WLS$withC_Errvar2 - 0.36)
data_withC_raw_WLS$b_withC_Errvar6 <- (data_withC_raw_WLS$withC_Errvar6 - 0.528)

# add Row number 
new_df <- select(data_withC_raw_WLS, Row, method, factors, nobs, b_withC_L2, b_withC_L6, b_withC_La, b_withC_L8, b_withC_L12, b_withC_Ti8.b, b_withC_Ti12.b, b_withC_C1, b_withC_Errvar2, b_withC_Errvar6)
new_df$Row <- rep(1:12, each = 250)

# calculating means of bias
Means_withC_L2 <- new_df %>% 
  group_by(Row) %>% 
  summarize(mean(b_withC_L2))

Means_withC_L6 <- new_df %>% 
  group_by(Row) %>% 
  summarize(mean(b_withC_L6)) 

Means_withC_La <- new_df %>% 
  group_by(Row) %>% 
  summarize(mean(b_withC_La)) 

Means_withC_L8 <- new_df %>% 
  group_by(Row) %>% 
  summarize(mean(b_withC_L8)) 

Means_withC_L12 <- new_df %>% 
  group_by(Row) %>% 
  summarize(mean(b_withC_L12)) 

Means_withC_Ti8.b <- new_df %>% 
  group_by(Row) %>% 
  summarize(mean(b_withC_Ti8.b)) 

Means_withC_Ti12.b <- new_df %>% 
  group_by(Row) %>% 
  summarize(mean(b_withC_Ti12.b))

Means_withC_C1 <- new_df %>% 
  group_by(Row) %>% 
  summarize(mean(b_withC_C1))

Means_withC_Errvar2 <- new_df %>% 
  group_by(Row) %>% 
  summarize(mean(b_withC_Errvar2)) 

Means_withC_Errvar6 <- new_df %>% 
  group_by(Row) %>% 
  summarize(mean(b_withC_Errvar6))


#combine all means of bias
Means_withC_WLS <- cbind(Means_withC_L2, Means_withC_L6, Means_withC_La, Means_withC_L8, Means_withC_L12, Means_withC_Ti8.b, Means_withC_Ti12.b, Means_withC_C1, Means_withC_Errvar2, Means_withC_Errvar6)

# keeping 1 Row column
Means_withC_WLS <- Means_withC_WLS %>% select(1,'mean(b_withC_L2)', 'mean(b_withC_L6)', 'mean(b_withC_La)', 'mean(b_withC_L8)', 'mean(b_withC_L12)', 'mean(b_withC_Ti8.b)', 'mean(b_withC_Ti12.b)', 'mean(b_withC_C1)', 'mean(b_withC_Errvar2)', 'mean(b_withC_Errvar6)')

# rename columns
colnames(Means_withC_WLS) <- c("Row", "bias_withC_L2_WLS", "bias_withC_L6_WLS", "bias_withC_La_WLS", "bias_withC_L8_WLS", "bias_withC_L12_WLS", "bias_withC_Ti8.b_WLS", "bias_withC_Ti12.b_WLS", "bias_withC_C1_WLS", "bias_withC_Errvar2_WLS", "bias_withC_Errvar6_WLS")

write.csv(Means_withC_WLS, file = "Means_withC_WLS.csv")


### MERGING ALL results
list.files()
Means_withC_PML <- read.csv("Means_withC_PML.csv")
Means_withC_WLS <- read.csv("Means_withC_WLS.csv")
Raw_means_withC_PML <- read.csv("Raw_means_withC_PML.csv")
Raw_means_withC_WLS <- read.csv("Raw_means_withC_WLS.csv")

# create column with true values
true_withC_L2 <- rep(0.8, 12)
true_withC_L6 <- rep(0.6, 12)
true_withC_La <- rep(0.2, 12)
true_withC_L8 <- rep(0.8, 12)
true_withC_L12 <- rep(0.6, 12)
true_withC_Ti8 <- rep(0, 12)
true_withC_Ti12 <- rep(0, 12)
true_withC_C1 <- rep(0.3, 12)
true_withC_Errvar2 <- rep(0.36, 12)
true_withC_Errvar6 <- rep(0.528, 12)

merged <- cbind(Means_withC_PML, 
                Means_withC_WLS, 
                Raw_means_withC_PML, 
                Raw_means_withC_WLS, 
                true_withC_L2,
                true_withC_L6, 
                true_withC_La, 
                true_withC_L8,
                true_withC_L12,
                true_withC_Ti8, 
                true_withC_Ti12, 
                true_withC_C1,
                true_withC_Errvar2, 
                true_withC_Errvar6)

merged2 <- merged %>% select(2, 
                             true_withC_L2, 
                             est_withC_L2_WLS,
                             bias_withC_L2_WLS,
                             est_withC_L2_PML, 
                             bias_withC_L2_PML, 
                             
                             true_withC_L6, 
                             est_withC_L6_WLS, 
                             bias_withC_L6_WLS,
                             est_withC_L6_PML, 
                             bias_withC_L6_PML, 
                             
                             true_withC_La, 
                             est_withC_La_WLS,
                             bias_withC_La_WLS,
                             est_withC_La_PML, 
                             bias_withC_La_PML, 
                             
                             true_withC_L8, 
                             est_withC_L8_WLS, 
                             bias_withC_L8_WLS,
                             est_withC_L8_PML, 
                             bias_withC_L8_PML, 
                             
                             true_withC_L12, 
                             est_withC_L12_WLS,
                             bias_withC_L12_WLS,
                             est_withC_L12_PML, 
                             bias_withC_L12_PML, 
                             
                             true_withC_Ti8,
                             est_withC_Ti8.b_WLS,
                             bias_withC_Ti8.b_WLS,
                             est_withC_Ti8.b_PML, 
                             bias_withC_Ti8.b_PML,
                             
                             true_withC_Ti12, 
                             est_withC_Ti12.b_WLS,
                             bias_withC_Ti12.b_WLS,
                             est_withC_Ti12.b_PML, 
                             bias_withC_Ti12.b_PML,
                             
                             true_withC_C1, 
                             est_withC_C1_WLS,
                             bias_withC_C1_WLS,
                             est_withC_C1_PML, 
                             bias_withC_C1_PML,
                             
                             true_withC_Errvar2, 
                             est_withC_Errvar2_WLS,
                             bias_withC_Errvar2_WLS,
                             est_withC_Errvar2_PML, 
                             bias_withC_Errvar2_PML,
                             
                             true_withC_Errvar6, 
                             est_withC_Errvar6_WLS,
                             bias_withC_Errvar6_WLS,
                             est_withC_Errvar6_PML, 
                             bias_withC_Errvar6_PML)

MIX_withC_perRow_Table <- write.csv(merged2, "MIX_withC_perRow_Table.csv")

MIX_withC_perRow_Table <- read_csv("MIX_withC_perRow_Table.csv")
MIX_withC_perRow_Table <- as.data.frame(MIX_withC_perRow_Table)
MIX_withC_perRow_Table <- MIX_withC_perRow_Table %>% mutate_if(is.numeric,round, digits = 2)
MIX_withC_perRow_Table <- write.csv(MIX_withC_perRow_Table, "MIX_withC_perRow_Table-.csv")

# transposing matrix
MIX_withC_perRow_Table <- read_csv("MIX_withC_perRow_Table-.csv")
MIX_withC_perRow_Table_transpose <- as.data.frame(t(as.matrix(MIX_withC_perRow_Table)))
write.csv(MIX_withC_perRow_Table_transpose, "MIX_withC_perRow_Table_T.csv")

# remove first few rows
MIX_withC_perRow_Table_T <- read_csv("MIX_withC_perRow_Table_T.csv")
MIX_withC_perRow_Table_T <- MIX_withC_perRow_Table_T[-1,]
MIX_withC_perRow_Table_T <- MIX_withC_perRow_Table_T[-1,]
MIX_withC_perRow_Table_T <- MIX_withC_perRow_Table_T[-1,]

Rows <- 1:12
colnames(MIX_withC_perRow_Table_T) <- c("Row", Rows)

write.csv(MIX_withC_perRow_Table_T, "MIX_withC_perRow_Table_T.csv")

# OTHER (shared) FOLDER:
df <- read.csv("MIX_withC_perRow_Table_T.csv", header = TRUE)

# delete first column, change column names
df <- df[,-1]
Rows <- 1:12
colnames(df) <- c("Row", Rows)

# separate first column into 4 factors
df <- df %>% separate(Row, c("Value", "Model", "Parameter", "Method"), sep = "_")

df <- df[,-2]
df[is.na(df)] = "Both"

# change names
df$Value <- as.factor(df$Value)
levels(df$Value) <- c("Raw bias", "Estimate", "True")

df$Method <- as.factor(df$Method)
levels(df$Method) <- c("Both", "PML", "WLS")

means <- df %>% keep(is.numeric) %>% rowMeans()
df$Mean <- means

write.csv(df, "MIX_withC_perRow_Table_T.csv")


###########################################################
########################### WITHOUT CROSSLOADING #####################
###########################################################
data_withoutC_raw <- read.csv("MIX_aov_withoutC_estR1_12.csv")
data_withoutC_raw$method <- factor(data_withoutC_raw$method)
levels(data_withoutC_raw$method) <- c("PML", "DWLS")
write_csv(data_withoutC_raw, "MIX_aov_withoutC_estR1_12.csv")

# create factors
data_withoutC_raw$method <- factor(data_withoutC_raw$method)
data_withoutC_raw$factors <- factor(data_withoutC_raw$factors)
data_withoutC_raw$nobs <- factor(data_withoutC_raw$nobs)
data_withoutC_raw$ncat <- factor(data_withoutC_raw$ncat)

names(data_withoutC_raw)
data_withoutC_raw$method

data_withoutC_raw_PML <- data_withoutC_raw[data_withoutC_raw$method == "PML",]

#change order (row 1 to 12)
data_withoutC_raw_PML <- data_withoutC_raw_PML %>% arrange(factors) %>% arrange(nobs)
str(data_withoutC_raw_PML)
View(data_withoutC_raw_PML)

# create labels values
data_withoutC_raw_PML$Row <- rep(1:12, each = 250)

# add labs value
data_withoutC_raw_PML <- select(data_withoutC_raw_PML, Row, method, factors, nobs, withoutC_L2, withoutC_L6, 
                                withoutC_L8, withoutC_L12, 
                                withoutC_Ti8.b, withoutC_Ti12.b, withoutC_C1, 
                                withoutC_Errvar2, withoutC_Errvar6)

data_withoutC_raw_PML$Row <- as.factor(data_withoutC_raw_PML$Row)

# calculating raw means
Raw_means_withoutC_L2 <- data_withoutC_raw_PML %>% 
  group_by(Row) %>% 
  summarize(mean(withoutC_L2)) 

Raw_means_withoutC_L6 <- data_withoutC_raw_PML %>% 
  group_by(Row) %>% 
  summarize(mean(withoutC_L6)) 

Raw_means_withoutC_L8 <- data_withoutC_raw_PML %>% 
  group_by(Row) %>% 
  summarize(mean(withoutC_L8))

Raw_means_withoutC_L12 <- data_withoutC_raw_PML %>% 
  group_by(Row) %>% 
  summarize(mean(withoutC_L12))

Raw_means_withoutC_Ti8.b <- data_withoutC_raw_PML %>% 
  group_by(Row) %>% 
  summarize(mean(withoutC_Ti8.b))

Raw_means_withoutC_Ti12.b <- data_withoutC_raw_PML %>% 
  group_by(Row) %>% 
  summarize(mean(withoutC_Ti12.b)) 

Raw_means_withoutC_C1 <- data_withoutC_raw_PML %>% 
  group_by(Row) %>% 
  summarize(mean(withoutC_C1))

Raw_means_withoutC_Errvar2 <- data_withoutC_raw_PML %>% 
  group_by(Row) %>% 
  summarize(mean(withoutC_Errvar2)) 

Raw_means_withoutC_Errvar6 <- data_withoutC_raw_PML %>% 
  group_by(Row) %>% 
  summarize(mean(withoutC_Errvar6))

#combine all means
Raw_means_withoutC_PML <- cbind(Raw_means_withoutC_L2, Raw_means_withoutC_L6, Raw_means_withoutC_L8, Raw_means_withoutC_L12, Raw_means_withoutC_Ti8.b, Raw_means_withoutC_Ti12.b, Raw_means_withoutC_C1, Raw_means_withoutC_Errvar2, Raw_means_withoutC_Errvar6)

# keeping 1 Row column
Raw_means_withoutC_PML <- Raw_means_withoutC_PML %>% select(1,'mean(withoutC_L2)', 'mean(withoutC_L6)', 'mean(withoutC_L8)', 'mean(withoutC_L12)', 'mean(withoutC_Ti8.b)', 'mean(withoutC_Ti12.b)', 'mean(withoutC_C1)', 'mean(withoutC_Errvar2)', 'mean(withoutC_Errvar6)')

colnames(Raw_means_withoutC_PML) <- c("Row","est_withoutC_L2_PML", "est_withoutC_L6_PML", "est_withoutC_L8_PML", "est_withoutC_L12_PML", "est_withoutC_Ti8.b_PML", "est_withoutC_Ti12.b_PML", "est_withoutC_C1_PML", "est_withoutC_Errvar2_PML", "est_withoutC_Errvar6_PML")

write.csv(Raw_means_withoutC_PML, file = "Raw_means_withoutC_PML.csv")

# BIAS #

# calculate bias (Est withoutC)
data_withoutC_raw_PML$b_withoutC_L2 <- (data_withoutC_raw_PML$withoutC_L2 - 0.8)
data_withoutC_raw_PML$b_withoutC_L6 <- (data_withoutC_raw_PML$withoutC_L6 - 0.6)
data_withoutC_raw_PML$b_withoutC_L8 <- (data_withoutC_raw_PML$withoutC_L8 - 0.8)
data_withoutC_raw_PML$b_withoutC_L12 <- (data_withoutC_raw_PML$withoutC_L12- 0.6)
data_withoutC_raw_PML$b_withoutC_Ti8.b <- (data_withoutC_raw_PML$withoutC_Ti8.b - 0)
data_withoutC_raw_PML$b_withoutC_Ti12.b <- (data_withoutC_raw_PML$withoutC_Ti12.b - 0)
data_withoutC_raw_PML$b_withoutC_C1 <- (data_withoutC_raw_PML$withoutC_C1 - 0.3)
data_withoutC_raw_PML$b_withoutC_Errvar2 <- (data_withoutC_raw_PML$withoutC_Errvar2 - 0.36)
data_withoutC_raw_PML$b_withoutC_Errvar6 <- (data_withoutC_raw_PML$withoutC_Errvar6 - 0.528)

# add Row number 
new_df <- select(data_withoutC_raw_PML, Row, method, factors, nobs, b_withoutC_L2, b_withoutC_L6, b_withoutC_L8, b_withoutC_L12, b_withoutC_Ti8.b, b_withoutC_Ti12.b, b_withoutC_C1, b_withoutC_Errvar2, b_withoutC_Errvar6)
new_df$Row <- rep(1:12, each = 250)

# calculating means of bias
Means_withoutC_L2 <- new_df %>% 
  group_by(Row) %>% 
  summarize(mean(b_withoutC_L2))

Means_withoutC_L6 <- new_df %>% 
  group_by(Row) %>% 
  summarize(mean(b_withoutC_L6)) 

Means_withoutC_L8 <- new_df %>% 
  group_by(Row) %>% 
  summarize(mean(b_withoutC_L8)) 

Means_withoutC_L12 <- new_df %>% 
  group_by(Row) %>% 
  summarize(mean(b_withoutC_L12)) 

Means_withoutC_Ti8.b <- new_df %>% 
  group_by(Row) %>% 
  summarize(mean(b_withoutC_Ti8.b)) 

Means_withoutC_Ti12.b <- new_df %>% 
  group_by(Row) %>% 
  summarize(mean(b_withoutC_Ti12.b))

Means_withoutC_C1 <- new_df %>% 
  group_by(Row) %>% 
  summarize(mean(b_withoutC_C1))

Means_withoutC_Errvar2 <- new_df %>% 
  group_by(Row) %>% 
  summarize(mean(b_withoutC_Errvar2)) 

Means_withoutC_Errvar6 <- new_df %>% 
  group_by(Row) %>% 
  summarize(mean(b_withoutC_Errvar6))


#combine all means of bias
Means_withoutC_PML <- cbind(Means_withoutC_L2, Means_withoutC_L6, Means_withoutC_L8, Means_withoutC_L12, Means_withoutC_Ti8.b, Means_withoutC_Ti12.b, Means_withoutC_C1, Means_withoutC_Errvar2, Means_withoutC_Errvar6)

# keeping 1 Row column
Means_withoutC_PML <- Means_withoutC_PML %>% select(1,'mean(b_withoutC_L2)', 'mean(b_withoutC_L6)', 'mean(b_withoutC_L8)', 'mean(b_withoutC_L12)', 'mean(b_withoutC_Ti8.b)', 'mean(b_withoutC_Ti12.b)', 'mean(b_withoutC_C1)', 'mean(b_withoutC_Errvar2)', 'mean(b_withoutC_Errvar6)')

# rename columns
colnames(Means_withoutC_PML) <- c("Row", "bias_withoutC_L2_PML", "bias_withoutC_L6_PML", "bias_withoutC_L8_PML", "bias_withoutC_L12_PML", "bias_withoutC_Ti8.b_PML", "bias_withoutC_Ti12.b_PML", "bias_withoutC_C1_PML", "bias_withoutC_Errvar2_PML", "bias_withoutC_Errvar6_PML")

write.csv(Means_withoutC_PML, file = "Means_withoutC_PML.csv")

######################################## DWLS
# All ANOVA results (raw data)
data_withoutC_raw <- read.csv("MIX_aov_withoutC_estR1_12.csv") 

data_withoutC_raw$method <- factor(data_withoutC_raw$method)
data_withoutC_raw$factors <- factor(data_withoutC_raw$factors)
data_withoutC_raw$nobs <- factor(data_withoutC_raw$nobs)
data_withoutC_raw$ncat <- factor(data_withoutC_raw$ncat)

names(data_withoutC_raw)
data_withoutC_raw$method # moet DWLS zijn!

data_withoutC_raw_WLS <- data_withoutC_raw[data_withoutC_raw$method == "DWLS",]

#change order (row 1 to 12)
data_withoutC_raw_WLS <- data_withoutC_raw_WLS %>% arrange(factors) %>% arrange(nobs)
str(data_withoutC_raw_WLS)
View(data_withoutC_raw_WLS)

# create labels values
data_withoutC_raw_WLS$Row <- rep(1:12, each = 250)

# add labs value
data_withoutC_raw_WLS <- select(data_withoutC_raw_WLS, Row, method, factors, nobs, withoutC_L2, withoutC_L6, 
                                withoutC_L8, withoutC_L12, 
                                withoutC_Ti8.b, withoutC_Ti12.b, withoutC_C1, 
                                withoutC_Errvar2, withoutC_Errvar6)

data_withoutC_raw_WLS$Row <- as.factor(data_withoutC_raw_WLS$Row)

# calculating raw means
Raw_means_withoutC_L2 <- data_withoutC_raw_WLS %>% 
  group_by(Row) %>% 
  summarize(mean(withoutC_L2)) 

Raw_means_withoutC_L6 <- data_withoutC_raw_WLS %>% 
  group_by(Row) %>% 
  summarize(mean(withoutC_L6)) 

Raw_means_withoutC_L8 <- data_withoutC_raw_WLS %>% 
  group_by(Row) %>% 
  summarize(mean(withoutC_L8))

Raw_means_withoutC_L12 <- data_withoutC_raw_WLS %>% 
  group_by(Row) %>% 
  summarize(mean(withoutC_L12))

Raw_means_withoutC_Ti8.b <- data_withoutC_raw_WLS %>% 
  group_by(Row) %>% 
  summarize(mean(withoutC_Ti8.b))

Raw_means_withoutC_Ti12.b <- data_withoutC_raw_WLS %>% 
  group_by(Row) %>% 
  summarize(mean(withoutC_Ti12.b)) 

Raw_means_withoutC_C1 <- data_withoutC_raw_WLS %>% 
  group_by(Row) %>% 
  summarize(mean(withoutC_C1))

Raw_means_withoutC_Errvar2 <- data_withoutC_raw_WLS %>% 
  group_by(Row) %>% 
  summarize(mean(withoutC_Errvar2)) 

Raw_means_withoutC_Errvar6 <- data_withoutC_raw_WLS %>% 
  group_by(Row) %>% 
  summarize(mean(withoutC_Errvar6))

#combine all means
Raw_means_withoutC_WLS <- cbind(Raw_means_withoutC_L2, Raw_means_withoutC_L6, Raw_means_withoutC_L8, Raw_means_withoutC_L12, Raw_means_withoutC_Ti8.b, Raw_means_withoutC_Ti12.b, Raw_means_withoutC_C1, Raw_means_withoutC_Errvar2, Raw_means_withoutC_Errvar6)

# keeping 1 Row column
Raw_means_withoutC_WLS <- Raw_means_withoutC_WLS %>% select(1,'mean(withoutC_L2)', 'mean(withoutC_L6)', 'mean(withoutC_L8)', 'mean(withoutC_L12)', 'mean(withoutC_Ti8.b)', 'mean(withoutC_Ti12.b)', 'mean(withoutC_C1)', 'mean(withoutC_Errvar2)', 'mean(withoutC_Errvar6)')

colnames(Raw_means_withoutC_WLS) <- c("Row","est_withoutC_L2_WLS", "est_withoutC_L6_WLS", "est_withoutC_L8_WLS", "est_withoutC_L12_WLS", "est_withoutC_Ti8.b_WLS", "est_withoutC_Ti12.b_WLS", "est_withoutC_C1_WLS", "est_withoutC_Errvar2_WLS", "est_withoutC_Errvar6_WLS")

write.csv(Raw_means_withoutC_WLS, file = "Raw_means_withoutC_WLS.csv")

# BIAS #

# calculate bias (Est withoutC)
data_withoutC_raw_WLS$b_withoutC_L2 <- (data_withoutC_raw_WLS$withoutC_L2 - 0.8)
data_withoutC_raw_WLS$b_withoutC_L6 <- (data_withoutC_raw_WLS$withoutC_L6 - 0.6)
data_withoutC_raw_WLS$b_withoutC_L8 <- (data_withoutC_raw_WLS$withoutC_L8 - 0.8)
data_withoutC_raw_WLS$b_withoutC_L12 <- (data_withoutC_raw_WLS$withoutC_L12- 0.6)
data_withoutC_raw_WLS$b_withoutC_Ti8.b <- (data_withoutC_raw_WLS$withoutC_Ti8.b - 0)
data_withoutC_raw_WLS$b_withoutC_Ti12.b <- (data_withoutC_raw_WLS$withoutC_Ti12.b - 0)
data_withoutC_raw_WLS$b_withoutC_C1 <- (data_withoutC_raw_WLS$withoutC_C1 - 0.3)
data_withoutC_raw_WLS$b_withoutC_Errvar2 <- (data_withoutC_raw_WLS$withoutC_Errvar2 - 0.36)
data_withoutC_raw_WLS$b_withoutC_Errvar6 <- (data_withoutC_raw_WLS$withoutC_Errvar6 - 0.528)

# add Row number 
new_df <- select(data_withoutC_raw_WLS, Row, method, factors, nobs, b_withoutC_L2, b_withoutC_L6, b_withoutC_L8, b_withoutC_L12, b_withoutC_Ti8.b, b_withoutC_Ti12.b, b_withoutC_C1, b_withoutC_Errvar2, b_withoutC_Errvar6)
new_df$Row <- rep(1:12, each = 250)

# calculating means of bias
Means_withoutC_L2 <- new_df %>% 
  group_by(Row) %>% 
  summarize(mean(b_withoutC_L2))

Means_withoutC_L6 <- new_df %>% 
  group_by(Row) %>% 
  summarize(mean(b_withoutC_L6)) 

Means_withoutC_L8 <- new_df %>% 
  group_by(Row) %>% 
  summarize(mean(b_withoutC_L8)) 

Means_withoutC_L12 <- new_df %>% 
  group_by(Row) %>% 
  summarize(mean(b_withoutC_L12)) 

Means_withoutC_Ti8.b <- new_df %>% 
  group_by(Row) %>% 
  summarize(mean(b_withoutC_Ti8.b)) 

Means_withoutC_Ti12.b <- new_df %>% 
  group_by(Row) %>% 
  summarize(mean(b_withoutC_Ti12.b))

Means_withoutC_C1 <- new_df %>% 
  group_by(Row) %>% 
  summarize(mean(b_withoutC_C1))

Means_withoutC_Errvar2 <- new_df %>% 
  group_by(Row) %>% 
  summarize(mean(b_withoutC_Errvar2)) 

Means_withoutC_Errvar6 <- new_df %>% 
  group_by(Row) %>% 
  summarize(mean(b_withoutC_Errvar6))


#combine all means of bias
Means_withoutC_WLS <- cbind(Means_withoutC_L2, Means_withoutC_L6, Means_withoutC_L8, Means_withoutC_L12, Means_withoutC_Ti8.b, Means_withoutC_Ti12.b, Means_withoutC_C1, Means_withoutC_Errvar2, Means_withoutC_Errvar6)

# keeping 1 Row column
Means_withoutC_WLS <- Means_withoutC_WLS %>% select(1,'mean(b_withoutC_L2)', 'mean(b_withoutC_L6)', 'mean(b_withoutC_L8)', 'mean(b_withoutC_L12)', 'mean(b_withoutC_Ti8.b)', 'mean(b_withoutC_Ti12.b)', 'mean(b_withoutC_C1)', 'mean(b_withoutC_Errvar2)', 'mean(b_withoutC_Errvar6)')

# rename columns
colnames(Means_withoutC_WLS) <- c("Row", "bias_withoutC_L2_WLS", "bias_withoutC_L6_WLS", "bias_withoutC_L8_WLS", "bias_withoutC_L12_WLS", "bias_withoutC_Ti8.b_WLS", "bias_withoutC_Ti12.b_WLS", "bias_withoutC_C1_WLS", "bias_withoutC_Errvar2_WLS", "bias_withoutC_Errvar6_WLS")

write.csv(Means_withoutC_WLS, file = "Means_withoutC_WLS.csv")

### MERGING ALL results
list.files()
Means_withoutC_PML <- read.csv("Means_withoutC_PML.csv")
Means_withoutC_WLS <- read.csv("Means_withoutC_WLS.csv")
Raw_means_withoutC_PML <- read.csv("Raw_means_withoutC_PML.csv")
Raw_means_withoutC_WLS <- read.csv("Raw_means_withoutC_WLS.csv")

names(merged)

# create column with true values
true_withoutC_L2 <- rep(0.8, 12)
true_withoutC_L6 <- rep(0.6, 12)
true_withoutC_L8 <- rep(0.8, 12)
true_withoutC_L12 <- rep(0.6, 12)
true_withoutC_Ti8 <- rep(0, 12)
true_withoutC_Ti12 <- rep(0, 12)
true_withoutC_C1 <- rep(0.3, 12)
true_withoutC_Errvar2 <- rep(0.36, 12)
true_withoutC_Errvar6 <- rep(0.528, 12)

merged <- cbind(Means_withoutC_PML, 
                Means_withoutC_WLS, 
                Raw_means_withoutC_PML, 
                Raw_means_withoutC_WLS, 
                true_withoutC_L2,
                true_withoutC_L6, 
                true_withoutC_L8,
                true_withoutC_L12,
                true_withoutC_Ti8, 
                true_withoutC_Ti12, 
                true_withoutC_C1,
                true_withoutC_Errvar2, 
                true_withoutC_Errvar6)

merged2 <- merged %>% select(2, 
                             true_withoutC_L2, 
                             est_withoutC_L2_WLS,
                             bias_withoutC_L2_WLS,
                             est_withoutC_L2_PML, 
                             bias_withoutC_L2_PML, 
                             
                             true_withoutC_L6, 
                             est_withoutC_L6_WLS, 
                             bias_withoutC_L6_WLS,
                             est_withoutC_L6_PML, 
                             bias_withoutC_L6_PML, 
                             
                             true_withoutC_L8, 
                             est_withoutC_L8_WLS, 
                             bias_withoutC_L8_WLS,
                             est_withoutC_L8_PML, 
                             bias_withoutC_L8_PML, 
                             
                             true_withoutC_L12, 
                             est_withoutC_L12_WLS,
                             bias_withoutC_L12_WLS,
                             est_withoutC_L12_PML, 
                             bias_withoutC_L12_PML, 
                             
                             true_withoutC_Ti8,
                             est_withoutC_Ti8.b_WLS,
                             bias_withoutC_Ti8.b_WLS,
                             est_withoutC_Ti8.b_PML, 
                             bias_withoutC_Ti8.b_PML,
                             
                             true_withoutC_Ti12, 
                             est_withoutC_Ti12.b_WLS,
                             bias_withoutC_Ti12.b_WLS,
                             est_withoutC_Ti12.b_PML, 
                             bias_withoutC_Ti12.b_PML,
                             
                             true_withoutC_C1, 
                             est_withoutC_C1_WLS,
                             bias_withoutC_C1_WLS,
                             est_withoutC_C1_PML, 
                             bias_withoutC_C1_PML,
                             
                             true_withoutC_Errvar2, 
                             est_withoutC_Errvar2_WLS,
                             bias_withoutC_Errvar2_WLS,
                             est_withoutC_Errvar2_PML, 
                             bias_withoutC_Errvar2_PML,
                             
                             true_withoutC_Errvar6, 
                             est_withoutC_Errvar6_WLS,
                             bias_withoutC_Errvar6_WLS,
                             est_withoutC_Errvar6_PML, 
                             bias_withoutC_Errvar6_PML)

MIX_withoutC_perRow_Table <- write.csv(merged2, "MIX_withoutC_perRow_Table.csv")

MIX_withoutC_perRow_Table <- read_csv("MIX_withoutC_perRow_Table.csv")
MIX_withoutC_perRow_Table <- as.data.frame(MIX_withoutC_perRow_Table)
MIX_withoutC_perRow_Table <- MIX_withoutC_perRow_Table %>% mutate_if(is.numeric,round, digits = 2)
MIX_withoutC_perRow_Table <- write.csv(MIX_withoutC_perRow_Table, "MIX_withoutC_perRow_Table-.csv")

#transposing matrix
MIX_withoutC_perRow_Table <- read_csv("MIX_withoutC_perRow_Table-.csv")
MIX_withoutC_perRow_Table_transpose <- as.data.frame(t(as.matrix(MIX_withoutC_perRow_Table)))
write.csv(MIX_withoutC_perRow_Table_transpose, "MIX_withoutC_perRow_Table_T.csv")


MIX_withoutC_perRow_Table_T <- read_csv("MIX_withoutC_perRow_Table_T.csv")
MIX_withoutC_perRow_Table_T <- MIX_withoutC_perRow_Table_T[-1,]
MIX_withoutC_perRow_Table_T <- MIX_withoutC_perRow_Table_T[-1,]
MIX_withoutC_perRow_Table_T <- MIX_withoutC_perRow_Table_T[-1,]

Rows <- 1:12
colnames(MIX_withoutC_perRow_Table_T) <- c("Row", Rows)

write.csv(MIX_withoutC_perRow_Table_T, "MIX_withoutC_perRow_Table_T.csv")

# OTHER (shared) FOLDER:
df <- read.csv("MIX_withoutC_perRow_Table_T.csv", header = TRUE)

# delete first column, change column names
df <- df[,-1]
Rows <- 1:12
colnames(df) <- c("Row", Rows)

# separate first column into 4 factors
df <- df %>% separate(Row, c("Value", "Model", "Parameter", "Method"), sep = "_")

df <- df[,-2]
df[is.na(df)] = "Both"

# change names
df$Value <- as.factor(df$Value)
levels(df$Value) <- c("Raw bias", "Estimate", "True")

df$Method <- as.factor(df$Method)
levels(df$Method) <- c("Both", "PML", "WLS")

means <- df %>% keep(is.numeric) %>% rowMeans()
df$Mean <- means

write.csv(df, "MIX_withoutC_perRow_Table_T.csv")

###################################################################
########################## ERRORS ################################
###################################################################
###################################################################
########################### WITH CROSSLOADING #####################
###################################################################

# Inladen bestanden met ruwe factor ladingen en verander WLS naar DWLS: 
data_withC_raw <- read.csv("MIX_aov_withC_errR1_12.csv")
data_withC_raw$method <- factor(data_withC_raw$method)
levels(data_withC_raw$method) <- c("PML", "DWLS")
colnames(data_withC_raw)
#write_csv(data_withC_raw, "MIX_aov_withC_errR1_12.csv")

# create factors
data_withC_raw$method <- factor(data_withC_raw$method)
data_withC_raw$factors <- factor(data_withC_raw$factors)
data_withC_raw$nobs <- factor(data_withC_raw$nobs)

names(data_withC_raw)
data_withC_raw$method

data_withC_raw_PML <- data_withC_raw[data_withC_raw$method == "PML",]

#change order (row 1 to 12)
data_withC_raw_PML <- data_withC_raw_PML %>% arrange(factors) %>% arrange(nobs)
str(data_withC_raw_PML)
View(data_withC_raw_PML)

# create labels values
data_withC_raw_PML$Row <- rep(1:12, each = 250)

# add labs value
data_withC_raw_PML <- select(data_withC_raw_PML, Row, method, factors, nobs, withC_L_SE2, withC_L_SE6, 
                             withC_L_SEa, withC_L_SE8, withC_L_SE12, 
                             withC_T_SEi8.b, withC_T_SEi12.b, withC_C_SE1, 
                             withC_Errvar_SE2, withC_Errvar_SE6)

data_withC_raw_PML$Row <- as.factor(data_withC_raw_PML$Row)

# calculating raw means
Raw_means_withC_L_SE2 <- data_withC_raw_PML %>% 
  group_by(Row) %>% 
  summarize(mean(withC_L_SE2)) 

Raw_means_withC_L_SE6 <- data_withC_raw_PML %>% 
  group_by(Row) %>% 
  summarize(mean(withC_L_SE6)) 

Raw_means_withC_L_SEa <- data_withC_raw_PML %>% 
  group_by(Row) %>% 
  summarize(mean(withC_L_SEa))

Raw_means_withC_L_SE8 <- data_withC_raw_PML %>% 
  group_by(Row) %>% 
  summarize(mean(withC_L_SE8))

Raw_means_withC_L_SE12 <- data_withC_raw_PML %>% 
  group_by(Row) %>% 
  summarize(mean(withC_L_SE12))

Raw_means_withC_T_SEi8.b <- data_withC_raw_PML %>% 
  group_by(Row) %>% 
  summarize(mean(withC_T_SEi8.b))

Raw_means_withC_T_SEi12.b <- data_withC_raw_PML %>% 
  group_by(Row) %>% 
  summarize(mean(withC_T_SEi12.b)) 

Raw_means_withC_C_SE1 <- data_withC_raw_PML %>% 
  group_by(Row) %>% 
  summarize(mean(withC_C_SE1))

Raw_means_withC_Errvar_SE2 <- data_withC_raw_PML %>% 
  group_by(Row) %>% 
  summarize(mean(withC_Errvar_SE2)) 

Raw_means_withC_Errvar_SE6 <- data_withC_raw_PML %>% 
  group_by(Row) %>% 
  summarize(mean(withC_Errvar_SE6))


#combine all means
Raw_means_withC_PML <- cbind(Raw_means_withC_L_SE2, Raw_means_withC_L_SE6, Raw_means_withC_L_SEa, Raw_means_withC_L_SE8, Raw_means_withC_L_SE12, Raw_means_withC_T_SEi8.b, Raw_means_withC_T_SEi12.b, Raw_means_withC_C_SE1, Raw_means_withC_Errvar_SE2, Raw_means_withC_Errvar_SE6)

# keeping 1 Row column
Raw_means_withC_PML <- Raw_means_withC_PML %>% select(1,'mean(withC_L_SE2)', 'mean(withC_L_SE6)', 'mean(withC_L_SEa)', 'mean(withC_L_SE8)', 'mean(withC_L_SE12)', 'mean(withC_T_SEi8.b)', 'mean(withC_T_SEi12.b)', 'mean(withC_C_SE1)', 'mean(withC_Errvar_SE2)', 'mean(withC_Errvar_SE6)')

colnames(Raw_means_withC_PML) <- c("Row","est_withC_L_SE2_PML", "est_withC_L_SE6_PML", "est_withC_L_SEa_PML", "est_withC_L_SE8_PML", "est_withC_L_SE12_PML", "est_withC_T_SEi8.b_PML", "est_withC_T_SEi12.b_PML", "est_withC_C_SE1_PML", "est_withC_Errvar_SE2_PML", "est_withC_Errvar_SE6_PML")

write.csv(Raw_means_withC_PML, file = "Raw_means_withC_PML(SE).csv")

# BIAS #
# For obtaining the true value, we need to take the sd of the estimate (not the SE)
data_withC_raw_SE <- read.csv("MIX_aov_withC_estR1_12.csv") 
data_withC_raw_SE_PML <- data_withC_raw_SE[data_withC_raw_SE$method == "PML",]

#change order (row 1 to 12)
data_withC_raw_SE_PML <- data_withC_raw_SE_PML %>% arrange(factors) %>% arrange(nobs)

# create true values
true_withC_L_SE2_PML <- sd(data_withC_raw_SE_PML$withC_L2)
true_withC_L_SE6_PML <- sd(data_withC_raw_SE_PML$withC_L6)
true_withC_L_SEa_PML <- sd(data_withC_raw_SE_PML$withC_La)
true_withC_L_SE8_PML <- sd(data_withC_raw_SE_PML$withC_L8)
true_withC_L_SE12_PML <- sd(data_withC_raw_SE_PML$withC_L12)
true_withC_T_SEi8_PML <- sd(data_withC_raw_SE_PML$withC_Ti8.b)
true_withC_T_SEi12_PML <- sd(data_withC_raw_SE_PML$withC_Ti12.b)
true_withC_Errvar_SE2_PML <- sd(data_withC_raw_SE_PML$withC_Errvar2)
true_withC_Errvar_SE6_PML <- sd(data_withC_raw_SE_PML$withC_Errvar6)
true_withC_C_SE1_PML <- sd(data_withC_raw_SE_PML$withC_C1)

# calculate bias (Err withC)
data_withC_raw_PML$b_withC_L_SE2 <- 
  true_withC_L_SE2_PML - data_withC_raw_PML$withC_L_SE2
data_withC_raw_PML$b_withC_L_SE6 <- 
  true_withC_L_SE6_PML - data_withC_raw_PML$withC_L_SE6
data_withC_raw_PML$b_withC_L_SEa <- 
  true_withC_L_SEa_PML - data_withC_raw_PML$withC_L_SEa
data_withC_raw_PML$b_withC_L_SE8 <- 
  true_withC_L_SE8_PML - data_withC_raw_PML$withC_L_SE8
data_withC_raw_PML$b_withC_L_SE12 <- 
  true_withC_L_SE12_PML - data_withC_raw_PML$withC_L_SE12
data_withC_raw_PML$b_withC_T_SEi8.b <- 
  true_withC_T_SEi8_PML - data_withC_raw_PML$withC_T_SEi8.b
data_withC_raw_PML$b_withC_T_SEi12.b <- 
  true_withC_T_SEi12_PML - data_withC_raw_PML$withC_T_SEi12.b
data_withC_raw_PML$b_withC_C_SE1 <- 
  true_withC_Errvar_SE2_PML - data_withC_raw_PML$withC_C_SE1
data_withC_raw_PML$b_withC_Errvar_SE2 <- 
  true_withC_Errvar_SE6_PML - data_withC_raw_PML$withC_Errvar_SE2
data_withC_raw_PML$b_withC_Errvar_SE6 <- 
  true_withC_C_SE1_PML - data_withC_raw_PML$withC_Errvar_SE6

# add Row number 
new_df <- select(data_withC_raw_PML, Row, method, factors, nobs, b_withC_L_SE2, b_withC_L_SE6, b_withC_L_SEa, b_withC_L_SE8, b_withC_L_SE12, b_withC_T_SEi8.b, b_withC_T_SEi12.b, b_withC_C_SE1, b_withC_Errvar_SE2, b_withC_Errvar_SE6)
new_df$Row <- rep(1:12, each = 250)

# calculating means of bias
Means_withC_L_SE2 <- new_df %>% 
  group_by(Row) %>% 
  summarize(mean(b_withC_L_SE2))

Means_withC_L_SE6 <- new_df %>% 
  group_by(Row) %>% 
  summarize(mean(b_withC_L_SE6)) 

Means_withC_L_SEa <- new_df %>% 
  group_by(Row) %>% 
  summarize(mean(b_withC_L_SEa)) 

Means_withC_L_SE8 <- new_df %>% 
  group_by(Row) %>% 
  summarize(mean(b_withC_L_SE8)) 

Means_withC_L_SE12 <- new_df %>% 
  group_by(Row) %>% 
  summarize(mean(b_withC_L_SE12)) 

Means_withC_T_SEi8.b <- new_df %>% 
  group_by(Row) %>% 
  summarize(mean(b_withC_T_SEi8.b)) 

Means_withC_T_SEi12.b <- new_df %>% 
  group_by(Row) %>% 
  summarize(mean(b_withC_T_SEi12.b))

Means_withC_C_SE1 <- new_df %>% 
  group_by(Row) %>% 
  summarize(mean(b_withC_C_SE1))

Means_withC_Errvar_SE2 <- new_df %>% 
  group_by(Row) %>% 
  summarize(mean(b_withC_Errvar_SE2)) 

Means_withC_Errvar_SE6 <- new_df %>% 
  group_by(Row) %>% 
  summarize(mean(b_withC_Errvar_SE6))

#combine all means of bias
Means_withC_PML <- cbind(Means_withC_L_SE2, Means_withC_L_SE6, Means_withC_L_SEa, Means_withC_L_SE8, Means_withC_L_SE12, Means_withC_T_SEi8.b, Means_withC_T_SEi12.b, Means_withC_C_SE1, Means_withC_Errvar_SE2, Means_withC_Errvar_SE6)

# keeping 1 Row column
Means_withC_PML <- Means_withC_PML %>% select(1,'mean(b_withC_L_SE2)', 'mean(b_withC_L_SE6)', 'mean(b_withC_L_SEa)', 'mean(b_withC_L_SE8)', 'mean(b_withC_L_SE12)', 'mean(b_withC_T_SEi8.b)', 'mean(b_withC_T_SEi12.b)', 'mean(b_withC_C_SE1)', 'mean(b_withC_Errvar_SE2)', 'mean(b_withC_Errvar_SE6)')

# rename columns
colnames(Means_withC_PML) <- c("Row", "bias_withC_L_SE2_PML", "bias_withC_L_SE6_PML", "bias_withC_L_SEa_PML", "bias_withC_L_SE8_PML", "bias_withC_L_SE12_PML", "bias_withC_T_SEi8.b_PML", "bias_withC_T_SEi12.b_PML", "bias_withC_C_SE1_PML", "bias_withC_Errvar_SE2_PML", "bias_withC_Errvar_SE6_PML")

write.csv(Means_withC_PML, file = "Means_withC_PML(SE).csv")


######################################## DWLS
# All ANOVA results (raw data)
data_withC_raw <- read.csv("MIX_aov_withC_errR1_12.csv") 

data_withC_raw$method <- factor(data_withC_raw$method)
data_withC_raw$factors <- factor(data_withC_raw$factors)
data_withC_raw$nobs <- factor(data_withC_raw$nobs)

names(data_withC_raw)
data_withC_raw$method # moet DWLS zijn!

data_withC_raw_WLS <- data_withC_raw[data_withC_raw$method == "DWLS",]

#change order (row 1 to 12)
data_withC_raw_WLS <- data_withC_raw_WLS %>% arrange(factors) %>% arrange(nobs)
str(data_withC_raw_WLS)
#View(data_withC_raw_WLS)

# create labels values
data_withC_raw_WLS$Row <- rep(1:12, each = 250)

# add labs value
data_withC_raw_WLS <- select(data_withC_raw_WLS, Row, method, factors, nobs, withC_L_SE2, withC_L_SE6, 
                             withC_L_SEa, withC_L_SE8, withC_L_SE12, 
                             withC_T_SEi8.b, withC_T_SEi12.b, withC_C_SE1, 
                             withC_Errvar_SE2, withC_Errvar_SE6)

data_withC_raw_WLS$Row <- as.factor(data_withC_raw_WLS$Row)

# calculating raw means
Raw_means_withC_L_SE2 <- data_withC_raw_WLS %>% 
  group_by(Row) %>% 
  summarize(mean(withC_L_SE2)) 

Raw_means_withC_L_SE6 <- data_withC_raw_WLS %>% 
  group_by(Row) %>% 
  summarize(mean(withC_L_SE6)) 

Raw_means_withC_L_SEa <- data_withC_raw_WLS %>% 
  group_by(Row) %>% 
  summarize(mean(withC_L_SEa))

Raw_means_withC_L_SE8 <- data_withC_raw_WLS %>% 
  group_by(Row) %>% 
  summarize(mean(withC_L_SE8))

Raw_means_withC_L_SE12 <- data_withC_raw_WLS %>% 
  group_by(Row) %>% 
  summarize(mean(withC_L_SE12))

Raw_means_withC_T_SEi8.b <- data_withC_raw_WLS %>% 
  group_by(Row) %>% 
  summarize(mean(withC_T_SEi8.b))

Raw_means_withC_T_SEi12.b <- data_withC_raw_WLS %>% 
  group_by(Row) %>% 
  summarize(mean(withC_T_SEi12.b)) 

Raw_means_withC_C_SE1 <- data_withC_raw_WLS %>% 
  group_by(Row) %>% 
  summarize(mean(withC_C_SE1))

Raw_means_withC_Errvar_SE2 <- data_withC_raw_WLS %>% 
  group_by(Row) %>% 
  summarize(mean(withC_Errvar_SE2)) 

Raw_means_withC_Errvar_SE6 <- data_withC_raw_WLS %>% 
  group_by(Row) %>% 
  summarize(mean(withC_Errvar_SE6))

#combine all means
Raw_means_withC_WLS <- cbind(Raw_means_withC_L_SE2, Raw_means_withC_L_SE6, Raw_means_withC_L_SEa, Raw_means_withC_L_SE8, Raw_means_withC_L_SE12, Raw_means_withC_T_SEi8.b, Raw_means_withC_T_SEi12.b, Raw_means_withC_C_SE1, Raw_means_withC_Errvar_SE2, Raw_means_withC_Errvar_SE6)

# keeping 1 Row column
Raw_means_withC_WLS <- Raw_means_withC_WLS %>% select(1,'mean(withC_L_SE2)', 'mean(withC_L_SE6)', 'mean(withC_L_SEa)', 'mean(withC_L_SE8)', 'mean(withC_L_SE12)', 'mean(withC_T_SEi8.b)', 'mean(withC_T_SEi12.b)', 'mean(withC_C_SE1)', 'mean(withC_Errvar_SE2)', 'mean(withC_Errvar_SE6)')

colnames(Raw_means_withC_WLS) <- c("Row","est_withC_L_SE2_WLS", "est_withC_L_SE6_WLS", "est_withC_L_SEa_WLS", "est_withC_L_SE8_WLS", "est_withC_L_SE12_WLS", "est_withC_T_SEi8.b_WLS", "est_withC_T_SEi12.b_WLS", "est_withC_C_SE1_WLS", "est_withC_Errvar_SE2_WLS", "est_withC_Errvar_SE6_WLS")

write.csv(Raw_means_withC_WLS, file = "Raw_means_withC_WLS(SE).csv")

# BIAS #
data_withC_raw_SE <- read.csv("MIX_aov_withC_estR1_12.csv") 
data_withC_raw_SE_WLS <- data_withC_raw_SE[data_withC_raw_SE$method == "DWLS",]

#change order (row 1 to 12)
data_withC_raw_SE_WLS <- data_withC_raw_SE_WLS %>% arrange(factors) %>% arrange(nobs)

# create true values
true_withC_L_SE2_WLS <- sd(data_withC_raw_SE_WLS$withC_L2)
true_withC_L_SE6_WLS <- sd(data_withC_raw_SE_WLS$withC_L6)
true_withC_L_SEa_WLS <- sd(data_withC_raw_SE_WLS$withC_La)
true_withC_L_SE8_WLS <- sd(data_withC_raw_SE_WLS$withC_L8)
true_withC_L_SE12_WLS <- sd(data_withC_raw_SE_WLS$withC_L12)
true_withC_T_SEi8_WLS <- sd(data_withC_raw_SE_WLS$withC_Ti8.b)
true_withC_T_SEi12_WLS <- sd(data_withC_raw_SE_WLS$withC_Ti12.b)
true_withC_Errvar_SE2_WLS <- sd(data_withC_raw_SE_WLS$withC_Errvar2)
true_withC_Errvar_SE6_WLS <- sd(data_withC_raw_SE_WLS$withC_Errvar6)
true_withC_C_SE1_WLS <- sd(data_withC_raw_SE_WLS$withC_C1)

# calculate bias (Err withC)
data_withC_raw_WLS$b_withC_L_SE2 <- 
  true_withC_L_SE2_WLS - data_withC_raw_WLS$withC_L_SE2
data_withC_raw_WLS$b_withC_L_SE6 <- 
  true_withC_L_SE6_WLS - data_withC_raw_WLS$withC_L_SE6
data_withC_raw_WLS$b_withC_L_SEa <- 
  true_withC_L_SEa_WLS - data_withC_raw_WLS$withC_L_SEa
data_withC_raw_WLS$b_withC_L_SE8 <- 
  true_withC_L_SE8_WLS - data_withC_raw_WLS$withC_L_SE8
data_withC_raw_WLS$b_withC_L_SE12 <- 
  true_withC_L_SE12_WLS - data_withC_raw_WLS$withC_L_SE12
data_withC_raw_WLS$b_withC_T_SEi8.b <- 
  true_withC_T_SEi8_WLS - data_withC_raw_WLS$withC_T_SEi8.b
data_withC_raw_WLS$b_withC_T_SEi12.b <- 
  true_withC_T_SEi12_WLS - data_withC_raw_WLS$withC_T_SEi12.b
data_withC_raw_WLS$b_withC_C_SE1 <- 
  true_withC_Errvar_SE2_WLS - data_withC_raw_WLS$withC_C_SE1
data_withC_raw_WLS$b_withC_Errvar_SE2 <- 
  true_withC_Errvar_SE6_WLS - data_withC_raw_WLS$withC_Errvar_SE2
data_withC_raw_WLS$b_withC_Errvar_SE6 <- 
  true_withC_C_SE1_WLS - data_withC_raw_WLS$withC_Errvar_SE6

# add Row number 
new_df <- select(data_withC_raw_WLS, Row, method, factors, nobs, b_withC_L_SE2, b_withC_L_SE6, b_withC_L_SEa, b_withC_L_SE8, b_withC_L_SE12, b_withC_T_SEi8.b, b_withC_T_SEi12.b, b_withC_C_SE1, b_withC_Errvar_SE2, b_withC_Errvar_SE6)
new_df$Row <- rep(1:12, each = 250)

# calculating means of bias
Means_withC_L_SE2 <- new_df %>% 
  group_by(Row) %>% 
  summarize(mean(b_withC_L_SE2))

Means_withC_L_SE6 <- new_df %>% 
  group_by(Row) %>% 
  summarize(mean(b_withC_L_SE6)) 

Means_withC_L_SEa <- new_df %>% 
  group_by(Row) %>% 
  summarize(mean(b_withC_L_SEa)) 

Means_withC_L_SE8 <- new_df %>% 
  group_by(Row) %>% 
  summarize(mean(b_withC_L_SE8)) 

Means_withC_L_SE12 <- new_df %>% 
  group_by(Row) %>% 
  summarize(mean(b_withC_L_SE12)) 

Means_withC_T_SEi8.b <- new_df %>% 
  group_by(Row) %>% 
  summarize(mean(b_withC_T_SEi8.b)) 

Means_withC_T_SEi12.b <- new_df %>% 
  group_by(Row) %>% 
  summarize(mean(b_withC_T_SEi12.b))

Means_withC_C_SE1 <- new_df %>% 
  group_by(Row) %>% 
  summarize(mean(b_withC_C_SE1))

Means_withC_Errvar_SE2 <- new_df %>% 
  group_by(Row) %>% 
  summarize(mean(b_withC_Errvar_SE2)) 

Means_withC_Errvar_SE6 <- new_df %>% 
  group_by(Row) %>% 
  summarize(mean(b_withC_Errvar_SE6))


#combine all means of bias
Means_withC_WLS <- cbind(Means_withC_L_SE2, Means_withC_L_SE6, Means_withC_L_SEa, Means_withC_L_SE8, Means_withC_L_SE12, Means_withC_T_SEi8.b, Means_withC_T_SEi12.b, Means_withC_C_SE1, Means_withC_Errvar_SE2, Means_withC_Errvar_SE6)

# keeping 1 Row column
Means_withC_WLS <- Means_withC_WLS %>% select(1,'mean(b_withC_L_SE2)', 'mean(b_withC_L_SE6)', 'mean(b_withC_L_SEa)', 'mean(b_withC_L_SE8)', 'mean(b_withC_L_SE12)', 'mean(b_withC_T_SEi8.b)', 'mean(b_withC_T_SEi12.b)', 'mean(b_withC_C_SE1)', 'mean(b_withC_Errvar_SE2)', 'mean(b_withC_Errvar_SE6)')

# rename columns
colnames(Means_withC_WLS) <- c("Row", "bias_withC_L_SE2_WLS", "bias_withC_L_SE6_WLS", "bias_withC_L_SEa_WLS", "bias_withC_L_SE8_WLS", "bias_withC_L_SE12_WLS", "bias_withC_T_SEi8.b_WLS", "bias_withC_T_SEi12.b_WLS", "bias_withC_C_SE1_WLS", "bias_withC_Errvar_SE2_WLS", "bias_withC_Errvar_SE6_WLS")

write.csv(Means_withC_WLS, file = "Means_withC_WLS(SE).csv")

### MERGING ALL results
list.files()
Means_withC_PML <- read.csv("Means_withC_PML(SE).csv")
Means_withC_WLS <- read.csv("Means_withC_WLS(SE).csv")
Raw_means_withC_PML <- read.csv("Raw_means_withC_PML(SE).csv")
Raw_means_withC_WLS <- read.csv("Raw_means_withC_WLS(SE).csv")

merged <- cbind(Means_withC_PML, 
                Means_withC_WLS, 
                Raw_means_withC_PML, 
                Raw_means_withC_WLS, 
                true_withC_L_SE2_WLS,
                true_withC_L_SE6_WLS, 
                true_withC_L_SEa_WLS, 
                true_withC_L_SE8_WLS,
                true_withC_L_SE12_WLS,
                true_withC_T_SEi8_WLS, 
                true_withC_T_SEi12_WLS, 
                true_withC_C_SE1_WLS,
                true_withC_Errvar_SE2_WLS, 
                true_withC_Errvar_SE6_WLS,
                true_withC_L_SE2_PML,
                true_withC_L_SE6_PML, 
                true_withC_L_SEa_PML, 
                true_withC_L_SE8_PML,
                true_withC_L_SE12_PML,
                true_withC_T_SEi8_PML, 
                true_withC_T_SEi12_PML, 
                true_withC_C_SE1_PML,
                true_withC_Errvar_SE2_PML, 
                true_withC_Errvar_SE6_PML)

merged2 <- merged %>% select(2, 
                             true_withC_L_SE2_WLS, 
                             est_withC_L_SE2_WLS,
                             bias_withC_L_SE2_WLS,
                             true_withC_L_SE2_PML, 
                             est_withC_L_SE2_PML, 
                             bias_withC_L_SE2_PML, 
                             
                             true_withC_L_SE6_WLS, 
                             est_withC_L_SE6_WLS, 
                             bias_withC_L_SE6_WLS,
                             true_withC_L_SE6_PML,
                             est_withC_L_SE6_PML, 
                             bias_withC_L_SE6_PML, 
                             
                             true_withC_L_SEa_WLS, 
                             est_withC_L_SEa_WLS,
                             bias_withC_L_SEa_WLS,
                             true_withC_L_SEa_PML,
                             est_withC_L_SEa_PML, 
                             bias_withC_L_SEa_PML, 
                             
                             true_withC_L_SE8_WLS, 
                             est_withC_L_SE8_WLS, 
                             bias_withC_L_SE8_WLS,
                             true_withC_L_SE8_PML,
                             est_withC_L_SE8_PML, 
                             bias_withC_L_SE8_PML, 
                             
                             true_withC_L_SE12_WLS, 
                             est_withC_L_SE12_WLS,
                             bias_withC_L_SE12_WLS,
                             true_withC_L_SE12_PML, 
                             est_withC_L_SE12_PML, 
                             bias_withC_L_SE12_PML, 
                             
                             true_withC_T_SEi8_WLS,
                             est_withC_T_SEi8.b_WLS,
                             bias_withC_T_SEi8.b_WLS,
                             true_withC_T_SEi8_PML,
                             est_withC_T_SEi8.b_PML, 
                             bias_withC_T_SEi8.b_PML,
                             
                             true_withC_T_SEi12_WLS, 
                             est_withC_T_SEi12.b_WLS,
                             bias_withC_T_SEi12.b_WLS,
                             true_withC_T_SEi12_PML,
                             est_withC_T_SEi12.b_PML, 
                             bias_withC_T_SEi12.b_PML,
                             
                             true_withC_C_SE1_WLS, 
                             est_withC_C_SE1_WLS,
                             bias_withC_C_SE1_WLS,
                             true_withC_C_SE1_PML,
                             est_withC_C_SE1_PML, 
                             bias_withC_C_SE1_PML,
                             
                             true_withC_Errvar_SE2_WLS, 
                             est_withC_Errvar_SE2_WLS,
                             bias_withC_Errvar_SE2_WLS,
                             true_withC_Errvar_SE2_PML,
                             est_withC_Errvar_SE2_PML, 
                             bias_withC_Errvar_SE2_PML,
                             
                             true_withC_Errvar_SE6_WLS, 
                             est_withC_Errvar_SE6_WLS,
                             bias_withC_Errvar_SE6_WLS,
                             true_withC_Errvar_SE6_PML,
                             est_withC_Errvar_SE6_PML, 
                             bias_withC_Errvar_SE6_PML)

MIX_withC_perRow_Table <- write.csv(merged2, "MIX_withC_perRow_Table(SE).csv")

MIX_withC_perRow_Table_SE <- read_csv("MIX_withC_perRow_Table(SE).csv")
MIX_withC_perRow_Table_SE <- as.data.frame(MIX_withC_perRow_Table_SE)
MIX_withC_perRow_Table_SE <- MIX_withC_perRow_Table_SE %>% mutate_if(is.numeric,round, digits = 2)
MIX_withC_perRow_Table_SE <- write.csv(MIX_withC_perRow_Table_SE, "MIX_withC_perRow_Table(SE)-.csv")

# transposing matrix
MIX_withC_perRow_Table_SE <- read_csv("MIX_withC_perRow_Table(SE)-.csv")
MIX_withC_perRow_Table_SE_transpose <- as.data.frame(t(as.matrix(MIX_withC_perRow_Table_SE)))
write.csv(MIX_withC_perRow_Table_SE_transpose, "MIX_withC_perRow_Table(SE)_T.csv")

MIX_withC_perRow_T_SEable_T_SE <- read_csv("MIX_withC_perRow_Table(SE)_T.csv")
MIX_withC_perRow_T_SEable_T_SE <- MIX_withC_perRow_T_SEable_T_SE[-1,]
MIX_withC_perRow_T_SEable_T_SE <- MIX_withC_perRow_T_SEable_T_SE[-1,]
MIX_withC_perRow_T_SEable_T_SE <- MIX_withC_perRow_T_SEable_T_SE[-1,]

Rows <- 1:12
colnames(MIX_withC_perRow_T_SEable_T_SE) <- c("Row", Rows)

write.csv(MIX_withC_perRow_T_SEable_T_SE, "MIX_withC_perRow_T_SEable_T_SE.csv")

df <- read.csv("MIX_withC_perRow_Table(SE)_T.csv", header = TRUE)

# delete first column, change column names
df <- df[,-1]
Rows <- 1:12
colnames(df) <- c("Row", Rows)

# separate first column into 4 factors
df <- df %>% separate(Row, c("Value", "Model", "Parameter1", "Parameter2", "Method"), sep = "_")

df <- df[,-2]
df[is.na(df)] = "Both"

# change names
df$Value <- as.factor(df$Value)
levels(df$Value) <- c("Raw bias", "Estimate", "True")

df$Method <- as.factor(df$Method)
levels(df$Method) <- c("Both", "PML", "WLS")

means <- df %>% keep(is.numeric) %>% rowMeans()
df$Mean <- means
df <- df[,-2]

write.csv(df, "MIX_withC_perRow_Table(SE)_T.csv")

###################################################################
########################### WITHOUT C #####################
###################################################################
data_withoutC_raw <- read.csv("MIX_aov_withoutC_errR1_12.csv") # ID toevoegen

data_withoutC_raw$method <- factor(data_withoutC_raw$method)
data_withoutC_raw$factors <- factor(data_withoutC_raw$factors)
data_withoutC_raw$nobs <- factor(data_withoutC_raw$nobs)

levels(data_withoutC_raw$method) <- c("PML", "DWLS")
write_csv(data_withoutC_raw, "MIX_aov_withoutC_errR1_12.csv")

names(data_withoutC_raw)
data_withoutC_raw$method

data_withoutC_raw_PML <- data_withoutC_raw[data_withoutC_raw$method == "PML",]

#change order (row 1 to 12)
data_withoutC_raw_PML <- data_withoutC_raw_PML %>% arrange(factors) %>% arrange(nobs)
str(data_withoutC_raw_PML)
#View(data_withoutC_raw_PML)

# create labels values
data_withoutC_raw_PML$Row <- rep(1:12, each = 250)

# add labs value
data_withoutC_raw_PML <- select(data_withoutC_raw_PML, Row, method, factors, nobs, withoutC_L_SE2, withoutC_L_SE6, withoutC_L_SE8, withoutC_L_SE12, withoutC_T_SEi8.b, withoutC_T_SEi12.b, withoutC_C_SE1, withoutC_Errvar_SE2, withoutC_Errvar_SE6)

data_withoutC_raw_PML$Row <- as.factor(data_withoutC_raw_PML$Row)

# calculating raw means
Raw_means_withoutC_L_SE2 <- data_withoutC_raw_PML %>% 
  group_by(Row) %>% 
  summarize(mean(withoutC_L_SE2)) 

Raw_means_withoutC_L_SE6 <- data_withoutC_raw_PML %>% 
  group_by(Row) %>% 
  summarize(mean(withoutC_L_SE6)) 

Raw_means_withoutC_L_SE8 <- data_withoutC_raw_PML %>% 
  group_by(Row) %>% 
  summarize(mean(withoutC_L_SE8))

Raw_means_withoutC_L_SE12 <- data_withoutC_raw_PML %>% 
  group_by(Row) %>% 
  summarize(mean(withoutC_L_SE12))

Raw_means_withoutC_T_SEi8.b <- data_withoutC_raw_PML %>% 
  group_by(Row) %>% 
  summarize(mean(withoutC_T_SEi8.b))

Raw_means_withoutC_T_SEi12.b <- data_withoutC_raw_PML %>% 
  group_by(Row) %>% 
  summarize(mean(withoutC_T_SEi12.b)) 

Raw_means_withoutC_C_SE1 <- data_withoutC_raw_PML %>% 
  group_by(Row) %>% 
  summarize(mean(withoutC_C_SE1))

Raw_means_withoutC_Errvar_SE2 <- data_withoutC_raw_PML %>% 
  group_by(Row) %>% 
  summarize(mean(withoutC_Errvar_SE2)) 

Raw_means_withoutC_Errvar_SE6 <- data_withoutC_raw_PML %>% 
  group_by(Row) %>% 
  summarize(mean(withoutC_Errvar_SE6))

#combine all means
Raw_means_withoutC_PML <- cbind(Raw_means_withoutC_L_SE2, Raw_means_withoutC_L_SE6, Raw_means_withoutC_L_SE8, Raw_means_withoutC_L_SE12, Raw_means_withoutC_T_SEi8.b, Raw_means_withoutC_T_SEi12.b, Raw_means_withoutC_C_SE1, Raw_means_withoutC_Errvar_SE2, Raw_means_withoutC_Errvar_SE6)


Raw_means_withoutC_PML <- Raw_means_withoutC_PML %>% select(1,'mean(withoutC_L_SE2)', 'mean(withoutC_L_SE6)', 'mean(withoutC_L_SE8)', 'mean(withoutC_L_SE12)', 'mean(withoutC_T_SEi8.b)', 'mean(withoutC_T_SEi12.b)', 'mean(withoutC_C_SE1)', 'mean(withoutC_Errvar_SE2)', 'mean(withoutC_Errvar_SE6)')

colnames(Raw_means_withoutC_PML) <- c("Row","est_withoutC_L_SE2_PML", "est_withoutC_L_SE6_PML", "est_withoutC_L_SE8_PML", "est_withoutC_L_SE12_PML", "est_withoutC_T_SEi8.b_PML", "est_withoutC_T_SEi12.b_PML", "est_withoutC_C_SE1_PML", "est_withoutC_Errvar_SE2_PML", "est_withoutC_Errvar_SE6_PML")

# keeping 1 Row column
write.csv(Raw_means_withoutC_PML, file = "Raw_means_withoutC_PML(SE).csv")

# BIAS #
# For obtaining the true value, we need to take the sd of the estimate
data_withoutC_raw_SE <- read.csv("MIX_aov_withoutC_estR1_12.csv") 
data_withoutC_raw_SE_PML <- data_withoutC_raw_SE[data_withoutC_raw_SE$method == "PML",]

#change order
data_withoutC_raw_SE_PML <- data_withoutC_raw_SE_PML %>% arrange(factors) %>% arrange(nobs)

# create true values
true_withoutC_L_SE2_PML <- sd(data_withoutC_raw_SE_PML$withoutC_L2)
true_withoutC_L_SE6_PML <- sd(data_withoutC_raw_SE_PML$withoutC_L6)
true_withoutC_L_SE8_PML <- sd(data_withoutC_raw_SE_PML$withoutC_L8)
true_withoutC_L_SE12_PML <- sd(data_withoutC_raw_SE_PML$withoutC_L12)
true_withoutC_T_SEi8_PML <- sd(data_withoutC_raw_SE_PML$withoutC_Ti8.b)
true_withoutC_T_SEi12_PML <- sd(data_withoutC_raw_SE_PML$withoutC_Ti12.b)
true_withoutC_Errvar_SE2_PML <- sd(data_withoutC_raw_SE_PML$withoutC_Errvar2)
true_withoutC_Errvar_SE6_PML <- sd(data_withoutC_raw_SE_PML$withoutC_Errvar6)
true_withoutC_C_SE1_PML <- sd(data_withoutC_raw_SE_PML$withoutC_C1)

# calculate bias (Err withoutC)
data_withoutC_raw_PML$b_withoutC_L_SE2 <- 
  true_withoutC_L_SE2_PML - data_withoutC_raw_PML$withoutC_L_SE2
data_withoutC_raw_PML$b_withoutC_L_SE6 <- 
  true_withoutC_L_SE6_PML - data_withoutC_raw_PML$withoutC_L_SE6
data_withoutC_raw_PML$b_withoutC_L_SE8 <- 
  true_withoutC_L_SE8_PML - data_withoutC_raw_PML$withoutC_L_SE8
data_withoutC_raw_PML$b_withoutC_L_SE12 <- 
  true_withoutC_L_SE12_PML - data_withoutC_raw_PML$withoutC_L_SE12
data_withoutC_raw_PML$b_withoutC_T_SEi8.b <- 
  true_withoutC_T_SEi8_PML - data_withoutC_raw_PML$withoutC_T_SEi8.b
data_withoutC_raw_PML$b_withoutC_T_SEi12.b <- 
  true_withoutC_T_SEi12_PML - data_withoutC_raw_PML$withoutC_T_SEi12.b
data_withoutC_raw_PML$b_withoutC_C_SE1 <- 
  true_withoutC_Errvar_SE2_PML - data_withoutC_raw_PML$withoutC_C_SE1
data_withoutC_raw_PML$b_withoutC_Errvar_SE2 <- 
  true_withoutC_Errvar_SE6_PML - data_withoutC_raw_PML$withoutC_Errvar_SE2
data_withoutC_raw_PML$b_withoutC_Errvar_SE6 <- 
  true_withoutC_C_SE1_PML - data_withoutC_raw_PML$withoutC_Errvar_SE6

# add Row number 
new_df <- select(data_withoutC_raw_PML, Row, method, factors, nobs, b_withoutC_L_SE2, b_withoutC_L_SE6, b_withoutC_L_SE8, b_withoutC_L_SE12, b_withoutC_T_SEi8.b, b_withoutC_T_SEi12.b, b_withoutC_C_SE1, b_withoutC_Errvar_SE2, b_withoutC_Errvar_SE6)
new_df$Row <- rep(1:12, each = 250)

# calculating means of bias for each row seperately 
Means_withoutC_L_SE2 <- new_df %>% 
  group_by(Row) %>% 
  summarize(mean(b_withoutC_L_SE2))

Means_withoutC_L_SE6 <- new_df %>% 
  group_by(Row) %>% 
  summarize(mean(b_withoutC_L_SE6)) 

Means_withoutC_L_SE8 <- new_df %>% 
  group_by(Row) %>% 
  summarize(mean(b_withoutC_L_SE8)) 

Means_withoutC_L_SE12 <- new_df %>% 
  group_by(Row) %>% 
  summarize(mean(b_withoutC_L_SE12)) 

Means_withoutC_T_SEi8.b <- new_df %>% 
  group_by(Row) %>% 
  summarize(mean(b_withoutC_T_SEi8.b)) 

Means_withoutC_T_SEi12.b <- new_df %>% 
  group_by(Row) %>% 
  summarize(mean(b_withoutC_T_SEi12.b))

Means_withoutC_C_SE1 <- new_df %>% 
  group_by(Row) %>% 
  summarize(mean(b_withoutC_C_SE1))

Means_withoutC_Errvar_SE2 <- new_df %>% 
  group_by(Row) %>% 
  summarize(mean(b_withoutC_Errvar_SE2)) 

Means_withoutC_Errvar_SE6 <- new_df %>% 
  group_by(Row) %>% 
  summarize(mean(b_withoutC_Errvar_SE6))

#combine all means of bias
Means_withoutC_PML <- cbind(Means_withoutC_L_SE2, Means_withoutC_L_SE6, Means_withoutC_L_SE8, Means_withoutC_L_SE12, Means_withoutC_T_SEi8.b, Means_withoutC_T_SEi12.b, Means_withoutC_C_SE1, Means_withoutC_Errvar_SE2, Means_withoutC_Errvar_SE6)

# keeping 1 Row column
Means_withoutC_PML <- Means_withoutC_PML %>% select(1,'mean(b_withoutC_L_SE2)', 'mean(b_withoutC_L_SE6)', 'mean(b_withoutC_L_SE8)', 'mean(b_withoutC_L_SE12)', 'mean(b_withoutC_T_SEi8.b)', 'mean(b_withoutC_T_SEi12.b)', 'mean(b_withoutC_C_SE1)', 'mean(b_withoutC_Errvar_SE2)', 'mean(b_withoutC_Errvar_SE6)')

# rename columns
colnames(Means_withoutC_PML) <- c("Row", "bias_withoutC_L_SE2_PML", "bias_withoutC_L_SE6_PML", "bias_withoutC_L_SE8_PML", "bias_withoutC_L_SE12_PML", "bias_withoutC_T_SEi8.b_PML", "bias_withoutC_T_SEi12.b_PML", "bias_withoutC_C_SE1_PML", "bias_withoutC_Errvar_SE2_PML", "bias_withoutC_Errvar_SE6_PML")

write.csv(Means_withoutC_PML, file = "Means_withoutC_PML(SE).csv")

######################################## WLS
# All ANOVA results (raw data)
data_withoutC_raw <- read.csv("MIX_aov_withoutC_errR1_12.csv") 

data_withoutC_raw$method <- factor(data_withoutC_raw$method)
data_withoutC_raw$factors <- factor(data_withoutC_raw$factors)
data_withoutC_raw$nobs <- factor(data_withoutC_raw$nobs)

names(data_withoutC_raw)
data_withoutC_raw$method

data_withoutC_raw_WLS <- data_withoutC_raw[data_withoutC_raw$method == "DWLS",]

#change order (row 1 to 12)
data_withoutC_raw_WLS <- data_withoutC_raw_WLS %>% arrange(factors) %>% arrange(nobs)  
str(data_withoutC_raw_WLS)
#View(data_withoutC_raw_WLS)

# create labels values
data_withoutC_raw_WLS$Row <- rep(1:12, each = 250)

# add labs value
data_withoutC_raw_WLS <- select(data_withoutC_raw_WLS, Row, method, factors, nobs, withoutC_L_SE2, withoutC_L_SE6, withoutC_L_SE8, withoutC_L_SE12, withoutC_T_SEi8.b, withoutC_T_SEi12.b, withoutC_C_SE1, withoutC_Errvar_SE2, withoutC_Errvar_SE6)

data_withoutC_raw_WLS$Row <- as.factor(data_withoutC_raw_WLS$Row)

# calculating raw means
Raw_means_withoutC_L_SE2 <- data_withoutC_raw_WLS %>% 
  group_by(Row) %>% 
  summarize(mean(withoutC_L_SE2)) 

Raw_means_withoutC_L_SE6 <- data_withoutC_raw_WLS %>% 
  group_by(Row) %>% 
  summarize(mean(withoutC_L_SE6)) 

Raw_means_withoutC_L_SE8 <- data_withoutC_raw_WLS %>% 
  group_by(Row) %>% 
  summarize(mean(withoutC_L_SE8))

Raw_means_withoutC_L_SE12 <- data_withoutC_raw_WLS %>% 
  group_by(Row) %>% 
  summarize(mean(withoutC_L_SE12))

Raw_means_withoutC_T_SEi8.b <- data_withoutC_raw_WLS %>% 
  group_by(Row) %>% 
  summarize(mean(withoutC_T_SEi8.b))

Raw_means_withoutC_T_SEi12.b <- data_withoutC_raw_WLS %>% 
  group_by(Row) %>% 
  summarize(mean(withoutC_T_SEi12.b)) 

Raw_means_withoutC_C_SE1 <- data_withoutC_raw_WLS %>% 
  group_by(Row) %>% 
  summarize(mean(withoutC_C_SE1))

Raw_means_withoutC_Errvar_SE2 <- data_withoutC_raw_WLS %>% 
  group_by(Row) %>% 
  summarize(mean(withoutC_Errvar_SE2)) 

Raw_means_withoutC_Errvar_SE6 <- data_withoutC_raw_WLS %>% 
  group_by(Row) %>% 
  summarize(mean(withoutC_Errvar_SE6))

#combine all means
Raw_means_withoutC_WLS <- cbind(Raw_means_withoutC_L_SE2, Raw_means_withoutC_L_SE6, Raw_means_withoutC_L_SE8, Raw_means_withoutC_L_SE12, Raw_means_withoutC_T_SEi8.b, Raw_means_withoutC_T_SEi12.b, Raw_means_withoutC_C_SE1, Raw_means_withoutC_Errvar_SE2, Raw_means_withoutC_Errvar_SE6)


Raw_means_withoutC_WLS <- Raw_means_withoutC_WLS %>% select(1,'mean(withoutC_L_SE2)', 'mean(withoutC_L_SE6)', 'mean(withoutC_L_SE8)', 'mean(withoutC_L_SE12)', 'mean(withoutC_T_SEi8.b)', 'mean(withoutC_T_SEi12.b)', 'mean(withoutC_C_SE1)', 'mean(withoutC_Errvar_SE2)', 'mean(withoutC_Errvar_SE6)')

colnames(Raw_means_withoutC_WLS) <- c("Row","est_withoutC_L_SE2_WLS", "est_withoutC_L_SE6_WLS", "est_withoutC_L_SE8_WLS", "est_withoutC_L_SE12_WLS", "est_withoutC_T_SEi8.b_WLS", "est_withoutC_T_SEi12.b_WLS", "est_withoutC_C_SE1_WLS", "est_withoutC_Errvar_SE2_WLS", "est_withoutC_Errvar_SE6_WLS")

# keeping 1 Row column
write.csv(Raw_means_withoutC_WLS, file = "Raw_means_withoutC_WLS(SE).csv")

# BIAS #
# For obtaining the true value, we need to take the sd of the estimate
data_withoutC_raw_SE <- read.csv("MIX_aov_withoutC_estR1_12.csv") 
data_withoutC_raw_SE_WLS <- data_withoutC_raw_SE[data_withoutC_raw_SE$method == "DWLS",]

#change order (row 1 to 12)
data_withoutC_raw_SE_WLS <- data_withoutC_raw_SE_WLS %>% arrange(factors) %>% arrange(nobs)

# create true values
true_withoutC_L_SE2_WLS <- sd(data_withoutC_raw_SE_WLS$withoutC_L2)
true_withoutC_L_SE6_WLS <- sd(data_withoutC_raw_SE_WLS$withoutC_L6)
true_withoutC_L_SE8_WLS <- sd(data_withoutC_raw_SE_WLS$withoutC_L8)
true_withoutC_L_SE12_WLS <- sd(data_withoutC_raw_SE_WLS$withoutC_L12)
true_withoutC_T_SEi8_WLS <- sd(data_withoutC_raw_SE_WLS$withoutC_Ti8.b)
true_withoutC_T_SEi12_WLS <- sd(data_withoutC_raw_SE_WLS$withoutC_Ti12.b)
true_withoutC_Errvar_SE2_WLS <- sd(data_withoutC_raw_SE_WLS$withoutC_Errvar2)
true_withoutC_Errvar_SE6_WLS <- sd(data_withoutC_raw_SE_WLS$withoutC_Errvar6)
true_withoutC_C_SE1_WLS <- sd(data_withoutC_raw_SE_WLS$withoutC_C1)

# calculate bias (Err withoutC)
data_withoutC_raw_WLS$b_withoutC_L_SE2 <- 
  true_withoutC_L_SE2_WLS - data_withoutC_raw_WLS$withoutC_L_SE2
data_withoutC_raw_WLS$b_withoutC_L_SE6 <- 
  true_withoutC_L_SE6_WLS - data_withoutC_raw_WLS$withoutC_L_SE6
data_withoutC_raw_WLS$b_withoutC_L_SE8 <- 
  true_withoutC_L_SE8_WLS - data_withoutC_raw_WLS$withoutC_L_SE8
data_withoutC_raw_WLS$b_withoutC_L_SE12 <- 
  true_withoutC_L_SE12_WLS - data_withoutC_raw_WLS$withoutC_L_SE12
data_withoutC_raw_WLS$b_withoutC_T_SEi8.b <- 
  true_withoutC_T_SEi8_WLS - data_withoutC_raw_WLS$withoutC_T_SEi8.b
data_withoutC_raw_WLS$b_withoutC_T_SEi12.b <- 
  true_withoutC_T_SEi12_WLS - data_withoutC_raw_WLS$withoutC_T_SEi12.b
data_withoutC_raw_WLS$b_withoutC_C_SE1 <- 
  true_withoutC_Errvar_SE2_WLS - data_withoutC_raw_WLS$withoutC_C_SE1
data_withoutC_raw_WLS$b_withoutC_Errvar_SE2 <- 
  true_withoutC_Errvar_SE6_WLS - data_withoutC_raw_WLS$withoutC_Errvar_SE2
data_withoutC_raw_WLS$b_withoutC_Errvar_SE6 <- 
  true_withoutC_C_SE1_WLS - data_withoutC_raw_WLS$withoutC_Errvar_SE6

# add Row number 
new_df <- select(data_withoutC_raw_WLS, Row, method, factors, nobs, b_withoutC_L_SE2, b_withoutC_L_SE6, b_withoutC_L_SE8, b_withoutC_L_SE12, b_withoutC_T_SEi8.b, b_withoutC_T_SEi12.b, b_withoutC_C_SE1, b_withoutC_Errvar_SE2, b_withoutC_Errvar_SE6)
new_df$Row <- rep(1:12, each = 250)

# calculating means of bias for each row seperately 
Means_withoutC_L_SE2 <- new_df %>% 
  group_by(Row) %>% 
  summarize(mean(b_withoutC_L_SE2))

Means_withoutC_L_SE6 <- new_df %>% 
  group_by(Row) %>% 
  summarize(mean(b_withoutC_L_SE6)) 

Means_withoutC_L_SE8 <- new_df %>% 
  group_by(Row) %>% 
  summarize(mean(b_withoutC_L_SE8)) 

Means_withoutC_L_SE12 <- new_df %>% 
  group_by(Row) %>% 
  summarize(mean(b_withoutC_L_SE12)) 

Means_withoutC_T_SEi8.b <- new_df %>% 
  group_by(Row) %>% 
  summarize(mean(b_withoutC_T_SEi8.b)) 

Means_withoutC_T_SEi12.b <- new_df %>% 
  group_by(Row) %>% 
  summarize(mean(b_withoutC_T_SEi12.b))

Means_withoutC_C_SE1 <- new_df %>% 
  group_by(Row) %>% 
  summarize(mean(b_withoutC_C_SE1))

Means_withoutC_Errvar_SE2 <- new_df %>% 
  group_by(Row) %>% 
  summarize(mean(b_withoutC_Errvar_SE2)) 

Means_withoutC_Errvar_SE6 <- new_df %>% 
  group_by(Row) %>% 
  summarize(mean(b_withoutC_Errvar_SE6))

#combine all means of bias
Means_withoutC_WLS <- cbind(Means_withoutC_L_SE2, Means_withoutC_L_SE6, Means_withoutC_L_SE8, Means_withoutC_L_SE12, Means_withoutC_T_SEi8.b, Means_withoutC_T_SEi12.b, Means_withoutC_C_SE1, Means_withoutC_Errvar_SE2, Means_withoutC_Errvar_SE6)

# keeping 1 Row column
Means_withoutC_WLS <- Means_withoutC_WLS %>% select(1,'mean(b_withoutC_L_SE2)', 'mean(b_withoutC_L_SE6)', 'mean(b_withoutC_L_SE8)', 'mean(b_withoutC_L_SE12)', 'mean(b_withoutC_T_SEi8.b)', 'mean(b_withoutC_T_SEi12.b)', 'mean(b_withoutC_C_SE1)', 'mean(b_withoutC_Errvar_SE2)', 'mean(b_withoutC_Errvar_SE6)')

# rename columns
colnames(Means_withoutC_WLS) <- c("Row", "bias_withoutC_L_SE2_WLS", "bias_withoutC_L_SE6_WLS", "bias_withoutC_L_SE8_WLS", "bias_withoutC_L_SE12_WLS", "bias_withoutC_T_SEi8.b_WLS", "bias_withoutC_T_SEi12.b_WLS", "bias_withoutC_C_SE1_WLS", "bias_withoutC_Errvar_SE2_WLS", "bias_withoutC_Errvar_SE6_WLS")

write.csv(Means_withoutC_WLS, file = "Means_withoutC_WLS(SE).csv")

### MERGING ALL results
list.files()
Means_withoutC_PML <- read.csv("Means_withoutC_PML(SE).csv")
Means_withoutC_WLS <- read.csv("Means_withoutC_WLS(SE).csv")
Raw_means_withoutC_PML <- read.csv("Raw_means_withoutC_PML(SE).csv")
Raw_means_withoutC_WLS <- read.csv("Raw_means_withoutC_WLS(SE).csv")

# create column with true values
merged <- cbind(Means_withoutC_PML, 
                Means_withoutC_WLS, 
                Raw_means_withoutC_PML, 
                Raw_means_withoutC_WLS, 
                true_withoutC_L_SE2_WLS,
                true_withoutC_L_SE6_WLS, 
                true_withoutC_L_SE8_WLS,
                true_withoutC_L_SE12_WLS,
                true_withoutC_T_SEi8_WLS, 
                true_withoutC_T_SEi12_WLS, 
                true_withoutC_C_SE1_WLS,
                true_withoutC_Errvar_SE2_WLS, 
                true_withoutC_Errvar_SE6_WLS,
                true_withoutC_L_SE2_PML,
                true_withoutC_L_SE6_PML, 
                true_withoutC_L_SE8_PML,
                true_withoutC_L_SE12_PML,
                true_withoutC_T_SEi8_PML, 
                true_withoutC_T_SEi12_PML, 
                true_withoutC_C_SE1_PML,
                true_withoutC_Errvar_SE2_PML, 
                true_withoutC_Errvar_SE6_PML)

merged2 <- merged %>% select(2, 
                             true_withoutC_L_SE2_WLS, 
                             est_withoutC_L_SE2_WLS,
                             bias_withoutC_L_SE2_WLS,
                             true_withoutC_L_SE2_PML, 
                             est_withoutC_L_SE2_PML, 
                             bias_withoutC_L_SE2_PML, 
                             
                             true_withoutC_L_SE6_WLS, 
                             est_withoutC_L_SE6_WLS, 
                             bias_withoutC_L_SE6_WLS,
                             true_withoutC_L_SE6_PML,
                             est_withoutC_L_SE6_PML, 
                             bias_withoutC_L_SE6_PML, 
                             
                             true_withoutC_L_SE8_WLS, 
                             est_withoutC_L_SE8_WLS, 
                             bias_withoutC_L_SE8_WLS,
                             true_withoutC_L_SE8_PML,
                             est_withoutC_L_SE8_PML, 
                             bias_withoutC_L_SE8_PML, 
                             
                             true_withoutC_L_SE12_WLS, 
                             est_withoutC_L_SE12_WLS,
                             bias_withoutC_L_SE12_WLS,
                             true_withoutC_L_SE12_PML, 
                             est_withoutC_L_SE12_PML, 
                             bias_withoutC_L_SE12_PML, 
                             
                             true_withoutC_T_SEi8_WLS,
                             est_withoutC_T_SEi8.b_WLS,
                             bias_withoutC_T_SEi8.b_WLS,
                             true_withoutC_T_SEi8_PML,
                             est_withoutC_T_SEi8.b_PML, 
                             bias_withoutC_T_SEi8.b_PML,
                             
                             true_withoutC_T_SEi12_WLS, 
                             est_withoutC_T_SEi12.b_WLS,
                             bias_withoutC_T_SEi12.b_WLS,
                             true_withoutC_T_SEi12_PML,
                             est_withoutC_T_SEi12.b_PML, 
                             bias_withoutC_T_SEi12.b_PML,
                             
                             true_withoutC_C_SE1_WLS, 
                             est_withoutC_C_SE1_WLS,
                             bias_withoutC_C_SE1_WLS,
                             true_withoutC_C_SE1_PML,
                             est_withoutC_C_SE1_PML, 
                             bias_withoutC_C_SE1_PML,
                             
                             true_withoutC_Errvar_SE2_WLS, 
                             est_withoutC_Errvar_SE2_WLS,
                             bias_withoutC_Errvar_SE2_WLS,
                             true_withoutC_Errvar_SE2_PML,
                             est_withoutC_Errvar_SE2_PML, 
                             bias_withoutC_Errvar_SE2_PML,
                             
                             true_withoutC_Errvar_SE6_WLS, 
                             est_withoutC_Errvar_SE6_WLS,
                             bias_withoutC_Errvar_SE6_WLS,
                             true_withoutC_Errvar_SE6_PML,
                             est_withoutC_Errvar_SE6_PML, 
                             bias_withoutC_Errvar_SE6_PML)

MIX_withoutC_perRow_Table <- write.csv(merged2, "MIX_withoutC_perRow_Table(SE).csv")

# round values to two decimals
MIX_withoutC_perRow_Table_SE <- read_csv("MIX_withoutC_perRow_Table(SE).csv")
MIX_withoutC_perRow_Table_SE <- as.data.frame(MIX_withoutC_perRow_Table_SE)
MIX_withoutC_perRow_Table_SE <- MIX_withoutC_perRow_Table_SE %>% mutate_if(is.numeric,round, digits = 2)
MIX_withoutC_perRow_Table_SE <- write.csv(MIX_withoutC_perRow_Table_SE, "MIX_withoutC_perRow_Table(SE)-.csv")

#transposing matrix
# transposing matrix
MIX_withoutC_perRow_Table_SE <- read_csv("MIX_withoutC_perRow_Table(SE)-.csv")
MIX_withoutC_perRow_Table_SE_transpose <- as.data.frame(t(as.matrix(MIX_withoutC_perRow_Table_SE)))
write.csv(MIX_withoutC_perRow_Table_SE_transpose, "MIX_withoutC_perRow_Table(SE)_T.csv")

MIX_withoutC_perRow_T_SEable_T_SE <- read_csv("MIX_withoutC_perRow_Table(SE)_T.csv")
MIX_withoutC_perRow_T_SEable_T_SE <- MIX_withoutC_perRow_T_SEable_T_SE[-1,]
MIX_withoutC_perRow_T_SEable_T_SE <- MIX_withoutC_perRow_T_SEable_T_SE[-1,]
MIX_withoutC_perRow_T_SEable_T_SE <- MIX_withoutC_perRow_T_SEable_T_SE[-1,]

Rows <- 1:12
colnames(MIX_withoutC_perRow_T_SEable_T_SE) <- c("Row", Rows)

write.csv(MIX_withoutC_perRow_T_SEable_T_SE, "MIX_withoutC_perRow_T_SEable_T_SE.csv")


# OTHER (shared) FOLDER:
df <- read.csv("MIX_withoutC_perRow_Table(SE)_T.csv", header = TRUE)

# delete first column, change column names
df <- df[,-1]
Rows <- 1:12
colnames(df) <- c("Row", Rows)

# separate first column into 4 factors
df <- df %>% separate(Row, c("Value", "Model", "Parameter1", "Parameter2", "Method"), sep = "_")

df <- df[,-2]
df[is.na(df)] = "Both"

# change names
df$Value <- as.factor(df$Value)
levels(df$Value) <- c("Raw bias", "Estimate", "True")

df$Method <- as.factor(df$Method)
levels(df$Method) <- c("Both", "PML", "WLS")

means <- df %>% keep(is.numeric) %>% rowMeans()
df$Mean <- means

write.csv(df, "MIX_withoutC_perRow_Table(SE)_T.csv")


