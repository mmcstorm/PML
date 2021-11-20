##### ORDINAL #####
library(purrr)
library(dplyr)
library(tidyr)
library(tidyverse)

# All ANOVA results (raw data)
###################################################################
########################## ESTIMATES ##############################
###################################################################
########################### WITH CROSSLOADING #####################
########################################################

# Inladen bestanden met ruwe factor ladingen en verander WLS naar DWLS: 
data_withC_raw <- read.csv("ORD_aov_withC_estR1_24.csv")
data_withC_raw$method <- factor(data_withC_raw$method)
levels(data_withC_raw$method) <- c("PML", "DWLS")
write_csv(data_withC_raw, "ORD_aov_withC_estR1_24.csv")

# create factors
data_withC_raw$method <- factor(data_withC_raw$method)
data_withC_raw$factors <- factor(data_withC_raw$factors)
data_withC_raw$nobs <- factor(data_withC_raw$nobs)
data_withC_raw$ncat <- factor(data_withC_raw$ncat)

data_withC_raw_PML <- data_withC_raw[data_withC_raw$method == "PML",]

#change order
data_withC_raw_PML <- data_withC_raw_PML %>% arrange(factors) %>% arrange(nobs) %>% arrange(ncat) 
str(data_withC_raw_PML)
View(data_withC_raw_PML)

# create labels values
data_withC_raw_PML$Row <- rep(1:24, each = 400)

# add labs value
data_withC_raw_PML <- select(data_withC_raw_PML, Row, method, factors, nobs, ncat, withC_L6, withC_La, withC_L7, withC_Ti6.b, withC_Ti7.b, withC_C1)

data_withC_raw_PML$Row <- as.factor(data_withC_raw_PML$Row)

# calculating raw means
Raw_means_withC_L6 <- data_withC_raw_PML %>% 
  group_by(Row) %>% 
  summarize(mean(withC_L6)) 

Raw_means_withC_La <- data_withC_raw_PML %>% 
  group_by(Row) %>% 
  summarize(mean(withC_La)) 

Raw_means_withC_L7 <- data_withC_raw_PML %>% 
  group_by(Row) %>% 
  summarize(mean(withC_L7)) 

Raw_means_withC_Ti6.b <- data_withC_raw_PML %>% 
  group_by(Row) %>% 
  summarize(mean(withC_Ti6.b)) 

Raw_means_withC_Ti7.b <- data_withC_raw_PML %>% 
  group_by(Row) %>% 
  summarize(mean(withC_Ti7.b))

Raw_means_withC_C1 <- data_withC_raw_PML %>% 
  group_by(Row) %>% 
  summarize(mean(withC_C1))

#combine all means
Raw_means_withC_PML <- cbind(Raw_means_withC_L6, Raw_means_withC_La, Raw_means_withC_L7, Raw_means_withC_Ti6.b, Raw_means_withC_Ti7.b, Raw_means_withC_C1)

# keeping 1 Row column
Raw_means_withC_PML <- Raw_means_withC_PML %>% select(1, 'mean(withC_L6)', 'mean(withC_La)', 'mean(withC_L7)', 'mean(withC_Ti6.b)', 'mean(withC_Ti7.b)', 'mean(withC_C1)')

colnames(Raw_means_withC_PML) <- c("Row", "est_withC_L6_PML", "est_withC_La_PML", "est_withC_L7_PML", "est_withC_Ti6.b_PML", "est_withC_Ti7.b_PML", "est_withC_C1_PML")

write.csv(Raw_means_withC_PML, file = "Raw_means_withC_PML.csv")

# BIAS #

# calculate bias (Est withC)
data_withC_raw_PML$b_withC_L6 <- (data_withC_raw_PML$withC_L6 - 0.6)
data_withC_raw_PML$b_withC_La <- (data_withC_raw_PML$withC_La - 0.2)
data_withC_raw_PML$b_withC_L7 <- (data_withC_raw_PML$withC_L7 - 0.8)
data_withC_raw_PML$b_withC_Ti6.b <- (data_withC_raw_PML$withC_Ti6.b - 0)
data_withC_raw_PML$b_withC_Ti7.b <- (data_withC_raw_PML$withC_Ti7.b - 0)
data_withC_raw_PML$b_withC_C1 <- (data_withC_raw_PML$withC_C1 - 0.3)

# add Row number 
new_df <- select(data_withC_raw_PML, Row, method, factors, nobs, ncat, b_withC_L6, b_withC_La, b_withC_L7, b_withC_Ti6.b, b_withC_Ti7.b, b_withC_C1)
new_df$Row <- rep(1:24, each = 400)

# calculating means of bias
Means_withC_L6 <- new_df %>% 
  group_by(Row) %>% 
  summarize(mean(b_withC_L6)) 

Means_withC_La <- new_df %>% 
  group_by(Row) %>% 
  summarize(mean(b_withC_La)) 

Means_withC_L7 <- new_df %>% 
  group_by(Row) %>% 
  summarize(mean(b_withC_L7)) 

Means_withC_Ti6.b <- new_df %>% 
  group_by(Row) %>% 
  summarize(mean(b_withC_Ti6.b)) 

Means_withC_Ti7.b <- new_df %>% 
  group_by(Row) %>% 
  summarize(mean(b_withC_Ti7.b))

Means_withC_C1 <- new_df %>% 
  group_by(Row) %>% 
  summarize(mean(b_withC_C1))

#combine all means of bias
Means_withC_PML <- cbind(Means_withC_L6, Means_withC_La, Means_withC_L7, Means_withC_Ti6.b, Means_withC_Ti7.b, Means_withC_C1)

# keeping 1 Row column
Means_withC_PML <- Means_withC_PML %>% select(1, 'mean(b_withC_L6)', 'mean(b_withC_La)', 'mean(b_withC_L7)', 'mean(b_withC_Ti6.b)', 'mean(b_withC_Ti7.b)', 'mean(b_withC_C1)')

# rename columns
colnames(Means_withC_PML) <- c("Row", "bias_withC_L6_PML", "bias_withC_La_PML", "bias_withC_L7_PML", "bias_withC_Ti6.b_PML", "bias_withC_Ti7.b_PML", "bias_withC_C1_PML")

write.csv(Means_withC_PML, file = "Means_withC_PML.csv")


######################################## DWLS
# All ANOVA results (raw data)

data_withC_raw <- read.csv("ORD_aov_withC_estR1_24.csv") 

data_withC_raw$method <- factor(data_withC_raw$method)
data_withC_raw$factors <- factor(data_withC_raw$factors)
data_withC_raw$nobs <- factor(data_withC_raw$nobs)
data_withC_raw$ncat <- factor(data_withC_raw$ncat)

names(data_withC_raw)
data_withC_raw$method

data_withC_raw_WLS <- data_withC_raw[data_withC_raw$method == "DWLS",]

#change order
data_withC_raw_WLS <- data_withC_raw_WLS %>% arrange(factors) %>% arrange(nobs) %>% arrange(ncat) 
str(data_withC_raw_WLS)
View(data_withC_raw_WLS)

# create labels values
data_withC_raw_WLS$Row <- rep(1:24, each = 400)

# add labs value
data_withC_raw_WLS <- select(data_withC_raw_WLS, Row, method, factors, nobs, ncat, withC_L6, withC_La, withC_L7, withC_Ti6.b, withC_Ti7.b, withC_C1)

data_withC_raw_WLS$Row <- as.factor(data_withC_raw_WLS$Row)

# calculating raw means
Raw_means_withC_L6 <- data_withC_raw_WLS %>% 
  group_by(Row) %>% 
  summarize(mean(withC_L6)) 

Raw_means_withC_La <- data_withC_raw_WLS %>% 
  group_by(Row) %>% 
  summarize(mean(withC_La)) 

Raw_means_withC_L7 <- data_withC_raw_WLS %>% 
  group_by(Row) %>% 
  summarize(mean(withC_L7)) 

Raw_means_withC_Ti6.b <- data_withC_raw_WLS %>% 
  group_by(Row) %>% 
  summarize(mean(withC_Ti6.b)) 

Raw_means_withC_Ti7.b <- data_withC_raw_WLS %>% 
  group_by(Row) %>% 
  summarize(mean(withC_Ti7.b))

Raw_means_withC_C1 <- data_withC_raw_WLS %>% 
  group_by(Row) %>% 
  summarize(mean(withC_C1))

#combine all means
Raw_means_withC_WLS <- cbind(Raw_means_withC_L6, Raw_means_withC_La, Raw_means_withC_L7, Raw_means_withC_Ti6.b, Raw_means_withC_Ti7.b, Raw_means_withC_C1)

# keeping 1 Row column
Raw_means_withC_WLS <- Raw_means_withC_WLS %>% select(1, 'mean(withC_L6)', 'mean(withC_La)', 'mean(withC_L7)', 'mean(withC_Ti6.b)', 'mean(withC_Ti7.b)', 'mean(withC_C1)')

colnames(Raw_means_withC_WLS) <- c("Row", "est_withC_L6_WLS", "est_withC_La_WLS", "est_withC_L7_WLS", "est_withC_Ti6.b_WLS", "est_withC_Ti7.b_WLS", "est_withC_C1_WLS")

write.csv(Raw_means_withC_WLS, file = "Raw_means_withC_WLS.csv")

# BIAS WLS #
# calculate bias (Est withC)
data_withC_raw_WLS$b_withC_L6 <- (data_withC_raw_WLS$withC_L6 - 0.6)
data_withC_raw_WLS$b_withC_La <- (data_withC_raw_WLS$withC_La - 0.2)
data_withC_raw_WLS$b_withC_L7 <- (data_withC_raw_WLS$withC_L7 - 0.8)
data_withC_raw_WLS$b_withC_Ti6.b <- (data_withC_raw_WLS$withC_Ti6.b - 0)
data_withC_raw_WLS$b_withC_Ti7.b <- (data_withC_raw_WLS$withC_Ti7.b - 0)
data_withC_raw_WLS$b_withC_C1 <- (data_withC_raw_WLS$withC_C1 - 0.3)

# add Row number 
new_df <- select(data_withC_raw_WLS, Row, method, factors, nobs, ncat, b_withC_L6, b_withC_La, b_withC_L7, b_withC_Ti6.b, b_withC_Ti7.b, b_withC_C1)
new_df$Row <- rep(1:24, each = 400)

# calculating means of bias
Means_withC_L6 <- new_df %>% 
  group_by(Row) %>% 
  summarize(mean(b_withC_L6)) 

Means_withC_La <- new_df %>% 
  group_by(Row) %>% 
  summarize(mean(b_withC_La)) 

Means_withC_L7 <- new_df %>% 
  group_by(Row) %>% 
  summarize(mean(b_withC_L7)) 

Means_withC_Ti6.b <- new_df %>% 
  group_by(Row) %>% 
  summarize(mean(b_withC_Ti6.b)) 

Means_withC_Ti7.b <- new_df %>% 
  group_by(Row) %>% 
  summarize(mean(b_withC_Ti7.b))

Means_withC_C1 <- new_df %>% 
  group_by(Row) %>% 
  summarize(mean(b_withC_C1))

#combine all means of bias
Means_withC_WLS <- cbind(Means_withC_L6, Means_withC_La, Means_withC_L7, Means_withC_Ti6.b, Means_withC_Ti7.b, Means_withC_C1)

# keeping 1 Row column
Means_withC_WLS <- Means_withC_WLS %>% select(1, 'mean(b_withC_L6)', 'mean(b_withC_La)', 'mean(b_withC_L7)', 'mean(b_withC_Ti6.b)', 'mean(b_withC_Ti7.b)', 'mean(b_withC_C1)')

# rename columns
colnames(Means_withC_WLS) <- c("Row", "bias_withC_L6_WLS", "bias_withC_La_WLS", "bias_withC_L7_WLS", "bias_withC_Ti6.b_WLS", "bias_withC_Ti7.b_WLS", "bias_withC_C1_WLS")

write.csv(Means_withC_WLS, file = "Means_withC_WLS.csv")

### MERGING ALL results
list.files()
Means_withC_PML <- read.csv("Means_withC_PML.csv")
Means_withC_WLS <- read.csv("Means_withC_WLS.csv")
Raw_means_withC_PML <- read.csv("Raw_means_withC_PML.csv")
Raw_means_withC_WLS <- read.csv("Raw_means_withC_WLS.csv")

names(merged)

# create column with true values
true_withC_L6 <- rep(0.6, 24)
true_withC_La <- rep(0.2, 24)
true_withC_L7 <- rep(0.8, 24)
true_withC_Ti6 <- rep(0, 24)
true_withC_Ti7 <- rep(0, 24)
true_withC_C1 <- rep(0.3, 24)

merged <- cbind(Means_withC_PML, 
                Means_withC_WLS, 
                Raw_means_withC_PML, 
                Raw_means_withC_WLS, 
                true_withC_L6, 
                true_withC_La, 
                true_withC_L7,
                true_withC_Ti6, 
                true_withC_Ti7, 
                true_withC_C1)

merged2 <- merged %>% select(2, 
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
                             
                             true_withC_L7, 
                             est_withC_L7_WLS,
                             bias_withC_L7_WLS,
                             est_withC_L7_PML, 
                             bias_withC_L7_PML, 
                             
                             true_withC_Ti6,
                             est_withC_Ti6.b_WLS,
                             bias_withC_Ti6.b_WLS,
                             est_withC_Ti6.b_PML, 
                             bias_withC_Ti6.b_PML,
                             
                             true_withC_Ti7, 
                             est_withC_Ti7.b_WLS,
                             bias_withC_Ti7.b_WLS,
                             est_withC_Ti7.b_PML, 
                             bias_withC_Ti7.b_PML,
                             
                             true_withC_C1, 
                             est_withC_C1_WLS,
                             bias_withC_C1_WLS,
                             est_withC_C1_PML, 
                             bias_withC_C1_PML)

ORD_withC_perRow_Table <- write.csv(merged2, "ORD_withC_perRow_Table.csv")

# round values to two decimals

ORD_withC_perRow_Table <- read_csv("ORD_withC_perRow_Table.csv")
ORD_withC_perRow_Table <- as.data.frame(ORD_withC_perRow_Table)
ORD_withC_perRow_Table <- ORD_withC_perRow_Table %>% mutate_if(is.numeric,round, digits = 2)
ORD_withC_perRow_Table <- write.csv(ORD_withC_perRow_Table, "ORD_withC_perRow_Table-.csv")

# transposing matrix
ORD_withC_perRow_Table <- read_csv("ORD_withC_perRow_Table-.csv")
ORD_withC_perRow_Table_transpose <- as.data.frame(t(as.matrix(ORD_withC_perRow_Table)))
write.csv(ORD_withC_perRow_Table_transpose, "ORD_withC_perRow_Table_T.csv")

# remove first few rows
ORD_withC_perRow_Table_T <- read_csv("ORD_withC_perRow_Table_T.csv")
ORD_withC_perRow_Table_T <- ORD_withC_perRow_Table_T[-1,]
ORD_withC_perRow_Table_T <- ORD_withC_perRow_Table_T[-1,]
ORD_withC_perRow_Table_T <- ORD_withC_perRow_Table_T[-1,]

Rows <- 1:24
colnames(ORD_withC_perRow_Table_T) <- c("Row", Rows)
write.csv(ORD_withC_perRow_Table_T, "ORD_withC_perRow_Table_T.csv")

# OTHER (shared) FOLDER:
df <- read.csv("ORD_withC_perRow_Table_T.csv", header = TRUE)

# delete first column, change column names
df <- df[,-1]
Rows <- 1:24
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

write.csv(df, "ORD_withC_perRow_Table_T.csv")

###########################################################
########################### WITHOUT CROSSLOADING #####################
###########################################################

# Inladen bestanden met ruwe factor ladingen: 
data_withoutC_raw <- read.csv("ORD_aov_withoutC_estR1_24.csv") 
data_withoutC_raw$method <- factor(data_withoutC_raw$method)
levels(data_withoutC_raw$method) <- c("PML", "DWLS")
write_csv(data_withoutC_raw, "ORD_aov_withoutC_estR1_24.csv")
data_withoutC_raw <- read.csv("ORD_aov_withoutC_estR1_24.csv")

data_withoutC_raw$method <- factor(data_withoutC_raw$method)
data_withoutC_raw$factors <- factor(data_withoutC_raw$factors)
data_withoutC_raw$nobs <- factor(data_withoutC_raw$nobs)
data_withoutC_raw$ncat <- factor(data_withoutC_raw$ncat)

names(data_withoutC_raw)
data_withoutC_raw$method

data_withoutC_raw_PML <- data_withoutC_raw[data_withoutC_raw$method == "PML",]

#change order
data_withoutC_raw_PML <- data_withoutC_raw_PML %>% arrange(factors) %>% arrange(nobs) %>% arrange(ncat) 
str(data_withoutC_raw_PML)
View(data_withoutC_raw_PML)

# create labels values
data_withoutC_raw_PML$Row <- rep(1:24, each = 400)

# add labs value
data_withoutC_raw_PML <- select(data_withoutC_raw_PML, Row, method, factors, nobs, ncat, withoutC_L6, withoutC_L7, withoutC_Ti6.b, withoutC_Ti7.b, withoutC_C1)

data_withoutC_raw_PML$Row <- as.factor(data_withoutC_raw_PML$Row)

# calculating raw means
Raw_means_withoutC_L6 <- data_withoutC_raw_PML %>% 
  group_by(Row) %>% 
  summarize(mean(withoutC_L6)) 

Raw_means_withoutC_L7 <- data_withoutC_raw_PML %>% 
  group_by(Row) %>% 
  summarize(mean(withoutC_L7)) 

Raw_means_withoutC_Ti6.b <- data_withoutC_raw_PML %>% 
  group_by(Row) %>% 
  summarize(mean(withoutC_Ti6.b)) 

Raw_means_withoutC_Ti7.b <- data_withoutC_raw_PML %>% 
  group_by(Row) %>% 
  summarize(mean(withoutC_Ti7.b))

Raw_means_withoutC_C1 <- data_withoutC_raw_PML %>% 
  group_by(Row) %>% 
  summarize(mean(withoutC_C1))

#combine all means
Raw_means_withoutC_PML <- cbind(Raw_means_withoutC_L6, Raw_means_withoutC_L7, Raw_means_withoutC_Ti6.b, Raw_means_withoutC_Ti7.b, Raw_means_withoutC_C1)

# keeping 1 Row column
Raw_means_withoutC_PML <- Raw_means_withoutC_PML %>% select(1, 'mean(withoutC_L6)', 'mean(withoutC_L7)', 'mean(withoutC_Ti6.b)', 'mean(withoutC_Ti7.b)', 'mean(withoutC_C1)')

colnames(Raw_means_withoutC_PML) <- c("Row", "est_withoutC_L6_PML", "est_withoutC_L7_PML", "est_withoutC_Ti6.b_PML", "est_withoutC_Ti7.b_PML", "est_withoutC_C1_PML")

write.csv(Raw_means_withoutC_PML, file = "Raw_means_withoutC_PML.csv")

# BIAS #

# calculate bias (Est withoutC)
data_withoutC_raw_PML$b_withoutC_L6 <- (data_withoutC_raw_PML$withoutC_L6 - 0.6)
data_withoutC_raw_PML$b_withoutC_L7 <- (data_withoutC_raw_PML$withoutC_L7 - 0.8)
data_withoutC_raw_PML$b_withoutC_Ti6.b <- (data_withoutC_raw_PML$withoutC_Ti6.b - 0)
data_withoutC_raw_PML$b_withoutC_Ti7.b <- (data_withoutC_raw_PML$withoutC_Ti7.b - 0)
data_withoutC_raw_PML$b_withoutC_C1 <- (data_withoutC_raw_PML$withoutC_C1 - 0.3)

# add Row number 
new_df <- select(data_withoutC_raw_PML, Row, method, factors, nobs, ncat, b_withoutC_L6, b_withoutC_L7, b_withoutC_Ti6.b, b_withoutC_Ti7.b, b_withoutC_C1)
new_df$Row <- rep(1:24, each = 400)

# calculating means of bias
Means_withoutC_L6 <- new_df %>% 
  group_by(Row) %>% 
  summarize(mean(b_withoutC_L6)) 

Means_withoutC_L7 <- new_df %>% 
  group_by(Row) %>% 
  summarize(mean(b_withoutC_L7)) 

Means_withoutC_Ti6.b <- new_df %>% 
  group_by(Row) %>% 
  summarize(mean(b_withoutC_Ti6.b)) 

Means_withoutC_Ti7.b <- new_df %>% 
  group_by(Row) %>% 
  summarize(mean(b_withoutC_Ti7.b))

Means_withoutC_C1 <- new_df %>% 
  group_by(Row) %>% 
  summarize(mean(b_withoutC_C1))

#combine all means of bias
Means_withoutC_PML <- cbind(Means_withoutC_L6, Means_withoutC_L7, Means_withoutC_Ti6.b, Means_withoutC_Ti7.b, Means_withoutC_C1)

# keeping 1 Row column
Means_withoutC_PML <- Means_withoutC_PML %>% select(1, 'mean(b_withoutC_L6)', 'mean(b_withoutC_L7)', 'mean(b_withoutC_Ti6.b)', 'mean(b_withoutC_Ti7.b)', 'mean(b_withoutC_C1)')

# rename columns
colnames(Means_withoutC_PML) <- c("Row", "bias_withoutC_L6_PML", "bias_withoutC_L7_PML", "bias_withoutC_Ti6.b_PML", "bias_withoutC_Ti7.b_PML", "bias_withoutC_C1_PML")

write.csv(Means_withoutC_PML, file = "Means_withoutC_PML.csv")


######################################## WLS
# All ANOVA results (raw data)

data_withoutC_raw <- read.csv("ORD_aov_withoutC_estR1_24.csv") # ID toevoegen

data_withoutC_raw$method <- factor(data_withoutC_raw$method)
data_withoutC_raw$factors <- factor(data_withoutC_raw$factors)
data_withoutC_raw$nobs <- factor(data_withoutC_raw$nobs)
data_withoutC_raw$ncat <- factor(data_withoutC_raw$ncat)

names(data_withoutC_raw)
data_withoutC_raw$method

data_withoutC_raw_WLS <- data_withoutC_raw[data_withoutC_raw$method == "DWLS",]

#change order
data_withoutC_raw_WLS <- data_withoutC_raw_WLS %>% arrange(factors) %>% arrange(nobs) %>% arrange(ncat) 
str(data_withoutC_raw_WLS)
View(data_withoutC_raw_WLS)

# create labels values
data_withoutC_raw_WLS$Row <- rep(1:24, each = 400)

# add labs value
data_withoutC_raw_WLS <- select(data_withoutC_raw_WLS, Row, method, factors, nobs, ncat, withoutC_L6, withoutC_L7, withoutC_Ti6.b, withoutC_Ti7.b, withoutC_C1)
data_withoutC_raw_WLS$Row <- as.factor(data_withoutC_raw_WLS$Row)

# calculating raw means
Raw_means_withoutC_L6 <- data_withoutC_raw_WLS %>% 
  group_by(Row) %>% 
  summarize(mean(withoutC_L6)) 

Raw_means_withoutC_L7 <- data_withoutC_raw_WLS %>% 
  group_by(Row) %>% 
  summarize(mean(withoutC_L7)) 

Raw_means_withoutC_Ti6.b <- data_withoutC_raw_WLS %>% 
  group_by(Row) %>% 
  summarize(mean(withoutC_Ti6.b)) 

Raw_means_withoutC_Ti7.b <- data_withoutC_raw_WLS %>% 
  group_by(Row) %>% 
  summarize(mean(withoutC_Ti7.b))

Raw_means_withoutC_C1 <- data_withoutC_raw_WLS %>% 
  group_by(Row) %>% 
  summarize(mean(withoutC_C1))

#combine all means
Raw_means_withoutC_WLS <- cbind(Raw_means_withoutC_L6, Raw_means_withoutC_L7, Raw_means_withoutC_Ti6.b, Raw_means_withoutC_Ti7.b, Raw_means_withoutC_C1)

# keeping 1 Row column
Raw_means_withoutC_WLS <- Raw_means_withoutC_WLS %>% select(1, 'mean(withoutC_L6)', 'mean(withoutC_L7)', 'mean(withoutC_Ti6.b)', 'mean(withoutC_Ti7.b)', 'mean(withoutC_C1)')

colnames(Raw_means_withoutC_WLS) <- c("Row", "est_withoutC_L6_WLS", "est_withoutC_L7_WLS", "est_withoutC_Ti6.b_WLS", "est_withoutC_Ti7.b_WLS", "est_withoutC_C1_WLS")

write.csv(Raw_means_withoutC_WLS, file = "Raw_means_withoutC_WLS.csv")

# BIAS WLS #
# calculate bias (Est withoutC)
data_withoutC_raw_WLS$b_withoutC_L6 <- (data_withoutC_raw_WLS$withoutC_L6 - 0.6)
data_withoutC_raw_WLS$b_withoutC_L7 <- (data_withoutC_raw_WLS$withoutC_L7 - 0.8)
data_withoutC_raw_WLS$b_withoutC_Ti6.b <- (data_withoutC_raw_WLS$withoutC_Ti6.b - 0)
data_withoutC_raw_WLS$b_withoutC_Ti7.b <- (data_withoutC_raw_WLS$withoutC_Ti7.b - 0)
data_withoutC_raw_WLS$b_withoutC_C1 <- (data_withoutC_raw_WLS$withoutC_C1 - 0.3)

# add Row number 
new_df <- select(data_withoutC_raw_WLS, Row, method, factors, nobs, ncat, b_withoutC_L6, b_withoutC_L7, b_withoutC_Ti6.b, b_withoutC_Ti7.b, b_withoutC_C1)
new_df$Row <- rep(1:24, each = 400)

# calculating means of bias
Means_withoutC_L6 <- new_df %>% 
  group_by(Row) %>% 
  summarize(mean(b_withoutC_L6)) 

Means_withoutC_L7 <- new_df %>% 
  group_by(Row) %>% 
  summarize(mean(b_withoutC_L7)) 

Means_withoutC_Ti6.b <- new_df %>% 
  group_by(Row) %>% 
  summarize(mean(b_withoutC_Ti6.b)) 

Means_withoutC_Ti7.b <- new_df %>% 
  group_by(Row) %>% 
  summarize(mean(b_withoutC_Ti7.b))

Means_withoutC_C1 <- new_df %>% 
  group_by(Row) %>% 
  summarize(mean(b_withoutC_C1))

#combine all means of bias
Means_withoutC_WLS <- cbind(Means_withoutC_L6, Means_withoutC_L7, Means_withoutC_Ti6.b, Means_withoutC_Ti7.b, Means_withoutC_C1)

# keeping 1 Row column
Means_withoutC_WLS <- Means_withoutC_WLS %>% select(1, 'mean(b_withoutC_L6)', 'mean(b_withoutC_L7)', 'mean(b_withoutC_Ti6.b)', 'mean(b_withoutC_Ti7.b)', 'mean(b_withoutC_C1)')

# rename columns
colnames(Means_withoutC_WLS) <- c("Row", "bias_withoutC_L6_WLS",  "bias_withoutC_L7_WLS", "bias_withoutC_Ti6.b_WLS", "bias_withoutC_Ti7.b_WLS", "bias_withoutC_C1_WLS")

write.csv(Means_withoutC_WLS, file = "Means_withoutC_WLS.csv")

### MERGING ALL results
list.files()
Means_withoutC_PML <- read.csv("Means_withoutC_PML.csv")
Means_withoutC_WLS <- read.csv("Means_withoutC_WLS.csv")
Raw_means_withoutC_PML <- read.csv("Raw_means_withoutC_PML.csv")
Raw_means_withoutC_WLS <- read.csv("Raw_means_withoutC_WLS.csv")

names(merged)

# create column with true values
true_withoutC_L6 <- rep(0.6, 24)
true_withoutC_L7 <- rep(0.8, 24)
true_withoutC_Ti6 <- rep(0, 24)
true_withoutC_Ti7 <- rep(0, 24)
true_withoutC_C1 <- rep(0.3, 24)

merged <- cbind(Means_withoutC_PML, 
                Means_withoutC_WLS, 
                Raw_means_withoutC_PML, 
                Raw_means_withoutC_WLS, 
                true_withoutC_L6, 
                true_withoutC_L7,
                true_withoutC_Ti6, 
                true_withoutC_Ti7, 
                true_withoutC_C1)

merged2 <- merged %>% select(2, 
                             true_withoutC_L6, 
                             est_withoutC_L6_WLS, 
                             bias_withoutC_L6_WLS,
                             est_withoutC_L6_PML, 
                             bias_withoutC_L6_PML,                              
                             true_withoutC_L7, 
                             est_withoutC_L7_WLS,
                             bias_withoutC_L7_WLS,
                             est_withoutC_L7_PML, 
                             bias_withoutC_L7_PML, 
                             
                             true_withoutC_Ti6,
                             est_withoutC_Ti6.b_WLS,
                             bias_withoutC_Ti6.b_WLS,
                             est_withoutC_Ti6.b_PML, 
                             bias_withoutC_Ti6.b_PML,
                             
                             true_withoutC_Ti7, 
                             est_withoutC_Ti7.b_WLS,
                             bias_withoutC_Ti7.b_WLS,
                             est_withoutC_Ti7.b_PML, 
                             bias_withoutC_Ti7.b_PML,
                             
                             true_withoutC_C1, 
                             est_withoutC_C1_WLS,
                             bias_withoutC_C1_WLS,
                             est_withoutC_C1_PML, 
                             bias_withoutC_C1_PML)

ORD_withoutC_perRow_Table <- write.csv(merged2, "ORD_withoutC_perRow_Table.csv")

# round values to two decimals
ORD_withoutC_perRow_Table <- read_csv("ORD_withoutC_perRow_Table.csv")
ORD_withoutC_perRow_Table <- as.data.frame(ORD_withoutC_perRow_Table)
ORD_withoutC_perRow_Table <- ORD_withoutC_perRow_Table %>% mutate_if(is.numeric,round, digits = 2)
ORD_withoutC_perRow_Table <- write.csv(ORD_withoutC_perRow_Table, "ORD_withoutC_perRow_Table-.csv")

# transposing matrix
ORD_withoutC_perRow_Table <- read_csv("ORD_withoutC_perRow_Table-.csv")
ORD_withoutC_perRow_Table_transpose <- as.data.frame(t(as.matrix(ORD_withoutC_perRow_Table)))
write.csv(ORD_withoutC_perRow_Table_transpose, "ORD_withoutC_perRow_Table_T.csv")

# remove first few rows
ORD_withoutC_perRow_Table_T <- read_csv("ORD_withoutC_perRow_Table_T.csv")
ORD_withoutC_perRow_Table_T <- ORD_withoutC_perRow_Table_T[,-1]
#ORD_withoutC_perRow_Table_T <- ORD_withoutC_perRow_Table_T[-1,]
#ORD_withoutC_perRow_Table_T <- ORD_withoutC_perRow_Table_T[-1,]

#Rows <- 1:24
colnames(ORD_withoutC_perRow_Table_T) <- c("Row", Rows)
write.csv(ORD_withoutC_perRow_Table_T, "ORD_withoutC_perRow_Table_T.csv")

df <- read.csv("ORD_withoutC_perRow_Table_T.csv", header = TRUE)

# delete first column, change column names
df <- df[,-1]
Rows <- 1:24
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

write.csv(df, "ORD_withoutC_perRow_Table_T.csv")



###################################################################
########################## ERRORS ################################
###################################################################
###################################################################
########################### WITH CROSSLOADING #####################
###################################################################
data_withC_raw <- read.csv("ORD_aov_withC_errR1_24.csv") # ID toevoegen
data_withC_raw$method <- factor(data_withC_raw$method)
levels(data_withC_raw$method) <- c("PML", "DWLS")
write_csv(data_withC_raw, "ORD_aov_withC_errR1_24.csv")

data_withC_raw$method <- factor(data_withC_raw$method)
data_withC_raw$factors <- factor(data_withC_raw$factors)
data_withC_raw$nobs <- factor(data_withC_raw$nobs)
data_withC_raw$ncat <- factor(data_withC_raw$ncat)

names(data_withC_raw)
data_withC_raw$method

data_withC_raw_PML <- data_withC_raw[data_withC_raw$method == "PML",]

#change order
data_withC_raw_PML <- data_withC_raw_PML %>% arrange(factors) %>% arrange(nobs) %>% arrange(ncat) 
str(data_withC_raw_PML)

# create labels values
data_withC_raw_PML$Row <- rep(1:24, each = 400)

# add labs value
data_withC_raw_PML <- select(data_withC_raw_PML, Row, method, factors, nobs, ncat, withC_L_SE6, withC_L_SEa, withC_L_SE7, withC_T_SEi6.b, withC_T_SEi7.b, withC_C_SE1)

data_withC_raw_PML$Row <- as.factor(data_withC_raw_PML$Row)

# For obtaining the true value, we need to take the sd of the estimate (not the SE)
data_withC_raw_SE <- read.csv("ORD_aov_withC_estR1_24.csv") 
data_withC_raw_SE_PML <- data_withC_raw_SE[data_withC_raw_SE$method == "PML",]

#change order
data_withC_raw_SE_PML <- data_withC_raw_SE_PML %>% arrange(factors) %>% arrange(nobs) %>% arrange(ncat) 

# create true values
true_withC_L_SE6_PML <- sd(data_withC_raw_SE_PML$withC_L6)
true_withC_L_SEa_PML <- sd(data_withC_raw_SE_PML$withC_La)
true_withC_L_SE7_PML <- sd(data_withC_raw_SE_PML$withC_L7)
true_withC_T_SEi6_PML <- sd(data_withC_raw_SE_PML$withC_Ti6.b)
true_withC_T_SEi7_PML <- sd(data_withC_raw_SE_PML$withC_Ti7.b)
true_withC_C_SE1_PML <- sd(data_withC_raw_SE_PML$withC_C1)

# calculating raw means
Raw_means_withC_L_SE6 <- data_withC_raw_PML %>% 
  group_by(Row) %>% 
  summarize(mean(withC_L_SE6)) 

Raw_means_withC_L_SEa <- data_withC_raw_PML %>% 
  group_by(Row) %>% 
  summarize(mean(withC_L_SEa)) 

Raw_means_withC_L_SE7 <- data_withC_raw_PML %>% 
  group_by(Row) %>% 
  summarize(mean(withC_L_SE7)) 

Raw_means_withC_T_SEi6.b <- data_withC_raw_PML %>% 
  group_by(Row) %>% 
  summarize(mean(withC_T_SEi6.b)) 

Raw_means_withC_T_SEi7.b <- data_withC_raw_PML %>% 
  group_by(Row) %>% 
  summarize(mean(withC_T_SEi7.b))

Raw_means_withC_C_SE1 <- data_withC_raw_PML %>% 
  group_by(Row) %>% 
  summarize(mean(withC_C_SE1))

#combine all means
Raw_means_withC_PML <- cbind(Raw_means_withC_L_SE6, Raw_means_withC_L_SEa, Raw_means_withC_L_SE7, Raw_means_withC_T_SEi6.b, Raw_means_withC_T_SEi7.b, Raw_means_withC_C_SE1)

colnames(Raw_means_withC_PML) <- c("Row", "err_withC_L_SE6_PML", "Row1", "err_withC_L_SEa_PML" ,"Row2", "err_withC_L_SE7_PML", "Row3", "err_withC_T_SEi6.b_PML", "Row4", "err_withC_T_SEi7.b_PML", "Row4", "err_withC_C_SE1_PML")

# keeping 1 Row column
Raw_means_withC_PML <- Raw_means_withC_PML %>% select("Row", "err_withC_L_SE6_PML", "err_withC_L_SEa_PML", "err_withC_L_SE7_PML", "err_withC_T_SEi6.b_PML", "err_withC_T_SEi7.b_PML", "err_withC_C_SE1_PML")

write.csv(Raw_means_withC_PML, file = "Raw_means_withC_PML(SE).csv")

# BIAS #
# calculate bias (Est withC)
data_withC_raw_PML$b_withC_L_SE6 <- data_withC_raw_PML$withC_L_SE6 - 
                                      true_withC_L_SE6_PML
data_withC_raw_PML$b_withC_L_SEa <- data_withC_raw_PML$withC_L_SEa - 
                                      true_withC_L_SEa_PML
data_withC_raw_PML$b_withC_L_SE7 <- data_withC_raw_PML$withC_L_SE7 - 
                                      true_withC_L_SE7_PML
data_withC_raw_PML$b_withC_T_SEi6.b <- data_withC_raw_PML$withC_T_SEi6.b - 
                                      true_withC_T_SEi6_PML
data_withC_raw_PML$b_withC_T_SEi7.b <- data_withC_raw_PML$withC_T_SEi7.b - 
                                      true_withC_T_SEi7_PML
data_withC_raw_PML$b_withC_C_SE1 <- data_withC_raw_PML$withC_C_SE1 - 
                                      true_withC_C_SE1_PML

# add Row number 
new_df <- select(data_withC_raw_PML, Row, method, factors, nobs, ncat, b_withC_L_SE6, b_withC_L_SEa, b_withC_L_SE7, b_withC_T_SEi6.b, b_withC_T_SEi7.b, b_withC_C_SE1)
new_df$Row <- rep(1:24, each = 400)

# calculating means of bias for each row seperately 
Means_withC_L_SE6 <- new_df %>% 
  group_by(Row) %>% 
  summarize(mean(b_withC_L_SE6)) 

Means_withC_L_SEa <- new_df %>% 
  group_by(Row) %>% 
  summarize(mean(b_withC_L_SEa)) 

Means_withC_L_SE7 <- new_df %>% 
  group_by(Row) %>% 
  summarize(mean(b_withC_L_SE7)) 

Means_withC_T_SEi6.b <- new_df %>% 
  group_by(Row) %>% 
  summarize(mean(b_withC_T_SEi6.b)) 

Means_withC_T_SEi7.b <- new_df %>% 
  group_by(Row) %>% 
  summarize(mean(b_withC_T_SEi7.b))

Means_withC_C_SE1 <- new_df %>% 
  group_by(Row) %>% 
  summarize(mean(b_withC_C_SE1))

#combine all means of bias
Means_withC_PML <- cbind(Means_withC_L_SE6, Means_withC_L_SEa, Means_withC_L_SE7, Means_withC_T_SEi6.b, Means_withC_T_SEi7.b, Means_withC_C_SE1)

colnames(Means_withC_PML) <- c("Row", "bias_withC_L_SE6_PML", "Row1", "bias_withC_L_SEa_PML","Row2", "bias_withC_L_SE7_PML", "Row3", "bias_withC_T_SEi6.b_PML", "Row4", "bias_withC_T_SEi7.b_PML", "Row4", "bias_withC_C_SE1_PML")

# keeping 1 Row column
# rename columns
Means_withC_PML <- Means_withC_PML %>% select("Row", "bias_withC_L_SE6_PML", "bias_withC_L_SEa_PML", "bias_withC_L_SE7_PML", "bias_withC_T_SEi6.b_PML", "bias_withC_T_SEi7.b_PML", "bias_withC_C_SE1_PML")

write.csv(Means_withC_PML, file = "Means_withC_PML(SE).csv")


######################################## WLS
# All ANOVA results (raw data)

# Inladen bestanden met ruwe factor ladingen: 
data_withC_raw <- read.csv("ORD_aov_withC_errR1_24.csv") # ID toevoegen?

data_withC_raw$method <- factor(data_withC_raw$method)
data_withC_raw$factors <- factor(data_withC_raw$factors)
data_withC_raw$nobs <- factor(data_withC_raw$nobs)
data_withC_raw$ncat <- factor(data_withC_raw$ncat)

names(data_withC_raw)
data_withC_raw$method

data_withC_raw_WLS <- data_withC_raw[data_withC_raw$method == "DWLS",]

#change order
data_withC_raw_WLS <- data_withC_raw_WLS %>% arrange(factors) %>% arrange(nobs) %>% arrange(ncat) 
str(data_withC_raw_WLS)
#View(data_withC_raw_WLS)

# create labels values
data_withC_raw_WLS$Row <- rep(1:24, each = 400)

# For obtaining the true value, we need to take the sd of the estimate (not the SE)
data_withC_raw_SE <- read.csv("ORD_aov_withC_estR1_24.csv") 
data_withC_raw_SE_WLS <- data_withC_raw_SE[data_withC_raw_SE$method == "DWLS",]

#change order
data_withC_raw_SE_WLS <- data_withC_raw_SE_WLS %>% arrange(factors) %>% arrange(nobs) %>% arrange(ncat) 

# create true values
true_withC_L_SE6_WLS <- sd(data_withC_raw_SE_WLS$withC_L6)
true_withC_L_SEa_WLS <- sd(data_withC_raw_SE_WLS$withC_La)
true_withC_L_SE7_WLS <- sd(data_withC_raw_SE_WLS$withC_L7)
true_withC_T_SEi6_WLS <- sd(data_withC_raw_SE_WLS$withC_Ti6.b)
true_withC_T_SEi7_WLS <- sd(data_withC_raw_SE_WLS$withC_Ti7.b)
true_withC_C_SE1_WLS <- sd(data_withC_raw_SE_WLS$withC_C1)

# add labs value
data_withC_raw_WLS <- select(data_withC_raw_WLS, Row, method, factors, nobs, ncat, withC_L_SE6, withC_L_SEa, withC_L_SE7, withC_T_SEi6.b, withC_T_SEi7.b, withC_C_SE1)
data_withC_raw_WLS$Row <- as.factor(data_withC_raw_WLS$Row)

# calculating raw means
Raw_means_withC_L_SE6 <- data_withC_raw_WLS %>% 
  group_by(Row) %>% 
  summarize(mean(withC_L_SE6)) 

Raw_means_withC_L_SEa <- data_withC_raw_WLS %>% 
  group_by(Row) %>% 
  summarize(mean(withC_L_SEa)) 

Raw_means_withC_L_SE7 <- data_withC_raw_WLS %>% 
  group_by(Row) %>% 
  summarize(mean(withC_L_SE7)) 

Raw_means_withC_T_SEi6.b <- data_withC_raw_WLS %>% 
  group_by(Row) %>% 
  summarize(mean(withC_T_SEi6.b))

Raw_means_withC_T_SEi7.b <- data_withC_raw_WLS %>% 
  group_by(Row) %>% 
  summarize(mean(withC_T_SEi7.b))

Raw_means_withC_C_SE1 <- data_withC_raw_WLS %>% 
  group_by(Row) %>% 
  summarize(mean(withC_C_SE1))

#combine all means
Raw_means_withC_WLS <- cbind(Raw_means_withC_L_SE6, Raw_means_withC_L_SEa, Raw_means_withC_L_SE7, Raw_means_withC_T_SEi6.b, Raw_means_withC_T_SEi7.b, Raw_means_withC_C_SE1)

colnames(Raw_means_withC_WLS) <- c("Row", "err_withC_L_SE6_WLS", "Row1", "err_withC_L_SEa_WLS","Row2", "err_withC_L_SE7_WLS", "Row3", "err_withC_T_SEi6.b_WLS", "Row4", "err_withC_T_SEi7.b_WLS", "Row4", "err_withC_C_SE1_WLS")

# keeping 1 Row column
Raw_means_withC_WLS <- Raw_means_withC_WLS %>% select("Row", "err_withC_L_SE6_WLS", "err_withC_L_SEa_WLS", "err_withC_L_SE7_WLS", "err_withC_T_SEi6.b_WLS", "err_withC_T_SEi7.b_WLS", "Row4", "err_withC_C_SE1_WLS")

write.csv(Raw_means_withC_WLS, file = "Raw_means_withC_WLS(SE).csv")

# BIAS #

# calculate bias (Est withC)
data_withC_raw_WLS$b_withC_L_SE6 <- data_withC_raw_WLS$withC_L_SE6 - 
                                       true_withC_L_SE6_WLS
data_withC_raw_WLS$b_withC_L_SEa <- data_withC_raw_WLS$withC_L_SEa - 
                                       true_withC_L_SEa_WLS
data_withC_raw_WLS$b_withC_L_SE7 <- data_withC_raw_WLS$withC_L_SE7 - 
                                       true_withC_L_SE7_WLS
data_withC_raw_WLS$b_withC_T_SEi6.b <- data_withC_raw_WLS$withC_T_SEi6.b - 
                                       true_withC_T_SEi6_WLS
data_withC_raw_WLS$b_withC_T_SEi7.b <- data_withC_raw_WLS$withC_T_SEi7.b - 
                                       true_withC_T_SEi7_WLS
data_withC_raw_WLS$b_withC_C_SE1 <- data_withC_raw_WLS$withC_C_SE1 - 
                                       true_withC_C_SE1_WLS

# add Row number 
new_df <- select(data_withC_raw_WLS, Row, method, factors, nobs, ncat, b_withC_L_SE6, b_withC_L_SEa, b_withC_L_SE7, b_withC_T_SEi6.b, b_withC_T_SEi7.b, b_withC_C_SE1)
new_df$Row <- rep(1:24, each = 400)

# calculating means of bias for each row seperately 
Means_withC_L_SE6 <- new_df %>% 
  group_by(Row) %>% 
  summarize(mean(b_withC_L_SE6)) 

Means_withC_L_SEa <- new_df %>% 
  group_by(Row) %>% 
  summarize(mean(b_withC_L_SEa)) 

Means_withC_L_SE7 <- new_df %>% 
  group_by(Row) %>% 
  summarize(mean(b_withC_L_SE7)) 

Means_withC_T_SEi6.b <- new_df %>% 
  group_by(Row) %>% 
  summarize(mean(b_withC_T_SEi6.b)) 

Means_withC_T_SEi7.b <- new_df %>% 
  group_by(Row) %>% 
  summarize(mean(b_withC_T_SEi7.b))

Means_withC_C_SE1 <- new_df %>% 
  group_by(Row) %>% 
  summarize(mean(b_withC_C_SE1))

#combine all means of bias
Means_withC_WLS <- cbind(Means_withC_L_SE6, Means_withC_L_SEa, Means_withC_L_SE7, Means_withC_T_SEi6.b, Means_withC_T_SEi7.b, Means_withC_C_SE1)

colnames(Means_withC_WLS) <- c("Row", "bias_withC_L_SE6_WLS", "Row1", "bias_withC_L_SEa_WLS","Row2", "bias_withC_L_SE7_WLS", "Row3", "bias_withC_T_SEi6.b_WLS", "Row4", "bias_withC_T_SEi7.b_WLS", "Row4", "bias_withC_C_SE1_WLS")

# keeping 1 Row column
# rename columns
Means_withC_WLS <- Means_withC_WLS %>% select("Row", "bias_withC_L_SE6_WLS", "bias_withC_L_SEa_WLS", "bias_withC_L_SE7_WLS", "bias_withC_T_SEi6.b_WLS", "bias_withC_T_SEi7.b_WLS", "bias_withC_C_SE1_WLS")

write.csv(Means_withC_WLS, file = "Means_withC_WLS(SE).csv")

### MERGING ALL results
list.files()
Means_withC_PML <- read.csv("Means_withC_PML(SE).csv")
Means_withC_WLS <- read.csv("Means_withC_WLS(SE).csv")
Raw_means_withC_PML <- read.csv("Raw_means_withC_PML(SE).csv")
Raw_means_withC_WLS <- read.csv("Raw_means_withC_WLS(SE).csv")

# create column with true values
merged <- cbind(Means_withC_PML, 
                Means_withC_WLS, 
                Raw_means_withC_PML, 
                Raw_means_withC_WLS, 
                true_withC_L_SE6_WLS, 
                true_withC_L_SEa_WLS, 
                true_withC_L_SE7_WLS,
                true_withC_T_SEi6_WLS, 
                true_withC_T_SEi7_WLS, 
                true_withC_C_SE1_WLS,
                true_withC_L_SE6_PML, 
                true_withC_L_SEa_PML, 
                true_withC_L_SE7_PML,
                true_withC_T_SEi6_PML, 
                true_withC_T_SEi7_PML, 
                true_withC_C_SE1_PML)

merged2 <- merged %>% select(2, 
                             true_withC_L_SE6_WLS, 
                             err_withC_L_SE6_WLS, 
                             bias_withC_L_SE6_WLS,
                             true_withC_L_SE6_PML,
                             err_withC_L_SE6_PML, 
                             bias_withC_L_SE6_PML, 
                             
                             true_withC_L_SEa_WLS, 
                             err_withC_L_SEa_WLS,
                             bias_withC_L_SEa_WLS,
                             true_withC_L_SEa_PML,
                             err_withC_L_SEa_PML, 
                             bias_withC_L_SEa_PML, 
                             
                             true_withC_L_SE7_WLS, 
                             err_withC_L_SE7_WLS,
                             bias_withC_L_SE7_WLS,
                             true_withC_L_SE7_PML,
                             err_withC_L_SE7_PML, 
                             bias_withC_L_SE7_PML, 
                             
                             true_withC_T_SEi6_WLS,
                             err_withC_T_SEi6.b_WLS,
                             bias_withC_T_SEi6.b_WLS,
                             true_withC_T_SEi6_PML,
                             err_withC_T_SEi6.b_PML, 
                             bias_withC_T_SEi6.b_PML,
                             
                             true_withC_T_SEi7_WLS, 
                             err_withC_T_SEi7.b_WLS,
                             bias_withC_T_SEi7.b_WLS,
                             true_withC_T_SEi7_PML,
                             err_withC_T_SEi7.b_PML, 
                             bias_withC_T_SEi7.b_PML,
                             
                             true_withC_C_SE1_WLS, 
                             err_withC_C_SE1_WLS,
                             bias_withC_C_SE1_WLS,
                             true_withC_C_SE1_PML,
                             err_withC_C_SE1_PML, 
                             bias_withC_C_SE1_PML)

ORD_withC_perRow_Table <- write.csv(merged2, "ORD_withC_perRow_Table(SE).csv")

# round values to two decimals

ORD_withC_perRow_Table_SE <- read_csv("ORD_withC_perRow_Table(SE).csv")
ORD_withC_perRow_Table_SE <- as.data.frame(ORD_withC_perRow_Table_SE)
ORD_withC_perRow_Table_SE <- ORD_withC_perRow_Table_SE %>% mutate_if(is.numeric,round, digits = 2)
ORD_withC_perRow_Table_SE <- write.csv(ORD_withC_perRow_Table_SE, "ORD_withC_perRow_Table(SE)-.csv")

# transposing matrix
ORD_withC_perRow_Table_SE <- read_csv("ORD_withC_perRow_Table(SE)-.csv")
ORD_withC_perRow_Table_SE_transpose <- as.data.frame(t(as.matrix(ORD_withC_perRow_Table_SE)))
write.csv(ORD_withC_perRow_Table_SE_transpose, "ORD_withC_perRow_Table(SE)_T.csv")

# remove first few rows
ORD_withC_perRow_Table_SE_T <- read_csv("ORD_withC_perRow_Table(SE)_T.csv")
ORD_withC_perRow_Table_SE_T <- ORD_withC_perRow_Table_SE_T[-1,]
ORD_withC_perRow_Table_SE_T <- ORD_withC_perRow_Table_SE_T[-1,]
ORD_withC_perRow_Table_SE_T <- ORD_withC_perRow_Table_SE_T[-1,]

Rows <- 1:24
colnames(ORD_withC_perRow_Table_SE_T) <- c("Row", Rows)
write.csv(ORD_withC_perRow_Table_SE_T, "ORD_withC_perRow_Table(SE)_T.csv")

# delete first column, change column names
df <- df[,-1]
Rows <- 1:24
colnames(df) <- c("Row", Rows)

# separate first column into 4 factors
df <- df %>% separate(Row, c("Value", "Model", "Parameter", "Parameter2", "Method"), sep = "_")

df <- df[,-2]
df[is.na(df)] = "Both"

# change names
df$Value <- as.factor(df$Value)
levels(df$Value) <- c("Raw bias", "Estimate", "True")

df$Method <- as.factor(df$Method)
levels(df$Method) <- c("Both", "PML", "WLS")

means <- df %>% keep(is.numeric) %>% rowMeans()
df$Mean <- means

write.csv(df, "ORD_withC_perRow_Table(SE)_T.csv")


###################################################################
########################### WITHOUT CROSSLOADING #####################
###################################################################
data_withoutC_raw <- read.csv("ORD_aov_withoutC_errR1_24.csv") # ID toevoegen

data_withoutC_raw$method <- factor(data_withoutC_raw$method)
data_withoutC_raw$factors <- factor(data_withoutC_raw$factors)
data_withoutC_raw$nobs <- factor(data_withoutC_raw$nobs)
data_withoutC_raw$ncat <- factor(data_withoutC_raw$ncat)

levels(data_withoutC_raw$method) <- c("PML", "DWLS")
write_csv(data_withoutC_raw, "ORD_aov_withoutC_errR1_24.csv")

names(data_withoutC_raw)
data_withoutC_raw$method

data_withoutC_raw_PML <- data_withoutC_raw[data_withoutC_raw$method == "PML",]

#change order
data_withoutC_raw_PML <- data_withoutC_raw_PML %>% arrange(factors) %>% arrange(nobs) %>% arrange(ncat) 
str(data_withoutC_raw_PML)
#View(data_withoutC_raw_PML)

# create labels values
data_withoutC_raw_PML$Row <- rep(1:24, each = 400)

# add labs value
data_withoutC_raw_PML <- select(data_withoutC_raw_PML, Row, method, factors, nobs, ncat, withoutC_L_SE6, withoutC_L_SE7, withoutC_T_SEi6.b, withoutC_T_SEi7.b, withoutC_C_SE1)
data_withoutC_raw_PML$Row <- as.factor(data_withoutC_raw_PML$Row)

# calculating raw means
Raw_means_withoutC_L_SE6 <- data_withoutC_raw_PML %>% 
  group_by(Row) %>% 
  summarize(mean(withoutC_L_SE6)) 

Raw_means_withoutC_L_SE7 <- data_withoutC_raw_PML %>% 
  group_by(Row) %>% 
  summarize(mean(withoutC_L_SE7)) 

Raw_means_withoutC_T_SEi6.b <- data_withoutC_raw_PML %>% 
  group_by(Row) %>% 
  summarize(mean(withoutC_T_SEi6.b))

Raw_means_withoutC_T_SEi7.b <- data_withoutC_raw_PML %>% 
  group_by(Row) %>% 
  summarize(mean(withoutC_T_SEi7.b))

Raw_means_withoutC_C_SE1 <- data_withoutC_raw_PML %>% 
  group_by(Row) %>% 
  summarize(mean(withoutC_C_SE1))

#combine all means
Raw_means_withoutC_PML <- cbind(Raw_means_withoutC_L_SE6, Raw_means_withoutC_L_SE7, Raw_means_withoutC_T_SEi6.b, Raw_means_withoutC_T_SEi7.b, Raw_means_withoutC_C_SE1)

colnames(Raw_means_withoutC_PML) <- c("Row", "err_withoutC_L_SE6_PML", "Row1", "err_withoutC_L_SE7_PML", "Row3", "err_withoutC_T_SEi6.b_PML", "Row4", "err_withoutC_T_SEi7.b_PML", "Row4", "err_withoutC_C_SE1_PML")

# keeping 1 Row column
Raw_means_withoutC_PML <- Raw_means_withoutC_PML %>% select("Row", "err_withoutC_L_SE6_PML", "err_withoutC_L_SE7_PML", "err_withoutC_T_SEi6.b_PML", "err_withoutC_T_SEi7.b_PML", "err_withoutC_C_SE1_PML")

write.csv(Raw_means_withoutC_PML, file = "Raw_means_withoutC_PML(SE).csv")

# BIAS #
# For obtaining the true value, we need to take the sd of the estimate (not the SE)
data_withoutC_raw_SE <- read.csv("ORD_aov_withoutC_estR1_24.csv") 
data_withoutC_raw_SE_PML <- data_withoutC_raw_SE[data_withoutC_raw_SE$method == "PML",]

#change order
data_withoutC_raw_SE_PML <- data_withoutC_raw_SE_PML %>% arrange(factors) %>% arrange(nobs) %>% arrange(ncat) 

# create true values
true_withoutC_L_SE6_PML <- sd(data_withoutC_raw_SE_PML$withoutC_L6)
true_withoutC_L_SE7_PML <- sd(data_withoutC_raw_SE_PML$withoutC_L7)
true_withoutC_T_SEi6_PML <- sd(data_withoutC_raw_SE_PML$withoutC_Ti6.b)
true_withoutC_T_SEi7_PML <- sd(data_withoutC_raw_SE_PML$withoutC_Ti7.b)
true_withoutC_C_SE1_PML <- sd(data_withoutC_raw_SE_PML$withoutC_C1)

# calculate bias (ERR withoutC)
data_withoutC_raw_PML$b_withoutC_L_SE6 <- data_withoutC_raw_PML$withoutC_L_SE6 - 
                                             true_withoutC_L_SE6_PML
data_withoutC_raw_PML$b_withoutC_L_SE7 <- data_withoutC_raw_PML$withoutC_L_SE7 - 
                                             true_withoutC_L_SE7_PML
data_withoutC_raw_PML$b_withoutC_T_SEi6.b <- data_withoutC_raw_PML$withoutC_T_SEi6.b - 
                                              true_withoutC_T_SEi6_PML
data_withoutC_raw_PML$b_withoutC_T_SEi7.b <- data_withoutC_raw_PML$withoutC_T_SEi7.b - 
                                              true_withoutC_T_SEi7_PML
data_withoutC_raw_PML$b_withoutC_C_SE1 <- data_withoutC_raw_PML$withoutC_C_SE1 - 
                                              true_withoutC_C_SE1_PML

# add Row number 
new_df <- select(data_withoutC_raw_PML, Row, method, factors, nobs, ncat, b_withoutC_L_SE6, b_withoutC_L_SE7, b_withoutC_T_SEi6.b, b_withoutC_T_SEi7.b, b_withoutC_C_SE1)
new_df$Row <- rep(1:24, each = 400)

# calculating means of bias for each row seperately 
Means_withoutC_L_SE6 <- new_df %>% 
  group_by(Row) %>% 
  summarize(mean(b_withoutC_L_SE6)) 

Means_withoutC_L_SE7 <- new_df %>% 
  group_by(Row) %>% 
  summarize(mean(b_withoutC_L_SE7)) 

Means_withoutC_T_SEi6.b <- new_df %>% 
  group_by(Row) %>% 
  summarize(mean(b_withoutC_T_SEi6.b)) 

Means_withoutC_T_SEi7.b <- new_df %>% 
  group_by(Row) %>% 
  summarize(mean(b_withoutC_T_SEi7.b))

Means_withoutC_C_SE1 <- new_df %>% 
  group_by(Row) %>% 
  summarize(mean(b_withoutC_C_SE1))

#combine all means of bias
Means_withoutC_PML <- cbind(Means_withoutC_L_SE6, Means_withoutC_L_SE7, Means_withoutC_T_SEi6.b, Means_withoutC_T_SEi7.b, Means_withoutC_C_SE1)

colnames(Means_withoutC_PML) <- c("Row", "bias_withoutC_L_SE6_PML", "Row1", "bias_withoutC_L_SE7_PML", "Row3", "bias_withoutC_T_SEi6.b_PML", "Row4", "bias_withoutC_T_SEi7.b_PML", "Row4", "bias_withoutC_C_SE1_PML")

# keeping 1 Row column
# rename columns
Means_withoutC_PML <- Means_withoutC_PML %>% select("Row", "bias_withoutC_L_SE6_PML", "bias_withoutC_L_SE7_PML", "bias_withoutC_T_SEi6.b_PML", "bias_withoutC_T_SEi7.b_PML", "bias_withoutC_C_SE1_PML")

write.csv(Means_withoutC_PML, file = "Means_withoutC_PML(SE).csv")


######################################## WLS
# All ANOVA results (raw data)

# Inladen bestanden met ruwe factor ladingen: 
data_withoutC_raw <- read.csv("ORD_aov_withoutC_errR1_24.csv") # ID toevoegen?

data_withoutC_raw$method <- factor(data_withoutC_raw$method)
data_withoutC_raw$factors <- factor(data_withoutC_raw$factors)
data_withoutC_raw$nobs <- factor(data_withoutC_raw$nobs)
data_withoutC_raw$ncat <- factor(data_withoutC_raw$ncat)

names(data_withoutC_raw)
data_withoutC_raw$method

data_withoutC_raw_WLS <- data_withoutC_raw[data_withoutC_raw$method == "DWLS",]

#change order
data_withoutC_raw_WLS <- data_withoutC_raw_WLS %>% arrange(factors) %>% arrange(nobs) %>% arrange(ncat) 
str(data_withoutC_raw_WLS)

# create labels values
data_withoutC_raw_WLS$Row <- rep(1:24, each = 400)

# add labs value
data_withoutC_raw_WLS <- select(data_withoutC_raw_WLS, Row, method, factors, nobs, ncat, withoutC_L_SE6, withoutC_L_SE7, withoutC_T_SEi6.b, withoutC_T_SEi7.b, withoutC_C_SE1)

data_withoutC_raw_WLS$Row <- as.factor(data_withoutC_raw_WLS$Row)

# calculating raw means
Raw_means_withoutC_L_SE6 <- data_withoutC_raw_WLS %>% 
  group_by(Row) %>% 
  summarize(mean(withoutC_L_SE6)) 

Raw_means_withoutC_L_SE7 <- data_withoutC_raw_WLS %>% 
  group_by(Row) %>% 
  summarize(mean(withoutC_L_SE7)) 

Raw_means_withoutC_T_SEi6.b <- data_withoutC_raw_WLS %>% 
  group_by(Row) %>% 
  summarize(mean(withoutC_T_SEi6.b)) 

Raw_means_withoutC_T_SEi7.b <- data_withoutC_raw_WLS %>% 
  group_by(Row) %>% 
  summarize(mean(withoutC_T_SEi7.b))

Raw_means_withoutC_C_SE1 <- data_withoutC_raw_WLS %>% 
  group_by(Row) %>% 
  summarize(mean(withoutC_C_SE1))

#combine all means
Raw_means_withoutC_WLS <- cbind(Raw_means_withoutC_L_SE6, Raw_means_withoutC_L_SE7, Raw_means_withoutC_T_SEi6.b, Raw_means_withoutC_T_SEi7.b, Raw_means_withoutC_C_SE1)

colnames(Raw_means_withoutC_WLS) <- c("Row", "err_withoutC_L_SE6_WLS", "Row1", "err_withoutC_L_SE7_WLS", "Row3", "err_withoutC_T_SEi6.b_WLS", "Row4", "err_withoutC_T_SEi7.b_WLS", "Row4", "err_withoutC_C_SE1_WLS")

# keeping 1 Row column
Raw_means_withoutC_WLS <- Raw_means_withoutC_WLS %>% select("Row", "err_withoutC_L_SE6_WLS", "err_withoutC_L_SE7_WLS", "err_withoutC_T_SEi6.b_WLS", "err_withoutC_T_SEi7.b_WLS", "Row4", "err_withoutC_C_SE1_WLS")

write.csv(Raw_means_withoutC_WLS, file = "Raw_means_withoutC_WLS(SE).csv")

# BIAS #
# For obtaining the true value, we need to take the sd of the estimate (not the SE)
data_withoutC_raw_SE <- read.csv("ORD_aov_withoutC_estR1_24.csv") 
data_withoutC_raw_SE_WLS <- data_withoutC_raw_SE[data_withoutC_raw_SE$method == "DWLS",]

#change order
data_withoutC_raw_SE_WLS <- data_withoutC_raw_SE_WLS %>% arrange(factors) %>% arrange(nobs) %>% arrange(ncat) 

# create true values
true_withoutC_L_SE6_WLS <- sd(data_withoutC_raw_SE_WLS$withoutC_L6)
true_withoutC_L_SE7_WLS <- sd(data_withoutC_raw_SE_WLS$withoutC_L7)
true_withoutC_T_SEi6_WLS <- sd(data_withoutC_raw_SE_WLS$withoutC_Ti6.b)
true_withoutC_T_SEi7_WLS <- sd(data_withoutC_raw_SE_WLS$withoutC_Ti7.b)
true_withoutC_C_SE1_WLS <- sd(data_withoutC_raw_SE_WLS$withoutC_C1)

# calculate bias (Est withoutC)
data_withoutC_raw_WLS$b_withoutC_L_SE6 <- data_withoutC_raw_WLS$withoutC_L_SE6 - 
                                             true_withoutC_L_SE6_WLS
data_withoutC_raw_WLS$b_withoutC_L_SE7 <- data_withoutC_raw_WLS$withoutC_L_SE7 - 
                                             true_withoutC_L_SE7_WLS
data_withoutC_raw_WLS$b_withoutC_T_SEi6.b <- data_withoutC_raw_WLS$withoutC_T_SEi6.b - 
                                             true_withoutC_T_SEi6_WLS
data_withoutC_raw_WLS$b_withoutC_T_SEi7.b <- data_withoutC_raw_WLS$withoutC_T_SEi7.b - 
                                              true_withoutC_T_SEi7_WLS
data_withoutC_raw_WLS$b_withoutC_C_SE1 <- data_withoutC_raw_WLS$withoutC_C_SE1 - 
                                            true_withoutC_C_SE1_WLS

# add Row number 
new_df <- select(data_withoutC_raw_WLS, Row, method, factors, nobs, ncat, b_withoutC_L_SE6, b_withoutC_L_SE7, b_withoutC_T_SEi6.b, b_withoutC_T_SEi7.b, b_withoutC_C_SE1)
new_df$Row <- rep(1:24, each = 400)

# calculating means of bias for each row seperately 
Means_withoutC_L_SE6 <- new_df %>% 
  group_by(Row) %>% 
  summarize(mean(b_withoutC_L_SE6)) 

Means_withoutC_L_SE7 <- new_df %>% 
  group_by(Row) %>% 
  summarize(mean(b_withoutC_L_SE7)) 

Means_withoutC_T_SEi6.b <- new_df %>% 
  group_by(Row) %>% 
  summarize(mean(b_withoutC_T_SEi6.b)) 

Means_withoutC_T_SEi7.b <- new_df %>% 
  group_by(Row) %>% 
  summarize(mean(b_withoutC_T_SEi7.b))

Means_withoutC_C_SE1 <- new_df %>% 
  group_by(Row) %>% 
  summarize(mean(b_withoutC_C_SE1))

#combine all means of bias
Means_withoutC_WLS <- cbind(Means_withoutC_L_SE6, Means_withoutC_L_SE7, Means_withoutC_T_SEi6.b, Means_withoutC_T_SEi7.b, Means_withoutC_C_SE1)

colnames(Means_withoutC_WLS) <- c("Row", "bias_withoutC_L_SE6_WLS", "Row1", "bias_withoutC_L_SE7_WLS", "Row3", "bias_withoutC_T_SEi6.b_WLS", "Row4", "bias_withoutC_T_SEi7.b_WLS", "Row4", "bias_withoutC_C_SE1_WLS")

# keeping 1 Row column
# rename columns
Means_withoutC_WLS <- Means_withoutC_WLS %>% select("Row", "bias_withoutC_L_SE6_WLS", "bias_withoutC_L_SE7_WLS", "bias_withoutC_T_SEi6.b_WLS", "bias_withoutC_T_SEi7.b_WLS", "bias_withoutC_C_SE1_WLS")

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
                true_withoutC_L_SE6_WLS, 
                true_withoutC_L_SE7_WLS,
                true_withoutC_T_SEi6_WLS, 
                true_withoutC_T_SEi7_WLS, 
                true_withoutC_C_SE1_WLS,
                true_withoutC_L_SE6_PML, 
                true_withoutC_L_SE7_PML,
                true_withoutC_T_SEi6_PML, 
                true_withoutC_T_SEi7_PML, 
                true_withoutC_C_SE1_PML)

merged2 <- merged %>% select(2, 
                             true_withoutC_L_SE6_WLS, 
                             err_withoutC_L_SE6_WLS, 
                             bias_withoutC_L_SE6_WLS,
                             true_withoutC_L_SE6_PML,
                             err_withoutC_L_SE6_PML, 
                             bias_withoutC_L_SE6_PML, 
                             
                             true_withoutC_L_SE7_WLS, 
                             err_withoutC_L_SE7_WLS,
                             bias_withoutC_L_SE7_WLS,
                             true_withoutC_L_SE7_PML,
                             err_withoutC_L_SE7_PML, 
                             bias_withoutC_L_SE7_PML, 
                             
                             true_withoutC_T_SEi6_WLS,
                             err_withoutC_T_SEi6.b_WLS,
                             bias_withoutC_T_SEi6.b_WLS,
                             true_withoutC_T_SEi6_PML,
                             err_withoutC_T_SEi6.b_PML, 
                             bias_withoutC_T_SEi6.b_PML,
                             
                             true_withoutC_T_SEi7_WLS, 
                             err_withoutC_T_SEi7.b_WLS,
                             bias_withoutC_T_SEi7.b_WLS,
                             true_withoutC_T_SEi7_PML,
                             err_withoutC_T_SEi7.b_PML, 
                             bias_withoutC_T_SEi7.b_PML,
                             
                             true_withoutC_C_SE1_WLS, 
                             err_withoutC_C_SE1_WLS,
                             bias_withoutC_C_SE1_WLS,
                             true_withoutC_C_SE1_PML,
                             err_withoutC_C_SE1_PML, 
                             bias_withoutC_C_SE1_PML)

ORD_withoutC_perRow_Table <- write.csv(merged2, "ORD_withoutC_perRow_Table(SE).csv")

ORD_withoutC_perRow_Table_SE <- read_csv("ORD_withoutC_perRow_Table(SE).csv")
ORD_withoutC_perRow_Table_SE <- as.data.frame(ORD_withoutC_perRow_Table_SE)
ORD_withoutC_perRow_Table_SE <- ORD_withoutC_perRow_Table_SE %>% mutate_if(is.numeric,round, digits = 2)
ORD_withoutC_perRow_Table_SE <- write.csv(ORD_withoutC_perRow_Table_SE, "ORD_withoutC_perRow_Table(SE)-.csv")

# transposing matrix
ORD_withoutC_perRow_Table_SE <- read_csv("ORD_withoutC_perRow_Table(SE)-.csv")
ORD_withoutC_perRow_Table_SE_transpose <- as.data.frame(t(as.matrix(ORD_withoutC_perRow_Table_SE)))
write.csv(ORD_withoutC_perRow_Table_SE_transpose, "ORD_withoutC_perRow_Table(SE)_T.csv")

# remove first few rows
ORD_withoutC_perRow_Table_SE_T <- read_csv("ORD_withoutC_perRow_Table(SE)_T.csv")
ORD_withoutC_perRow_Table_SE_T <- ORD_withoutC_perRow_Table_SE_T[-1,]
ORD_withoutC_perRow_Table_SE_T <- ORD_withoutC_perRow_Table_SE_T[-1,]
ORD_withoutC_perRow_Table_SE_T <- ORD_withoutC_perRow_Table_SE_T[-1,]

Rows <- 1:24
colnames(ORD_withoutC_perRow_Table_SE_T) <- c("Row", Rows)

write.csv(ORD_withoutC_perRow_Table_SE_T, "ORD_withoutC_perRow_Table(SE)_T.csv")

# OTHER (shared) FOLDER:
df <- read.csv("ORD_withoutC_perRow_Table(SE)_T.csv", header = TRUE)

# delete first column, change column names
df <- df[,-1]
Rows <- 1:24
colnames(df) <- c("Row", Rows)

# separate first column into 4 factors
df <- df %>% separate(Row, c("Value", "Model", "Parameter", "Parameter2", "Method"), sep = "_")

df <- df[,-2]
df[is.na(df)] = "Both"

# change names
df$Value <- as.factor(df$Value)
levels(df$Value) <- c("Raw bias", "Estimate", "True")

levels(df$Method) <- c("Both", "PML", "WLS")

means <- df %>% keep(is.numeric) %>% rowMeans()
df$Mean <- means

write.csv(df, "ORD_withoutC_perRow_Table(SE)_T.csv")