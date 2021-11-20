###### Analysing significant ANOVA RESULTS (relative bias) #####
library(ez)
library(ggplot2)
library(jtools)
library(cowplot)
library(apa)
library(tidyverse)
library(ggpubr)

# MIXED simulation study

################################### Estimates ###################################
data_withC_rb <- read.csv("MIX_aov_withC_estR1_12_ID.csv")
data_withoutC_rb <- read.csv("MIX_aov_withoutC_estR1_12_ID.csv")
str(data_withoutC_rb)

######### create factors ######### 
data_withC_rb$method <- factor(data_withC_rb$method)
data_withC_rb$factors <- factor(data_withC_rb$factors)
data_withC_rb$nobs <- factor(data_withC_rb$nobs)
data_withC_rb$id <- factor(data_withC_rb$id)

data_withoutC_rb$method <- factor(data_withoutC_rb$method)
data_withoutC_rb$factors <- factor(data_withoutC_rb$factors)
data_withoutC_rb$nobs <- factor(data_withoutC_rb$nobs)
data_withoutC_rb$id <- factor(data_withoutC_rb$id)

################# Perform ANOVA ######################
############# correctly specified model ############# 
############# Dependent variable = factor loading 6 (continuous) ###########
sim_anova <- ezANOVA(
  data = data_withC_rb
  , dv = bias_withC_L6
  , wid = id
  , within = method
  , between = .(factors, nobs)
)

sim_anova$ANOVA

# difference in relative bias
data_withC_rb_WLS <- data_withC_rb[data_withC_rb$method == "DWLS",]
data_withC_rb_PML <- data_withC_rb[data_withC_rb$method == "PML",]

mean(data_withC_rb_WLS$bias_withC_L6) # relative bias of WLS: -0.2799963
mean(data_withC_rb_PML$bias_withC_L6) # relative bias of PML: -0.1924438

MIX_withC_perRow_Table <- read.csv("MIX_withC_perRow_Table.CSV")
mean(MIX_withC_perRow_Table$est_withC_L6_WLS) # 0.5988453
mean(MIX_withC_perRow_Table$est_withC_L6_PML) # 0.59832

################# Perform ANOVA ######################
############# correctly specified model ############# 
############# Dependent variable = factor loading a (continuous) ###########
sim_anova <- ezANOVA(
  data = data_withC_rb
  , dv = bias_withC_La
  , wid = id
  , within = method
  , between = .(factors, nobs)
)
sim_anova$ANOVA


# difference in relative bias
data_withC_rb_WLS <- data_withC_rb[data_withC_rb$method == "DWLS",]
data_withC_rb_PML <- data_withC_rb[data_withC_rb$method == "PML",]

mean(data_withC_rb_WLS$bias_withC_La) # relative bias of WLS: -0.6523466
mean(data_withC_rb_PML$bias_withC_La) # relative bias of PML: -0.3253505


MIX_withC_perRow_Table <- read.csv("MIX_withC_perRow_Table.CSV")
mean(MIX_withC_perRow_Table$est_withC_La_WLS) 
mean(MIX_withC_perRow_Table$est_withC_La_PML) 

MIX_withC_perRow_Table <- read.csv("MIX_withC_perRow_Table.CSV")
mean(MIX_withC_perRow_Table$est_withC_La_WLS) # 0.1993493
mean(MIX_withC_perRow_Table$est_withC_La_PML) # 0.1986953

################# Perform ANOVA ######################
############# correctly specified model ############# 
############# Dependent variable = factor loading 8 ###########
sim_anova <- ezANOVA(
  data = data_withC_rb
  , dv = bias_withC_L8
  , wid = id
  , within = method
  , between = .(factors, nobs)
)
sim_anova$ANOVA

str(data_withC_rb)

#three way interaction 
levels(data_withC_rb$factors) <- c("Factors = 2", "Factors = 4", "Factors = 6", "Factors = 8")

#zooming in on the difference (narrow y-axis)
data_withC_rb %>% 
  group_by(factors,method, nobs) %>% 
  summarise(L8 = mean(bias_withC_L8)) %>% 
  ggplot(mapping = aes(x = nobs, y = L8, 
                       shape = factors, linetype = method,
                       group = method)) + 
  geom_point(show.legend = F) + 
  geom_line(show.legend = F) +
  jtools::theme_apa() +
  labs(x = "N", 
       y = '% bias', 
       shape = "Number of factors",
       linetype = "Estimation method",
       title = expression("Relative bias of the estimated "~lambda[paste(8, ",", 2, sep = "")]), subtitle = "Data obtained from the correctly specified model") + 
  ylim(c(-1,1)) + facet_wrap(~factors)


data_withC_rb %>% 
  group_by(factors,method, nobs) %>% 
  summarise(L8 = mean(bias_withC_L8)) %>% 
  ggplot(mapping = aes(x = nobs, y = L8, 
                       shape = factors, linetype = method,
                       group = method)) + 
  geom_point(show.legend = F) + 
  geom_line(show.legend = F) +
  jtools::theme_apa() +
  labs(x = "N", 
       y = '% bias', 
       shape = "Number of factors",
       linetype = "Estimation method",
       title = expression("Relative bias of the estimated "~lambda[paste(8, ",", 2, sep = "")]), subtitle = "Data obtained from the correctly specified model") + 
  ylim(c(-40,30)) + facet_wrap(~factors)

#save plot
ggsave("M_Plot relative bias estimated lambda8 (withC).pdf",width = 7, height = 7)

################# Perform ANOVA ######################
############# correctly specified model ############# 
############# Dependent variable = factor loading 12 ###########
sim_anova <- ezANOVA(
  data = data_withC_rb
  , dv = bias_withC_L12
  , wid = id
  , within = method
  , between = .(factors, nobs)
)
sim_anova$ANOVA

# change levels of factors back to digits
levels(data_withC_rb$factors) <- c(2, 4, 6, 8)

# first two-way interaction effect = factors:method
plot1 <- data_withC_rb %>% 
  group_by(factors, method) %>% 
  summarise(L12 = mean(bias_withC_L12)) %>% 
  ggplot(mapping = aes(x = factors, y = L12, 
                       linetype = method, group = method)) +
  geom_point() + 
  geom_line(show.legend = F) +
  jtools::theme_apa() +
  labs(x = "Number of factors", 
       y = '% bias', 
       linetype = "Estimation method", title = expression("Relative bias of the estimated "~lambda[paste(12, ",", 2, sep = "")]), subtitle = "Data obtained from the correctly specified model") +  ylim(c(-40,30))

# second two-way interaction effect = nobs:method
plot2 <- data_withC_rb %>% 
  group_by(nobs, method) %>% 
  summarise(L12 = mean(bias_withC_L12)) %>% 
  ggplot(mapping = aes(x = nobs, y = L12, 
                       linetype = method, group = method)) +
  geom_point() + 
  geom_line(show.legend = F) +
  jtools::theme_apa() +
  labs(x = "N", 
       y = '% bias', 
       linetype = "Estimation method") + ylim(c(-40,30))

ggarrange(plot1, plot2, ncol = 1, labels=c("(a)", "(b)"), nrow = 2)

#save plot
ggsave("M_Plot relative bias estimated lambda12 (withC).pdf", width = 7, height = 9)

################# Perform ANOVA ######################
############# correctly specified model ############# 
############# Dependent variable = theta 6 (err val) ###########
sim_anova <- ezANOVA(
  data = data_withC_rb
  , dv = bias_withC_Errvar6
  , wid = id
  , within = method
  , between = .(factors, nobs)
)
sim_anova$ANOVA

data_withC_rb %>% 
  group_by(nobs, method) %>% 
  summarise(E6 = mean(bias_withC_Errvar6)) %>% 
  ggplot(mapping = aes(x = nobs, y = E6, 
                       linetype = method, group = method)) +
  geom_point() + 
  geom_line(show.legend = F) +
  jtools::theme_apa() +
  labs(x = "N", 
       y = '% bias', 
       linetype = "Estimation method",
       title = expression("Relative bias of the estimated "~theta[paste(6, ",", 1, sep = "")]), subtitle = "Data obtained from the correctly specified model") + ylim(c(-40,30))

#save plot
ggsave("M_Plot relative bias estimated errvar6 (withC).pdf", width = 7, height = 3.5)


################# Perform ANOVA ######################
################# correctly specified model ################# 
############# Dependent variable = correlation between factors ###########

sim_anova <- ezANOVA(
  data = data_withC_rb
  , dv = bias_withC_C1
  , wid = id
  , within = method
  , between = .(factors, nobs)
)
sim_anova$ANOVA

#three way interaction 
levels(data_withC_rb$factors) <- c("Factors = 2", "Factors = 4", "Factors = 6", "Factors = 8")

data_withC_rb %>% 
  group_by(factors,method, nobs) %>% 
  summarise(C1 = mean(bias_withC_C1)) %>% 
  ggplot(mapping = aes(x = nobs, y = C1, 
                      linetype = method,
                       group = method)) + 
  geom_point(show.legend = F) + 
  geom_line(show.legend = F) +
  jtools::theme_apa() +
  labs(x = "N", 
       y = '% bias', 
       linetype = "Estimation method",
       title = expression("Relative bias of the estimated "~psi[paste(1, ",", 2, sep = "")]), subtitle = "Data obtained from the correctly specified model") + 
  ylim(c(-40,30)) + facet_wrap(~factors)

#save plot
ggsave("M_Plot relative bias estimated correlation (withC).pdf",width = 7, height = 7)


################# Perform ANOVA ######################
################# misspecified model ################# 
############# Dependent variable = factor loading 2 (continuous) ###########
sim_anova <- ezANOVA(
  data = data_withoutC_rb
  , dv = bias_withoutC_L2
  , wid = id
  , within = method
  , between = .(factors, nobs)
)

sim_anova$ANOVA

data_withoutC_rb_WLS <- data_withoutC_rb[data_withoutC_rb$method == "DWLS",]
data_withoutC_rb_PML <- data_withoutC_rb[data_withoutC_rb$method == "PML",]

mean(data_withoutC_rb_WLS$bias_withoutC_L2) # relative bias of WLS: -3.108595
mean(data_withoutC_rb_PML$bias_withoutC_L2) # relative bias of PML: -1.28705

MIX_withoutC_perRow_Table <- read.csv("MIX_withoutC_perRow_Table.CSV")
mean(MIX_withoutC_perRow_Table$est_withoutC_L2_WLS) # 0.7751312
mean(MIX_withoutC_perRow_Table$est_withoutC_L2_PML) # 0.7897036

################# Perform ANOVA ######################
############# misspecified model ############# 
############# Dependent variable = factor loading 6 (continuous) ###########
sim_anova <- ezANOVA(
  data = data_withoutC_rb
  , dv = bias_withoutC_L6
  , wid = id
  , within = method
  , between = .(factors, nobs)
)

sim_anova$ANOVA

# two-way interaction effect = factors:method 
data_withoutC_rb %>% 
  group_by(factors, method) %>% 
  summarise(L6 = mean(bias_withoutC_L6)) %>% 
  ggplot(mapping = aes(x = factors, y = L6, 
                       linetype = method, group = method)) +
  geom_point() + 
  geom_line(show.legend = F) +
  jtools::theme_apa() +
  labs(x = "Number of factors", 
       y = '% bias', 
       linetype = "Estimation method", title = expression("Relative bias of the estimated "~lambda[paste(6, ",", 1, sep = "")]), subtitle = "Data obtained from the misspecified model") +  ylim(c(-40,30))

#save plot
ggsave("M_Plot relative bias estimated lambda6 (withoutC).pdf",width = 7, height = 3.5)

################# Perform ANOVA ######################
############# misspecified model ############# 
############# Dependent variable = factor loading 8 ###########
sim_anova <- ezANOVA(
  data = data_withoutC_rb
  , dv = bias_withoutC_L8
  , wid = id
  , within = method
  , between = .(factors, nobs)
)
sim_anova$ANOVA

#three way interaction
levels(data_withoutC_rb$factors) <- c("Factors = 2", "Factors = 4", "Factors = 6", "Factors = 8")

data_withoutC_rb %>% 
  group_by(factors,method, nobs) %>% 
  summarise(L8 = mean(bias_withoutC_L8)) %>% 
  ggplot(mapping = aes(x = nobs, y = L8, 
                       shape = factors, linetype = method,
                       group = method)) + 
  geom_point(show.legend = F) + 
  geom_line(show.legend = F) +
  jtools::theme_apa() +
  labs(x = "N", 
       y = '% bias', 
       shape = "Number of factors",
       linetype = "Estimation method",
       title = expression("Relative bias of the estimated "~lambda[paste(8, ",", 2, sep = "")]), subtitle = "Data obtained from the misspecified model") + 
  ylim(c(-40,30)) + facet_wrap(~factors)

#save plot
ggsave("M_Plot relative bias estimated lambda8 (withoutC).pdf",width = 7, height = 7)


################# Perform ANOVA ######################
############# misspecified model ############# 
############# Dependent variable = factor loading 12 ###########
sim_anova <- ezANOVA(
  data = data_withoutC_rb
  , dv = bias_withoutC_L12
  , wid = id
  , within = method
  , between = .(factors, nobs)
)
sim_anova$ANOVA

# change levels of factors back to digits
levels(data_withoutC_rb$factors) <- c(2, 4, 6, 8)

# first two-way interaction effect = factors:method
plot1 <- data_withoutC_rb %>% 
  group_by(factors, method) %>% 
  summarise(L12 = mean(bias_withoutC_L12)) %>% 
  ggplot(mapping = aes(x = factors, y = L12, 
                       linetype = method, group = method)) +
  geom_point() + 
  geom_line(show.legend = F) +
  jtools::theme_apa() +
  labs(x = "Number of factors", 
       y = '% bias', 
       linetype = "Estimation method", title = expression("Relative bias of the estimated "~lambda[paste(12, ",", 2, sep = "")]), subtitle = "Data obtained from the misspecified model") +  ylim(c(-40,30))

# second two-way interaction effect = nobs:method
plot2 <- data_withoutC_rb %>% 
  group_by(nobs, method) %>% 
  summarise(L12 = mean(bias_withoutC_L12)) %>% 
  ggplot(mapping = aes(x = nobs, y = L12, 
                       linetype = method, group = method)) +
  geom_point() + 
  geom_line(show.legend = F) +
  jtools::theme_apa() +
  labs(x = "N", 
       y = '% bias', 
       linetype = "Estimation method") + ylim(c(-40,30))

ggarrange(plot1, plot2, ncol = 1, labels=c("(a)", "(b)"), nrow = 2)

#save plot
ggsave("M_Plot relative bias estimated lambda12 (withoutC).pdf", width = 7, height = 9)

################# Perform ANOVA ######################
############# misspecified model ############# 
############# Dependent variable = theta 2 (err var) ###########
sim_anova <- ezANOVA(
  data = data_withoutC_rb
  , dv = bias_withoutC_Errvar2
  , wid = id
  , within = method
  , between = .(factors, nobs)
)
sim_anova$ANOVA

# main effect method

data_withoutC_rb_WLS <- data_withoutC_rb[data_withoutC_rb$method == "DWLS",]
data_withoutC_rb_PML <- data_withoutC_rb[data_withoutC_rb$method == "PML",]

mean(data_withoutC_rb_WLS$bias_withoutC_Errvar2) # relative bias of WLS: 8.998732
mean(data_withoutC_rb_PML$bias_withoutC_Errvar2) # relative bias of PML: 2.957053

str(MIX_withoutC_perRow_Table)
MIX_withoutC_perRow_Table <- read.csv("MIX_withoutC_perRow_Table.CSV")
mean(MIX_withoutC_perRow_Table$est_withoutC_Errvar2_WLS) # 0.3923954
mean(MIX_withoutC_perRow_Table$est_withoutC_Errvar2_PML) # 0.3706454

################# Perform ANOVA ######################
############# misspecified model ############# 
############# Dependent variable = theta 6 (err val) ###########

sim_anova <- ezANOVA(
  data = data_withoutC_rb
  , dv = bias_withoutC_Errvar6
  , wid = id
  , within = method
  , between = .(factors, nobs)
)
sim_anova$ANOVA

data_withoutC_rb %>% 
  group_by(nobs, method) %>% 
  summarise(E6 = mean(bias_withoutC_Errvar6)) %>% 
  ggplot(mapping = aes(x = nobs, y = E6, 
                       linetype = method, group = method)) +
  geom_point() + 
  geom_line(show.legend = F) +
  jtools::theme_apa() +
  labs(x = "N", 
       y = '% bias', 
       linetype = "Estimation method", title = expression("Relative bias of the estimated "~theta[paste(6, ",", 1, sep = "")]), subtitle = "Data obtained from the misspecified model") + ylim(c(-40,30))

#save plot
ggsave("M_Plot relative bias estimated errvar6 (withoutC).pdf", width = 7, height = 3.5)

################# Perform ANOVA ######################
################# misspecified model ################# 
############# Dependent variable = correlation between factors ###########

sim_anova <- ezANOVA(
  data = data_withoutC_rb
  , dv = bias_withoutC_C1
  , wid = id
  , within = method
  , between = .(factors, nobs)
)
sim_anova$ANOVA

#three way interaction 
levels(data_withoutC_rb$factors) <- c("Factors = 2", "Factors = 4", "Factors = 6", "Factors = 8")

data_withoutC_rb %>% 
  group_by(factors,method, nobs) %>% 
  summarise(C1 = mean(bias_withoutC_C1)) %>% 
  ggplot(mapping = aes(x = nobs, y = C1, 
                       shape = factors, linetype = method,
                       group = method)) + 
  geom_point(show.legend = F) + 
  geom_line(show.legend = F) +
  jtools::theme_apa() +
  labs(x = "N", 
       y = '% bias', 
       shape = "Number of factors",
       linetype = "Estimation method",
       title = expression("Relative bias of the estimated "~psi[paste(1, ",", 2, sep = "")]), subtitle = "Data obtained from the misspecified model") + 
  ylim(c(-40,30)) + facet_wrap(~factors)

#save plot
ggsave("M_Plot relative bias estimated correlation (withoutC).pdf",width = 7, height = 7)


#####################################################################    
#####################################################################  
######################## Standard Errors ############################
#####################################################################    
##################################################################### 

##############################################################################
################################### Errors ###################################
################################### With Crossloading ########################
##############################################################################
data_withC_rb <- read.csv("MIX_aov_withC_errR1_12_ID.csv")
#View(data_withC_rb)
data_withoutC_rb <- read.csv("MIX_aov_withoutC_errR1_12_ID.csv")
#View(data_withoutC_rb)

######### create factors (zodat deze kunnen wMIXen meegenomen in ANOVA) ######### 
data_withC_rb$method <- factor(data_withC_rb$method)
data_withC_rb$factors <- factor(data_withC_rb$factors)
data_withC_rb$nobs <- factor(data_withC_rb$nobs)
data_withC_rb$id <- factor(data_withC_rb$id)

data_withoutC_rb$method <- factor(data_withoutC_rb$method)
data_withoutC_rb$factors <- factor(data_withoutC_rb$factors)
data_withoutC_rb$nobs <- factor(data_withoutC_rb$nobs)
data_withoutC_rb$id <- factor(data_withoutC_rb$id)

################# Perform ANOVA ######################
############# correctly specified model ############# 
############# Dependent variable = factor loading 2 ###########
sim_anova <- ezANOVA(
  data = data_withC_rb
  , dv = bias_withC_L_SE2
  , wid = id
  , within = method
  , between = .(factors, nobs)
)
sim_anova$ANOVA

# two two-way interactions: 
#factors:method
#nobs:method

# change labels of factors 
plot1 <- data_withC_rb %>% 
  group_by(method, factors) %>% 
  summarise(SE_L2 = mean(bias_withC_L_SE2)) %>% 
  ggplot(mapping = aes(x = factors, y = SE_L2, 
                       group = method,
                       linetype = method)) + 
  geom_point(show.legend = F) + 
  geom_line(show.legend = F) +
  jtools::theme_apa(legend.use.title = F) +
  labs(x = "Number of factors", 
       y = '% bias', 
       shape = "Number of answer categories",
       linetype = "Estimation method",
       title = expression("Relative bias of the estimated SE of "~lambda[paste(2, ",", 1, sep = "")]), subtitle = "Data obtained from the correctly specified model") + ylim(c(-5,5))

plot2 <- data_withC_rb %>% 
  group_by(method, nobs) %>% 
  summarise(SE_L2 = mean(bias_withC_L_SE2)) %>% 
  ggplot(mapping = aes(x = nobs, y = SE_L2, 
                       group = method,
                       linetype = method)) + 
  geom_point() + 
  geom_line(show.legend = F) +
  jtools::theme_apa(legend.use.title = F) +
  labs(x = "N", 
       y = '% bias', 
       shape = "Number of answer categories",
       linetype = "Estimation method") + ylim(c(-5,5))

ggarrange(plot1, plot2, ncol = 1, labels=c("(a)", "(b)"), nrow = 2)

#save plot
ggsave("M_Plot relative bias estimated SE lambda2 (withC).pdf",width = 7, height = 9)


################# Perform ANOVA ######################
############# correctly specified model ############# 
############# Dependent variable = factor loading 6 ###########
sim_anova <- ezANOVA(
  data = data_withC_rb
  , dv = bias_withC_L_SE6
  , wid = id
  , within = method
  , between = .(factors, nobs)
)
sim_anova$ANOVA

# two two-way interactions: 
#factors:method
#nobs:method

# change labels of factors 
plot1 <- data_withC_rb %>% 
  group_by(method, factors) %>% 
  summarise(SE_L6 = mean(bias_withC_L_SE6)) %>% 
  ggplot(mapping = aes(x = factors, y = SE_L6, 
                       group = method,
                       linetype = method)) + 
  geom_point() + 
  geom_line(show.legend = F) +
  jtools::theme_apa(legend.use.title = F) +
  labs(x = "Number of factors", 
       y = '% bias', 
       shape = "Number of answer categories",
       linetype = "Estimation method",
       title = expression("Relative bias of the estimated SE of "~lambda[paste(6, ",", 1, sep = "")]), subtitle = "Data obtained from the correctly specified model") + ylim(c(-5,5))

plot2 <- data_withC_rb %>% 
  group_by(method, nobs) %>% 
  summarise(SE_L6 = mean(bias_withC_L_SE6)) %>% 
  ggplot(mapping = aes(x = nobs, y = SE_L6, 
                       group = method,
                       linetype = method)) + 
  geom_point() + 
  geom_line(show.legend = F) +
  jtools::theme_apa(legend.use.title = F) +
  labs(x = "N", 
       y = '% bias', 
       linetype = "Estimation method") + ylim(c(-5,5))

ggarrange(plot1, plot2, ncol = 1, labels=c("(a)", "(b)"), nrow = 2)

#save plot
ggsave("M_Plot relative bias estimated SE lambda6 (withC).pdf",width = 7, height = 9)

################# Perform ANOVA ######################
############# correctly specified model ############# 
############# Dependent variable = factor loading a ###########
sim_anova <- ezANOVA(
  data = data_withC_rb
  , dv = bias_withC_L_SEa
  , wid = id
  , within = method
  , between = .(factors, nobs)
)
sim_anova$ANOVA

levels(data_withC_rb$factors) <- c("Factors = 2", "Factors = 4", "Factors = 6", "Factors = 8")

data_withC_rb %>% 
  group_by(method, nobs, factors) %>% 
  summarise(SE_La = mean(bias_withC_L_SEa)) %>% 
  ggplot(mapping = aes(x = nobs, y = SE_La, 
                       shape = , 
                       group = method,
                       linetype = method)) + 
  geom_point() + 
  geom_line(show.legend = F) +
  jtools::theme_apa() +
  labs(x = "N", 
       y = '% bias', 
       shape = "Number of answer categories",
       linetype = "Estimation method",
       title = expression("Relative bias of the estimated SE of "~lambda[paste(6, ",", 2, sep = "")]), subtitle = "Data obtained from the correctly specified model") + ylim(c(-5,5)) + facet_wrap(~factors)

#save plot
ggsave("M_Plot relative bias estimated SE lambda_a (withC).pdf",width = 7, height = 7)

################# Perform ANOVA ######################
############# correctly specified model ############# 
############# Dependent variable = factor loading 8 ###########
sim_anova <- ezANOVA(
  data = data_withC_rb
  , dv = bias_withC_L_SE8
  , wid = id
  , within = method
  , between = .(factors, nobs)
)

sim_anova$ANOVA

levels(data_withC_rb$factors) <- c("Factors = 2", "Factors = 4", "Factors = 6", "Factors = 8")

data_withC_rb %>% 
  group_by(method, nobs, factors) %>% 
  summarise(SE_L8 = mean(bias_withC_L_SE8)) %>% 
  ggplot(mapping = aes(x = nobs, y = SE_L8, 
                       group = method,
                       linetype = method)) + 
  geom_point() + 
  geom_line(show.legend = F) +
  jtools::theme_apa(legend.use.title = F) +
  labs(x = "N", 
       y = '% bias', 
       shape = "Number of answer categories",
       linetype = "Estimation method",
       title = expression("Relative bias of the estimated SE of "~lambda[paste(8, ",", 2, sep = "")]), subtitle = "Data obtained from the correctly specified model") +
  ylim(c(-5,5)) + facet_wrap(~factors)

#save plot
ggsave("M_Plot relative bias estimated SE lambda8 (withC).pdf",width = 7, height = 7)

################# Perform ANOVA ######################
############# correctly specified model ############# 
############# Dependent variable = factor loading 12 ###########
sim_anova <- ezANOVA(
  data = data_withC_rb
  , dv = bias_withC_L_SE12
  , wid = id
  , within = method
  , between = .(factors, nobs)
)

sim_anova$ANOVA

levels(data_withC_rb$factors) <- c("Factors = 2", "Factors = 4", "Factors = 6", "Factors = 8")

data_withC_rb %>% 
  group_by(method, nobs, factors) %>% 
  summarise(SE_L12 = mean(bias_withC_L_SE12)) %>% 
  ggplot(mapping = aes(x = nobs, y = SE_L12, 
                       group = method,
                       linetype = method)) + 
  geom_point() + 
  geom_line(show.legend = F) +
  jtools::theme_apa(legend.use.title = F) +
  labs(x = "N", 
       y = '% bias', 
       shape = "Number of answer categories",
       linetype = "Estimation method",
       title = expression("Relative bias of the estimated SE of "~lambda[paste(12, ",", 2, sep = "")]), subtitle = "Data obtained from the correctly specified model") +
  ylim(c(-5,5)) + facet_wrap(~factors)

#save plot
ggsave("M_Plot relative bias estimated SE lambda12 (withC).pdf",width = 7, height = 7)

################# Perform ANOVA ######################
############# correctly specified model ############# 
############# Dependent variable = theta 2 (err val) ###########
sim_anova <- ezANOVA(
  data = data_withC_rb
  , dv = bias_withC_Errvar_SE2
  , wid = id
  , within = method
  , between = .(factors, nobs)
)
sim_anova$ANOVA

levels(data_withC_rb$factors) <- c("Factors = 2", "Factors = 4", "Factors = 6", "Factors = 8")

data_withC_rb %>% 
  group_by(method, nobs, factors) %>% 
  summarise(SE_E2 = mean(bias_withC_Errvar_SE2)) %>% 
  ggplot(mapping = aes(x = nobs, y = SE_E2, 
                       group = method,
                       linetype = method)) + 
  geom_point() + 
  geom_line(show.legend = F) +
  jtools::theme_apa(legend.use.title = F) +
  labs(x = "N", 
       y = '% bias', 
       shape = "Number of answer categories",
       linetype = "Estimation method",
       title = expression("Relative bias of the estimated SE of "~theta[paste(2, ",", 1, sep = "")]), subtitle = "Data obtained from the correctly specified model")  +
  ylim(c(-5,5)) + facet_wrap(~factors)

#save plot
ggsave("M_Plot relative bias estimated SE Errvar2 (withC).pdf", width = 7, height = 7)


################# Perform ANOVA ######################
############# correctly specified model ############# 
############# Dependent variable = theta 6 (err val) ###########
sim_anova <- ezANOVA(
  data = data_withC_rb
  , dv = bias_withC_Errvar_SE6
  , wid = id
  , within = method
  , between = .(factors, nobs)
)
sim_anova$ANOVA


data_withC_rb %>% 
  group_by(method, nobs, factors) %>% 
  summarise(SE_E6 = mean(bias_withC_Errvar_SE6)) %>% 
  ggplot(mapping = aes(x = nobs, y = SE_E6, 
                       group = method,
                       linetype = method)) + 
  geom_point() + 
  geom_line(show.legend = F) +
  jtools::theme_apa(legend.use.title = F) +
  labs(x = "N", 
       y = '% bias', 
       shape = "Number of answer categories",
       linetype = "Estimation method",
       title = expression("Relative bias of the estimated SE of "~theta[paste(6, ",", 1, sep = "")]), subtitle = "Data obtained from the correctly specified model")  +
  ylim(c(-5,5)) + facet_wrap(~factors)

#save plot
ggsave("M_Plot relative bias estimated SE Errvar6 (withC).pdf", width = 7, height = 7)

################# Perform ANOVA ######################
################# correctly specified model ################# 
############# Dependent variable = factor loading SE Threshold item 8 ###########
sim_anova <- ezANOVA(
  data = data_withC_rb, 
  dv = bias_withC_T_SEi8.b, 
  wid = id, 
  within = method, 
  between = .(factors, nobs), 
  detailed = T
)
sim_anova$ANOVA

data_withC_rb %>% 
  group_by(method, nobs, factors) %>% 
  summarise(T_SEi8 = mean(bias_withC_T_SEi8.b)) %>% 
  ggplot(mapping = aes(x = nobs, y = T_SEi8, 
                       group = method,
                       linetype = method)) + 
  geom_point() + 
  geom_line(show.legend = F) +
  jtools::theme_apa(legend.use.title = F) +
  labs(x = "N", 
       y = '% bias', 
       shape = "Number of answer categories",
       linetype = "Estimation method",
       title = expression("Relative bias of the estimated SE of "~tau[paste(8, ",", 2, sep = "")]), subtitle = "Data obtained from the correctly specified model")  +
  ylim(c(-5,5)) + facet_wrap(~factors)

#save plot
ggsave("M_Plot relative bias estimated SE Threshold lambda8 (withC).pdf", width = 7, height = 7)

################# Perform ANOVA ######################
################# correctly specified model ################# 
############# Dependent variable = factor loading SE Threshold item 12 ###########
sim_anova <- ezANOVA(
  data = data_withC_rb, 
  dv = bias_withC_T_SEi12.b, 
  wid = id, 
  within = method, 
  between = .(factors, nobs), 
  detailed = T
)
sim_anova$ANOVA

data_withC_rb %>% 
  group_by(method, nobs, factors) %>% 
  summarise(T_SEi12 = mean(bias_withC_T_SEi12.b)) %>% 
  ggplot(mapping = aes(x = nobs, y = T_SEi12, 
                       group = method,
                       linetype = method)) + 
  geom_point() + 
  geom_line(show.legend = F) +
  jtools::theme_apa(legend.use.title = F) +
  labs(x = "N", 
       y = '% bias', 
       shape = "Number of answer categories",
       linetype = "Estimation method",
       title = expression("Relative bias of the estimated SE of "~tau[paste(12, ",", 2, sep = "")]), subtitle = "Data obtained from the correctly specified model")  +
  ylim(c(-5,5)) + facet_wrap(~factors)

#save plot
ggsave("M_Plot relative bias estimated SE Threshold lambda12 (withC).pdf", width = 7, height = 7)

################# Perform ANOVA ######################
################# correctly specified model ################# 
############# Dependent variable = factor loading SE correlation ###########
sim_anova <- ezANOVA(
  data = data_withC_rb, 
  dv = bias_withC_C_SE1, 
  wid = id, 
  within = method, 
  between = .(factors, nobs), 
  detailed = T
)
sim_anova$ANOVA

# change labels of factors 
levels(data_withC_rb$factors) <- c(2, 4, 6, 8)

plot1 <- data_withC_rb %>% 
  group_by(method, factors) %>% 
  summarise(C_SE1 = mean(bias_withC_C_SE1)) %>% 
  ggplot(mapping = aes(x = factors, y = C_SE1, 
                       group = method,
                       linetype = method)) + 
  geom_point() + 
  geom_line(show.legend = F) +
  jtools::theme_apa(legend.use.title = F) +
  labs(x = "Number of factors", 
       y = '% bias', 
       shape = "Number of answer categories",
       linetype = "Estimation method",
       title = expression("Relative bias of the estimated SE of "~psi[paste(1, ",", 2, sep = "")]), subtitle = "Data obtained from the correctly specified model") + ylim(c(-5,5))

plot2 <- data_withC_rb %>% 
  group_by(method, nobs) %>% 
  summarise(C_SE1 = mean(bias_withC_C_SE1)) %>% 
  ggplot(mapping = aes(x = nobs, y = C_SE1, 
                       group = method,
                       linetype = method)) + 
  geom_point() + 
  geom_line(show.legend = F) +
  jtools::theme_apa(legend.use.title = F) +
  labs(x = "N", 
       y = '% bias', 
       shape = "Number of answer categories",
       linetype = "Estimation method") + ylim(c(-5,5))

ggarrange(plot1, plot2, ncol = 1, labels=c("(a)", "(b)"), nrow = 2)

#save plot
ggsave("M_Plot relative bias estimated SE correlation (withC).pdf",width = 7, height = 9)


##############################################################################
################################### Errors ###################################
################################### Without Crossloading #####################

##############################################################################
################# Perform ANOVA ##############################################
############# misspecified model ############# 
############# Dependent variable = factor loading 2 ###########
sim_anova <- ezANOVA(
  data = data_withoutC_rb
  , dv = bias_withoutC_L_SE2
  , wid = id
  , within = method
  , between = .(factors, nobs)
)
sim_anova$ANOVA

# two two-way interactions: 
#factors:method
#nobs:method

# change labels of factors 
plot1 <- data_withoutC_rb %>% 
  group_by(method, factors) %>% 
  summarise(SE_L2 = mean(bias_withoutC_L_SE2)) %>% 
  ggplot(mapping = aes(x = factors, y = SE_L2, 
                       group = method,
                       linetype = method)) + 
  geom_point() + 
  geom_line(show.legend = F) +
  jtools::theme_apa(legend.use.title = F) +
  labs(x = "Number of factors", 
       y = '% bias', 
       shape = "Number of answer categories",
       linetype = "Estimation method",
       title = expression("Relative bias of the estimated SE of "~lambda[paste(2, ",", 1, sep = "")]), subtitle = "Data obtained from the misspecified model") + ylim(c(-5,5))

plot2 <- data_withoutC_rb %>% 
  group_by(method, nobs) %>% 
  summarise(SE_L2 = mean(bias_withoutC_L_SE2)) %>% 
  ggplot(mapping = aes(x = nobs, y = SE_L2, 
                       group = method,
                       linetype = method)) + 
  geom_point() + 
  geom_line(show.legend = F) +
  jtools::theme_apa(legend.use.title = F) +
  labs(x = "N", 
       y = '% bias', 
       shape = "Number of answer categories",
       linetype = "Estimation method") + ylim(c(-5,5))

ggarrange(plot1, plot2, ncol = 1, labels=c("(a)", "(b)"), nrow = 2)

#save plot
ggsave("M_Plot relative bias estimated SE lambda2 (withoutC).pdf",width = 7, height = 9)


################# Perform ANOVA ######################
############# misspecified model ############# 
############# Dependent variable = factor loading 6 ###########
sim_anova <- ezANOVA(
  data = data_withoutC_rb
  , dv = bias_withoutC_L_SE6
  , wid = id
  , within = method
  , between = .(factors, nobs)
)
sim_anova$ANOVA

levels(data_withoutC_rb$factors) <- c("Factors = 2", "Factors = 4", "Factors = 6", "Factors = 8")

data_withoutC_rb %>% 
  group_by(method, nobs, factors) %>% 
  summarise(SE_L6 = mean(bias_withoutC_L_SE6)) %>% 
  ggplot(mapping = aes(x = nobs, y = SE_L6, 
                       shape = , 
                       group = method,
                       linetype = method)) + 
  geom_point() + 
  geom_line(show.legend = F) +
  jtools::theme_apa() +
  labs(x = "N", 
       y = '% bias', 
       shape = "Number of answer categories",
       linetype = "Estimation method",
       title = expression("Relative bias of the estimated SE of "~lambda[paste(6, ",", 1, sep = "")]), subtitle = "Data obtained from the misspecified model") + ylim(c(-5,5)) + facet_wrap(~factors)

#save plot
ggsave("M_Plot relative bias estimated SE lambda6 (withoutC).pdf",width = 7, height = 7)

################# Perform ANOVA ######################
############# misspecified model ############# 
############# Dependent variable = factor loading 8 ###########
sim_anova <- ezANOVA(
  data = data_withoutC_rb
  , dv = bias_withoutC_L_SE8
  , wid = id
  , within = method
  , between = .(factors, nobs)
)

sim_anova$ANOVA

levels(data_withoutC_rb$factors) <- c("Factors = 2", "Factors = 4", "Factors = 6", "Factors = 8")

data_withoutC_rb %>% 
  group_by(method, nobs, factors) %>% 
  summarise(SE_L8 = mean(bias_withoutC_L_SE8)) %>% 
  ggplot(mapping = aes(x = nobs, y = SE_L8, 
                       shape = , 
                       group = method,
                       linetype = method)) + 
  geom_point() + 
  geom_line(show.legend = F) +
  jtools::theme_apa(legend.use.title = F) +
  labs(x = "N", 
       y = '% bias', 
       shape = "Number of answer categories",
       linetype = "Estimation method",
       title = expression("Relative bias of the estimated SE of "~lambda[paste(8, ",", 2, sep = "")]), subtitle = "Data obtained from the misspecified model") +
  ylim(c(-5,5)) + facet_wrap(~factors)

#save plot
ggsave("M_Plot relative bias estimated SE lambda8 (withoutC).pdf",width = 7, height = 7)

################# Perform ANOVA ######################
############# misspecified model ############# 
############# Dependent variable = factor loading 12 ###########
sim_anova <- ezANOVA(
  data = data_withoutC_rb
  , dv = bias_withoutC_L_SE12
  , wid = id
  , within = method
  , between = .(factors, nobs)
)

sim_anova$ANOVA

levels(data_withoutC_rb$factors) <- c("Factors = 2", "Factors = 4", "Factors = 6", "Factors = 8")

data_withoutC_rb %>% 
  group_by(method, nobs, factors) %>% 
  summarise(SE_L12 = mean(bias_withoutC_L_SE12)) %>% 
  ggplot(mapping = aes(x = nobs, y = SE_L12, 
                       group = method,
                       linetype = method)) + 
  geom_point() + 
  geom_line(show.legend = F) +
  jtools::theme_apa(legend.use.title = F) +
  labs(x = "N", 
       y = '% bias', 
       shape = "Number of answer categories",
       linetype = "Estimation method",
       title = expression("Relative bias of the estimated SE of "~lambda[paste(12, ",", 2, sep = "")]), subtitle = "Data obtained from the misspecified model") +
  ylim(c(-5,5)) + facet_wrap(~factors)

#save plot
ggsave("M_Plot relative bias estimated SE lambda12 (withoutC).pdf",width = 7, height = 7)

################# Perform ANOVA ######################
############# misspecified model ############# 
############# Dependent variable = theta 2 (err val) ###########
sim_anova <- ezANOVA(
  data = data_withoutC_rb
  , dv = bias_withoutC_Errvar_SE2
  , wid = id
  , within = method
  , between = .(factors, nobs)
)
sim_anova$ANOVA

levels(data_withoutC_rb$factors) <- c("Factors = 2", "Factors = 4", "Factors = 6", "Factors = 8")

data_withoutC_rb %>% 
  group_by(method, nobs, factors) %>% 
  summarise(SE_E2 = mean(bias_withoutC_Errvar_SE2)) %>% 
  ggplot(mapping = aes(x = nobs, y = SE_E2, 
                       group = method,
                       linetype = method)) + 
  geom_point() + 
  geom_line(show.legend = F) +
  jtools::theme_apa(legend.use.title = F) +
  labs(x = "N", 
       y = '% bias', 
       shape = "Number of answer categories",
       linetype = "Estimation method",
       title = expression("Relative bias of the estimated SE of "~theta[paste(2, ",", 1, sep = "")]), subtitle = "Data obtained from the misspecified model")  +
  ylim(c(-5,5)) + facet_wrap(~factors)

#save plot
ggsave("M_Plot relative bias estimated SE Errvar2 (withoutC).pdf", width = 7, height = 7)


################# Perform ANOVA ######################
############# misspecified model ############# 
############# Dependent variable = theta 6 (err val) ###########
sim_anova <- ezANOVA(
  data = data_withoutC_rb
  , dv = bias_withoutC_Errvar_SE6
  , wid = id
  , within = method
  , between = .(factors, nobs)
)
sim_anova$ANOVA

data_withoutC_rb %>% 
  group_by(method, nobs, factors) %>% 
  summarise(SE_E6 = mean(bias_withoutC_Errvar_SE6)) %>% 
  ggplot(mapping = aes(x = nobs, y = SE_E6, 
                       group = method,
                       linetype = method)) + 
  geom_point() + 
  geom_line(show.legend = F) +
  jtools::theme_apa(legend.use.title = F) +
  labs(x = "N", 
       y = '% bias', 
       shape = "Number of answer categories",
       linetype = "Estimation method",
       title = expression("Relative bias of the estimated SE of "~theta[paste(6, ",", 1, sep = "")]), subtitle = "Data obtained from the misspecified model")  +
  ylim(c(-5,5)) + facet_wrap(~factors)

#save plot
ggsave("M_Plot relative bias estimated SE Errvar6 (withoutC).pdf", width = 7, height = 7)


################# Perform ANOVA ######################
################# misspecified model ################# 
############# Dependent variable = factor loading SE Threshold item 8 ###########
sim_anova <- ezANOVA(
  data = data_withoutC_rb, 
  dv = bias_withoutC_T_SEi8.b, 
  wid = id, 
  within = method, 
  between = .(factors, nobs), 
  detailed = T
)
sim_anova$ANOVA

data_withoutC_rb %>% 
  group_by(method, nobs, factors) %>% 
  summarise(T_SEi8 = mean(bias_withoutC_T_SEi8.b)) %>% 
  ggplot(mapping = aes(x = nobs, y = T_SEi8, 
                       group = method,
                       linetype = method)) + 
  geom_point() + 
  geom_line(show.legend = F) +
  jtools::theme_apa(legend.use.title = F) +
  labs(x = "N", 
       y = '% bias', 
       shape = "Number of answer categories",
       linetype = "Estimation method",
       title = expression("Relative bias of the estimated SE of "~tau[paste(8, ",", 2, sep = "")]), subtitle = "Data obtained from the misspecified model")  +
  ylim(c(-5,5)) + facet_wrap(~factors)

#save plot
ggsave("M_Plot relative bias estimated SE Threshold lambda8 (withoutC).pdf", width = 7, height = 7)

################# Perform ANOVA ######################
################# misspecified model ################# 
############# Dependent variable = factor loading SE Threshold item 12 ###########
sim_anova <- ezANOVA(
  data = data_withoutC_rb, 
  dv = bias_withoutC_T_SEi12.b, 
  wid = id, 
  within = method, 
  between = .(factors, nobs), 
  detailed = T
)
sim_anova$ANOVA

data_withoutC_rb %>% 
  group_by(method, nobs, factors) %>% 
  summarise(T_SEi12 = mean(bias_withoutC_T_SEi12.b)) %>% 
  ggplot(mapping = aes(x = nobs, y = T_SEi12, 
                       group = method,
                       linetype = method)) + 
  geom_point() + 
  geom_line(show.legend = F) +
  jtools::theme_apa(legend.use.title = F) +
  labs(x = "N", 
       y = '% bias', 
       shape = "Number of answer categories",
       linetype = "Estimation method",
       title = expression("Relative bias of the estimated SE of "~tau[paste(12, ",", 2, sep = "")]), subtitle = "Data obtained from the misspecified model")  +
  ylim(c(-5,5)) + facet_wrap(~factors)

#save plot
ggsave("M_Plot relative bias estimated SE Threshold lambda12 (withoutC).pdf", width = 7, height = 7)


################# Perform ANOVA ######################
################# misspecified model ################# 
############# Dependent variable = factor loading SE correlation ###########
sim_anova <- ezANOVA(
  data = data_withoutC_rb, 
  dv = bias_withoutC_C_SE1, 
  wid = id, 
  within = method, 
  between = .(factors, nobs), 
  detailed = T
)
sim_anova$ANOVA


data_withoutC_rb %>% 
  group_by(method, nobs) %>% 
  summarise(C_SE1 = mean(bias_withoutC_C_SE1)) %>% 
  ggplot(mapping = aes(x = nobs, y = C_SE1, 
                       group = method,
                       linetype = method)) + 
  geom_point() + 
  geom_line(show.legend = F) +
  jtools::theme_apa(legend.use.title = F) +
  labs(x = "N", 
       y = '% bias', 
       shape = "Number of answer categories",
       linetype = "Estimation method", 
       title = expression("Relative bias of the estimated SE of "~psi[paste(1, ",", 2, sep = "")]), subtitle = "Data obtained from the misspecified model") + ylim(c(-5,5))

#save plot
ggsave("M_Plot relative bias estimated SE correlation (withoutC).pdf",width = 7, height = 3.5)

