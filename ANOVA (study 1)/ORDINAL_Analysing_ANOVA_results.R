###### Analysing significant ANOVA results (relative bias) #####
library(ez)
library(ggplot2)
library(jtools)
library(cowplot)
library(apa)
library(tidyverse)
library(ggpubr)

# ORDINAL simulation study
################################### Estimates ###################################
data_withC_rb <- read.csv("ORD_aov_withC_estR1_24_ID.csv")
data_withoutC_rb <- read.csv("ORD_aov_withoutC_estR1_24_ID.csv")

######### create factors ######### 
data_withC_rb$method <- factor(data_withC_rb$method)
data_withC_rb$factors <- factor(data_withC_rb$factors)
data_withC_rb$nobs <- factor(data_withC_rb$nobs)
data_withC_rb$ncat <- factor(data_withC_rb$ncat)
data_withC_rb$id <- factor(data_withC_rb$id)

data_withoutC_rb$method <- factor(data_withoutC_rb$method)
data_withoutC_rb$factors <- factor(data_withoutC_rb$factors)
data_withoutC_rb$nobs <- factor(data_withoutC_rb$nobs)
data_withoutC_rb$ncat <- factor(data_withoutC_rb$ncat)
data_withoutC_rb$id <- factor(data_withoutC_rb$id)

################# Perform ANOVA ######################
############# correctly specified model ############# 
############# Dependent variable = factor loading 6 ###########
sim_anova <- ezANOVA(
  data = data_withC_rb
  , dv = bias_withC_L6
  , wid = id
  , within = method
  , between = .(factors, nobs, ncat)
)

AnovaResult <- cbind(sim_anova$ANOVA[1:3],round(sim_anova$ANOVA[,c(4,5,7)],digits=2))
AnovaResult$size <- ifelse(AnovaResult$ges >= 0.001, '*', '')
AnovaResult
sim_anova$ANOVA

# relative bias 
data_withC_rb_WLS <- data_withC_rb[data_withC_rb$method == "DWLS",]
data_withC_rb_PML <- data_withC_rb[data_withC_rb$method == "PML",]

mean(data_withC_rb_WLS$bias_withC_L6) # relative bias of WLS
mean(data_withC_rb_PML$bias_withC_L6) # relative bias of PML

ORD_withC_perRow_Table <- read.csv("ORD_withC_perRow_Table.CSV")
mean(ORD_withC_perRow_Table$est_withC_L6_WLS) # 0.6017396
mean(ORD_withC_perRow_Table$est_withC_L6_PML) # 0.5998829

################# Perform ANOVA ######################
############# correctly specified model ############# 
############# Dependent variable = factor loading 7 ###########
sim_anova <- ezANOVA(
  data = data_withC_rb
  , dv = bias_withC_L7
  , wid = id
  , within = method
  , between = .(factors, nobs, ncat)
)
sim_anova$ANOVA

AnovaResult <- cbind(sim_anova$ANOVA[1:3],round(sim_anova$ANOVA[,c(4,5,7)],digits=2))
AnovaResult$size <- ifelse(AnovaResult$ges >= 0.001, '*', '')
AnovaResult
sim_anova$ANOVA

ORD_withC_perRow_Table <- read.csv("ORD_withC_perRow_Table.CSV")
mean(ORD_withC_perRow_Table$est_withC_L7_WLS) # 0.8003068
mean(ORD_withC_perRow_Table$est_withC_L7_PML) # 0.7986991

#interaction effect ncat:method

data_withC_rb %>% 
  group_by(ncat, method) %>% 
  summarise(L7 = mean(bias_withC_L7)) %>% 
  ggplot(mapping = aes(x = ncat, y = L7, linetype = method,
                       group = method)) + 
  geom_point() + geom_line() +
  jtools::theme_apa(legend.use.title = T) + 
  labs(x = "Number of answer categories", 
       y = '% bias',
       linetype = "Estimation method",
       title = expression("Relative bias of the estimated "~lambda[paste(7, ",", 2, sep = "")]),
       subtitle = "Data obtained from the correctly specified model") +
  ylim(c(-1,22))

#save plot
ggsave("Plot relative bias estimated lambda7 (withC).pdf", width = 7, height = 3.5)

################# Perform ANOVA ######################
################# misspecified model ################# 
############# Dependent variable = factor loading 6 and 7 ###########
sim_anova <- ezANOVA(
  data = data_withoutC_rb, 
  dv = bias_withoutC_L6, 
  wid = id, 
  within = method, 
  between = .(factors, nobs, ncat), 
  detailed = T
)

AnovaResult <- cbind(sim_anova$ANOVA[1:3],round(sim_anova$ANOVA[,c(4,5,7)],digits=2))
AnovaResult$size <- ifelse(AnovaResult$ges >= 0.001, '*', '')
AnovaResult
sim_anova$ANOVA

AnovaResult <- cbind(sim_anova$ANOVA[1:3],round(sim_anova$ANOVA[,c(4,5,7)],digits=2))
AnovaResult$size <- ifelse(AnovaResult$ges >= 0.06, '*', '')
AnovaResult

sim_anova <- ezANOVA(
  data = data_withoutC_rb, 
  dv = bias_withoutC_L7, 
  wid = id, 
  within = method, 
  between = .(factors, nobs, ncat), 
  detailed = T
)

AnovaResult <- cbind(sim_anova$ANOVA[1:3],round(sim_anova$ANOVA[,c(4,5,7)],digits=2))
AnovaResult$size <- ifelse(AnovaResult$ges >= 0.001, '*', '')
AnovaResult
sim_anova$ANOVA

data_withoutC_rb %>% 
  group_by(ncat, method, nobs) %>% 
  summarise("6,1" = mean(bias_withoutC_L6),
            "7,2" = mean(bias_withoutC_L7)) %>%
  pivot_longer(cols = "6,1":"7,2", 
               names_to = "L_name", 
               values_to = "L_val") %>%
  ggplot(mapping = aes(x = nobs, y = L_val, 
                       shape = ncat, 
                       group = interaction(method,ncat),
                       linetype = method)) + 
  geom_point() + 
  geom_line() +
  jtools::theme_apa(legend.use.title = T) +
  labs(x = "N", 
       y = '% bias', 
       shape = "Number of answer categories",
       linetype = "Estimation method",
       title = expression("Relative bias of the estimated "~lambda[paste(6, ",", 1, sep = "")] ~and ~lambda[paste(7, ",", 2, sep = "")]),
       subtitle = "Data obtained from the misspecified model") + 
  ylim(c(-1,22)) +
  facet_wrap(~L_name, labeller = label_bquote(lambda[.(L_name)]))

#save plot
ggsave("Plot relative bias estimated lambda6 and lambda7 (withoutC).pdf", width = 7, height = 3.5)

################# Perform ANOVA ######################
################# misspecified model ################# 
############# Dependent variable = correlation between factors ###########

sim_anova <- ezANOVA(
  data = data_withoutC_rb, 
  dv = bias_withoutC_C1, 
  wid = id, 
  within = method, 
  between = .(factors, nobs, ncat), 
  detailed = T
)

AnovaResult <- cbind(sim_anova$ANOVA[1:3],round(sim_anova$ANOVA[,c(4,5,7)],digits=2))
AnovaResult$size <- ifelse(AnovaResult$ges >= 0.001, '*', '')
AnovaResult
sim_anova$ANOVA

# x-axis = factors
plot1 <- data_withoutC_rb %>% 
  group_by(factors, method) %>% 
  summarise(C1 = mean(bias_withoutC_C1)) %>% 
  ggplot(mapping = aes(x = factors, y = C1, 
                       linetype = method, group = method)) +
  geom_point() + 
  geom_line(show.legend = F) +
  jtools::theme_apa() +
  labs(x = "Number of factors", 
       y = '% bias', 
       linetype = "Estimation method", title = expression("Relative bias of the estimated "~psi[paste(1, ",", 2, sep = "")]), subtitle = "Data obtained from the misspecified model") +  ylim(c(-1,22)) 


# x-axis = nobs
plot2 <- data_withoutC_rb %>% 
  group_by(nobs, method) %>% 
  summarise(C1 = mean(bias_withoutC_C1)) %>% 
  ggplot(mapping = aes(x = nobs, y = C1, 
                       linetype = method, group = method)) +
  geom_point() + 
  geom_line(show.legend = F) +
  jtools::theme_apa() +
  labs(x = "N", 
       y = '% bias', 
       linetype = "Estimation method") + ylim(c(-1,22)) 


# x-axis = ncat
plot3 <- data_withoutC_rb %>% 
  group_by(ncat, method) %>% 
  summarise(C1 = mean(bias_withoutC_C1)) %>% 
  ggplot(mapping = aes(x = ncat, y = C1, 
                       linetype = method, group = method)) +
  geom_point() + 
  geom_line(show.legend = F) +
  jtools::theme_apa() +
  labs(x = "Number of answer categories", 
       y = '% bias', 
       linetype = "Estimation method") + ylim(c(-1,22))

ggarrange(plot1, plot2, plot3, ncol = 1, labels=c("(a)", "(b)", "(c)"), nrow = 3)

#save plot
ggsave("Plot relative bias estimated correlation (withoutC).pdf", width = 7, height = 9)

################################### Errors ###################################
################################### With C ###################################
# Inladen bestanden met relative bias:
data_withC_rb <- read.csv("ORD_aov_withC_errR1_24_ID.csv")
data_withoutC_rb <- read.csv("ORD_aov_withoutC_errR1_24_ID.csv")

######### create factors ######### 
data_withC_rb$method <- factor(data_withC_rb$method)
data_withC_rb$factors <- factor(data_withC_rb$factors)
data_withC_rb$nobs <- factor(data_withC_rb$nobs)
data_withC_rb$ncat <- factor(data_withC_rb$ncat)
data_withC_rb$id <- factor(data_withC_rb$id)

data_withoutC_rb$method <- factor(data_withoutC_rb$method)
data_withoutC_rb$factors <- factor(data_withoutC_rb$factors)
data_withoutC_rb$nobs <- factor(data_withoutC_rb$nobs)
data_withoutC_rb$ncat <- factor(data_withoutC_rb$ncat)
data_withoutC_rb$id <- factor(data_withoutC_rb$id)

################# Perform ANOVA ######################
############# correctly specified model ############# 
############# Dependent variable = factor loading 6 ###########
sim_anova <- ezANOVA(
  data = data_withC_rb
  , dv = bias_withC_L_SE6
  , wid = id
  , within = method
  , between = .(factors, nobs, ncat)
)
AnovaResult <- cbind(sim_anova$ANOVA[1:3],round(sim_anova$ANOVA[,c(4,5,7)],digits=2))
AnovaResult$size <- ifelse(AnovaResult$ges >= 0.001, '*', '')
AnovaResult
sim_anova$ANOVA

# change labels of factors 
levels(data_withC_rb$factors) <- c("Factors = 2", "Factors = 4", "Factors = 6", "Factors = 8")

data_withC_rb %>% 
  group_by(ncat, method, nobs, factors) %>% 
  summarise(SE_L6 = mean(bias_withC_L_SE6)) %>% 
  ggplot(mapping = aes(x = nobs, y = SE_L6, 
                       shape = ncat, 
                       group = interaction(method,ncat),
                       linetype = method)) + 
  geom_point() + 
  geom_line() +
  jtools::theme_apa(legend.use.title = T) +
  labs(x = "N", 
       y = '% bias', 
       shape = "Number of answer categories",
       linetype = "Estimation method",
       title = expression("Relative bias of the estimated SE of "~lambda[paste(6, ",", 1, sep = "")]), subtitle = "Data obtained from the correctly specified model") +
  ylim(c(-5,5)) + facet_wrap(~factors)

#save plot
ggsave("Plot relative bias estimated SE lambda6 (withC).pdf",width = 7, height = 7)

################# Perform ANOVA ######################
############# correctly specified model ############# 
############# Dependent variable = factor loading a ###########
sim_anova <- ezANOVA(
  data = data_withC_rb
  , dv = bias_withC_L_SEa
  , wid = id
  , within = method
  , between = .(factors, nobs, ncat)
)
sim_anova$ANOVA

data_withC_rb %>% 
  group_by(ncat, method, nobs, factors) %>% 
  summarise(SE_La = mean(bias_withC_L_SEa)) %>% 
  ggplot(mapping = aes(x = nobs, y = SE_La, 
                       shape = ncat, 
                       group = interaction(method,ncat),
                       linetype = method)) + 
  geom_point() + 
  geom_line() +
  jtools::theme_apa(legend.use.title = T) +
  labs(x = "N", 
       y = '% bias', 
       shape = "Number of answer categories",
       linetype = "Estimation method",
       title = expression("Relative bias of the estimated SE of "~lambda[paste(6, ",", 2, sep = "")]), subtitle = "Data obtained from the correctly specified model") + ylim(c(-5,5)) + facet_wrap(~factors)

#save plot
ggsave("Plot relative bias estimated SE lambda_a (withC).pdf",width = 7, height = 7)

################# Perform ANOVA ######################
############# correctly specified model ############# 
############# Dependent variable = factor loading 7 ###########
sim_anova <- ezANOVA(
  data = data_withC_rb
  , dv = bias_withC_L_SE7
  , wid = id
  , within = method
  , between = .(factors, nobs, ncat)
)
sim_anova$ANOVA

data_withC_rb %>% 
  group_by(ncat, method, nobs, factors) %>% 
  summarise(SE_L7 = mean(bias_withC_L_SE7)) %>% 
  ggplot(mapping = aes(x = nobs, y = SE_L7, 
                       shape = ncat, 
                       group = interaction(method,ncat),
                       linetype = method)) + 
  geom_point() + 
  geom_line() +
  jtools::theme_apa(legend.use.title = T) +
  labs(x = "N", 
       y = '% bias', 
       shape = "Number of answer categories",
       linetype = "Estimation method",
       title = expression("Relative bias of the estimated SE of "~lambda[paste(7, ",", 2, sep = "")]), subtitle = "Data obtained from the correctly specified model") +
  ylim(c(-5,5)) + facet_wrap(~factors)

#save plot
ggsave("Plot relative bias estimated SE lambda7 (withC).pdf",width = 7, height = 7)

################# Perform ANOVA ######################
################# correctly specified model ################# 
############# Dependent variable = factor loading SE Ti6 ###########
sim_anova <- ezANOVA(
  data = data_withC_rb, 
  dv = bias_withC_T_SEi6, 
  wid = id, 
  within = method, 
  between = .(factors, nobs, ncat), 
  detailed = T
)
sim_anova$ANOVA

data_withC_rb %>% 
  group_by(ncat, method, nobs, factors) %>% 
  summarise(T_SEi6 = mean(bias_withC_T_SEi6)) %>% 
  ggplot(mapping = aes(x = nobs, y = T_SEi6, 
                       shape = ncat, 
                       group = interaction(method,ncat),
                       linetype = method)) + 
  geom_point() + 
  geom_line() +
  jtools::theme_apa(legend.use.title = T) +
  labs(x = "N", 
       y = '% bias', 
       shape = "Number of answer categories",
       linetype = "Estimation method",
       title = expression("Relative bias of the estimated SE of "~tau[paste(6, ",", 1, sep = "")]), subtitle = "Data obtained from the correctly specified model") +
  ylim(c(-5,5)) + facet_wrap(~factors)

#save plot
ggsave("Plot relative bias estimated SE Threshold lambda6 (withC).pdf",width = 7, height = 7)

################# Perform ANOVA ######################
################# correctly specified model ################# 
############# Dependent variable = factor loading SE Ti7 ###########
sim_anova <- ezANOVA(
  data = data_withC_rb, 
  dv = bias_withC_T_SEi7, 
  wid = id, 
  within = method, 
  between = .(factors, nobs, ncat), 
  detailed = T
)
sim_anova$ANOVA


data_withC_rb %>% 
  group_by(ncat, method, nobs, factors) %>% 
  summarise(T_SEi7 = mean(bias_withC_T_SEi7)) %>% 
  ggplot(mapping = aes(x = nobs, y = T_SEi7, 
                       shape = ncat, 
                       group = interaction(method,ncat),
                       linetype = method)) + 
  geom_point() + 
  geom_line() +
  jtools::theme_apa(legend.use.title = T) +
  labs(x = "N", 
       y = '% bias', 
       shape = "Number of answer categories",
       linetype = "Estimation method",
       title = expression("Relative bias of the estimated SE of "~tau[paste(7, ",", 2, sep = "")]), subtitle = "Data obtained from the correctly specified model") +
  ylim(c(-5,5)) + facet_wrap(~factors)

#save plot
ggsave("Plot relative bias estimated SE Threshold lambda7 (withC).pdf",width = 7, height = 7)

################# Perform ANOVA ######################
################# correctly specified model ################# 
############# Dependent variable = factor loading SE correlation ###########
sim_anova <- ezANOVA(
  data = data_withC_rb, 
  dv = bias_withC_C_SE1, 
  wid = id, 
  within = method, 
  between = .(factors, nobs, ncat), 
  detailed = T
)
sim_anova$ANOVA

data_withC_rb %>% 
  group_by(ncat, method, nobs) %>% 
  summarise(C_SE1 = mean(bias_withC_C_SE1)) %>% 
  ggplot(mapping = aes(x = nobs, y = C_SE1, 
                       shape = ncat, 
                       group = interaction(method,ncat),
                       linetype = method)) + 
  geom_point() + 
  geom_line() +
  jtools::theme_apa(legend.use.title = T) +
  labs(x = "N", 
       y = '% bias', 
       shape = "Number of answer categories",
       linetype = "Estimation method", 
       title = expression("Relative bias of the estimated SE of "~psi[paste(1, ",", 2, sep = "")]), subtitle = "Data obtained from the correctly model") + ylim(c(-5,5))

#save plot
ggsave("Plot relative bias estimated SE correlation (withC).pdf",width = 7, height = 7)

################################### Without C ###################################
################# Perform ANOVA ######################
############# misspecified model ############# 
############# Dependent variable = factor loading 6 ###########
sim_anova <- ezANOVA(
  data = data_withoutC_rb
  , dv = bias_withoutC_L_SE6
  , wid = id
  , within = method
  , between = .(factors, nobs, ncat)
)

sim_anova$ANOVA

# change labels of factors 
levels(data_withoutC_rb$factors) <- c("Factors = 2", "Factors = 4", "Factors = 6", "Factors = 8")

data_withoutC_rb %>% 
  group_by(ncat, method, nobs, factors) %>% 
  summarise(SE_L6 = mean(bias_withoutC_L_SE6)) %>% 
  ggplot(mapping = aes(x = nobs, y = SE_L6, 
                       shape = ncat, 
                       group = interaction(method,ncat),
                       linetype = method)) + 
  geom_point() + 
  geom_line() +
  jtools::theme_apa(legend.use.title = T) +
  labs(x = "N", 
       y = '% bias', 
       shape = "Number of answer categories",
       linetype = "Estimation method",
       title = expression("Relative bias of the estimated SE of "~lambda[paste(6, ",", 1, sep = "")]),
       subtitle = "Data obtained from the misspecified model") + 
  ylim(c(-5,5)) + facet_wrap(~factors)

#save plot
ggsave("Plot relative bias estimated SE lambda6 (withoutC).pdf",width = 7, height = 7)

################# Perform ANOVA ######################
############# misspecified model ############# 
############# Dependent variable = factor loading 7 ###########
sim_anova <- ezANOVA(
  data = data_withoutC_rb
  , dv = bias_withoutC_L_SE7
  , wid = id
  , within = method
  , between = .(factors, nobs, ncat)
)

sim_anova$ANOVA

data_withoutC_rb %>% 
  group_by(ncat, method, nobs, factors) %>% 
  summarise(SE_L7 = mean(bias_withoutC_L_SE7)) %>% 
  ggplot(mapping = aes(x = nobs, y = SE_L7, 
                       shape = ncat, 
                       group = interaction(method,ncat),
                       linetype = method)) + 
  geom_point() + 
  geom_line() +
  jtools::theme_apa(legend.use.title = T) +
  labs(x = "N", 
       y = '% bias', 
       shape = "Number of answer categories",
       linetype = "Estimation method",
       title = expression("Relative bias of the estimated SE of "~lambda[paste(7, ",", 2, sep = "")]),
       subtitle = "Data obtained from the misspecified model") + 
  ylim(c(-5,5)) + facet_wrap(~factors)

#save plot
ggsave("Plot relative bias estimated SE lambda7 (withoutC).pdf",width = 7, height = 7)

################# Perform ANOVA ######################
################# misspecified model ################# 
############# Dependent variable = factor loading SE Ti6 ###########
sim_anova <- ezANOVA(
  data = data_withoutC_rb, 
  dv = bias_withoutC_T_SEi6, 
  wid = id, 
  within = method, 
  between = .(factors, nobs, ncat), 
  detailed = T
)
sim_anova$ANOVA

data_withoutC_rb %>% 
  group_by(ncat, method, nobs, factors) %>% 
  summarise(T_SEi6 = mean(bias_withoutC_T_SEi6)) %>% 
  ggplot(mapping = aes(x = nobs, y = T_SEi6, 
                       shape = ncat, 
                       group = interaction(method,ncat),
                       linetype = method)) + 
  geom_point() + 
  geom_line() +
  jtools::theme_apa(legend.use.title = T) +
  labs(x = "N", 
       y = '% bias', 
       shape = "Number of answer categories",
       linetype = "Estimation method",
       title = expression("Relative bias of the estimated SE of "~tau[paste(6, ",", 1, sep = "")]),
       subtitle = "Data obtained from the misspecified model") +
  ylim(c(-5,5)) + facet_wrap(~factors)

#save plot
ggsave("Plot relative bias estimated SE Threshold lambda6 (withoutC).pdf",width = 7, height = 7)


################# Perform ANOVA ######################
################# misspecified model ################# 
############# Dependent variable = factor loading SE Ti7 ###########
sim_anova <- ezANOVA(
  data = data_withoutC_rb, 
  dv = bias_withoutC_T_SEi7, 
  wid = id, 
  within = method, 
  between = .(factors, nobs, ncat), 
  detailed = T
)
sim_anova$ANOVA

data_withoutC_rb %>% 
  group_by(ncat, method, nobs, factors) %>% 
  summarise(T_SEi7 = mean(bias_withoutC_T_SEi7)) %>% 
  ggplot(mapping = aes(x = nobs, y = T_SEi7, 
                       shape = ncat, 
                       group = interaction(method,ncat),
                       linetype = method)) + 
  geom_point() + 
  geom_line() +
  jtools::theme_apa(legend.use.title = T) +
  labs(x = "N", 
       y = '% bias', 
       shape = "Number of answer categories",
       linetype = "Estimation method",
       title = expression("Relative bias of the estimated SE of "~tau[paste(7, ",", 2, sep = "")]),
       subtitle = "Data obtained from the misspecified model") +
  ylim(c(-5,5)) + facet_wrap(~factors)

#save plot
ggsave("Plot relative bias estimated SE Threshold lambda7 (withoutC).pdf",width = 7, height = 7)


################# Perform ANOVA ######################
################# misspecified model ################# 
############# Dependent variable = factor loading SE correlation ###########
sim_anova <- ezANOVA(
  data = data_withoutC_rb,
  dv = bias_withoutC_C_SE1,
  wid = id,
  within = method,
  between = .(factors, nobs, ncat),
  detailed = T
)
sim_anova$ANOVA

data_withoutC_rb %>% 
  group_by(ncat, method, nobs) %>% 
  summarise(C_SE1 = mean(bias_withoutC_C_SE1)) %>% 
  ggplot(mapping = aes(x = nobs, y = C_SE1, 
                       shape = ncat, 
                       group = interaction(method,ncat),
                       linetype = method)) + 
  geom_point() + 
  geom_line() +
  jtools::theme_apa(legend.use.title = T) +
  labs(x = "N", 
       y = '% bias', 
       shape = "Number of answer categories",
       linetype = "Estimation method",
       title = expression("Relative bias of the estimated SE of "~psi[paste(1, ",", 2, sep = "")]),
       subtitle = "Data obtained from the misspecified model") + 
  ylim(c(-5,5))

#save plot
ggsave("Plot relative bias estimated SE correlation (withoutC).pdf",width = 7, height = 7)
