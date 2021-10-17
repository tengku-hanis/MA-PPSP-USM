#========================================================================#
# Title: Example of meta-analysis in R
# Author: Tengku Muhd Hanis (https://tengkuhanis.netlify.app/)
# Date: 18-10-2021
#========================================================================#

# 1) EXAMPLE -----------------------------------------------------------------

# Install packages
install.packages("meta")
install.packages("devtools")
devtools::install_github("MathiasHarrer/dmetar", upgrade = "never")
install.packages("robvis")

# Packages
library(meta)
library(dmetar)
library(dplyr)

# Data
source("https://raw.githubusercontent.com/tengku-hanis/MA-PPSP-USM/main/data-ivermectin.R")
head(iver)

# Fixed and random effect model ----
ma_iver <- metabin(event.e = event.e, 
                   n.e = n.e,
                   event.c = event.c,
                   n.c = n.c, 
                   studlab = studyID,
                   data = iver,
                   method.tau = "PM", #estimator
                   sm = "OR",
                   fixed = T, 
                   random = T,
                   prediction = T, 
                   hakn = T, #reduce false positive
                   adhoc.hakn = "iqwig6") #adjust the possible narrow ci caused by hakn
ma_iver

## Update chosen model
ma_iver_RE <- update(ma_iver, fixed = F)

# Forest plot ----
forest(ma_iver_RE, sortvar = TE)

# Funnel plot ----
funnel(ma_iver_RE, studlab = T)

# Publication bias ----
metabias(ma_iver_RE, plotit = T, method.bias = "Egger") #generic
metabias(ma_iver_RE, plotit = T, method.bias = "Begg") #generic
metabias(ma_iver_RE, plotit = T, method.bias = "Peters") #specific for binary outcome

# Assess outlier (I^2 > 50%) ----
ma_no_NAs <- update(ma_iver_RE, subset = -c(3, 6:8, 21, 22)) #cannot have NAs for this

find.outliers(ma_no_NAs)

# Influential diagnostics ----
baujat(ma_iver_RE)

ma_inf <- InfluenceAnalysis(ma_no_NAs, random = T) #better

plot(ma_inf, "baujat")
plot(ma_inf, "influence")
plot(ma_inf, "ES")
plot(ma_inf, "I2")

# Update final model ----
ma_iver_RE2 <- update(ma_iver_RE, fixed = F, subset = -c(17, 27))

## Rerun everything from the beginning
forest(ma_iver_RE2, sortvar = TE)

funnel(ma_iver_RE2, studlab = T)

metabias(ma_iver_RE2, plotit = T, method.bias = "Egger")
metabias(ma_iver_RE2, plotit = T, method.bias = "Begg")
metabias(ma_iver_RE2, plotit = T, method.bias = "peters")


# 2) EXTENSION-1 -------------------------------------------------------------
# For significant publication bias (our model not significant)

# Trim and fill method (I^2 should be low) ----
tf <- trimfill(ma_iver_RE2)
tf

funnel(tf, studlab = T)


# 3) EXTENSION-2 -------------------------------------------------------------
# To explain high heterogeneity (noted that our I^2 is low)

# Subgroup analysis (k > 10) ----
ma_sub <- update(ma_iver_RE2, subgroup = study_type)
ma_sub

forest(ma_sub, sortvar = TE, bylab = "Type of study")

# Meta-regression (~ k > 10) ----
ma_iver_reg <- metareg(ma_iver_RE, ~ study_type, 
                       hakn = T, 
                       method.tau = "REML", intercept = T) 

ma_iver_reg 
exp(ma_iver_reg$beta) #effect estimate of nonClinical is 1.2 or 20% higher than the clinical

## Bubble plot of meta-regression
bubble(ma_iver_reg, lwd = 2, lty = 2, col.line = "red", ylim = c(-3, 2), regline = TRUE)


# 4) REVISION (for fun) ----------------------------------------------
library(ggplot2)

# Bubble plot (manually using ggplot)
iver %>% 
  slice(-c(17,27)) %>% 
  mutate(weights = ma_iver_RE2$w.random, 
         effect = ma_iver_RE2$TE) %>% 
  ggplot(aes(x = study_type, y = effect, size = weights)) +
  geom_point(shape = 1) + #add scatter
  geom_abline(intercept = ma_iver_reg$b[1], slope = ma_iver_reg$b[2], linetype = "dashed", color = "red") + #add regression line
  labs(y = "Treatment effects (log odds ratio)", x = "Type of study") +
  theme_bw() + #apply black and white theme
  theme(legend.position = "none") #remove legend


# 5) MISCELLANEOUS -----------------------------------------------------------

# Risk of bias ----
library(robvis)

## Available tools/templates
rob_tools()

## Risk of bias data (clinical only)
bias <- read.csv("https://raw.githubusercontent.com/tengku-hanis/MA-PPSP-USM/main/bias-clinical.csv")
bias$Overall <- "Not Applicable"
bias$Weight <- 1

## Plot
rob_traffic_light(bias, tool = "ROB1", psize = 13)

rob_summary(bias, tool = "ROB1", overall = F, weighted = F) #data_rob2

# Prisma flow of diagram ----

# Shinyapps - https://estech.shinyapps.io/prisma_flowdiagram/
# Not R - http://prisma-statement.org/prismastatement/flowdiagram.aspx


# Resources:
# https://www.medrxiv.org/content/10.1101/2021.04.30.21256415v2.full-text
# https://bookdown.org/MathiasHarrer/Doing_Meta_Analysis_in_R/
# https://www.metafor-project.org/  
# https://mcguinlu.shinyapps.io/robvis/
# Rtools - https://clanfear.github.io/CSSS508/docs/compiling.html
