# Author: Alina Tu
# Contact: alinat2@uci.edu
# Last Updated: 4/12/2023
# About: This script is the structural analysis of the Maze Learning Individual Differences (MLINDIV) project,
#        specifically looking at young adult hippocampal subfield volumes/thicknesses and their relationship
#        with navigation ability.

# Before running analyses in the following sections, run everything in the RUN FIRST section.

# Note if using RStudio: I recommend collapsing/expanding sections using the small arrows on the left pane next to 
#                        the line numbers.

#### ------------------------------------------------ RUN FIRST ------------------------------------------------ ####
# Getting started [PACKAGES AND FUNCTIONS]
# install.packages("readxl")
# install.packages("rlang")
# install.packages("ggplot2")
# install.packages("tidyr")
# install.packages("dplyr")
# install.packages("ggcorrplot")
# install.packages("stringr")
# install.packages("ppcor", repos='http://cran.us.r-project.org')
# install.packages("base64enc")
# install.packages("Hmisc")
# install.packages("readr")
# install.packages("corrplot")

library(readxl)
library(ggplot2)
library(tidyr)
library(dplyr)
library(corrplot)
library(ggcorrplot)
library(stringr)
library(ppcor)
library(Hmisc)
library(readr)
library(corrplot)
select <- dplyr::select

read_excel_allsheets <- function(filename, tibble = FALSE) {
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
  if(!tibble) x <- lapply(x, as.data.frame)
  names(x) <- sheets
  x
}
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

# Working directory
workdir <- "/mnt/chrastil/lab/users/alina/analysis/"
setwd(workdir)

# Load data
## Behavioral measures from Maze-Learning task (all participants)
data_nav_measures <- read.csv("MLINDIV_participant_full.csv") %>% 
  select("Subject","tm_accuracy","tm_path_efficiencies","tm_path_efficiencies_acc_only", "n_trials")
colnames(data_nav_measures) <- c("sub_id","propcorrect","patheff","patheff_acc_only","n_completed")
data_nav_measures <- data_nav_measures[data_nav_measures["n_completed"] >= 36, ] # Removing rows w/ incomplete behavioral data

## Subject log for demographic info (all participants)
data_full <- read_excel("MLINDIV_Subject_Run_Info.xlsx")
  names(data_full)[names(data_full) == "Spatial Neuro ID"] <- "sub_id"
  names(data_full)[names(data_full) == "Sex"] <- "sex"
  names(data_full)[names(data_full) == "Age"] <- "age"
  data_full$sub_id <- extract_numeric(data_full$sub_id)
data_full <- merge(data_full[,c(1:5,7:23)], data_nav_measures[,c(1,2)], by = "sub_id", all = T) 
data_full_clean <- data_full %>% drop_na("sub_id") # Remove empty rows

## Total brain volume (all participants)
data_tiv <- read.csv(file = "TIV_comparison.csv")
  names(data_tiv)[names(data_tiv) == "SubjectNum"] <- "sub_id"
data_tiv$CaitTIV <- data_tiv$CaitTIV/1000

## Volume measures and slice counts from high-res T2 HC scans
data_hrT2 <- read_excel("ASHS_volumes0323.xlsx", sheet = 1) 
  hres_sub_id <- c(81:83, 85, 86, 88, 89, 92:96, 98:103, 106:113)
  names(data_hrT2)[names(data_hrT2) == "Spatial Neuro ID"] <- "sub_id"
raw_vol_full <- as_tibble(read.csv("subfield_vol/raw_vol_full.csv")) # FINAL volumes
data_hrT2[,2:15] <- raw_vol_full[,3:16] # Replace data with FINAL volumes
data_hrT2 <- merge(data_hrT2[,c(1:31,35:36)], data_nav_measures, by = "sub_id")

post_cutoff <- c(which(data_hrT2$post_cutoff == "TRUE")) # index of sub with posterior cutoffs
data_cut <- data_hrT2[-c(post_cutoff),] # n=14, 7 subfield volumes with no posterior cutoffs
nrow(data_cut) == 14

## Volume measures and slice counts from high-res T2 HC scans (body only)
data_HCbody <- read_excel("ASHS_volumes0323.xlsx", sheet = 2) 
  names(data_HCbody)[names(data_HCbody) == "Spatial Neuro ID"] <- "sub_id"
body_vol_full <- as_tibble(read.csv("subfield_vol/body_vol_full.csv"))
data_HCbody[,2:13] <- body_vol_full[,3:14]
data_HCbody <- merge(data_HCbody[,c(1:29,33:34)], data_nav_measures, by = "sub_id")

post_cutoff_body <- post_cutoff[-c(5,6,9)] # posterior cutoffs for body same as full post-cutoff except 101, 102, 109
data_HCbody_cut <- data_HCbody[-c(post_cutoff_body),] # n=17, 6 subfield volumes with no posterior cutoffs
nrow(data_HCbody_cut) == 17

## Volume measures and slice counts from low-res T2 whole-brain scans (high-res participants only)
data_regT2 <- read_excel("ASHS_volumes0323.xlsx", sheet = 5)
  names(data_regT2)[names(data_regT2) == "Spatial Neuro ID"] <- "sub_id"
data_regT2 <- merge(data_regT2[,c(1:39,43:44)], data_nav_measures, by = "sub_id") 

## Volume measures and slice counts from low-res T1 whole-brain scans (all participants)
data_total_vol <- read_excel("ASHS_volumes0323.xlsx", sheet = 4) 
  data_total_vol$sub_id <- as.integer(data_total_vol$sub_id)
antpost_vol_full <- read.csv("antpost_vol/antpost_vol_full.csv")
data_total_vol <- merge(data_total_vol[,c(1,4,9,11,14,19,21)], antpost_vol_full, by = "sub_id")
data_total_vol <- data_total_vol %>% 
  select("sub_id", "LHC_vol", "sLHC", "LaHC_vol", "sLaHC", "LpHC_vol", "sLpHC", 
         "RHC_vol", "sRHC", "RaHC_vol", "sRaHC", "RpHC_vol", "sRpHC", "totalHC_vol", 
         "aHC_vol", "pHC_vol", "Lpa_ratio", "Rpa_ratio", "meanpa_ratio")
data_total_vol <- merge(data_total_vol, data_nav_measures, by = "sub_id")
data_total_vol <- merge(data_total_vol, data_full_clean[,c(1,4:5)], by = "sub_id") 
data_total_vol <- merge(data_total_vol, data_tiv[,c(2,8)], by = "sub_id")
  names(data_total_vol)[names(data_total_vol) == "VaisTIV"] <- "tiv"
  
data_total_vol_ratio <- data_total_vol %>%
  mutate(aHC_vol = LaHC_vol + RaHC_vol, pHC_vol = LpHC_vol + RpHC_vol,
         Lpa_ratio = LpHC_vol/LaHC_vol, Rpa_ratio = RpHC_vol/RaHC_vol,
         meanpa_ratio = (Lpa_ratio + Rpa_ratio)/2) # pa_ratio = postHC/antHC vol ratio
  
## Pen/paper and questionnaire measures (all participants)
data_survey_scores <- read_excel_allsheets("RA_mlindiv_surveys.xlsx")
  data_survey_scores <- data_survey_scores$`Summary Scores`[,1:8]
  data_survey_scores <- merge(data_survey_scores, data_nav_measures, by = "sub_id", all = T) 
  data_survey_scores <- merge(data_survey_scores, data_full_clean[,c(1,4)], by = "sub_id", all = T) 
  data_survey_scores <- data_survey_scores %>% drop_na("propcorrect") 

# Clean data
## Define new numeric sex variable
sex_to_numeric <- function(df) {
  sex <- as.character((strsplit(gsub("[{}]","",df$`sex`), split = ",")))
  sex[sex == "M"] <- 1
  sex[sex == "F"] <- 0
  df$sex <- as.numeric(sex)
}
  
  data_hrT2$sex = sex_to_numeric(data_hrT2) #26
  data_cut$sex = sex_to_numeric(data_cut) #13
  data_HCbody$sex = sex_to_numeric(data_HCbody)
  data_total_vol$sex = sex_to_numeric(data_total_vol)
  data_total_vol_ratio$sex = sex_to_numeric(data_total_vol_ratio)
  data_regT2$sex = sex_to_numeric(data_regT2)
  data_HCbody_cut$sex = sex_to_numeric(data_HCbody_cut)
  data_survey_scores$sex = sex_to_numeric(data_survey_scores)

## Thickness
data_tdata <- data_hrT2 %>% 
  mutate(tLCA1 = LCA1/sLCA1/2, tLCA23 = LCA23/sLCA23/2, tLDG = LDG/sLDG/2, tLSUB = LSUB/sLSUB/2,
         tLERC = LERC/sLERC/2, tLPRC = LPRC/sLPRC/2, tLPHC = LPHC/sLPHC/2,
         tRCA1 = RCA1/sRCA1/2, tRCA23 = RCA23/sRCA23/2, tRDG = RDG/sRDG/2, tRSUB = RSUB/sRSUB/2,
         tRERC = RERC/sRERC/2, tRPRC = RPRC/sRPRC/2, tRPHC = RPHC/sRPHC/2) %>% # 2mm
  dplyr::select("sub_id","tLCA1","tLCA23","tLDG","tLSUB","tLERC","tLPRC","tLPHC","tRCA1","tRCA23","tRDG","tRSUB",
                "tRERC","tRPRC","tRPHC","sex","age","propcorrect","patheff","patheff_acc_only","tiv") 
  # n=26, 7 HC subfield

data_tHCbody_cut <- data_HCbody_cut %>% 
  mutate(tLCA1 = LCA1/sLCA1/2, tLCA23 = LCA23/sLCA23/2, tLDG = LDG/sLDG/2, tLSUB = LSUB/sLSUB/2, 
         tLPRC = LPRC/sLPRC/2, tLPHC = LPHC/sLPHC/2,
         tRCA1 = RCA1/sRCA1/2, tRCA23 = RCA23/sRCA23/2, tRDG = RDG/sRDG/2, tRSUB = RSUB/sRSUB/2,
         tRPRC = RPRC/sRPRC/2, tRPHC = RPHC/sRPHC/2) %>% # 2mm
  dplyr::select("sub_id","tLCA1","tLCA23","tLDG","tLSUB","tLPRC","tLPHC","tRCA1","tRCA23","tRDG","tRSUB",
                "tRPRC","tRPHC","sex","age","propcorrect","patheff","patheff_acc_only","tiv") # n=17, 6 body subfields

data_tregT2 <- data_regT2 %>%
  mutate(tLaHC = LaHC/sLaHC/0.94, tLpHC = LpHC/sLpHC/0.94, 
         tLERC = LERC/sLERC/0.94, tLPRC = LPRC/sLPRC/0.94, tLPHC = LPHC/sLPHC/0.94, 
         tRaHC = RaHC/sRaHC/0.94, tRpHC = RpHC/sRpHC/0.94, 
         tRERC = RERC/sRERC/0.94, tRPRC = RPRC/sRPRC/0.94, tRPHC = RPHC/sRPHC/0.94) %>% # 0.94mm
  dplyr::select("sub_id","tLaHC","tLpHC","tLERC","tLPRC","tLPHC","tRaHC","tRpHC","tRERC","tRPRC","tRPHC",
                "sex","age","propcorrect","patheff","patheff_acc_only","tiv") # n=26, 5 HC/MTL subfields

data_total_thick <- data_total_vol %>% 
  mutate(tLHC = LHC_vol/sLHC/0.94, tLaHC = LaHC_vol/sLaHC/0.94, tLpHC = LpHC_vol/sLpHC/0.94,
         tRHC = RHC_vol/sRHC/0.94, tRaHC = RaHC_vol/sRaHC/0.94, tRpHC = RpHC_vol/sRpHC/0.94) %>% # 0.94mm
  dplyr::select("sub_id","tLHC","tLaHC","tLpHC","tRHC","tRaHC","tRpHC",
                "sex","age","propcorrect","patheff","patheff_acc_only","tiv") # n=99, L/R ant/post/total

# Define relevant variables
subfields_LR <- c("LCA1", "RCA1", "LCA23", "RCA23", "LDG", "RDG", "LSUB", "RSUB",
                  "LERC", "RERC", "LPRC", "RPRC", "LPHC", "RPHC")

# Compare TIV values between UCI and UCSB (Vaisakh, Vini, Caitlin)
t.test(data_tiv$VaisTIV, data_tiv$ViniTIV)
t.test(data_tiv$VaisTIV, data_tiv$CaitTIV)

# Color palettes + graph themes
library(tidyr)
# Colorblind-friendly palette (https://venngage.com/blog/color-blind-friendly-palette/)
palette <- c("#a1caec", "#fdbc7a", "#cfcfcf", "#5e9ed0", "#ff800e", "#ababab", "#0469a6", "#c45400", "#898989",
             "#595959") # light blue/orange/gray, medium blue/orange/gray, dark blue/orange/gray, darker gray
palette_sbfd <- c("#bd0000", "#49a12f", "#1306cf", "#4e819c", "#c9991e", "#921c94", "#438f8e", "#8e7ba8", "#74917b")
                  # CA1, CA2/3, DG, SUB, ERC, PRC, PHC, aHC, pHC
baseTheme <- theme_get() # store the default theme
theme_set(theme_gray()) 
theme_update1 <- theme_update(text = element_text(colour = rgb(12, 35, 75, maxColorValue = 255)), 
                             axis.title = element_text(size = 12, color = "black"),
                             axis.text = element_text(size = 10, color = "black"),
                             axis.line.x = element_blank(),
                             axis.line.y = element_blank(),
                             legend.position = c(0.8, 0.8),
                             legend.background = element_rect(fill = "white", size = 0.2, linetype = "solid"),
                             legend.box.background = element_rect(color = "black"),
                             legend.direction = "horizontal",
                             legend.box = "vertical",
                             legend.title = element_text(size = 9, color = "black"),
                             legend.text = element_text(size = 8, color = "black"),
                             plot.title = element_text(size = 14, hjust = 0.5, color = "black"),
                             strip.text = element_text(size = 10, color = "black"),
                             panel.grid.major = element_line(color = "#fafafa"),
                             panel.grid.minor = element_line(color = "#fafafa"),
                             panel.border = element_rect(color = "transparent", fill="transparent", size=0.5),
                             plot.background = element_rect(fill = "transparent", color = "transparent"))
theme1 <- theme_get()

# New theme
baseTheme <- theme_get() # store the default theme
theme_set(theme_minimal()) 
theme_update2 <- theme_update(text = element_text(colour = rgb(12, 35, 75, maxColorValue = 255)), 
                              axis.title = element_text(size = 12, color = "black"),
                              axis.text = element_text(size = 10, color = "black"),
                              axis.line.x = element_blank(),
                              axis.line.y = element_blank(),
                              legend.position = c(0.8, 0.8),
                              legend.background = element_rect(fill = "white", size = 0.2, linetype = "solid"),
                              legend.box.background = element_rect(color = "black"),
                              legend.direction = "horizontal",
                              legend.box = "vertical",
                              legend.title = element_text(size = 9, color = "black"),
                              legend.text = element_text(size = 8, color = "black"),
                              plot.title = element_text(size = 14, hjust = 0.5, color = "black"),
                              strip.text = element_text(size = 10, color = "black"),
                              panel.grid.major = element_line(color = "#fafafa"),
                              panel.grid.minor = element_line(color = "#fafafa"),
                              panel.border = element_rect(color = "black", fill="transparent", size=0.5),
                              plot.background = element_rect(fill = "transparent", color = "transparent"))
theme2 <- theme_get()

#### ------------------------------------------------- Summary ------------------------------------------------- ####
dim(data_total_vol)
print(nrow(data_total_vol)-sum(data_total_vol$sex)) # number of females
mean(data_total_vol$age)
sd(data_total_vol$age)
summary(data_total_vol$age)

dim(data_hrT2)
print(nrow(data_hrT2)-sum(data_hrT2$sex)) # number of females
mean(data_hrT2$age)
sd(data_hrT2$age)
summary(data_hrT2$age)

cor.test(data_total_vol$propcorrect, data_total_vol$patheff_acc_only)

#### -------------------------- Section 1A: Total Volumes + Propcorr or Patheff (n=99) ------------------------- ####
# Testing null hypotheses
# Check for outliers
which(abs(scale(data_total_vol$LHC_vol)) > 3)
which(abs(scale(data_total_vol$RHC_vol)) > 3)
which(abs(scale(data_total_vol$totalHC_vol)) > 3)

dim(data_total_vol)

## Function that outputs correlation table for all 3 regions
## Inputs: (1) list of correlation (r) values and p-values
## Output: a correlation matrix (mat)/ table for the 3 subfields' r and p-values
subfields_total <- c("LHC_vol", "RHC_vol", "totalHC_vol")
corr_mat_total <- function(r_list) {
  corr_mat_total <- matrix(unlist(t(r_list)), byrow=T, 3, 2)
  colnames(corr_mat_total) <- c("r", "p-value")
  rownames(corr_mat_total) <- c("LHC", "RHC", "totalHC")
  return(corr_mat_total)
}

# Partial correlation tables between L/R/total HC volumes and propcorrect, patheff, patheff_acc_only (n=99)
nav_dv <- c("propcorrect", "patheff", "patheff_acc_only")
for (dv in nav_dv) {
  r <- NULL;
  
  for (subfd_name in subfields_total) { 
    subfield <- paste(subfd_name, "_pcor", sep = "")
    PC_subfield_pcor <- pcor.test(pull(data_total_vol[subfd_name]), data_total_vol[dv], 
                                  c(data_total_vol$sex, data_total_vol$age, data_total_vol$tiv), method = "pearson")
    assign(subfd_name, PC_subfield_pcor)
    r <- rbind(r, round(PC_subfield_pcor$estimate, digits = 3), round(PC_subfield_pcor$p.value, digits = 5))
    # print(paste(i, "estimate is:", round(PC_subfield_pcor$estimate, digits = 2), 
    #             "and p-value is:", round(PC_subfield_pcor$p.value, digits = 3))) 
  } # ex. LCA1_pcor
  
  print(dv)
  print(corr_mat_total(r))
}

ggplot(data = data_total_vol, aes(y = propcorrect, x = LHC_vol)) +
  geom_point(color = palette[9]) + geom_smooth(method = "lm", color = palette[9]) +
  ggtitle("Left HC Volume") + xlab("Left Hippocampal (HC) Volume (mm^3)") +
  ylab("Proportion Correct") + ylim(0, 1)
ggplot(data = data_total_vol, aes(y = propcorrect, x = RHC_vol)) +
  geom_point(color = palette[9]) + geom_smooth(method = "lm", color = palette[9]) +
  ggtitle("Right HC Volume") + xlab("Right Hippocampal (HC) Volume (mm^3)") +
  ylab("Proportion Correct") + ylim(0, 1)
ggplot(data = data_total_vol, aes(y = propcorrect, x = totalHC_vol)) +
  geom_point(color = palette[9]) + geom_smooth(method = "lm", color = palette[9]) +
  ggtitle("Total HC Volume") + xlab("Total Hippocampal (HC) Volume (mm^3)") +
  ylab("Proportion Correct") + ylim(0, 1)

# Filter df to get data to be highlighted
highlight_df <- data_total_vol[c(28,57,68,83),]
print(highlight_df$sub_id) # FreeSurfer HC edit: 33, 79, 95; Echo (instead of T1) scan: 67

data_total_vol %>% 
  ggplot(aes(x = LHC_vol, y = propcorrect)) + 
  geom_point(color = palette[9]) + geom_smooth(method = "lm", color = palette[9]) +
  ggtitle("Left HC Volume") + xlab("Left Hippocampal (HC) Volume (mm^3)") +
  ylab("Proportion Correct") + ylim(0, 1) +
  geom_point(data = highlight_df, aes(x = LHC_vol, y = propcorrect), 
             color = 'blue')
data_total_vol %>% 
  ggplot(aes(x = RHC_vol, y = propcorrect)) + 
  geom_point(color = palette[9]) + geom_smooth(method = "lm", color = palette[9]) +
  ggtitle("Right HC Volume") + xlab("Right Hippocampal (HC) Volume (mm^3)") +
  ylab("Proportion Correct") + ylim(0, 1) +
  geom_point(data = highlight_df, aes(x = RHC_vol, y = propcorrect), 
             color = 'blue')
data_total_vol %>% 
  ggplot(aes(x = totalHC_vol, y = propcorrect)) + 
  geom_point(color = palette[9]) + geom_smooth(method = "lm", color = palette[9]) +
  ggtitle("Total HC Volume") + xlab("Total Hippocampal (HC) Volume (mm^3)") +
  ylab("Proportion Correct") + ylim(0, 1) +
  geom_point(data = highlight_df, aes(x = totalHC_vol, y = propcorrect), 
             color = 'blue')

# MULTIPLE LINEAR REGRESSION - CANNOT DO PARTIAL CORRECTION WITH SEX?
## Pearson correlation: Assumptions check
  # Homoscedasticity = constant variance
  model <- lm(propcorrect ~ LHC_vol, data = data_total_vol) # Replace the X value with diff HC subdivisions
  resid <- resid(model)
  plot(fitted(model), resid)
  # Linearity
  plot(data_total_vol$LHC_vol, data_total_vol$propcorrect) # Replace the X value with diff HC subdivisions
  # Good to use pearson's correlation :)
  
## Pearson's correlation tables between L/R/total HC volumes and propcorrect, patheff, patheff_acc_only (n=99)
nav_dv <- c("propcorrect", "patheff", "patheff_acc_only")
for (dv in nav_dv) {
  r <- NULL;
    
  for (subfd_name in subfields_total) { 
    subfield <- paste(subfd_name, "_pcor", sep = "")
    PC_subfield_cor <- cor.test(pull(data_total_vol[subfd_name]), data_total_vol[,dv], method = "pearson")
    assign(subfd_name, PC_subfield_cor)
    r <- rbind(r, round(PC_subfield_cor$estimate, digits = 3), round(PC_subfield_cor$p.value, digits = 5))
    # print(paste(i, "estimate is:", round(PC_subfield_cor$estimate, digits = 2), 
    #             "and p-value is:", round(PC_subfield_cor$p.value, digits = 3))) 
  } # ex. LCA1_cor
    
  print(dv)
  print(corr_mat_total(r))
}

## Multiple linear regression
model_LHC_propcorr <- lm(propcorrect ~ LHC_vol + sex + age + tiv, data = data_total_vol)
model_LHC_patheff <- lm(patheff ~ LHC_vol + sex + age + tiv, data = data_total_vol)
model_LHC_patheffacc <- lm(patheff_acc_only ~ LHC_vol + sex + age + tiv, data = data_total_vol)
model_RHC_propcorr <- lm(propcorrect ~ RHC_vol + sex + age + tiv, data = data_total_vol)
model_RHC_patheff <- lm(patheff ~ RHC_vol + sex + age + tiv, data = data_total_vol)
model_RHC_patheffacc <- lm(patheff_acc_only ~ RHC_vol + sex + age + tiv, data = data_total_vol)
model_totalHC_propcorr <- lm(propcorrect ~ totalHC_vol + sex + age + tiv, data = data_total_vol)
model_totalHC_patheff <- lm(patheff ~ totalHC_vol + sex + age + tiv, data = data_total_vol)
model_totalHC_patheffacc <- lm(patheff_acc_only ~ totalHC_vol + sex + age + tiv, data = data_total_vol)

  # Assumptions check:
  # Linearity
  plot(data_total_vol$age, data_total_vol$propcorrect)
  # No multicollinearity
  vif(model_LHC_propcorr) # Replace the value with diff models. outcome values should be less than 5
  # Independent observations
  # Homoscedasticity = constant variance
  resid <- resid(model_LHC_propcorr) # Replace the value with diff models
  plot(fitted(model_LHC_propcorr), resid) 
  # Multivariate normality
  
summary(model_LHC_propcorr)
summary(model_LHC_patheff)
summary(model_LHC_patheffacc)
summary(model_RHC_propcorr) # *the only ones steve looked at*
summary(model_RHC_patheff) # *the only ones steve looked at*
summary(model_RHC_patheffacc) # *the only ones steve looked at*
summary(model_totalHC_propcorr)
summary(model_totalHC_patheff)
summary(model_totalHC_patheffacc)
  
  model_LaHC <- lm(propcorrect ~ LaHC_vol + sex + age + tiv, data = data_total_vol)
  model_LpHC <- lm(propcorrect ~ LpHC_vol + sex + age + tiv, data = data_total_vol)
  model_RaHC <- lm(propcorrect ~ RaHC_vol + sex + age + tiv, data = data_total_vol)
  model_RpHC <- lm(propcorrect ~ RpHC_vol + sex + age + tiv, data = data_total_vol)
  summary(model_LaHC)
  summary(model_LpHC)
  summary(model_RaHC)
  summary(model_RpHC) # *the only one steve looked at*

# # Remove outliers
# data_total_vol_rm <- data_total_vol[-c(28, 82),]
# dim(data_total_vol_rm)
# 
# # Outliers removed: Partial correlation between left, right, total HC volumes and propcorrect (n=96)
# pcor.test(data_total_vol_rm$propcorrect, data_total_vol_rm$LHC_vol,
#           c(data_total_vol_rm$sex, data_total_vol_rm$age, data_total_vol_rm$tiv), method = "pearson")
# pcor.test(data_total_vol_rm$propcorrect, data_total_vol_rm$RHC_vol,
#           c(data_total_vol_rm$sex, data_total_vol_rm$age, data_total_vol_rm$tiv), method = "pearson")
# pcor.test(data_total_vol_rm$propcorrect, data_total_vol_rm$totalHC_vol,
#           c(data_total_vol_rm$sex, data_total_vol_rm$age, data_total_vol_rm$tiv), method = "pearson")
# 
# ggplot(data = data_total_vol_rm, aes(y = propcorrect, x = LHC_vol)) +
#   geom_point(color = palette[9]) + geom_smooth(method = "lm", color = palette[9]) +
#   ggtitle("Wayfinding Success and Left HC Volume") + xlab("Left Hippocampal (HC) Volume (mm^3)") +
#   ylab("Proportion Correct") + ylim(0, 1)
# ggplot(data = data_total_vol_rm, aes(y = propcorrect, x = RHC_vol)) +
#   geom_point(color = palette[9]) + geom_smooth(method = "lm", color = palette[9]) +
#   ggtitle("Wayfinding Success and Right HC Volume") + xlab("Right Hippocampal (HC) Volume (mm^3)") +
#   ylab("Proportion Correct") + ylim(0, 1)
# ggplot(data = data_total_vol_rm, aes(y = propcorrect, x = totalHC_vol)) +
#   geom_point(color = palette[9]) + geom_smooth(method = "lm", color = palette[9]) +
#   ggtitle("Wayfinding Success and Total HC Volume") + xlab("Total Hippocampal (HC) Volume (mm^3)") +
#   ylab("Proportion Correct") + ylim(0, 1)

#### ------------------------ Section 1B: Total Thickness + Propcorr or Patheff (n=98) ------------------------- ####
# Testing null hypotheses
# Check for outliers
which(abs(scale(data_total_thick$tLHC)) > 3)
which(abs(scale(data_total_thick$tRHC)) > 3) # 20

dim(data_total_thick)

# Remove outliers
data_total_thick_rm <- data_total_thick[-c(20),] # Participant 24
dim(data_total_thick_rm)

## Function that outputs correlation table for all 2 regions
## Inputs: (1) list of correlation (r) values and p-values
## Output: a correlation matrix (mat)/ table for the 2 subfields' r and p-values
subfields_total <- c("tLHC", "tRHC")
corr_mat_total <- function(r_list) {
  corr_mat_total <- matrix(unlist(t(r_list)), byrow=T, 2, 2)
  colnames(corr_mat_total) <- c("r", "p-value")
  rownames(corr_mat_total) <- c("tLHC", "tRHC")
  return(corr_mat_total)
}

# Outliers removed: Partial correlation tables between L/R HC thickness and propcorrect, patheff, patheff_acc_only (n=98)
nav_dv <- c("propcorrect", "patheff", "patheff_acc_only")
for (dv in nav_dv) {
  r <- NULL;
  
  for (subfd_name in subfields_total) { 
    subfield <- paste(subfd_name, "_pcor", sep = "")
    PC_subfield_pcor <- pcor.test(pull(data_total_thick_rm[subfd_name]), data_total_thick_rm[dv], 
                                  c(data_total_thick_rm$sex, data_total_thick_rm$age, data_total_thick_rm$tiv), method = "pearson")
    assign(subfd_name, PC_subfield_pcor)
    r <- rbind(r, round(PC_subfield_pcor$estimate, digits = 3), round(PC_subfield_pcor$p.value, digits = 5))
    # print(paste(i, "estimate is:", round(PC_subfield_pcor$estimate, digits = 2), 
    #             "and p-value is:", round(PC_subfield_pcor$p.value, digits = 3))) 
  } # ex. LCA1_pcor
  
  print(dv)
  print(corr_mat_total(r))
}

# ggplot(data = data_total_thick_rm, aes(y = propcorrect, x = tLHC)) + 
#   geom_point(color = palette[6]) + geom_smooth(method = "lm", color = palette[6]) +
#   ggtitle("Wayfinding Success and Left HC Thickness") + xlab("Normalized Thickness Ratio") +
#   ylab("Proportion Correct") + ylim(0, 1)
# ggplot(data = data_total_thick_rm, aes(y = propcorrect, x = tRHC)) + 
#   geom_point(color = palette[6]) + geom_smooth(method = "lm", color = palette[6]) +
#   ggtitle("Wayfinding Success and Right HC Thickness") + xlab("Normalized Thickness Ratio") +
#   ylab("Proportion Correct") + ylim(0, 1)

# MULTIPLE LINEAR REGRESSION - CANNOT DO PARTIAL CORRECTION WITH SEX?
## Pearson correlation: Assumptions check
  # Homoscedasticity = constant variance
  model <- lm(propcorrect ~ tLHC, data = data_total_thick_rm) # Replace the X and Y with diff value
  resid <- resid(model)
  plot(fitted(model), resid)
  # Linearity
  plot(data_total_thick_rm$tLHC, data_total_thick_rm$propcorrect) # Replace the X value with diff HC subdivisions
  # Good to use pearson's correlation :)

## Outliers removed: Pearson's correlation tables between L/R HC thickness and propcorrect, patheff, patheff_acc_only (n=98)
nav_dv <- c("propcorrect", "patheff", "patheff_acc_only")
for (dv in nav_dv) {
  r <- NULL;
  
  for (subfd_name in subfields_total) { 
    subfield <- paste(subfd_name, "_pcor", sep = "")
    PC_subfield_cor <- cor.test(pull(data_total_thick_rm[subfd_name]), data_total_thick_rm[,dv], method = "pearson")
    assign(subfd_name, PC_subfield_cor)
    r <- rbind(r, round(PC_subfield_cor$estimate, digits = 3), round(PC_subfield_cor$p.value, digits = 5))
    # print(paste(i, "estimate is:", round(PC_subfield_cor$estimate, digits = 2), 
    #             "and p-value is:", round(PC_subfield_cor$p.value, digits = 3))) 
  } # ex. LCA1_cor
  
  print(dv)
  print(corr_mat_total(r))
}

## Multiple linear regression
model_tLHC_propcorr <- lm(propcorrect ~ tLHC + sex + age + tiv, data = data_total_thick_rm)
model_tLHC_patheff <- lm(patheff ~ tLHC + sex + age + tiv, data = data_total_thick_rm)
model_tLHC_patheffacc <- lm(patheff_acc_only ~ tLHC + sex + age + tiv, data = data_total_thick_rm)
model_tRHC_propcorr <- lm(propcorrect ~ tRHC + sex + age + tiv, data = data_total_thick_rm)
model_tRHC_patheff <- lm(patheff ~ tRHC + sex + age + tiv, data = data_total_thick_rm)
model_tRHC_patheffacc <- lm(patheff_acc_only ~ tRHC + sex + age + tiv, data = data_total_thick_rm)

  # Assumptions check:
  # Linearity
  plot(data_total_thick_rm$age, data_total_thick_rm$propcorrect)
  # No multicollinearity
  vif(model_tLHC_propcorr) # Replace the value with diff models. outcome values should be less than 5
  # Independent observations
  # Homoscedasticity = constant variance
  resid <- resid(model_tLHC_propcorr) # Replace the value with diff models
  plot(fitted(model_tLHC_propcorr), resid) 
  # Multivariate normality

summary(model_tLHC_propcorr)
summary(model_tLHC_patheff)
summary(model_tLHC_patheffacc)
summary(model_tRHC_propcorr) # *the only ones steve looked at*
summary(model_tRHC_patheff) # *the only ones steve looked at*
summary(model_tRHC_patheffacc) # *the only ones steve looked at*

#### ------------------- Section 2A: Bilateral Ant/Post Volumes + Propcorr or Patheff (n~99) ------------------- ####
# Check for outliers
which(abs(scale(data_total_vol$LaHC_vol)) > 3)
which(abs(scale(data_total_vol$LpHC_vol)) > 3)
which(abs(scale(data_total_vol$RaHC_vol)) > 3)
which(abs(scale(data_total_vol$RpHC_vol)) > 3)

dim(data_total_vol)

## Function that outputs correlation table for all 4 subregions
## Inputs: (1) list of correlation (r) values and p-values
## Output: a correlation matrix (mat)/ table for the 4 subregions' r and p-values
subfields_antpost <- c("LaHC_vol", "LpHC_vol", "RaHC_vol", "RpHC_vol")
corr_mat_antpost <- function(r_list) {
  corr_mat_antpost <- matrix(unlist(t(r_list)), byrow=T, 4, 2)
  colnames(corr_mat_antpost) <- c("r", "p-value")
  rownames(corr_mat_antpost) <- c("LaHC", "LpHC", "RaHC", "RpHC")
  return(corr_mat_antpost)
}

# Partial correlation tables between L/R ant/post volumes and propcorrect, patheff, patheff_acc_only (n=99)
nav_dv <- c("propcorrect", "patheff", "patheff_acc_only")
for (dv in nav_dv) {
  r <- NULL;
  
  for (subfd_name in subfields_antpost) { 
    subfield <- paste(subfd_name, "_pcor", sep = "")
    PC_subfield_pcor <- pcor.test(pull(data_total_vol[subfd_name]), data_total_vol[dv], 
                                  c(data_total_vol$sex, data_total_vol$age, data_total_vol$tiv), method = "pearson")
    assign(subfd_name, PC_subfield_pcor)
    r <- rbind(r, round(PC_subfield_pcor$estimate, digits = 3), round(PC_subfield_pcor$p.value, digits = 5))
    # print(paste(i, "estimate is:", round(PC_subfield_pcor$estimate, digits = 2), 
    #             "and p-value is:", round(PC_subfield_pcor$p.value, digits = 3))) 
  } # ex. LCA1_pcor
  
  print(dv)
  print(corr_mat_antpost(r))
}

ggplot(data = data_total_vol, aes(y = propcorrect, x = LaHC_vol)) +
  geom_point(color = palette_sbfd[8]) + geom_smooth(method = "lm", color = palette_sbfd[8]) +
  ggtitle("Left aHC Volume") + xlab("Left Anterior HC Volume (mm^3)") +
  ylab("Proportion Correct") + ylim(0, 1)
ggplot(data = data_total_vol, aes(y = propcorrect, x = RaHC_vol)) +
  geom_point(color = palette_sbfd[8]) + geom_smooth(method = "lm", color = palette_sbfd[8]) +
  ggtitle("Right aHC Volume") + xlab("Right Anterior HC Volume (mm^3)") +
  ylab("Proportion Correct") + ylim(0, 1)

# MULTIPLE LINEAR REGRESSION - CANNOT DO PARTIAL CORRECTION WITH SEX?
## Pearson correlation: Assumptions check
  # Homoscedasticity = constant variance
  model <- lm(propcorrect ~ LHC_vol, data = data_total_vol) # Replace the X value with diff HC subdivisions
  resid <- resid(model)
  plot(fitted(model), resid)
  # Linearity
  plot(data_total_vol$LaHC_vol, data_total_vol$propcorrect) # Replace the X value with diff HC subdivisions
  # Good to use pearson's correlation :)

## Pearson's correlation tables between L/R ant/post volumes and propcorrect, patheff, patheff_acc_only (n=99)
nav_dv <- c("propcorrect", "patheff", "patheff_acc_only")
for (dv in nav_dv) {
  r <- NULL;
  
  for (subfd_name in subfields_antpost) { 
    subfield <- paste(subfd_name, "_pcor", sep = "")
    PC_subfield_cor <- cor.test(pull(data_total_vol[subfd_name]), data_total_vol[,dv], method = "pearson")
    assign(subfd_name, PC_subfield_cor)
    r <- rbind(r, round(PC_subfield_cor$estimate, digits = 3), round(PC_subfield_cor$p.value, digits = 5))
    # print(paste(i, "estimate is:", round(PC_subfield_cor$estimate, digits = 2), 
    #             "and p-value is:", round(PC_subfield_cor$p.value, digits = 3))) 
  } # ex. LCA1_cor
  
  print(dv)
  print(corr_mat_antpost(r))
}

## Multiple linear regression
model_LaHC_propcorr <- lm(propcorrect ~ LaHC_vol + sex + age + tiv, data = data_total_vol)
model_LaHC_patheff <- lm(patheff ~ LaHC_vol + sex + age + tiv, data = data_total_vol)
model_LaHC_patheffacc <- lm(patheff_acc_only ~ LaHC_vol + sex + age + tiv, data = data_total_vol)
model_LpHC_propcorr <- lm(propcorrect ~ LpHC_vol + sex + age + tiv, data = data_total_vol)
model_LpHC_patheff <- lm(patheff ~ LpHC_vol + sex + age + tiv, data = data_total_vol)
model_LpHC_patheffacc <- lm(patheff_acc_only ~ LpHC_vol + sex + age + tiv, data = data_total_vol)
model_RaHC_propcorr <- lm(propcorrect ~ RaHC_vol + sex + age + tiv, data = data_total_vol)
model_RaHC_patheff <- lm(patheff ~ RaHC_vol + sex + age + tiv, data = data_total_vol)
model_RaHC_patheffacc <- lm(patheff_acc_only ~ RaHC_vol + sex + age + tiv, data = data_total_vol)
model_RpHC_propcorr <- lm(propcorrect ~ RpHC_vol + sex + age + tiv, data = data_total_vol)
model_RpHC_patheff <- lm(patheff ~ RpHC_vol + sex + age + tiv, data = data_total_vol)
model_RpHC_patheffacc <- lm(patheff_acc_only ~ RpHC_vol + sex + age + tiv, data = data_total_vol)

  # Assumptions check:
  # Linearity
  plot(data_total_vol$age, data_total_vol$propcorrect)
  # No multicollinearity
  vif(model_LaHC_propcorr) # Replace the value with diff models. outcome values should be less than 5
  # Independent observations
  # Homoscedasticity = constant variance
  resid <- resid(model_LaHC_propcorr) # Replace the value with diff models
  plot(fitted(model_LaHC_propcorr), resid) 
  # Multivariate normality

summary(model_LaHC_propcorr)
summary(model_LaHC_patheff)
summary(model_LaHC_patheffacc)
summary(model_LpHC_propcorr)
summary(model_LpHC_patheff)
summary(model_LpHC_patheffacc)
summary(model_RaHC_propcorr)
summary(model_RaHC_patheff) 
summary(model_RaHC_patheffacc) 
summary(model_RpHC_propcorr) # *the only ones steve looked at*
summary(model_RpHC_patheff) # *the only ones steve looked at*
summary(model_RpHC_patheffacc) # *the only ones steve looked at*

#### ------------------ Section 2B: Bilateral Ant/Post Thickness + Propcorr or Patheff (n~98) ------------------ ####
# Check for outliers
which(abs(scale(data_total_thick$tLaHC)) > 3) # 20
which(abs(scale(data_total_thick$tLpHC)) > 3) 
which(abs(scale(data_total_thick$tRaHC)) > 3) # 20
which(abs(scale(data_total_thick$tRpHC)) > 3)

# Remove outliers
data_total_thick_rm <- data_total_thick[-c(20),]
dim(data_total_thick_rm)

## Function that outputs correlation table for all 4 subregions
## Inputs: (1) list of correlation (r) values and p-values
## Output: a correlation matrix (mat)/ table for the 4 subregions' r and p-values
subfields_antpost <- c("tLaHC", "tLpHC", "tRaHC", "tRpHC")
corr_mat_antpost <- function(r_list) {
  corr_mat_antpost <- matrix(unlist(t(r_list)), byrow=T, 4, 2)
  colnames(corr_mat_antpost) <- c("r", "p-value")
  rownames(corr_mat_antpost) <- subfields_antpost
  return(corr_mat_antpost)
}

# Partial correlation tables between L/R ant/post thickness and propcorrect, patheff, patheff_acc_only (n=98)
nav_dv <- c("propcorrect", "patheff", "patheff_acc_only")
for (dv in nav_dv) {
  r <- NULL;
  
  for (subfd_name in subfields_antpost) { 
    subfield <- paste(subfd_name, "_pcor", sep = "")
    PC_subfield_pcor <- pcor.test(pull(data_total_thick_rm[subfd_name]), data_total_thick_rm[dv], 
                                  c(data_total_thick_rm$sex, data_total_thick_rm$age, data_total_thick_rm$tiv), method = "pearson")
    assign(subfd_name, PC_subfield_pcor)
    r <- rbind(r, round(PC_subfield_pcor$estimate, digits = 3), round(PC_subfield_pcor$p.value, digits = 5))
    # print(paste(i, "estimate is:", round(PC_subfield_pcor$estimate, digits = 2), 
    #             "and p-value is:", round(PC_subfield_pcor$p.value, digits = 3))) 
  } # ex. LCA1_pcor
  
  print(dv)
  print(corr_mat_antpost(r))
}

ggplot(data = data_total_thick_rm, aes(y = propcorrect, x = tLaHC)) +
  geom_point(color = palette_sbfd[8]) + geom_smooth(method = "lm", color = palette_sbfd[8]) +
  ggtitle("Left aHC Thickness") + xlab("Normalized Thickness Ratio") +
  ylab("Proportion Correct") + ylim(0, 1)

# MULTIPLE LINEAR REGRESSION - CANNOT DO PARTIAL CORRECTION WITH SEX?
## Pearson correlation: Assumptions check
  # Homoscedasticity = constant variance
  model <- lm(propcorrect ~ tLpHC, data = data_total_thick_rm) # Replace the X value with diff HC subdivisions
  resid <- resid(model)
  plot(fitted(model), resid)
  # Linearity
  plot(data_total_thick_rm$tLaHC, data_total_thick_rm$propcorrect) # Replace the X value with diff HC subdivisions
  # Good to use pearson's correlation :)

## Pearson's correlation tables between L/R ant/post thickness and propcorrect, patheff, patheff_acc_only (n=99)
nav_dv <- c("propcorrect", "patheff", "patheff_acc_only")
for (dv in nav_dv) {
  r <- NULL;
  
  for (subfd_name in subfields_antpost) { 
    subfield <- paste(subfd_name, "_pcor", sep = "")
    PC_subfield_cor <- cor.test(pull(data_total_thick_rm[subfd_name]), data_total_thick_rm[,dv], method = "pearson")
    assign(subfd_name, PC_subfield_cor)
    r <- rbind(r, round(PC_subfield_cor$estimate, digits = 3), round(PC_subfield_cor$p.value, digits = 5))
    # print(paste(i, "estimate is:", round(PC_subfield_cor$estimate, digits = 2), 
    #             "and p-value is:", round(PC_subfield_cor$p.value, digits = 3))) 
  } # ex. LCA1_cor
  
  print(dv)
  print(corr_mat_antpost(r))
}

## Multiple linear regression
model_tLaHC_propcorr <- lm(propcorrect ~ tLaHC + sex + age + tiv, data = data_total_thick_rm)
model_tLaHC_patheff <- lm(patheff ~ tLaHC + sex + age + tiv, data = data_total_thick_rm)
model_tLaHC_patheffacc <- lm(patheff_acc_only ~ tLaHC + sex + age + tiv, data = data_total_thick_rm)
model_tLpHC_propcorr <- lm(propcorrect ~ tLpHC + sex + age + tiv, data = data_total_thick_rm)
model_tLpHC_patheff <- lm(patheff ~ tLpHC + sex + age + tiv, data = data_total_thick_rm)
model_tLpHC_patheffacc <- lm(patheff_acc_only ~ tLpHC + sex + age + tiv, data = data_total_thick_rm)
model_tRaHC_propcorr <- lm(propcorrect ~ tRaHC + sex + age + tiv, data = data_total_thick_rm)
model_tRaHC_patheff <- lm(patheff ~ tRaHC + sex + age + tiv, data = data_total_thick_rm)
model_tRaHC_patheffacc <- lm(patheff_acc_only ~ tRaHC + sex + age + tiv, data = data_total_thick_rm)
model_tRpHC_propcorr <- lm(propcorrect ~ tRpHC + sex + age + tiv, data = data_total_thick_rm)
model_tRpHC_patheff <- lm(patheff ~ tRpHC + sex + age + tiv, data = data_total_thick_rm)
model_tRpHC_patheffacc <- lm(patheff_acc_only ~ tRpHC + sex + age + tiv, data = data_total_thick_rm)

  # Assumptions check:
  # Linearity
  plot(data_total_thick_rm$age, data_total_thick_rm$propcorrect)
  # No multicollinearity
  vif(model_tLaHC_propcorr) # Replace the value with diff models. outcome values should be less than 5
  # Independent observations
  # Homoscedasticity = constant variance
  resid <- resid(model_tLaHC_propcorr) # Replace the value with diff models
  plot(fitted(model_tLaHC_propcorr), resid) 
  # Multivariate normality

summary(model_tLaHC_propcorr)
summary(model_tLaHC_patheff)
summary(model_tLaHC_patheffacc)
summary(model_tLpHC_propcorr)
summary(model_tLpHC_patheff)
summary(model_tLpHC_patheffacc)
summary(model_tRaHC_propcorr)
summary(model_tRaHC_patheff) 
summary(model_tRaHC_patheffacc) 
summary(model_tRpHC_propcorr) 
summary(model_tRpHC_patheff) 
summary(model_tRpHC_patheffacc) 

#### --------------------- Section 2C: Total Ant/Post Volumes + Propcorr or Patheff (n=99) --------------------- ####
# Check for outliers
which(abs(scale(data_total_vol$aHC_vol)) > 3)
which(abs(scale(data_total_vol$pHC_vol)) > 3)

## Function that outputs correlation table for 2 subregions
## Inputs: (1) list of correlation (r) values and p-values
## Output: a correlation matrix (mat)/ table for the 2 subregions' r and p-values
subfields_antpost <- c("aHC_vol", "pHC_vol")
corr_mat_antpost <- function(r_list) {
  corr_mat_antpost <- matrix(unlist(t(r_list)), byrow=T, 2, 2)
  colnames(corr_mat_antpost) <- c("r", "p-value")
  rownames(corr_mat_antpost) <- c("aHC", "pHC")
  return(corr_mat_antpost)
}

# Partial correlation tables between total ant/post volumes and propcorrect, patheff, patheff_acc_only (n=99)
nav_dv <- c("propcorrect", "patheff", "patheff_acc_only")
for (dv in nav_dv) {
  r <- NULL;
  
  for (subfd_name in subfields_antpost) { 
    subfield <- paste(subfd_name, "_pcor", sep = "")
    PC_subfield_pcor <- pcor.test(pull(data_total_vol[subfd_name]), data_total_vol[dv], 
                                  c(data_total_vol$sex, data_total_vol$age, data_total_vol$tiv), method = "pearson")
    assign(subfd_name, PC_subfield_pcor)
    r <- rbind(r, round(PC_subfield_pcor$estimate, digits = 3), round(PC_subfield_pcor$p.value, digits = 5))
    # print(paste(i, "estimate is:", round(PC_subfield_pcor$estimate, digits = 2), 
    #             "and p-value is:", round(PC_subfield_pcor$p.value, digits = 3))) 
  } # ex. LCA1_pcor
  
  print(dv)
  print(corr_mat_antpost(r))
}

ggplot(data = data_total_vol, aes(y = propcorrect, x = aHC_vol)) +
  geom_point(color = palette_sbfd[8]) + geom_smooth(method = "lm", color = palette_sbfd[8]) +
  ggtitle("Total aHC Volume") + xlab("Anterior HC Volume (mm^3)") +
  ylab("Proportion Correct") + ylim(0, 1)

# MULTIPLE LINEAR REGRESSION - CANNOT DO PARTIAL CORRECTION WITH SEX?
## Pearson correlation: Assumptions check
  # Homoscedasticity = constant variance
  model <- lm(propcorrect ~ aHC_vol, data = data_total_vol) # Replace the X value with diff HC subdivisions
  resid <- resid(model)
  plot(fitted(model), resid)
  # Linearity
  plot(data_total_vol$pHC_vol, data_total_vol$propcorrect) # Replace the X value with diff HC subdivisions
  # Good to use pearson's correlation :)

## Pearson's correlation tables between total ant/post volumes and propcorrect, patheff, patheff_acc_only (n=99)
nav_dv <- c("propcorrect", "patheff", "patheff_acc_only")
for (dv in nav_dv) {
  r <- NULL;
  
  for (subfd_name in subfields_antpost) { 
    subfield <- paste(subfd_name, "_pcor", sep = "")
    PC_subfield_cor <- cor.test(pull(data_total_vol[subfd_name]), data_total_vol[,dv], method = "pearson")
    assign(subfd_name, PC_subfield_cor)
    r <- rbind(r, round(PC_subfield_cor$estimate, digits = 3), round(PC_subfield_cor$p.value, digits = 5))
    # print(paste(i, "estimate is:", round(PC_subfield_cor$estimate, digits = 2), 
    #             "and p-value is:", round(PC_subfield_cor$p.value, digits = 3))) 
  } # ex. LCA1_cor
  
  print(dv)
  print(corr_mat_antpost(r))
}

## Multiple linear regression
model_aHC_propcorr <- lm(propcorrect ~ aHC_vol + sex + age + tiv, data = data_total_vol)
model_aHC_patheff <- lm(patheff ~ aHC_vol + sex + age + tiv, data = data_total_vol)
model_aHC_patheffacc <- lm(patheff_acc_only ~ aHC_vol + sex + age + tiv, data = data_total_vol)
model_pHC_propcorr <- lm(propcorrect ~ pHC_vol + sex + age + tiv, data = data_total_vol)
model_pHC_patheff <- lm(patheff ~ pHC_vol + sex + age + tiv, data = data_total_vol)
model_pHC_patheffacc <- lm(patheff_acc_only ~ pHC_vol + sex + age + tiv, data = data_total_vol)

  # Assumptions check:
  # Linearity
  plot(data_total_vol$aHC_vol, data_total_vol$propcorrect)
  # No multicollinearity
  vif(model_aHC_propcorr) # Replace the value with diff models. outcome values should be less than 5
  # Independent observations
  # Homoscedasticity = constant variance
  resid <- resid(model_aHC_propcorr) # Replace the value with diff models
  plot(fitted(model_aHC_propcorr), resid) 
  # Multivariate normality

summary(model_aHC_propcorr)
summary(model_aHC_patheff)
summary(model_aHC_patheffacc)
summary(model_pHC_propcorr)
summary(model_pHC_patheff)
summary(model_pHC_patheffacc)

#### -------------------- Section 2D: L/R/Mean Post-Ant Ratio + Propcorr or Patheff (n=96) --------------------- ####
# Check for outliers
which(abs(scale(data_total_vol_ratio$Lpa_ratio)) > 3) # 66
which(abs(scale(data_total_vol_ratio$Rpa_ratio)) > 3) # 21, 26
which(abs(scale(data_total_vol_ratio$meanpa_ratio)) > 3) # 21

# Remove outliers
data_total_vol_ratio_rm <- data_total_vol_ratio[-c(21, 26, 66),] # Participants 25, 31, 76

## Function that outputs correlation table for 3 measures
## Inputs: (1) list of correlation (r) values and p-values
## Output: a correlation matrix (mat)/ table for the 3 measures' r and p-values
subfields_ratio <- c("Lpa_ratio", "Rpa_ratio", "meanpa_ratio")
corr_mat_ratio <- function(r_list) {
  corr_mat_ratio <- matrix(unlist(t(r_list)), byrow=T, 3, 2)
  colnames(corr_mat_ratio) <- c("r", "p-value")
  rownames(corr_mat_ratio) <- subfields_ratio
  return(corr_mat_ratio)
}

# Outliers removed: Partial correlation between L/R post/ant HC vol ratio and propcorrect, patheff, patheff_acc_only (n=96)
nav_dv <- c("propcorrect", "patheff", "patheff_acc_only")
for (dv in nav_dv) {
  r <- NULL;
  
  for (subfd_name in subfields_ratio) { 
    subfield <- paste(subfd_name, "_pcor", sep = "")
    PC_subfield_pcor <- pcor.test(pull(data_total_vol_ratio_rm[subfd_name]), data_total_vol_ratio_rm[dv], 
                                  c(data_total_vol_ratio_rm$sex, data_total_vol_ratio_rm$age, data_total_vol_ratio_rm$tiv), method = "pearson")
    assign(subfd_name, PC_subfield_pcor)
    r <- rbind(r, round(PC_subfield_pcor$estimate, digits = 3), round(PC_subfield_pcor$p.value, digits = 5))
    # print(paste(i, "estimate is:", round(PC_subfield_pcor$estimate, digits = 2), 
    #             "and p-value is:", round(PC_subfield_pcor$p.value, digits = 3))) 
  } # ex. LCA1_pcor
  
  print(dv)
  print(corr_mat_ratio(r))
}

ggplot(data = data_total_vol_ratio_rm, aes(y = propcorrect, x = Lpa_ratio)) + 
  geom_point(color = palette[9]) + geom_smooth(method = "lm", color = palette[9]) +
  ggtitle("Left pHC/aHC Ratio") + xlab("Left PostHC/AntHC Ratio") +
  ylab("Proportion Correct") + ylim(0, 1)
ggplot(data = data_total_vol_ratio_rm, aes(y = propcorrect, x = Rpa_ratio)) + 
  geom_point(color = palette[9]) + geom_smooth(method = "lm", color = palette[9]) +
  ggtitle("Right pHC/aHC Ratio") + xlab("Right PostHC/AntHC Ratio") +
  ylab("Proportion Correct") + ylim(0, 1)
ggplot(data = data_total_vol_ratio_rm, aes(y = propcorrect, x = meanpa_ratio)) + 
  geom_point(color = palette[9]) + geom_smooth(method = "lm", color = palette[9]) +
  ggtitle("Mean pHC/aHC Ratio") + xlab("Mean PostHC/AntHC Ratio") +
  ylab("Proportion Correct") + ylim(0, 1)
# ggplot(data = data_total_vol_ratio_rm, aes(y = patheff, x = Rpa_ratio)) + 
#   geom_point(color = palette[9]) + geom_smooth(method = "lm", color = palette[9]) +
#   ggtitle("Right pHC/aHC Ratio") + xlab("Right PostHC/AntHC Ratio") +
#   ylab("Path Inefficiency (all trials)") + scale_y_continuous(breaks = seq(1, 10, by = 0.1))
# ggplot(data = data_total_vol_ratio_rm, aes(y = patheff, x = meanpa_ratio)) + 
#   geom_point(color = palette[9]) + geom_smooth(method = "lm", color = palette[9]) +
#   ggtitle("Mean pHC/aHC Ratio") + xlab("Mean PostHC/AntHC Ratio") +
#   ylab("Path Inefficiency (all trials)") + scale_y_continuous(breaks = seq(1, 10, by = 0.1))

# MULTIPLE LINEAR REGRESSION - CANNOT DO PARTIAL CORRECTION WITH SEX?
## Pearson correlation: Assumptions check
  # Homoscedasticity = constant variance
  model <- lm(propcorrect ~ Lpa_ratio, data = data_total_vol_ratio_rm) # Replace the X value with diff HC subdivisions
  resid <- resid(model)
  plot(fitted(model), resid)
  # Linearity
  plot(data_total_vol_ratio_rm$Lpa_ratio, data_total_vol_ratio_rm$propcorrect) # Replace the X value with diff HC subdivisions
  # Good to use pearson's correlation :)

## Pearson's correlation tables between total ant/post volumes and propcorrect, patheff, patheff_acc_only (n=96)
nav_dv <- c("propcorrect", "patheff", "patheff_acc_only")
for (dv in nav_dv) {
  r <- NULL;
  
  for (subfd_name in subfields_ratio) { 
    subfield <- paste(subfd_name, "_pcor", sep = "")
    PC_subfield_cor <- cor.test(pull(data_total_vol_ratio_rm[subfd_name]), data_total_vol_ratio_rm[,dv], method = "pearson")
    assign(subfd_name, PC_subfield_cor)
    r <- rbind(r, round(PC_subfield_cor$estimate, digits = 3), round(PC_subfield_cor$p.value, digits = 5))
    # print(paste(i, "estimate is:", round(PC_subfield_cor$estimate, digits = 2), 
    #             "and p-value is:", round(PC_subfield_cor$p.value, digits = 3))) 
  } # ex. LCA1_cor
  
  print(dv)
  print(corr_mat_ratio(r))
}

## Multiple linear regression
model_Lratio_propcorr <- lm(propcorrect ~ Lpa_ratio + sex + age + tiv, data = data_total_vol_ratio_rm)
model_Lratio_patheff <- lm(patheff ~ Lpa_ratio + sex + age + tiv, data = data_total_vol_ratio_rm)
model_Lratio_patheffacc <- lm(patheff_acc_only ~ Lpa_ratio + sex + age + tiv, data = data_total_vol_ratio_rm)
model_Rratio_propcorr <- lm(propcorrect ~ Rpa_ratio + sex + age + tiv, data = data_total_vol_ratio_rm)
model_Rratio_patheff <- lm(patheff ~ Rpa_ratio + sex + age + tiv, data = data_total_vol_ratio_rm)
model_Rratio_patheffacc <- lm(patheff_acc_only ~ Rpa_ratio + sex + age + tiv, data = data_total_vol_ratio_rm)
model_meanratio_propcorr <- lm(propcorrect ~ meanpa_ratio + sex + age + tiv, data = data_total_vol_ratio_rm)
model_meanratio_patheff <- lm(patheff ~ meanpa_ratio + sex + age + tiv, data = data_total_vol_ratio_rm)
model_meanratio_patheffacc <- lm(patheff_acc_only ~ meanpa_ratio + sex + age + tiv, data = data_total_vol_ratio_rm)

  # Assumptions check:
  # Linearity
  plot(data_total_vol_ratio_rm$Lpa_ratio, data_total_vol_ratio_rm$propcorrect)
  # No multicollinearity
  vif(model_Lratio_propcorr) # Replace the value with diff models. outcome values should be less than 5
  # Independent observations
  # Homoscedasticity = constant variance
  resid <- resid(model_Lratio_propcorrect) # Replace the value with diff models
  plot(fitted(model_Lratio_propcorrect), resid) 
  # Multivariate normality

summary(model_Lratio_propcorr)
summary(model_Lratio_patheff)
summary(model_Lratio_patheffacc)
summary(model_Rratio_propcorr)
summary(model_Rratio_patheff)
summary(model_Rratio_patheffacc)
summary(model_meanratio_propcorr)
summary(model_meanratio_patheff)
summary(model_meanratio_patheffacc)

#### --------------------- Section 3: Ant/Post/Total HC + SBSOD Multiple Lin. Regr. (n=96) --------------------- ####
# Data
data_survey_total_vol_rm <- merge(data_survey_scores[,1:8], data_total_vol_rm, by = "sub_id") %>% na.omit()
dim(data_survey_total_vol_rm)

# Multiple Linear Regression !!!!!!! IN PROGRESS !!!!!!!!!
regionHC_LR <- c("LHC_vol", "RHC_vol", "totalHC_vol", "LaHC_vol", "LpHC_vol", "RaHC_vol", "RpHC_vol")
for (region in regionHC_LR) {
  lm_HCvol <- lm(propcorrect ~ SBSOD_score + region + tiv + SBSOD_score*region, data = data_survey_total_vol_rm)
  
  print(region)
  print(summary(lm_HCvol))
}

#### -------------- Section 4A: *No cut-off* HR-T2 Subfield Volumes + Propcorr or Patheff (n=14) --------------- ####
## Function that outputs correlation table for all 7 subfields
## Inputs: (1) list of correlation (r) values and p-values
## Output: a correlation matrix (mat)/ table for the 7 subfields' r and p-values
subfields_all <- c("CA1", "CA2/3", "DG", "SUB", "ERC", "PRC", "PHC")
corr_mat_allsub <- function(r_list) {
  corr_mat_allsub <- matrix(unlist(t(r_list)), byrow=T, 7, 4)
  colnames(corr_mat_allsub) <- c("(Left) r", "p-value", "(Right) r", "p-value")
  rownames(corr_mat_allsub) <- subfields_all
  return(corr_mat_allsub)
}

dim(data_cut)

# Partial correlation tables between no cutoff subfield volumes and propcorrect, patheff, patheff_acc_only (n=14)
nav_dv <- c("propcorrect", "patheff", "patheff_acc_only")
for (dv in nav_dv) {
  r <- NULL;
  
  for (subfd_name in subfields_LR) { 
    subfield <- paste(subfd_name, "_pcor", sep = "")
    PC_subfield_pcor <- pcor.test(pull(data_cut[subfd_name]), data_cut[dv], 
                                  c(data_cut$sex, data_cut$age, data_cut$tiv), method = "pearson")
    assign(subfd_name, PC_subfield_pcor)
    r <- rbind(r, round(PC_subfield_pcor$estimate, digits = 2), round(PC_subfield_pcor$p.value, digits = 4))
    # print(paste(i, "estimate is:", round(PC_subfield_pcor$estimate, digits = 2), 
    #             "and p-value is:", round(PC_subfield_pcor$p.value, digits = 3))) 
  } # ex. LCA1_pcor
  
  print(dv)
  print(corr_mat_allsub(r))
}

theme_set(theme2)
# ggplot(data = data_cut, aes(y = patheff, x = LSUB)) +
#   geom_point(color = palette_sbfd[4]) + geom_smooth(method = "lm", color = palette_sbfd[4]) +
#   ggtitle("Path Efficiency and Left SUB Volume") +
#   xlab("Left SUB Volume (mm^3)") + ylab("Path Inefficiency") + scale_y_continuous(breaks = seq(1, 10, by = 0.1)) # indiv subfield
ggplot(data = data_cut, aes(y = patheff_acc_only, x = LCA23)) +
  geom_point(color = palette_sbfd[2]) + geom_smooth(method = "lm", color = palette_sbfd[2]) +
  ggtitle("Left CA2/3 Volume") +
  xlab("Left CA2/3 Volume (mm^3)") + ylab("Path Inefficiency (Correct Trials Only)") + 
  scale_y_continuous(breaks = seq(1, 10, by = 0.05)) + ylim(1, 1.25) # indiv subfield

#### -------------- Section 4AA: *No cut-off* HR-T2 Subfield Volumes + Propcorr or Patheff (n=14) -------------- ####
## Function that outputs correlation table for all 7 subfields ((combined hemisphere))
## Inputs: (1) list of correlation (r) values and p-values
## Output: a correlation matrix (mat)/ table for the 7 subfields' r and p-values
subfields_all <- c("CA1", "CA23", "DG", "SUB", "ERC", "PRC", "PHC")
corr_mat_allsub <- function(r_list) {
  corr_mat_allsub <- matrix(unlist(t(r_list)), byrow=T, 7, 2)
  colnames(corr_mat_allsub) <- c("r", "p-value")
  rownames(corr_mat_allsub) <- subfields_all
  return(corr_mat_allsub)
}

# Combine left and right hemispheres
data_cut_LR <- data_cut %>% mutate(CA1 = LCA1 + RCA1, CA23 = LCA23 + RCA23, DG = LDG + RDG, SUB = LSUB + RSUB, 
                                   ERC = LERC + RERC, PRC = LPRC + RPRC, PHC = LPHC + RPHC)

# Partial correlation table between no cutoff subfield volumes and propcorrect
r <- NULL;

for (subfd_name in subfields_all) { 
  subfield <- paste(subfd_name, "_pcor", sep = "")
  PC_subfield_pcor <- pcor.test(pull(data_cut_LR[subfd_name]), data_cut_LR$propcorrect, 
                                c(data_cut_LR$sex, data_cut_LR$age, data_cut_LR$tiv), method = "pearson")
  assign(subfd_name, PC_subfield_pcor)
  r <- rbind(r, round(PC_subfield_pcor$estimate, digits = 2), round(PC_subfield_pcor$p.value, digits = 4))
  # print(paste(i, "estimate is:", round(PC_subfield_pcor$estimate, digits = 2), 
  #             "and p-value is:", round(PC_subfield_pcor$p.value, digits = 3))) 
} # ex. LCA1_pcor

corr_mat_allsub(r)

# Partial correlation table between no cutoff subfield volumes and patheff
r <- NULL;

for (subfd_name in subfields_all) { 
  subfield <- paste(subfd_name, "_pcor", sep = "")
  PC_subfield_pcor <- pcor.test(pull(data_cut_LR[subfd_name]), data_cut_LR$tm_pathefficiencies, 
                                c(data_cut_LR$sex, data_cut_LR$age, data_cut_LR$tiv), method = "pearson")
  assign(subfd_name, PC_subfield_pcor)
  r <- rbind(r, round(PC_subfield_pcor$estimate, digits = 2), round(PC_subfield_pcor$p.value, digits = 4))
  # print(paste(i, "estimate is:", round(PC_subfield_pcor$estimate, digits = 2), 
  #             "and p-value is:", round(PC_subfield_pcor$p.value, digits = 3))) 
} # ex. LCA1_pcor

corr_mat_allsub(r)

#### ------------- Section 4B: *No cut-off* HR-T2 Subfield Thickness + Propcorr or Patheff (n=26) -------------- ####
# Subfield thickness
subfields_tHC_LR <- NULL;

for (subfd_name in subfields_LR) {
  subfields_tHC_LR <- rbind(subfields_tHC_LR, paste("t", subfd_name, sep = ""))
}
subfields_tHC_LR <- subfields_tHC_LR[,1]

dim(data_tdata)

# Partial correlation tables between subfield thickness and propcorrect, patheff, patheff_acc_only (n=26)
nav_dv <- c("propcorrect", "patheff", "patheff_acc_only")
for (dv in nav_dv) {
  r <- NULL;
  
  for (subfd_name in subfields_tHC_LR) { 
    subfield <- paste(subfd_name, "_pcor", sep = "")
    PC_tsubfield_pcor <- pcor.test(pull(data_tdata[subfd_name]), data_tdata[dv], 
                                   c(data_tdata$sex, data_tdata$age, data_tdata$tiv), method = "pearson")
    assign(subfd_name, PC_tsubfield_pcor)
    r <- rbind(r, round(PC_tsubfield_pcor$estimate, digits = 2), round(PC_tsubfield_pcor$p.value, digits = 4))
    # print(paste(i, "estimate is:", round(PC_tsubfield_pcor$estimate, digits = 2), 
    #             "and p-value is:", round(PC_tsubfield_pcor$p.value, digits = 3))) 
  } # ex. tLCA1_pcor
  
  print(dv)
  print(corr_mat_allsub(r))
}

#### -------------- Section 5A: HR-T2 *Body-only* Subfield Volumes + Propcorr or Patheff (n=17) ---------------- ####
## Function that outputs correlation table for 6 subfields
## Inputs: (1) list of correlation (r) values and p-values
## Output: a correlation matrix (mat)/ table for the 6 subfields' r and p-values
subfields_6 <- c("CA1", "CA2/3", "DG", "SUB", "PRC", "PHC")
corr_mat_6sub <- function(r_list) {
  corr_mat_6sub <- matrix(unlist(t(r_list)), byrow=T, 6, 4)
  colnames(corr_mat_6sub) <- c("(Left) r", "p-value", "(Right) r", "p-value")
  rownames(corr_mat_6sub) <- subfields_6
  return(corr_mat_6sub)
}

subfields_LR6 <- subfields_LR[-c(9:10)]

dim(data_HCbody_cut)

# Partial correlation tables between body-only subfield volumes and propcorrect, patheff, patheff_acc_only (n=17)
nav_dv <- c("propcorrect", "patheff", "patheff_acc_only")
for (dv in nav_dv) {
  r <- NULL;
  
  for (subfd_name in subfields_LR6) { 
    subfield <- paste(subfd_name, "_pcor", sep = "")
    PC_subfield_pcor <- pcor.test(pull(data_HCbody_cut[subfd_name]), data_HCbody_cut[dv], 
                                  c(data_HCbody_cut$sex, data_HCbody_cut$age, data_HCbody_cut$tiv), method = "pearson")
    assign(subfd_name, PC_subfield_pcor)
    r <- rbind(r, round(PC_subfield_pcor$estimate, digits = 2), round(PC_subfield_pcor$p.value, digits = 4))
    # print(paste(i, "estimate is:", round(PC_subfield_pcor$estimate, digits = 2), 
    #             "and p-value is:", round(PC_subfield_pcor$p.value, digits = 3))) 
  } # ex. LCA1_pcor
  
  print(dv)
  print(corr_mat_6sub(r))
}

ggplot(data = data_HCbody_cut, aes(y = patheff_acc_only, x = RCA1)) +
  geom_point(color = palette_sbfd[1]) + geom_smooth(method = "lm", color = palette_sbfd[1]) +
  ggtitle("Right CA1 Volume") +
  xlab("Right CA1 Volume (mm^3)") + ylab("Path Inefficiency (Correct Trials Only)") + 
  scale_y_continuous(breaks = seq(1, 10, by = 0.05)) + ylim(1, 1.25) # indiv subfield

#### ---------------- Section 5B: HR-T2 *Body-only* Subfield Thick + Propcorr or Patheff (n=17) ---------------- ####
subfields_tLR6 <- subfields_tHC_LR[-c(9:10)]

# Partial correlation tables between body-only subfield thickness and propcorrect, patheff, patheff_acc_only (n=17)
nav_dv <- c("propcorrect", "patheff", "patheff_acc_only")
for (dv in nav_dv) {
  r <- NULL;
  
  for (subfd_name in subfields_tLR6) { 
    subfield <- paste(subfd_name, "_pcor", sep = "")
    PC_subfield_pcor <- pcor.test(pull(data_tHCbody_cut[subfd_name]), data_tHCbody_cut[dv], 
                                  c(data_tHCbody_cut$sex, data_tHCbody_cut$age, data_tHCbody_cut$tiv), method = "pearson")
    assign(subfd_name, PC_subfield_pcor)
    r <- rbind(r, round(PC_subfield_pcor$estimate, digits = 2), round(PC_subfield_pcor$p.value, digits = 4))
    # print(paste(i, "estimate is:", round(PC_subfield_pcor$estimate, digits = 2), 
    #             "and p-value is:", round(PC_subfield_pcor$p.value, digits = 3))) 
  } # ex. LCA1_pcor
  
  print(dv)
  print(corr_mat_6sub(r))
}

ggplot(data = data_tHCbody_cut, aes(y = patheff_acc_only, x = tLCA1)) +
  geom_point(color = palette_sbfd[1]) + geom_smooth(method = "lm", color = palette_sbfd[1])+
  ggtitle("Left CA1 Thickness") +
  xlab("Normalized Thickness Ratio") + ylab("Path Inefficiency (Correct Trials Only)") + 
  scale_y_continuous(breaks = seq(1, 10, by = 0.05)) + ylim(1, 1.25) # indiv subfield
# ggplot(data = data_tHCbody_cut, aes(y = patheff_acc_only, x = tLPHC)) +
#   geom_point(color = palette_sbfd[7]) + geom_smooth(method = "lm", color = palette_sbfd[7])+
#   ggtitle("Left PHC Thickness") +
#   xlab("Normalized Thickness Ratio") + ylab("Path Inefficiency (Correct Trials Only)") + 
#   scale_y_continuous(breaks = seq(1, 10, by = 0.05)) # indiv subfield

#### ------------------ Section 6A: Full Reg-T2 Subfield Volumes + Propcorr or Patheff (n=26) ------------------ ####
## Function that outputs correlation table for all 5 subfields
## Inputs: (1) list of correlation (r) values and p-values
## Output: a correlation matrix (mat)/ table for the 5 subfields' r and p-values
subfields_regT2 <- c("aHC", "pHC", "ERC", "PRC", "PHC")
subfields_LRregT2 <- c("LaHC", "RaHC", "LpHC", "RpHC", "LERC", "RERC", "LPRC", "RPRC", "LPHC", "RPHC")
corr_mat_5sub <- function(r_list) {
  corr_mat_5sub <- matrix(unlist(t(r_list)), byrow=T, 5, 4)
  colnames(corr_mat_5sub) <- c("(Left) r", "p-value", "(Right) r", "p-value")
  rownames(corr_mat_5sub) <- subfields_regT2
  return(corr_mat_5sub)
}

dim(data_regT2)

# Partial correlation tables between manual regT2 subfield volumes and propcorrect, patheff, patheff_acc_only (n=26)
nav_dv <- c("propcorrect", "patheff", "patheff_acc_only")
for (dv in nav_dv) {
  r <- NULL;
  
  for (subfd_name in subfields_LRregT2) { 
    subfield <- paste(subfd_name, "_pcor", sep = "")
    PC_subfield_pcor <- pcor.test(pull(data_regT2[subfd_name]), data_regT2[dv], 
                                  c(data_regT2$sex, data_regT2$age, data_regT2$tiv), method = "pearson")
    assign(subfd_name, PC_subfield_pcor)
    r <- rbind(r, round(PC_subfield_pcor$estimate, digits = 2), round(PC_subfield_pcor$p.value, digits = 4))
    # print(paste(i, "estimate is:", round(PC_subfield_pcor$estimate, digits = 2), 
    #             "and p-value is:", round(PC_subfield_pcor$p.value, digits = 3))) 
  } # ex. LCA1_pcor
  
  print(dv)
  print(corr_mat_5sub(r))
}

ggplot(data = data_regT2, aes(y = patheff_acc_only, x = RERC)) + 
  geom_point(color = palette_sbfd[5]) + geom_smooth(method = "lm", color = palette_sbfd[5]) +
  ggtitle("Right ERC Volume") + 
  xlab("Right ERC Volume (mm^3)") + ylab("Path Inefficiency (Correct Trials Only)") + 
  scale_y_continuous(breaks = seq(1, 10, by = 0.05)) + ylim(1, 1.25) # indiv subfield
ggplot(data = data_regT2, aes(y = patheff_acc_only, x = LPHC)) + 
  geom_point(color = palette_sbfd[7]) + geom_smooth(method = "lm", color = palette_sbfd[7]) +
  ggtitle("Left PHC Volume") + 
  xlab("Left PHC Volume (mm^3)") + ylab("Path Inefficiency (Correct Trials Only)") + 
  scale_y_continuous(breaks = seq(1, 10, by = 0.05)) + ylim(1, 1.25) # indiv subfield

#### ----------------- Section 6B: Full Reg-T2 Subfield Thickness + Propcorr or Patheff (n=26) ----------------- ####
# Create function: correlation table for 5 subfields
subfields_regT2 <- c("aHC", "pHC", "ERC", "PRC", "PHC")
corr_mat_5sub <- function(r_list) {
  corr_mat_5sub <- matrix(unlist(t(r_list)), byrow=T, 5, 4)
  colnames(corr_mat_5sub) <- c("(Left) r", "p-value", "(Right) r", "p-value")
  rownames(corr_mat_5sub) <- subfields_regT2
  return(corr_mat_5sub)
}

subfields_tLRregT2 <- NULL;
for (subfd_name in subfields_LRregT2) {
  subfields_tLRregT2 <- rbind(subfields_tLRregT2, paste("t", subfd_name, sep = ""))
}
subfields_tLRregT2 <- subfields_tLRregT2[,1]

# Partial correlation tables between manual regT2 subfield thickness and propcorrect, patheff, patheff_acc_only (n=26)
nav_dv <- c("propcorrect", "patheff", "patheff_acc_only")
for (dv in nav_dv) {
  r <- NULL;
  
  for (subfd_name in subfields_tLRregT2) { 
    subfield <- paste(subfd_name, "_pcor", sep = "")
    PC_tsubfield_pcor <- pcor.test(pull(data_tregT2[subfd_name]), data_tregT2[dv], 
                                   c(data_tregT2$sex, data_tregT2$age, data_tregT2$tiv), method = "pearson")
    assign(subfd_name, PC_tsubfield_pcor)
    r <- rbind(r, round(PC_tsubfield_pcor$estimate, digits = 2), round(PC_tsubfield_pcor$p.value, digits = 4))
    # print(paste(i, "estimate is:", round(PC_tsubfield_pcor$estimate, digits = 2), 
    #             "and p-value is:", round(PC_tsubfield_pcor$p.value, digits = 3))) 
  } # ex. tLCA1_pcor
  
  print(dv)
  print(corr_mat_5sub(r))
}

ggplot(data = data_tregT2, aes(y = patheff_acc_only, x = tRERC)) + 
  geom_point(color = palette_sbfd[5]) + geom_smooth(method = "lm", color = palette_sbfd[5]) +
  ggtitle("Right ERC Thickness") + 
  xlab("Right ERC Thickness") + ylab("Path Inefficiency (Correct Trials Only)") + 
  scale_y_continuous(breaks = seq(1, 10, by = 0.05)) + ylim(1, 1.25) # indiv subfield
