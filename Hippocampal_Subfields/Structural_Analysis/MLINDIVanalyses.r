# Author: Alina Tu
# Contact: alinat2@uci.edu
# Last Updated: 3/14/2023
# About: This script is the structural analysis of the Maze Learning Individual Differences (MLINDIV) project,
#        specifically looking at young adult hippocampal subfield volumes/thicknesses and their relationship
#        with navigation ability.

# Before running analyses in the following sections, run everything in the RUN FIRST section.

# Note if using RStudio: I recommend collapsing/expanding sections using the small arrows on the left pane next to 
#                        the line numbers.

#### ------------------------------------------------ RUN FIRST ------------------------------------------------ ####
# Getting started [PACKAGES AND FUNCTIONS]
install.packages("readxl")
install.packages("rlang")
install.packages("ggplot2")
install.packages("tidyr")
install.packages("dplyr")
install.packages("ggcorrplot")
install.packages("stringr")
install.packages("ppcor", repos='http://cran.us.r-project.org')
install.packages("base64enc")
install.packages("Hmisc")
install.packages("readr")
install.packages("corrplot")

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
data_nav_measures <- read.csv("MLINDIV_participant_full.csv") %>% 
  select("Subject","tm_accuracy","tm_path_efficiencies","tm_path_efficiencies_acc_only")
colnames(data_nav_measures) <- c("sub_id","propcorrect","patheff","patheff_acc_only")
data_nav_measures_cut <- data_nav_measures[-c(3,21,28,45,48),] # Removing rows w/ incomplete MLINDIV behavioral data

data <- read_excel("ASHS_volumes0223.xlsx", sheet = 1)
  hres_sub_id <- c(81:83, 85, 86, 88, 89, 92:96, 98:103, 106:113)
  names(data)[names(data) == "Spatial Neuro ID"] <- "sub_id"
raw_vol_full <- as_tibble(read.csv("subfield_vol/raw_vol_full.csv")) # FINAL volumes
data[,2:15] <- raw_vol_full[,3:16]
data <- merge(data[,c(1:31,35:36)], data_nav_measures_cut, by = "sub_id")

data_HCbody <- read_excel("ASHS_volumes0223.xlsx", sheet = 2)
  names(data_HCbody)[names(data_HCbody) == "Spatial Neuro ID"] <- "sub_id"
body_vol_full <- as_tibble(read.csv("subfield_vol/body_vol_full.csv"))
data_HCbody[,2:13] <- body_vol_full[,3:14]
data_HCbody <- merge(data_HCbody[,c(1:29,33:34)], data_nav_measures_cut, by = "sub_id")

data_total_vol <- read_excel("ASHS_volumes0223.xlsx", sheet = 4)
  data_total_vol <- data_total_vol[-97,]
  data_total_vol$sub_id <- as.integer(data_total_vol$sub_id)
antpost_vol_full <- read.csv("antpost_vol/antpost_vol_full.csv")
  data_total_vol <- left_join(data_total_vol, antpost_vol_full, by = "sub_id")
  data_total_vol <- data_total_vol %>% 
    select("sub_id", "L_volmm3", "sLHC", "LaHC", "sLaHC", "LpHC", "sLpHC", 
           "R_volmm3", "sRHC", "RaHC", "sRaHC", "RpHC", "sRpHC", "total_volmm3")
  colnames(data_total_vol) <- c("sub_id", "LHC_vol", "sLHC", "LaHC_vol", "sLaHC", "LpHC_vol", "sLpHC", 
                                "RHC_vol", "sRHC", "RaHC_vol", "sRaHC", "RpHC_vol", "sRpHC", "totalHC_vol")
  data_total_vol <- merge(data_total_vol, data_nav_measures_cut, by = "sub_id", all = T)

data_regT2 <- read_excel("ASHS_volumes0223.xlsx", sheet = 5)
  names(data_regT2)[names(data_regT2) == "Spatial Neuro ID"] <- "sub_id"
data_regT2 <- merge(data_regT2[,c(1:39,43:44)], data_nav_measures_cut, by = "sub_id") 

data_full <- read_excel("MLINDIV_Subject_Run_Info.xlsx") # Full Subject Log
  names(data_full)[names(data_full) == "Spatial Neuro ID"] <- "sub_id"
  data_full$sub_id <- extract_numeric(data_full$sub_id)
  data_full <- merge(data_full[,c(1:5,7:23)], data_nav_measures[,c(1,2)], by = "sub_id", all = T) 
  
data_survey_scores <- read_excel_allsheets("RA_mlindiv_surveys.xlsx") # n=113 for questionnaires

data_tiv <- read.csv(file = "TIV_comparison.csv")
data_tiv <- data_tiv[-97,]
data_tiv$CaitTIV <- data_tiv$CaitTIV/1000

# Define custom data
data_full_clean <- data_full %>% drop_na("Sex") # n=112 with demographics/propcorrect
full_sex <- data_full_clean$Sex

data_survey_scores <- data_survey$`Summary Scores`[-97,1:8]
  data_survey_scores <- merge(data_survey_scores, data_nav_measures, by = "sub_id", all = T) 
  data_survey_scores$sex <- full_sex
data_total_vol$sex <- full_sex

post_cutoff <- c(which(data$post_cutoff == "TRUE")) # index of sub with posterior cutoffs
data_cut <- data[-c(post_cutoff),] # n=14, 7 subfield volumes with no posterior cutoffs
nrow(data_cut) == 14

post_cutoff_body <- post_cutoff[-c(5,6,9)] # posterior cutoffs for body same as full post-cutoff except 101, 102, 109
data_HCbody_cut <- data_HCbody[-c(post_cutoff_body),] # n=17, 6 subfield volumes with no posterior cutoffs
nrow(data_HCbody_cut) == 17

  # Define new numeric sex variable
  sex_to_numeric <- function(df) {
    sex <- as.character((strsplit(gsub("[{}]","",df$`sex`), split = ",")))
    sex[sex == "M"] <- 1
    sex[sex == "F"] <- 0
    df$sex <- as.numeric(sex)
  }
  
  data$sex = sex_to_numeric(data) #26
  data_cut$sex = sex_to_numeric(data_cut) #13
  data_HCbody$sex = sex_to_numeric(data_HCbody)
  data_total_vol$sex = sex_to_numeric(data_total_vol)
  data_regT2$sex = sex_to_numeric(data_regT2)
  data_HCbody_cut$sex = sex_to_numeric(data_HCbody_cut)
  data_survey_scores$sex = sex_to_numeric(data_survey_scores)

data_tdata <- data %>% 
  mutate(tLCA1 = LCA1/sLCA1/2, tLCA23 = LCA23/sLCA23/2, tLDG = LDG/sLDG/2, tLSUB = LSUB/sLSUB/2,
         tLERC = LERC/sLERC/2, tLPRC = LPRC/sLPRC/2, tLPHC = LPHC/sLPHC/2,
         tRCA1 = RCA1/sRCA1/2, tRCA23 = RCA23/sRCA23/2, tRDG = RDG/sRDG/2, tRSUB = RSUB/sRSUB/2,
         tRERC = RERC/sRERC/2, tRPRC = RPRC/sRPRC/2, tRPHC = RPHC/sRPHC/2) %>% # 2mm
  dplyr::select("sub_id","tLCA1","tLCA23","tLDG","tLSUB","tLERC","tLPRC","tLPHC","tRCA1","tRCA23","tRDG","tRSUB",
                "tRERC","tRPRC","tRPHC","sex","age","propcorrect","patheff","patheff_acc_only","tiv") 
  # n=26, 7 HC subfield thickness

data_tbody <- data_HCbody_cut %>% 
  mutate(tLCA1 = LCA1/sLCA1/2, tLCA23 = LCA23/sLCA23/2, tLDG = LDG/sLDG/2, tLSUB = LSUB/sLSUB/2, 
         tLPRC = LPRC/sLPRC/2, tLPHC = LPHC/sLPHC/2,
         tRCA1 = RCA1/sRCA1/2, tRCA23 = RCA23/sRCA23/2, tRDG = RDG/sRDG/2, tRSUB = RSUB/sRSUB/2,
         tRPRC = RPRC/sRPRC/2, tRPHC = RPHC/sRPHC/2) %>% # 2mm
  dplyr::select("sub_id","tLCA1","tLCA23","tLDG","tLSUB","tLPRC","tLPHC","tRCA1","tRCA23","tRDG","tRSUB",
                "tRPRC","tRPHC","sex","age","propcorrect","patheff","patheff_acc_only","tiv") # n=17, 6 body thick

data_tT2w <- data_regT2 %>%
  mutate(tLaHC = LaHC/sLaHC/0.94, tLpHC = LpHC/sLpHC/0.94, 
         tLERC = LERC/sLERC/0.94, tLPRC = LPRC/sLPRC/0.94, tLPHC = LPHC/sLPHC/0.94, 
         tRaHC = RaHC/sRaHC/0.94, tRpHC = RpHC/sRpHC/0.94, 
         tRERC = RERC/sRERC/0.94, tRPRC = RPRC/sRPRC/0.94, tRPHC = RPHC/sRPHC/0.94) %>% # 0.94mm
  dplyr::select("sub_id","tLaHC","tLpHC","tLERC","tLPRC","tLPHC","tRaHC","tRpHC","tRERC","tRPRC","tRPHC",
                "sex","age","propcorrect","patheff","patheff_acc_only","tiv") # n=26, all 5 thickness

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

#### ------------------------- Section 1A: Questionnaires + Proportion Correct (n~100) ------------------------- ####
# Correlation matrix
survey_corr <- cor(data_survey_scores[,-c(1,12)] %>% na.omit())
survey_rcorr <- rcorr(as.matrix(data_survey_scores[,-c(1,12)] %>% na.omit()))
survey_rcorr_coeff <- survey_rcorr$r
survey_rcorr_pval <- survey_rcorr$P

# corrplot.mixed(survey_corr, order = "AOE",
#                tl.col = "black", tl.srt = 45, tl.cex = 0.75,
#                cl.cex = 0.75, number.cex = 0.65)
# corrplot(survey_corr, method = "circle",
#          tl.col = "black", tl.srt = 45, tl.cex = 0.75,
#          cl.cex = 0.75,
#          addCoef.col = "black", number.cex = 0.65)
corrplot(survey_corr, method = "ellipse", type = "lower",
               tl.col = "black", tl.cex = 0.75,
               cl.cex = 0.75)

## Function that plots correlation between X questionnaire and proportion correct separated by sex
## Inputs: (1) data- df, (2) score- column with questionnaire outcome measure values, (3) propcorrect- column with 
##            propcorrect values, (4) title- graph title (within quotes), (5) xlab- x-axis title (within quotes)
## Output: a scatterplot with custom settings that correlates the survey outcome measure of interest with propcorrect
ggplot_survey_MF <- function(data, score_of_interest, propcorrect, title, xlab) {
  ggplot(data = data, aes(x = score_of_interest, y = propcorrect, # color by sex
                          color = factor(sex, 
                                         levels = c("F", "M"), 
                                         labels = c("Female", "Male")))) + 
    geom_point() + 
    geom_smooth(method = "lm", aes(group = sex, fill = sex)) + 
    scale_color_manual(values = c(palette[4], palette[5]), na.translate = F) +
    scale_fill_manual(values = c(palette[4], palette[5]),
                      name = "Biological Sex",
                      labels = c("Female", "Male")) +
    ggtitle(title) +
    xlab(xlab) + ylab("Proportion Correct") + ylim(0, 1) +
    labs(color = "Biological Sex") +
    theme(legend.position = "bottom",
          legend.direction = "horizontal",
          legend.box = "horizontal")
}

## Video Game (n=100)
data_VG <- data_survey_scores %>% 
  dplyr::select("sub_id", "VG_hr", "VG_yr", "VG_score", "propcorrect", "sex")

cor.test(data_VG$VG_hr, data_VG$propcorrect, na.action = na.omit)
cor.test(data_VG$VG_yr, data_VG$propcorrect, na.action = na.omit) 
cor.test(data_VG$VG_score, data_VG$propcorrect, na.action = na.omit) 

theme_set(theme1)
ggplot(data = data_VG, aes(x = data_VG$VG_hr, y = data_VG$propcorrect)) + 
  geom_point(color = palette[10]) + geom_smooth(method = "lm", color = palette[10]) +
  ggtitle("Maze Accuracy and Video Game Experience") +
  xlab("Video Game Experience (hr/wk score)") + ylab("Proportion Correct") + ylim(0, 1) # non-linear?
ggplot(data = data_VG, aes(x = data_VG$VG_yr, y = data_VG$propcorrect)) + 
  geom_point(color = palette[10]) + geom_smooth(method = "lm", color = palette[10]) +
  ggtitle("Maze Accuracy and Video Game Experience") +
  xlab("Video Game Experience (years)") + ylab("Proportion Correct") + ylim(0, 1) # non-linear?
ggplot(data = data_VG, aes(x = data_VG$VG_score, y = data_VG$propcorrect)) + 
  geom_point(color = palette[10]) + geom_smooth(method = "lm", color = palette[10]) +
  ggtitle("Maze Accuracy and Video Game Experience") +
  xlab("Overall Video Game Experience Score") + ylab("Proportion Correct") + ylim(0, 1)

ggplot_survey_MF(data_VG, data_VG$VG_score, data_VG$propcorrect, 
                 "Sex Differences in Maze Accuracy and Video Game Experience", "Overall Video Game Experience Score")

data_VG_rm <- data_VG
data_VG_rm <- data_VG_rm[-c(34),] # remove sub-034: age 25 and 25 yrs of playing video games

cor.test(data_VG_rm$VG_yr, data_VG_rm$propcorrect, na.action = na.omit) # r=.5314872, p=5.005e-08, n=99 -> no difference

## Road Map (n=103)
data_RM <- data_survey_scores %>% 
  dplyr::select("sub_id", "RM_score", "propcorrect", "sex")

cor.test(data_RM$RM_score, data_RM$propcorrect, na.action = na.omit) 

ggplot(data = data_RM, aes(x = data_RM$RM_score, y = data_RM$propcorrect)) + 
  geom_point(color = palette[10]) + geom_smooth(method = "lm", color = palette[10]) +
  ggtitle("Maze Accuracy and Road Map Task") +
  xlab("Total Correct Turns in Road Map Task") + ylab("Proportion Correct") + ylim(0, 1)

ggplot_survey_MF(data_RM, data_RM$RM_score, data_RM$propcorrect, 
                 "Sex Differences in Maze Accuracy and Road Map Task", "Total Correct Turns in Road Map Task")

## SBSOD (n=101)
data_SBSOD <- data_survey_scores %>% 
  dplyr::select("sub_id", "SBSOD_score", "propcorrect", "sex")

cor.test(data_SBSOD$SBSOD_score, data_SBSOD$propcorrect, na.action = na.omit) 

ggplot(data = data_SBSOD, aes(x = data_SBSOD$SBSOD_score, y = data_SBSOD$propcorrect)) + 
  geom_point(color = palette[10]) + geom_smooth(method = "lm", color = palette[10]) +
  ggtitle("Maze Accuracy and SB Sense of Direction Scale (SBSOD)") +
  xlab("SBSOD Score") + ylab("Proportion Correct") + ylim(0, 1)

ggplot_survey_MF(data_SBSOD, data_SBSOD$SBSOD_score, data_SBSOD$propcorrect, 
                 "Sex Differences in Maze Accuracy and SBSOD", "SBSOD Score")

## Handedness (n=100)
data_hand <- data_survey_scores %>% 
  dplyr::select("sub_id", "Hand_score", "propcorrect", "sex")

cor.test(data_hand$Hand_score, data_hand$propcorrect, na.action = na.omit) 

## Spatial Orientation (n=100)
data_SOT <- data_survey_scores %>% 
  dplyr::select("sub_id", "SOT_score", "propcorrect", "sex")

cor.test(data_SOT$SOT_score, data_SOT$propcorrect, na.action = na.omit) 

ggplot(data = data_SOT, aes(x = data_SOT$SOT_score, y = data_SOT$propcorrect)) +
  geom_point(color = palette[10]) + geom_smooth(method = "lm", color = palette[10]) +
  ggtitle("Maze Accuracy and Spatial Orientation Task (SOT)") +
  xlab("SOT Angular Error (degrees)") + ylab("Proportion Correct") + ylim(0, 1)

ggplot_survey_MF(data_SOT, data_SOT$SOT_score, data_SOT$propcorrect, 
                 "Sex Differences in Maze Accuracy and SOT", "SOT Angular Error (degrees)")

#### ------------------------- Section 1B: Questionnaires Summary Demographics (n~100) ------------------------- ####
## Function that plots correlation between questionnaires and proportion correct separated by sex
## Inputs: (1) data- df, (2) sex, (3) score- column with questionnaire outcome measure values, (4) title- graph 
##            title (within quotes), (5) ylab- y-axis title (within quotes)
## Output: a violin plot + barplot with custom settings showing the distribution of the survey outcome measure of 
##            interest separated by sex
ggplot_survey_MF_bar <- function(data, sex, score_of_interest, title, ylab) {
  data %>%
    ggplot(aes(x = sex, y = score_of_interest, col = sex, fill = sex)) + # color by sex
    geom_violin(show.legend = FALSE) + 
    geom_boxplot(show.legend = FALSE, width = .2) +
    geom_jitter(show.legend = FALSE, width = .1) +
    scale_x_discrete(na.translate = FALSE) +
    scale_color_manual(values = c(palette[7], palette[8]), na.translate = F) +
    scale_fill_manual(values = c(palette[1], palette[2])) +
    ggtitle(title) +
    xlab("Biological Sex") + ylab(ylab) +
    labs(color = "Biological Sex") +
    theme(legend.position = "bottom",
          legend.direction = "horizontal",
          legend.box = "horizontal")
}

## Video Game
summary(data_VG$VG_hr)
ggplot(data_VG, aes(x = VG_score)) + geom_histogram(binwidth = 2, color = "black", fill = "darkgray") +
  ggtitle("Overall Video Game Experience") + 
  xlab("Overall Video Game Experience Score") + ylab("Count")
ggplot(data_VG, aes(x = VG_score, color = sex, fill = sex)) + 
  geom_histogram(binwidth = 2, alpha = 0.5, position = "dodge") +
  ggtitle("Overall Video Game Experience Separated by Sex") + 
  xlab("Overall Video Game Experience Score") + ylab("Count")
  # Why is there an NA for sex????
ggplot_survey_MF_bar(data_VG, sex, data_VG$VG_hr, 
                     "Sex Differences in Hours/Week Playing Video Games", "Video Game Experience (hr/wk score)")
ggplot_survey_MF_bar(data_VG, sex, data_VG$VG_yr, 
                     "Sex Differences in Years Playing Video Games", "Video Game Experience (years)")
ggplot_survey_MF_bar(data_VG, sex, data_VG$VG_score, 
                     "Sex Differences in Video Game Experience", "Overall Video Game Experience Score")

## Road Map
summary(data_RM)
ggplot(data_RM, aes(x = RM_score)) + geom_histogram(binwidth = 3, color = "black", fill = "darkgray") +
  ggtitle("Road Map Task Performance") + 
  xlab("Total Correct Turns") + ylab("Count")
ggplot(data_RM, aes(x = RM_score, color = sex, fill = sex)) + 
  geom_histogram(binwidth = 3, alpha = 0.5, position = "dodge") +
  ggtitle("Road Map Task Performance Separated by Sex") + 
  xlab("Total Correct Turns") + ylab("Count")
ggplot_survey_MF_bar(data_RM, sex, data_RM$RM_score, 
                     "Sex Differences in Road Map Task Performance", "Total Correct Turns in Road Map Task")

## SBSOD
summary(data_SBSOD)
ggplot(data_SBSOD, aes(x = SBSOD_score)) + geom_histogram(binwidth = 1, color = "black", fill = "darkgray") +
  ggtitle("Santa Barbara Sense of Direction (SBSOD) Scale") + 
  xlab("SBSOD Score") + ylab("Count")
ggplot(data_SBSOD, aes(x = SBSOD_score, color = sex, fill = sex)) + 
  geom_histogram(binwidth = 1, alpha = 0.5, position = "dodge") +
  ggtitle("SB Sense of Direction (SBSOD) Scale Separated by Sex") + 
  xlab("SBSOD Score") + ylab("Count")
ggplot_survey_MF_bar(data_SBSOD, sex, data_SBSOD$SBSOD_score, 
                     "Sex Differences in Santa Barbara Sense of Direction Scale (SBSOD)", "SBSOD Score")

## Hand
summary(data_hand)
ggplot(data_hand, aes(x = Hand_score)) + geom_histogram(binwidth = 1, color = "black", fill = "darkgray") +
  ggtitle("Handedness") + xlim(0.5,5.5) + 
  xlab("Handedness Score") + ylab("Count")
ggplot(data_hand, aes(x = Hand_score, color = sex, fill = sex)) + 
  geom_histogram(binwidth = 1, alpha = 0.5, position = "dodge") +
  ggtitle("Handedness Separated by Sex") + xlim(0.5,5.5) +
  xlab("Handedness Score") + ylab("Count")
ggplot_survey_MF_bar(data_hand, sex, data_hand$Hand_score, 
                     "Sex Differences in Handedness", "Handedness Score")

## SOT
summary(data_SOT)
ggplot(data_SOT, aes(x = SOT_score)) + geom_histogram(binwidth = 5, color = "black", fill = "darkgray") +
  ggtitle("Spatial Orientation Task (SOT) Performance") + 
  xlab("SOT Angular Error (degrees)") + ylab("Count")
ggplot(data_SOT, aes(x = SOT_score, color = sex, fill = sex)) + 
  geom_histogram(binwidth = 5, alpha = 0.5, position = "dodge") +
  ggtitle("Spatial Orientation Task (SOT) Performance Separated by Sex") +
  xlab("SOT Angular Error (degrees)") + ylab("Count")
ggplot_survey_MF_bar(data_SOT, sex, data_SOT$SOT_score, 
                     "Sex Differences in Spatial Orientation Task (SOT) Performance", "SOT Angular Error (degrees)")

#### -------------------------- Section 3A: Total Volumes + Proportion Correct (n=96) -------------------------- ####
# Testing null hypotheses
# Data clean up
data_total_vol$age <- data_full_clean$Age
data_total_vol$tiv <- data_tiv$VaisTIV
data_total_vol <- data_total_vol[complete.cases(data_total_vol), ]

data_total_vol$aHC_vol <- data_total_vol$LaHC_vol + data_total_vol$RaHC_vol
data_total_vol$pHC_vol <- data_total_vol$LpHC_vol + data_total_vol$RpHC_vol
dim(data_total_vol)

# # Partial correlation between left, right, total HC volumes and propcorrect (n=98)
# pcor.test(data_total_vol$propcorrect, data_total_vol$LHC_vol, 
#           c(data_total_vol$sex, data_total_vol$age, data_total_vol$tiv), method = "pearson")
# pcor.test(data_total_vol$propcorrect, data_total_vol$RHC_vol, 
#           c(data_total_vol$sex, data_total_vol$age, data_total_vol$tiv), method = "pearson")
# pcor.test(data_total_vol$propcorrect, data_total_vol$totalHC_vol, 
#           c(data_total_vol$sex, data_total_vol$age, data_total_vol$tiv), method = "pearson")
# 
# ggplot(data = data_total_vol, aes(y = propcorrect, x = LHC_vol)) + 
#   geom_point(color = palette[6]) + geom_smooth(method = "lm", color = palette[6]) +
#   ggtitle("Wayfinding Success and Left HC Volume") + xlab("Left Hippocampal (HC) Volume (mm^3)") +
#   ylab("Proportion Correct") + ylim(0, 1)
# ggplot(data = data_total_vol, aes(y = propcorrect, x = RHC_vol)) + 
#   geom_point(color = palette[6]) + geom_smooth(method = "lm", color = palette[6]) +
#   ggtitle("Wayfinding Success and Right HC Volume") + xlab("Right Hippocampal (HC) Volume (mm^3)") +
#   ylab("Proportion Correct") + ylim(0, 1)
# ggplot(data = data_total_vol, aes(y = propcorrect, x = totalHC_vol)) + 
#   geom_point(color = palette[6]) + geom_smooth(method = "lm", color = palette[6]) +
#   ggtitle("Wayfinding Success and Total HC Volume") + xlab("Total Hippocampal (HC) Volume (mm^3)") +
#   ylab("Proportion Correct") + ylim(0, 1)

# Check for outliers
which(abs(scale(data_total_vol$LHC_vol)) > 3) # 82
which(abs(scale(data_total_vol$RHC_vol)) > 3) # 28
which(abs(scale(data_total_vol$totalHC_vol)) > 3)

# Remove outliers
data_total_vol_rm <- data_total_vol[-c(28, 82),]
dim(data_total_vol_rm)

# Outliers removed: Partial correlation between left, right, total HC volumes and propcorrect (n=96)
pcor.test(data_total_vol_rm$propcorrect, data_total_vol_rm$LHC_vol, 
          c(data_total_vol_rm$sex, data_total_vol_rm$age, data_total_vol_rm$tiv), method = "pearson")
pcor.test(data_total_vol_rm$propcorrect, data_total_vol_rm$RHC_vol, 
          c(data_total_vol_rm$sex, data_total_vol_rm$age, data_total_vol_rm$tiv), method = "pearson")
pcor.test(data_total_vol_rm$propcorrect, data_total_vol_rm$totalHC_vol, 
          c(data_total_vol_rm$sex, data_total_vol_rm$age, data_total_vol_rm$tiv), method = "pearson")

ggplot(data = data_total_vol_rm, aes(y = propcorrect, x = LHC_vol)) + 
  geom_point(color = palette[9]) + geom_smooth(method = "lm", color = palette[9]) +
  ggtitle("Wayfinding Success and Left HC Volume") + xlab("Left Hippocampal (HC) Volume (mm^3)") +
  ylab("Proportion Correct") + ylim(0, 1)
ggplot(data = data_total_vol_rm, aes(y = propcorrect, x = RHC_vol)) + 
  geom_point(color = palette[9]) + geom_smooth(method = "lm", color = palette[9]) +
  ggtitle("Wayfinding Success and Right HC Volume") + xlab("Right Hippocampal (HC) Volume (mm^3)") +
  ylab("Proportion Correct") + ylim(0, 1)
ggplot(data = data_total_vol_rm, aes(y = propcorrect, x = totalHC_vol)) + 
  geom_point(color = palette[9]) + geom_smooth(method = "lm", color = palette[9]) +
  ggtitle("Wayfinding Success and Total HC Volume") + xlab("Total Hippocampal (HC) Volume (mm^3)") +
  ylab("Proportion Correct") + ylim(0, 1)

#### ------------------------------- Section 3B: Total Volumes + Path Eff (n=96) ------------------------------- ####
# # Partial correlation between left, right, total HC volumes and patheff + patheff_acc_only (n=98)
# pcor.test(data_total_vol$patheff, data_total_vol$LHC_vol, 
#           c(data_total_vol$sex, data_total_vol$age, data_total_vol$tiv), method = "pearson")
# pcor.test(data_total_vol$patheff, data_total_vol$RHC_vol, 
#           c(data_total_vol$sex, data_total_vol$age, data_total_vol$tiv), method = "pearson")
# pcor.test(data_total_vol$patheff, data_total_vol$totalHC_vol, 
#           c(data_total_vol$sex, data_total_vol$age, data_total_vol$tiv), method = "pearson")
# 
# ggplot(data = data_total_vol, aes(y = patheff, x = LHC_vol)) + 
#   geom_point(color = palette[6]) + geom_smooth(method = "lm", color = palette[6]) +
#   ggtitle("Path Efficiency and Left HC Volume") + xlab("Left Hippocampal (HC) Volume (mm^3)") +
#   ylab("Path Inefficiency") + scale_y_continuous(breaks = seq(1, 10, by = 0.1))
# ggplot(data = data_total_vol, aes(y = patheff, x = RHC_vol)) + 
#   geom_point(color = palette[6]) + geom_smooth(method = "lm", color = palette[6]) +
#   ggtitle("Path Efficiency and Right HC Volume") + xlab("Right Hippocampal (HC) Volume (mm^3)") +
#   ylab("Path Inefficiency") + scale_y_continuous(breaks = seq(1, 10, by = 0.1))
# ggplot(data = data_total_vol, aes(y = patheff, x = totalHC_vol)) + 
#   geom_point(color = palette[6]) + geom_smooth(method = "lm", color = palette[6]) +
#   ggtitle("Path Efficiency and Total HC Volume") + xlab("Total Hippocampal (HC) Volume (mm^3)") +
#   ylab("Path Inefficiency") + scale_y_continuous(breaks = seq(1, 10, by = 0.1))
# 
# pcor.test(data_total_vol$patheff_acc_only, data_total_vol$LHC_vol, 
#           c(data_total_vol$sex, data_total_vol$age, data_total_vol$tiv), method = "pearson")
# pcor.test(data_total_vol$patheff_acc_only, data_total_vol$RHC_vol, 
#           c(data_total_vol$sex, data_total_vol$age, data_total_vol$tiv), method = "pearson")
# pcor.test(data_total_vol$patheff_acc_only, data_total_vol$totalHC_vol, 
#           c(data_total_vol$sex, data_total_vol$age, data_total_vol$tiv), method = "pearson")
# 
# ggplot(data = data_total_vol, aes(y = patheff_acc_only, x = LHC_vol)) + 
#   geom_point(color = palette[6]) + geom_smooth(method = "lm", color = palette[6]) +
#   ggtitle("Path Efficiency and Left HC Volume") + xlab("Left Hippocampal (HC) Volume (mm^3)") +
#   ylab("Path Inefficiency (Correct Trials Only)") + scale_y_continuous(breaks = seq(1, 10, by = 0.05))
# ggplot(data = data_total_vol, aes(y = patheff_acc_only, x = RHC_vol)) + 
#   geom_point(color = palette[6]) + geom_smooth(method = "lm", color = palette[6]) +
#   ggtitle("Path Efficiency and Right HC Volume") + xlab("Right Hippocampal (HC) Volume (mm^3)") +
#   ylab("Path Inefficiency (Correct Trials Only)") + scale_y_continuous(breaks = seq(1, 10, by = 0.05))
# ggplot(data = data_total_vol, aes(y = patheff_acc_only, x = totalHC_vol)) + 
#   geom_point(color = palette[6]) + geom_smooth(method = "lm", color = palette[6]) +
#   ggtitle("Path Efficiency and Total HC Volume") + xlab("Total Hippocampal (HC) Volume (mm^3)") +
#   ylab("Path Inefficiency (Correct Trials Only)") + scale_y_continuous(breaks = seq(1, 10, by = 0.05))

# Outliers removed: Partial correlation between left, right, total HC volumes and patheff + patheff_acc_only (n=96)
pcor.test(data_total_vol_rm$patheff, data_total_vol_rm$LHC_vol, 
          c(data_total_vol_rm$sex, data_total_vol_rm$age, data_total_vol_rm$tiv), method = "pearson")
pcor.test(data_total_vol_rm$patheff, data_total_vol_rm$RHC_vol, 
          c(data_total_vol_rm$sex, data_total_vol_rm$age, data_total_vol_rm$tiv), method = "pearson")
pcor.test(data_total_vol_rm$patheff, data_total_vol_rm$totalHC_vol, 
          c(data_total_vol_rm$sex, data_total_vol_rm$age, data_total_vol_rm$tiv), method = "pearson")

ggplot(data = data_total_vol_rm, aes(y = patheff, x = LHC_vol)) + 
  geom_point(color = palette[9]) + geom_smooth(method = "lm", color = palette[9]) +
  ggtitle("Path Efficiency and Left HC Volume") + xlab("Left Hippocampal (HC) Volume (mm^3)") +
  ylab("Path Inefficiency") + scale_y_continuous(breaks = seq(1, 10, by = 0.1))
ggplot(data = data_total_vol_rm, aes(y = patheff, x = RHC_vol)) + 
  geom_point(color = palette[9]) + geom_smooth(method = "lm", color = palette[9]) +
  ggtitle("Path Efficiency and Right HC Volume") + xlab("Right Hippocampal (HC) Volume (mm^3)") +
  ylab("Path Inefficiency") + scale_y_continuous(breaks = seq(1, 10, by = 0.1))
ggplot(data = data_total_vol_rm, aes(y = patheff, x = totalHC_vol)) + 
  geom_point(color = palette[9]) + geom_smooth(method = "lm", color = palette[9]) +
  ggtitle("Path Efficiency and Total HC Volume") + xlab("Total Hippocampal (HC) Volume (mm^3)") +
  ylab("Path Inefficiency") + scale_y_continuous(breaks = seq(1, 10, by = 0.1))

pcor.test(data_total_vol_rm$patheff_acc_only, data_total_vol_rm$LHC_vol, 
          c(data_total_vol_rm$sex, data_total_vol_rm$age, data_total_vol_rm$tiv), method = "pearson")
pcor.test(data_total_vol_rm$patheff_acc_only, data_total_vol_rm$RHC_vol, 
          c(data_total_vol_rm$sex, data_total_vol_rm$age, data_total_vol_rm$tiv), method = "pearson")
pcor.test(data_total_vol_rm$patheff_acc_only, data_total_vol_rm$totalHC_vol, 
          c(data_total_vol_rm$sex, data_total_vol_rm$age, data_total_vol_rm$tiv), method = "pearson")

ggplot(data = data_total_vol_rm, aes(y = patheff_acc_only, x = LHC_vol)) + 
  geom_point(color = palette[9]) + geom_smooth(method = "lm", color = palette[9]) +
  ggtitle("Path Efficiency and Left HC Volume") + xlab("Left Hippocampal (HC) Volume (mm^3)") +
  ylab("Path Inefficiency (Correct Trials Only)") + scale_y_continuous(breaks = seq(1, 10, by = 0.05))
ggplot(data = data_total_vol_rm, aes(y = patheff_acc_only, x = RHC_vol)) + 
  geom_point(color = palette[9]) + geom_smooth(method = "lm", color = palette[9]) +
  ggtitle("Path Efficiency and Right HC Volume") + xlab("Right Hippocampal (HC) Volume (mm^3)") +
  ylab("Path Inefficiency (Correct Trials Only)") + scale_y_continuous(breaks = seq(1, 10, by = 0.05))
ggplot(data = data_total_vol_rm, aes(y = patheff_acc_only, x = totalHC_vol)) + 
  geom_point(color = palette[9]) + geom_smooth(method = "lm", color = palette[9]) +
  ggtitle("Path Efficiency and Total HC Volume") + xlab("Total Hippocampal (HC) Volume (mm^3)") +
  ylab("Path Inefficiency (Correct Trials Only)") + scale_y_continuous(breaks = seq(1, 10, by = 0.05))

#### ------------------------ Section 3C: Total Thickness + Proportion Correct (n=95) -------------------------- ####
# Testing null hypotheses
# Data clean up
data_total_thick <- data_total_vol %>% 
  mutate(tLHC = LHC_vol/sLHC/0.94, tLaHC = LaHC_vol/sLaHC/0.94, tLpHC = LpHC_vol/sLpHC/0.94,
         tRHC = RHC_vol/sRHC/0.94, tRaHC = RaHC_vol/sRaHC/0.94, tRpHC = RpHC_vol/sRpHC/0.94) %>% # 0.94mm
  dplyr::select("sub_id","tLHC","tLaHC","tLpHC","tRHC","tRaHC","tRpHC",
                "propcorrect","patheff","patheff_acc_only","sex","age","tiv")
dim(data_total_thick)

# # Partial correlation between left, right HC thickness and propcorrect (n=98)
# pcor.test(data_total_thick$propcorrect, data_total_thick$tLHC, 
#           c(data_total_thick$sex, data_total_thick$age, data_total_thick$tiv), method = "pearson")
# pcor.test(data_total_thick$propcorrect, data_total_thick$tRHC, 
#           c(data_total_thick$sex, data_total_thick$age, data_total_thick$tiv), method = "pearson")
# 
# ggplot(data = data_total_thick, aes(y = propcorrect, x = tLHC)) + 
#   geom_point(color = palette[6]) + geom_smooth(method = "lm", color = palette[6]) +
#   ggtitle("Wayfinding Success and Left HC Thickness") + xlab("Left Hippocampal (HC) Thickness") +
#   ylab("Proportion Correct") + ylim(0, 1)
# ggplot(data = data_total_thick, aes(y = propcorrect, x = tRHC)) + 
#   geom_point(color = palette[6]) + geom_smooth(method = "lm", color = palette[6]) +
#   ggtitle("Wayfinding Success and Right HC Thickness") + xlab("Right Hippocampal (HC) Thickness") +
#   ylab("Proportion Correct") + ylim(0, 1)

# Check for outliers
which(abs(scale(data_total_thick$tLHC)) > 3) # 82
which(abs(scale(data_total_thick$tRHC)) > 3) # 20, 28

# Remove outliers
data_total_thick_rm <- data_total_thick[-c(20, 28, 82),]
dim(data_total_thick_rm)

# Outliers removed: Partial correlation between left, right HC thickness and propcorrect (n=95)
pcor.test(data_total_thick_rm$propcorrect, data_total_thick_rm$tLHC, 
          c(data_total_thick_rm$sex, data_total_thick_rm$age, data_total_thick_rm$tiv), method = "pearson")
pcor.test(data_total_thick_rm$propcorrect, data_total_thick_rm$tRHC, 
          c(data_total_thick_rm$sex, data_total_thick_rm$age, data_total_thick_rm$tiv), method = "pearson")

ggplot(data = data_total_thick_rm, aes(y = propcorrect, x = tLHC)) + 
  geom_point(color = palette[9]) + geom_smooth(method = "lm", color = palette[9]) +
  ggtitle("Wayfinding Success and Left HC Thickness") + xlab("Left Hippocampal (HC) Thickness") +
  ylab("Proportion Correct") + ylim(0, 1)
ggplot(data = data_total_thick_rm, aes(y = propcorrect, x = tRHC)) + 
  geom_point(color = palette[9]) + geom_smooth(method = "lm", color = palette[9]) +
  ggtitle("Wayfinding Success and Right HC Thickness") + xlab("Right Hippocampal (HC) Thickness") +
  ylab("Proportion Correct") + ylim(0, 1)

#### ----------------------------- Section 3D: Total Thickness + Path Eff (n=95) ------------------------------- ####
# # Partial correlation between left, right HC thicknesses and patheff + patheff_acc_only (n=98)
# pcor.test(data_total_thick$patheff, data_total_thick$tLHC, 
#           c(data_total_thick$sex, data_total_thick$age, data_total_vol$tiv), method = "pearson")
# pcor.test(data_total_thick$patheff, data_total_thick$tRHC, 
#           c(data_total_thick$sex, data_total_thick$age, data_total_vol$tiv), method = "pearson")
# 
# ggplot(data = data_total_thick, aes(y = patheff, x = tLHC)) + 
#   geom_point(color = palette[6]) + geom_smooth(method = "lm", color = palette[6]) +
#   ggtitle("Path Efficiency and Left HC Thickness") + xlab("Left Hippocampal (HC) Thickness") +
#   ylab("Path Inefficiency") + scale_y_continuous(breaks = seq(1, 10, by = 0.1))
# ggplot(data = data_total_thick, aes(y = patheff, x = tRHC)) + 
#   geom_point(color = palette[6]) + geom_smooth(method = "lm", color = palette[6]) +
#   ggtitle("Path Efficiency and Right HC Thickness") + xlab("Right Hippocampal (HC) Thickness") +
#   ylab("Path Inefficiency") + scale_y_continuous(breaks = seq(1, 10, by = 0.1))
# 
# pcor.test(data_total_thick$patheff_acc_only, data_total_thick$tLHC, 
#           c(data_total_thick$sex, data_total_thick$age, data_total_vol$tiv), method = "pearson")
# pcor.test(data_total_thick$patheff_acc_only, data_total_thick$tRHC, 
#           c(data_total_thick$sex, data_total_thick$age, data_total_vol$tiv), method = "pearson")
# 
# ggplot(data = data_total_thick, aes(y = patheff_acc_only, x = tLHC)) + 
#   geom_point(color = palette[6]) + geom_smooth(method = "lm", color = palette[6]) +
#   ggtitle("Path Efficiency and Left HC Thickness") + xlab("Left Hippocampal (HC) Thickness") +
#   ylab("Path Inefficiency (Correct Trials Only)") + scale_y_continuous(breaks = seq(1, 10, by = 0.05))
# ggplot(data = data_total_thick, aes(y = patheff_acc_only, x = tRHC)) + 
#   geom_point(color = palette[6]) + geom_smooth(method = "lm", color = palette[6]) +
#   ggtitle("Path Efficiency and Right HC Thickness") + xlab("Right Hippocampal (HC) Thickness") +
#   ylab("Path Inefficiency (Correct Trials Only)") + scale_y_continuous(breaks = seq(1, 10, by = 0.05))

# Outliers removed: Partial correlation between left, right HC thicknesses and patheff + patheff_acc_only (n=95)
pcor.test(data_total_thick_rm$patheff, data_total_thick_rm$tLHC, 
          c(data_total_thick_rm$sex, data_total_thick_rm$age, data_total_thick_rm$tiv), method = "pearson")
pcor.test(data_total_thick_rm$patheff, data_total_thick_rm$tRHC, 
          c(data_total_thick_rm$sex, data_total_thick_rm$age, data_total_thick_rm$tiv), method = "pearson")

ggplot(data = data_total_thick_rm, aes(y = patheff, x = tLHC)) + 
  geom_point(color = palette[9]) + geom_smooth(method = "lm", color = palette[9]) +
  ggtitle("Path Efficiency and Left HC Thickness") + xlab("Left Hippocampal (HC) Thickness") +
  ylab("Path Inefficiency") + scale_y_continuous(breaks = seq(1, 10, by = 0.1))
ggplot(data = data_total_thick_rm, aes(y = patheff, x = tRHC)) + 
  geom_point(color = palette[9]) + geom_smooth(method = "lm", color = palette[9]) +
  ggtitle("Path Efficiency and Right HC Thickness") + xlab("Right Hippocampal (HC) Thickness") +
  ylab("Path Inefficiency") + scale_y_continuous(breaks = seq(1, 10, by = 0.1))

pcor.test(data_total_thick_rm$patheff_acc_only, data_total_thick_rm$tLHC, 
          c(data_total_thick_rm$sex, data_total_thick_rm$age, data_total_thick_rm$tiv), method = "pearson")
pcor.test(data_total_thick_rm$patheff_acc_only, data_total_thick_rm$tRHC, 
          c(data_total_thick_rm$sex, data_total_thick_rm$age, data_total_thick_rm$tiv), method = "pearson")

ggplot(data = data_total_thick_rm, aes(y = patheff_acc_only, x = tLHC)) + 
  geom_point(color = palette[9]) + geom_smooth(method = "lm", color = palette[9]) +
  ggtitle("Path Efficiency and Left HC Thickness") + xlab("Left Hippocampal (HC) Thickness") +
  ylab("Path Inefficiency (Correct Trials Only)") + scale_y_continuous(breaks = seq(1, 10, by = 0.05))
ggplot(data = data_total_thick_rm, aes(y = patheff_acc_only, x = tRHC)) + 
  geom_point(color = palette[9]) + geom_smooth(method = "lm", color = palette[9]) +
  ggtitle("Path Efficiency and Right HC Thickness") + xlab("Right Hippocampal (HC) Thickness") +
  ylab("Path Inefficiency (Correct Trials Only)") + scale_y_continuous(breaks = seq(1, 10, by = 0.05))

#### ------------------------- Section 4A: Ant/Post Volumes + Proportion Correct (n~96) ------------------------ ####
# # Partial correlation between left ant/post and right ant/post HC volumes and propcorrect (n=98)
# pcor.test(data_total_vol$propcorrect, data_total_vol$LaHC_vol, 
#           c(data_total_vol$sex, data_total_vol$age, data_total_vol$tiv), method = "pearson")
# pcor.test(data_total_vol$propcorrect, data_total_vol$LpHC_vol, 
#           c(data_total_vol$sex, data_total_vol$age, data_total_vol$tiv), method = "pearson")
# pcor.test(data_total_vol$propcorrect, data_total_vol$RaHC_vol, 
#           c(data_total_vol$sex, data_total_vol$age, data_total_vol$tiv), method = "pearson")
# pcor.test(data_total_vol$propcorrect, data_total_vol$RpHC_vol, 
#           c(data_total_vol$sex, data_total_vol$age, data_total_vol$tiv), method = "pearson")
# 
# ggplot(data = data_total_vol, aes(y = propcorrect, x = LaHC_vol)) + 
#   geom_point(color = palette[6]) + geom_smooth(method = "lm", color = palette[6]) +
#   ggtitle("Wayfinding Success and Left Ant. HC Volume") + xlab("Left Anterior HC Volume (mm^3)") +
#   ylab("Proportion Correct") + ylim(0, 1)
# ggplot(data = data_total_vol, aes(y = propcorrect, x = LpHC_vol)) + 
#   geom_point(color = palette[6]) + geom_smooth(method = "lm", color = palette[6]) +
#   ggtitle("Wayfinding Success and Left Post. HC Volume") + xlab("Left Posterior HC Volume (mm^3)") +
#   ylab("Proportion Correct") + ylim(0, 1)
# ggplot(data = data_total_vol, aes(y = propcorrect, x = RaHC_vol)) + 
#   geom_point(color = palette[6]) + geom_smooth(method = "lm", color = palette[6]) +
#   ggtitle("Wayfinding Success and Right Ant. HC Volume") + xlab("Right Anterior HC Volume (mm^3)") +
#   ylab("Proportion Correct") + ylim(0, 1)
# ggplot(data = data_total_vol, aes(y = propcorrect, x = RpHC_vol)) + 
#   geom_point(color = palette[6]) + geom_smooth(method = "lm", color = palette[6]) +
#   ggtitle("Wayfinding Success and Right Post. HC Volume") + xlab("Right Posterior HC Volume (mm^3)") +
#   ylab("Proportion Correct") + ylim(0, 1)

# Check for outliers
which(abs(scale(data_total_vol$LaHC_vol)) > 3)
which(abs(scale(data_total_vol$LpHC_vol)) > 3)
which(abs(scale(data_total_vol$RaHC_vol)) > 3)
which(abs(scale(data_total_vol$RpHC_vol)) > 3) # 28

# Remove outliers
data_total_vol_rm <- data_total_vol[-c(28),]
dim(data_total_vol_rm)

# Outliers removed: Partial correlation between left ant/post and right ant/post HC volumes and propcorrect (n=97)
pcor.test(data_total_vol_rm$propcorrect, data_total_vol_rm$LaHC_vol, 
          c(data_total_vol_rm$sex, data_total_vol_rm$age, data_total_vol_rm$tiv), method = "pearson")
pcor.test(data_total_vol_rm$propcorrect, data_total_vol_rm$LpHC_vol, 
          c(data_total_vol_rm$sex, data_total_vol_rm$age, data_total_vol_rm$tiv), method = "pearson")
pcor.test(data_total_vol_rm$propcorrect, data_total_vol_rm$RaHC_vol, 
          c(data_total_vol_rm$sex, data_total_vol_rm$age, data_total_vol_rm$tiv), method = "pearson")
pcor.test(data_total_vol_rm$propcorrect, data_total_vol_rm$RpHC_vol, 
          c(data_total_vol_rm$sex, data_total_vol_rm$age, data_total_vol_rm$tiv), method = "pearson")

ggplot(data = data_total_vol_rm, aes(y = propcorrect, x = LaHC_vol)) + 
  geom_point(color = palette[9]) + geom_smooth(method = "lm", color = palette[9]) +
  ggtitle("Wayfinding Success and Left Ant. HC Volume") + xlab("Left Anterior HC Volume (mm^3)") +
  ylab("Proportion Correct") + ylim(0, 1)
ggplot(data = data_total_vol_rm, aes(y = propcorrect, x = LpHC_vol)) + 
  geom_point(color = palette[9]) + geom_smooth(method = "lm", color = palette[9]) +
  ggtitle("Wayfinding Success and Left Post. HC Volume") + xlab("Left Posterior HC Volume (mm^3)") +
  ylab("Proportion Correct") + ylim(0, 1)
ggplot(data = data_total_vol_rm, aes(y = propcorrect, x = RaHC_vol)) + 
  geom_point(color = palette[9]) + geom_smooth(method = "lm", color = palette[9]) +
  ggtitle("Wayfinding Success and Right Ant. HC Volume") + xlab("Right Anterior HC Volume (mm^3)") +
  ylab("Proportion Correct") + ylim(0, 1)
ggplot(data = data_total_vol_rm, aes(y = propcorrect, x = RpHC_vol)) + 
  geom_point(color = palette[9]) + geom_smooth(method = "lm", color = palette[9]) +
  ggtitle("Wayfinding Success and Right Post. HC Volume") + xlab("Right Posterior HC Volume (mm^3)") +
  ylab("Proportion Correct") + ylim(0, 1)

# Partial correlation between total ant/post and ant/post HC volumes and propcorrect (n=98)
pcor.test(data_total_vol$propcorrect, data_total_vol$aHC_vol,
          c(data_total_vol$sex, data_total_vol$age, data_total_vol$tiv), method = "pearson")
pcor.test(data_total_vol$propcorrect, data_total_vol$pHC_vol,
          c(data_total_vol$sex, data_total_vol$age, data_total_vol$tiv), method = "pearson")

ggplot(data = data_total_vol, aes(y = propcorrect, x = aHC_vol)) +
  geom_point(color = palette[9]) + geom_smooth(method = "lm", color = palette[6]) +
  ggtitle("Wayfinding Success and Anterior HC Volume") + xlab("Anterior HC Volume (mm^3)") +
  ylab("Proportion Correct") + ylim(0, 1)
ggplot(data = data_total_vol, aes(y = propcorrect, x = pHC_vol)) +
  geom_point(color = palette[9]) + geom_smooth(method = "lm", color = palette[6]) +
  ggtitle("Wayfinding Success and Posterior HC Volume") + xlab("Posterior HC Volume (mm^3)") +
  ylab("Proportion Correct") + ylim(0, 1)

# Check for outliers - NONE
which(abs(scale(data_total_vol$aHC_vol)) > 3)
which(abs(scale(data_total_vol$pHC_vol)) > 3)

# Data clean up
data_total_vol_ratio <- data_total_vol %>%
  mutate(aHC_vol = LaHC_vol + RaHC_vol, pHC_vol = LpHC_vol + RpHC_vol,
         Lpa_ratio = LpHC_vol/LaHC_vol, Rpa_ratio = RpHC_vol/RaHC_vol,
         meanpa_ratio = (Lpa_ratio + Rpa_ratio)/2) # pa_ratio = postHC/antHC vol ratio

# # Partial correlation between left and right post/ant HC volume ratio and propcorrect (n=98)
# pcor.test(data_total_vol_ratio$propcorrect, data_total_vol_ratio$Lpa_ratio, 
#           c(data_total_vol_ratio$sex, data_total_vol_ratio$age, data_total_vol_ratio$tiv), method = "pearson")
# pcor.test(data_total_vol_ratio$propcorrect, data_total_vol_ratio$Rpa_ratio, 
#           c(data_total_vol_ratio$sex, data_total_vol_ratio$age, data_total_vol_ratio$tiv), method = "pearson")
# pcor.test(data_total_vol_ratio$propcorrect, data_total_vol_ratio$meanpa_ratio, 
#           c(data_total_vol_ratio$sex, data_total_vol_ratio$age, data_total_vol_ratio$tiv), method = "pearson")
# 
# ggplot(data = data_total_vol_ratio, aes(y = propcorrect, x = Lpa_ratio)) + 
#   geom_point(color = palette[6]) + geom_smooth(method = "lm", color = palette[6]) +
#   ggtitle("Wayfinding Success and Left PostHC/AntHC Ratio") + xlab("Left PostHC/AntHC Ratio") +
#   ylab("Proportion Correct") + ylim(0, 1)
# ggplot(data = data_total_vol_ratio, aes(y = propcorrect, x = Rpa_ratio)) + 
#   geom_point(color = palette[6]) + geom_smooth(method = "lm", color = palette[6]) +
#   ggtitle("Wayfinding Success and Right PostHC/AntHC Ratio") + xlab("Right PostHC/AntHC Ratio") +
#   ylab("Proportion Correct") + ylim(0, 1)
# ggplot(data = data_total_vol_ratio, aes(y = propcorrect, x = meanpa_ratio)) + 
#   geom_point(color = palette[6]) + geom_smooth(method = "lm", color = palette[6]) +
#   ggtitle("Wayfinding Success and Mean PostHC/AntHC Ratio") + xlab("Mean PostHC/AntHC Ratio") +
#   ylab("Proportion Correct") + ylim(0, 1)

# Check for outliers
which(abs(scale(data_total_vol_ratio$Lpa_ratio)) > 3) # 51
which(abs(scale(data_total_vol_ratio$Rpa_ratio)) > 3) # 75
which(abs(scale(data_total_vol_ratio$meanpa_ratio)) > 3) # 51, 75

# Remove outliers
data_total_vol_ratio_rm <- data_total_vol_ratio[-c(51, 75),]

# Outliers removed: Partial correlation between left and right post/ant HC volume ratio and propcorrect (n=96)
pcor.test(data_total_vol_ratio_rm$propcorrect, data_total_vol_ratio_rm$Lpa_ratio, 
          c(data_total_vol_ratio_rm$sex, data_total_vol_ratio_rm$age, data_total_vol_ratio_rm$tiv), method = "pearson")
pcor.test(data_total_vol_ratio_rm$propcorrect, data_total_vol_ratio_rm$Rpa_ratio, 
          c(data_total_vol_ratio_rm$sex, data_total_vol_ratio_rm$age, data_total_vol_ratio_rm$tiv), method = "pearson")
pcor.test(data_total_vol_ratio_rm$propcorrect, data_total_vol_ratio_rm$meanpa_ratio, 
          c(data_total_vol_ratio_rm$sex, data_total_vol_ratio_rm$age, data_total_vol_ratio_rm$tiv), method = "pearson")

ggplot(data = data_total_vol_ratio_rm, aes(y = propcorrect, x = Lpa_ratio)) + 
  geom_point(color = palette[9]) + geom_smooth(method = "lm", color = palette[9]) +
  ggtitle("Wayfinding Success and Left PostHC/AntHC Ratio") + xlab("Left PostHC/AntHC Ratio") +
  ylab("Proportion Correct") + ylim(0, 1)
ggplot(data = data_total_vol_ratio_rm, aes(y = propcorrect, x = Rpa_ratio)) + 
  geom_point(color = palette[9]) + geom_smooth(method = "lm", color = palette[9]) +
  ggtitle("Wayfinding Success and Right PostHC/AntHC Ratio") + xlab("Right PostHC/AntHC Ratio") +
  ylab("Proportion Correct") + ylim(0, 1)
ggplot(data = data_total_vol_ratio_rm, aes(y = propcorrect, x = meanpa_ratio)) + 
  geom_point(color = palette[9]) + geom_smooth(method = "lm", color = palette[9]) +
  ggtitle("Wayfinding Success and Mean PostHC/AntHC Ratio") + xlab("Mean PostHC/AntHC Ratio") +
  ylab("Proportion Correct") + ylim(0, 1)

#### ---------------------------- Section 4B: Ant/Post Volumes + Path Eff (n~96) ------------------------------- ####
# # Partial correlation between left ant/post and right ant/post HC volumes and patheff + patheff_acc_only (n=98)
# pcor.test(data_total_vol$patheff, data_total_vol$LaHC_vol, 
#           c(data_total_vol$sex, data_total_vol$age, data_total_vol$tiv), method = "pearson")
# pcor.test(data_total_vol$patheff, data_total_vol$LpHC_vol, 
#           c(data_total_vol$sex, data_total_vol$age, data_total_vol$tiv), method = "pearson")
# pcor.test(data_total_vol$patheff, data_total_vol$RaHC_vol, 
#           c(data_total_vol$sex, data_total_vol$age, data_total_vol$tiv), method = "pearson")
# pcor.test(data_total_vol$patheff, data_total_vol$RpHC_vol, 
#           c(data_total_vol$sex, data_total_vol$age, data_total_vol$tiv), method = "pearson")
# 
# ggplot(data = data_total_vol, aes(y = patheff, x = LaHC_vol)) + 
#   geom_point(color = palette[6]) + geom_smooth(method = "lm", color = palette[6]) +
#   ggtitle("Path Efficiency and Left Ant. HC Volume") + xlab("Left Anterior HC Volume (mm^3)") +
#   ylab("Path Inefficiency") + scale_y_continuous(breaks = seq(1, 10, by = 0.1))
# ggplot(data = data_total_vol, aes(y = patheff, x = LpHC_vol)) + 
#   geom_point(color = palette[6]) + geom_smooth(method = "lm", color = palette[6]) +
#   ggtitle("Path Efficiency and Left Post. HC Volume") + xlab("Left Posterior HC Volume (mm^3)") +
#   ylab("Path Inefficiency") + scale_y_continuous(breaks = seq(1, 10, by = 0.1))
# ggplot(data = data_total_vol, aes(y = patheff, x = RaHC_vol)) + 
#   geom_point(color = palette[6]) + geom_smooth(method = "lm", color = palette[6]) +
#   ggtitle("Path Efficiency and Right Ant. HC Volume") + xlab("Right Anterior HC Volume (mm^3)") +
#   ylab("Path Inefficiency") + scale_y_continuous(breaks = seq(1, 10, by = 0.1))
# ggplot(data = data_total_vol, aes(y = patheff, x = RpHC_vol)) + 
#   geom_point(color = palette[6]) + geom_smooth(method = "lm", color = palette[6]) +
#   ggtitle("Path Efficiency and Right Post. HC Volume") + xlab("Right Posterior HC Volume (mm^3)") +
#   ylab("Path Inefficiency") + scale_y_continuous(breaks = seq(1, 10, by = 0.1))
# 
# pcor.test(data_total_vol$patheff_acc_only, data_total_vol$LaHC_vol,
#           c(data_total_vol$sex, data_total_vol$age, data_total_vol$tiv), method = "pearson")
# pcor.test(data_total_vol$patheff_acc_only, data_total_vol$LpHC_vol,
#           c(data_total_vol$sex, data_total_vol$age, data_total_vol$tiv), method = "pearson")
# pcor.test(data_total_vol$patheff_acc_only, data_total_vol$RaHC_vol,
#           c(data_total_vol$sex, data_total_vol$age, data_total_vol$tiv), method = "pearson")
# pcor.test(data_total_vol$patheff_acc_only, data_total_vol$RpHC_vol,
#           c(data_total_vol$sex, data_total_vol$age, data_total_vol$tiv), method = "pearson")
# 
# ggplot(data = data_total_vol, aes(y = patheff_acc_only, x = LaHC_vol)) +
#   geom_point(color = palette[6]) + geom_smooth(method = "lm", color = palette[6]) +
#   ggtitle("Path Efficiency and Left Ant. HC Volume") + xlab("Left Anterior HC Volume (mm^3)") +
#   ylab("Path Inefficiency (Correct Trials Only)") + scale_y_continuous(breaks = seq(1, 10, by = 0.05))
# ggplot(data = data_total_vol, aes(y = patheff_acc_only, x = LpHC_vol)) +
#   geom_point(color = palette[6]) + geom_smooth(method = "lm", color = palette[6]) +
#   ggtitle("Path Efficiency and Left Post. HC Volume") + xlab("Left Posterior HC Volume (mm^3)") +
#   ylab("Path Inefficiency (Correct Trials Only)") + scale_y_continuous(breaks = seq(1, 10, by = 0.05))
# ggplot(data = data_total_vol, aes(y = patheff_acc_only, x = RaHC_vol)) +
#   geom_point(color = palette[6]) + geom_smooth(method = "lm", color = palette[6]) +
#   ggtitle("Path Efficiency and Right Ant. HC Volume") + xlab("Right Anterior HC Volume (mm^3)") +
#   ylab("Path Inefficiency (Correct Trials Only)") + scale_y_continuous(breaks = seq(1, 10, by = 0.05))
# ggplot(data = data_total_vol, aes(y = patheff_acc_only, x = RpHC_vol)) +
#   geom_point(color = palette[6]) + geom_smooth(method = "lm", color = palette[6]) +
#   ggtitle("Path Efficiency and Right Post. HC Volume") + xlab("Right Posterior HC Volume (mm^3)") +
#   ylab("Path Inefficiency (Correct Trials Only)") + scale_y_continuous(breaks = seq(1, 10, by = 0.05))

# Outliers removed: Partial correlation between left ant/post and right ant/post HC volumes and patheff + patheff_acc_only (n=97)
pcor.test(data_total_vol_rm$patheff, data_total_vol_rm$LaHC_vol, 
          c(data_total_vol_rm$sex, data_total_vol_rm$age, data_total_vol_rm$tiv), method = "pearson")
pcor.test(data_total_vol_rm$patheff, data_total_vol_rm$LpHC_vol, 
          c(data_total_vol_rm$sex, data_total_vol_rm$age, data_total_vol_rm$tiv), method = "pearson")
pcor.test(data_total_vol_rm$patheff, data_total_vol_rm$RaHC_vol, 
          c(data_total_vol_rm$sex, data_total_vol_rm$age, data_total_vol_rm$tiv), method = "pearson")
pcor.test(data_total_vol_rm$patheff, data_total_vol_rm$RpHC_vol, 
          c(data_total_vol_rm$sex, data_total_vol_rm$age, data_total_vol_rm$tiv), method = "pearson")

ggplot(data = data_total_vol_rm, aes(y = patheff, x = LaHC_vol)) + 
  geom_point(color = palette[9]) + geom_smooth(method = "lm", color = palette[6]) +
  ggtitle("Path Efficiency and Left Ant. HC Volume") + xlab("Left Anterior HC Volume (mm^3)") +
  ylab("Path Inefficiency") + scale_y_continuous(breaks = seq(1, 10, by = 0.1))
ggplot(data = data_total_vol_rm, aes(y = patheff, x = LpHC_vol)) + 
  geom_point(color = palette[9]) + geom_smooth(method = "lm", color = palette[6]) +
  ggtitle("Path Efficiency and Left Post. HC Volume") + xlab("Left Posterior HC Volume (mm^3)") +
  ylab("Path Inefficiency") + scale_y_continuous(breaks = seq(1, 10, by = 0.1))
ggplot(data = data_total_vol_rm, aes(y = patheff, x = RaHC_vol)) + 
  geom_point(color = palette[9]) + geom_smooth(method = "lm", color = palette[6]) +
  ggtitle("Path Efficiency and Right Ant. HC Volume") + xlab("Right Anterior HC Volume (mm^3)") +
  ylab("Path Inefficiency") + scale_y_continuous(breaks = seq(1, 10, by = 0.1))
ggplot(data = data_total_vol_rm, aes(y = patheff, x = RpHC_vol)) + 
  geom_point(color = palette[9]) + geom_smooth(method = "lm", color = palette[6]) +
  ggtitle("Path Efficiency and Right Post. HC Volume") + xlab("Right Posterior HC Volume (mm^3)") +
  ylab("Path Inefficiency") + scale_y_continuous(breaks = seq(1, 10, by = 0.1))

pcor.test(data_total_vol_rm$patheff_acc_only, data_total_vol_rm$LaHC_vol, 
          c(data_total_vol_rm$sex, data_total_vol_rm$age, data_total_vol_rm$tiv), method = "pearson")
pcor.test(data_total_vol_rm$patheff_acc_only, data_total_vol_rm$LpHC_vol, 
          c(data_total_vol_rm$sex, data_total_vol_rm$age, data_total_vol_rm$tiv), method = "pearson")
pcor.test(data_total_vol_rm$patheff_acc_only, data_total_vol_rm$RaHC_vol, 
          c(data_total_vol_rm$sex, data_total_vol_rm$age, data_total_vol_rm$tiv), method = "pearson")
pcor.test(data_total_vol_rm$patheff_acc_only, data_total_vol_rm$RpHC_vol, 
          c(data_total_vol_rm$sex, data_total_vol_rm$age, data_total_vol_rm$tiv), method = "pearson")

ggplot(data = data_total_vol_rm, aes(y = patheff_acc_only, x = LaHC_vol)) + 
  geom_point(color = palette[9]) + geom_smooth(method = "lm", color = palette[6]) +
  ggtitle("Path Efficiency and Left Ant. HC Volume") + xlab("Left Anterior HC Volume (mm^3)") +
  ylab("Path Inefficiency (Correct Trials Only)") + scale_y_continuous(breaks = seq(1, 10, by = 0.05))
ggplot(data = data_total_vol_rm, aes(y = patheff_acc_only, x = LpHC_vol)) + 
  geom_point(color = palette[9]) + geom_smooth(method = "lm", color = palette[6]) +
  ggtitle("Path Efficiency and Left Post. HC Volume") + xlab("Left Posterior HC Volume (mm^3)") +
  ylab("Path Inefficiency (Correct Trials Only)") + scale_y_continuous(breaks = seq(1, 10, by = 0.05))
ggplot(data = data_total_vol_rm, aes(y = patheff_acc_only, x = RaHC_vol)) + 
  geom_point(color = palette[9]) + geom_smooth(method = "lm", color = palette[6]) +
  ggtitle("Path Efficiency and Right Ant. HC Volume") + xlab("Right Anterior HC Volume (mm^3)") +
  ylab("Path Inefficiency (Correct Trials Only)") + scale_y_continuous(breaks = seq(1, 10, by = 0.05))
ggplot(data = data_total_vol_rm, aes(y = patheff_acc_only, x = RpHC_vol)) + 
  geom_point(color = palette[9]) + geom_smooth(method = "lm", color = palette[6]) +
  ggtitle("Path Efficiency and Right Post. HC Volume") + xlab("Right Posterior HC Volume (mm^3)") +
  ylab("Path Inefficiency (Correct Trials Only)") + scale_y_continuous(breaks = seq(1, 10, by = 0.05))

# Partial correlation between total ant/post and ant/post HC volumes and patheff + patheff_acc_only (n=98)
pcor.test(data_total_vol$patheff, data_total_vol$aHC_vol, 
          c(data_total_vol$sex, data_total_vol$age, data_total_vol$tiv), method = "pearson")
pcor.test(data_total_vol$patheff, data_total_vol$pHC_vol, 
          c(data_total_vol$sex, data_total_vol$age, data_total_vol$tiv), method = "pearson")

ggplot(data = data_total_vol, aes(y = patheff, x = aHC_vol)) + 
  geom_point(color = palette[9]) + geom_smooth(method = "lm", color = palette[6]) +
  ggtitle("Path Efficiency and Anterior HC Volume") + xlab("Anterior HC Volume (mm^3)") +
  ylab("Path Inefficiency") + scale_y_continuous(breaks = seq(1, 10, by = 0.1))
ggplot(data = data_total_vol, aes(y = patheff, x = pHC_vol)) + 
  geom_point(color = palette[9]) + geom_smooth(method = "lm", color = palette[6]) +
  ggtitle("Path Efficiency and Posterior HC Volume") + xlab("Posterior HC Volume (mm^3)") +
  ylab("Path Inefficiency") + scale_y_continuous(breaks = seq(1, 10, by = 0.1))

pcor.test(data_total_vol$patheff_acc_only, data_total_vol$aHC_vol, 
          c(data_total_vol$sex, data_total_vol$age, data_total_vol$tiv), method = "pearson")
pcor.test(data_total_vol$patheff_acc_only, data_total_vol$pHC_vol, 
          c(data_total_vol$sex, data_total_vol$age, data_total_vol$tiv), method = "pearson")

ggplot(data = data_total_vol, aes(y = patheff_acc_only, x = aHC_vol)) + 
  geom_point(color = palette[9]) + geom_smooth(method = "lm", color = palette[6]) +
  ggtitle("Path Efficiency and Anterior HC Volume") + xlab("Anterior HC Volume (mm^3)") +
  ylab("Path Inefficiency (Correct Trials Only)") + scale_y_continuous(breaks = seq(1, 10, by = 0.05))
ggplot(data = data_total_vol, aes(y = patheff_acc_only, x = pHC_vol)) + 
  geom_point(color = palette[9]) + geom_smooth(method = "lm", color = palette[6]) +
  ggtitle("Path Efficiency and Posterior HC Volume") + xlab("Posterior HC Volume (mm^3)") +
  ylab("Path Inefficiency (Correct Trials Only)") + scale_y_continuous(breaks = seq(1, 10, by = 0.05))

# # Partial correlation between left and right post/ant HC volume ratio and patheff + patheff_acc_only (n=98)
# pcor.test(data_total_vol_ratio$patheff, data_total_vol_ratio$Lpa_ratio, 
#           c(data_total_vol_ratio$sex, data_total_vol_ratio$age, data_total_vol_ratio$tiv), method = "pearson")
# pcor.test(data_total_vol_ratio$patheff, data_total_vol_ratio$Rpa_ratio, 
#           c(data_total_vol_ratio$sex, data_total_vol_ratio$age, data_total_vol_ratio$tiv), method = "pearson")
# pcor.test(data_total_vol_ratio$patheff, data_total_vol_ratio$meanpa_ratio, 
#           c(data_total_vol_ratio$sex, data_total_vol_ratio$age, data_total_vol_ratio$tiv), method = "pearson")
# 
# ggplot(data = data_total_vol_ratio, aes(y = patheff, x = Lpa_ratio)) + 
#   geom_point(color = palette[6]) + geom_smooth(method = "lm", color = palette[6]) +
#   ggtitle("Path Efficiency and Left PostHC/AntHC Ratio") + xlab("Left PostHC/AntHC Ratio") +
#   ylab("Path Inefficiency") + scale_y_continuous(breaks = seq(1, 10, by = 0.1))
# ggplot(data = data_total_vol_ratio, aes(y = patheff, x = Rpa_ratio)) + 
#   geom_point(color = palette[6]) + geom_smooth(method = "lm", color = palette[6]) +
#   ggtitle("Path Efficiency and Right PostHC/AntHC Ratio") + xlab("Right PostHC/AntHC Ratio") +
#   ylab("Path Inefficiency") + scale_y_continuous(breaks = seq(1, 10, by = 0.1))
# ggplot(data = data_total_vol_ratio, aes(y = patheff, x = totalpa_ratio)) + 
#   geom_point(color = palette[6]) + geom_smooth(method = "lm", color = palette[6]) +
#   ggtitle("Path Efficiency and Total PostHC/AntHC Ratio") + xlab("Total PostHC/AntHC Ratio") +
#   ylab("Path Inefficiency") + scale_y_continuous(breaks = seq(1, 10, by = 0.1))
# 
# pcor.test(data_total_vol_ratio$patheff_acc_only, data_total_vol_ratio$Lpa_ratio, 
#           c(data_total_vol_ratio$sex, data_total_vol_ratio$age, data_total_vol_ratio$tiv), method = "pearson")
# pcor.test(data_total_vol_ratio$patheff_acc_only, data_total_vol_ratio$Rpa_ratio, 
#           c(data_total_vol_ratio$sex, data_total_vol_ratio$age, data_total_vol_ratio$tiv), method = "pearson")
# pcor.test(data_total_vol_ratio$patheff_acc_only, data_total_vol_ratio$meanpa_ratio, 
#           c(data_total_vol_ratio$sex, data_total_vol_ratio$age, data_total_vol_ratio$tiv), method = "pearson")
# 
# ggplot(data = data_total_vol_ratio, aes(y = patheff_acc_only, x = Lpa_ratio)) + 
#   geom_point(color = palette[6]) + geom_smooth(method = "lm", color = palette[6]) +
#   ggtitle("Path Efficiency and Left PostHC/AntHC Ratio") + xlab("Left PostHC/AntHC Ratio") +
#   ylab("Path Inefficiency (Correct Trials Only)") + scale_y_continuous(breaks = seq(1, 10, by = 0.05))
# ggplot(data = data_total_vol_ratio, aes(y = patheff_acc_only, x = Rpa_ratio)) + 
#   geom_point(color = palette[6]) + geom_smooth(method = "lm", color = palette[6]) +
#   ggtitle("Path Efficiency and Right PostHC/AntHC Ratio") + xlab("Right PostHC/AntHC Ratio") +
#   ylab("Path Inefficiency (Correct Trials Only)") + scale_y_continuous(breaks = seq(1, 10, by = 0.05))
# ggplot(data = data_total_vol_ratio, aes(y = patheff_acc_only, x = totalpa_ratio)) + 
#   geom_point(color = palette[6]) + geom_smooth(method = "lm", color = palette[6]) +
#   ggtitle("Path Efficiency and Total PostHC/AntHC Ratio") + xlab("Total PostHC/AntHC Ratio") +
#   ylab("Path Inefficiency (Correct Trials Only)") + scale_y_continuous(breaks = seq(1, 10, by = 0.05))

# Outliers removed: Partial correlation between left and right post/ant HC volume ratio and patheff + patheff_acc_only (n=96)
pcor.test(data_total_vol_ratio_rm$patheff, data_total_vol_ratio_rm$Lpa_ratio, 
          c(data_total_vol_ratio_rm$sex, data_total_vol_ratio_rm$age, data_total_vol_ratio_rm$tiv), method = "pearson")
pcor.test(data_total_vol_ratio_rm$patheff, data_total_vol_ratio_rm$Rpa_ratio, 
          c(data_total_vol_ratio_rm$sex, data_total_vol_ratio_rm$age, data_total_vol_ratio_rm$tiv), method = "pearson")
pcor.test(data_total_vol_ratio_rm$patheff, data_total_vol_ratio_rm$meanpa_ratio, 
          c(data_total_vol_ratio_rm$sex, data_total_vol_ratio_rm$age, data_total_vol_ratio_rm$tiv), method = "pearson")

ggplot(data = data_total_vol_ratio_rm, aes(y = patheff, x = Lpa_ratio)) + 
  geom_point(color = palette[9]) + geom_smooth(method = "lm", color = palette[9]) +
  ggtitle("Path Efficiency and Left PostHC/AntHC Ratio") + xlab("Left PostHC/AntHC Ratio") +
  ylab("Path Inefficiency") + scale_y_continuous(breaks = seq(1, 10, by = 0.1))
ggplot(data = data_total_vol_ratio_rm, aes(y = patheff, x = Rpa_ratio)) + 
  geom_point(color = palette[9]) + geom_smooth(method = "lm", color = palette[9]) +
  ggtitle("Path Efficiency and Right PostHC/AntHC Ratio") + xlab("Right PostHC/AntHC Ratio") +
  ylab("Path Inefficiency") + scale_y_continuous(breaks = seq(1, 10, by = 0.1))
ggplot(data = data_total_vol_ratio_rm, aes(y = patheff, x = meanpa_ratio)) + 
  geom_point(color = palette[9]) + geom_smooth(method = "lm", color = palette[9]) +
  ggtitle("Path Efficiency and Mean PostHC/AntHC Ratio") + xlab("Mean PostHC/AntHC Ratio") +
  ylab("Path Inefficiency") + scale_y_continuous(breaks = seq(1, 10, by = 0.1))

pcor.test(data_total_vol_ratio_rm$patheff_acc_only, data_total_vol_ratio_rm$Lpa_ratio, 
          c(data_total_vol_ratio_rm$sex, data_total_vol_ratio_rm$age, data_total_vol_ratio_rm$tiv), method = "pearson")
pcor.test(data_total_vol_ratio_rm$patheff_acc_only, data_total_vol_ratio_rm$Rpa_ratio, 
          c(data_total_vol_ratio_rm$sex, data_total_vol_ratio_rm$age, data_total_vol_ratio_rm$tiv), method = "pearson")
pcor.test(data_total_vol_ratio_rm$patheff_acc_only, data_total_vol_ratio_rm$meanpa_ratio, 
          c(data_total_vol_ratio_rm$sex, data_total_vol_ratio_rm$age, data_total_vol_ratio_rm$tiv), method = "pearson")

ggplot(data = data_total_vol_ratio_rm, aes(y = patheff_acc_only, x = Lpa_ratio)) + 
  geom_point(color = palette[9]) + geom_smooth(method = "lm", color = palette[9]) +
  ggtitle("Path Efficiency and Left PostHC/AntHC Ratio") + xlab("Left PostHC/AntHC Ratio") +
  ylab("Path Inefficiency (Correct Trials Only)") + scale_y_continuous(breaks = seq(1, 10, by = 0.05))
ggplot(data = data_total_vol_ratio_rm, aes(y = patheff_acc_only, x = Rpa_ratio)) + 
  geom_point(color = palette[9]) + geom_smooth(method = "lm", color = palette[9]) +
  ggtitle("Path Efficiency and Right PostHC/AntHC Ratio") + xlab("Right PostHC/AntHC Ratio") +
  ylab("Path Inefficiency (Correct Trials Only)") + scale_y_continuous(breaks = seq(1, 10, by = 0.05))
ggplot(data = data_total_vol_ratio_rm, aes(y = patheff_acc_only, x = meanpa_ratio)) + 
  geom_point(color = palette[9]) + geom_smooth(method = "lm", color = palette[9]) +
  ggtitle("Path Efficiency and Mean PostHC/AntHC Ratio") + xlab("Mean PostHC/AntHC Ratio") +
  ylab("Path Inefficiency (Correct Trials Only)") + scale_y_continuous(breaks = seq(1, 10, by = 0.05))

#### ------------------------ Section 4C: Ant/Post Thickness + Proportion Correct (n=96) ----------------------- ####
# # Partial correlation between left ant/post and right ant/post HC thickness and propcorrect (n=98)
# pcor.test(data_total_thick$propcorrect, data_total_thick$tLaHC, 
#           c(data_total_thick$sex, data_total_thick$age, data_total_thick$tiv), method = "pearson")
# pcor.test(data_total_thick$propcorrect, data_total_thick$tLpHC, 
#           c(data_total_thick$sex, data_total_thick$age, data_total_thick$tiv), method = "pearson")
# pcor.test(data_total_thick$propcorrect, data_total_thick$tRaHC, 
#           c(data_total_thick$sex, data_total_thick$age, data_total_thick$tiv), method = "pearson")
# pcor.test(data_total_thick$propcorrect, data_total_thick$tRpHC, 
#           c(data_total_thick$sex, data_total_thick$age, data_total_thick$tiv), method = "pearson")
# 
# ggplot(data = data_total_thick, aes(y = propcorrect, x = tLaHC)) + 
#   geom_point(color = palette[6]) + geom_smooth(method = "lm", color = palette[6]) +
#   ggtitle("Wayfinding Success and Left Ant. HC Thickness") + xlab("Left Anterior HC Thickness") +
#   ylab("Proportion Correct") + ylim(0, 1)
# ggplot(data = data_total_thick, aes(y = propcorrect, x = tLpHC)) + 
#   geom_point(color = palette[6]) + geom_smooth(method = "lm", color = palette[6]) +
#   ggtitle("Wayfinding Success and Left Post. HC Thickness") + xlab("Left Posterior HC Thickness") +
#   ylab("Proportion Correct") + ylim(0, 1)
# ggplot(data = data_total_thick, aes(y = propcorrect, x = tRaHC)) + 
#   geom_point(color = palette[6]) + geom_smooth(method = "lm", color = palette[6]) +
#   ggtitle("Wayfinding Success and Right Ant. HC Thickness") + xlab("Right Anterior HC Thickness") +
#   ylab("Proportion Correct") + ylim(0, 1)
# ggplot(data = data_total_thick, aes(y = propcorrect, x = tRpHC)) + 
#   geom_point(color = palette[6]) + geom_smooth(method = "lm", color = palette[6]) +
#   ggtitle("Wayfinding Success and Right Post. HC Thickness") + xlab("Right Posterior HC Thickness") +
#   ylab("Proportion Correct") + ylim(0, 1)

# Check for outliers
which(abs(scale(data_total_thick$tLaHC)) > 3) # 21
which(abs(scale(data_total_thick$tLpHC)) > 3) # 51
which(abs(scale(data_total_thick$tRaHC)) > 3) # 21
which(abs(scale(data_total_thick$tRpHC)) > 3) # 28

# Remove outliers
data_total_thick_rm <- data_total_thick[-c(21, 28, 51),]
dim(data_total_thick_rm)

# Outliers removed: Partial correlation between left ant/post and right ant/post HC thickness and propcorrect (n=95)
pcor.test(data_total_thick_rm$propcorrect, data_total_thick_rm$tLaHC, 
          c(data_total_thick_rm$sex, data_total_thick_rm$age, data_total_thick_rm$tiv), method = "pearson")
pcor.test(data_total_thick_rm$propcorrect, data_total_thick_rm$tLpHC, 
          c(data_total_thick_rm$sex, data_total_thick_rm$age, data_total_thick_rm$tiv), method = "pearson")
pcor.test(data_total_thick_rm$propcorrect, data_total_thick_rm$tRaHC, 
          c(data_total_thick_rm$sex, data_total_thick_rm$age, data_total_thick_rm$tiv), method = "pearson")
pcor.test(data_total_thick_rm$propcorrect, data_total_thick_rm$tRpHC, 
          c(data_total_thick_rm$sex, data_total_thick_rm$age, data_total_thick_rm$tiv), method = "pearson")

ggplot(data = data_total_thick_rm, aes(y = propcorrect, x = tLaHC)) + 
  geom_point(color = palette[9]) + geom_smooth(method = "lm", color = palette[9]) +
  ggtitle("Wayfinding Success and Left Ant. HC Thickness") + xlab("Left Anterior Thickness") +
  ylab("Proportion Correct") + ylim(0, 1)
ggplot(data = data_total_thick_rm, aes(y = propcorrect, x = tLpHC)) + 
  geom_point(color = palette[9]) + geom_smooth(method = "lm", color = palette[9]) +
  ggtitle("Wayfinding Success and Left Post. HC Thickness") + xlab("Left Posterior HC Thickness") +
  ylab("Proportion Correct") + ylim(0, 1)
ggplot(data = data_total_thick_rm, aes(y = propcorrect, x = tRaHC)) + 
  geom_point(color = palette[9]) + geom_smooth(method = "lm", color = palette[9]) +
  ggtitle("Wayfinding Success and Right Ant. HC Thickness") + xlab("Right Anterior HC Thickness") +
  ylab("Proportion Correct") + ylim(0, 1)
ggplot(data = data_total_thick_rm, aes(y = propcorrect, x = tRpHC)) + 
  geom_point(color = palette[9]) + geom_smooth(method = "lm", color = palette[9]) +
  ggtitle("Wayfinding Success and Right Post. HC Thickness") + xlab("Right Posterior HC Thickness") +
  ylab("Proportion Correct") + ylim(0, 1)

#### --------------------------- Section 4D: Ant/Post Thickness + Path Eff (n=96) ------------------------------ ####
# # Partial correlation between left ant/post and right ant/post HC thickness and patheff + patheff_acc_only (n=98)
# pcor.test(data_total_thick$patheff, data_total_thick$tLaHC, 
#           c(data_total_thick$sex, data_total_thick$age, data_total_thick$tiv), method = "pearson")
# pcor.test(data_total_thick$patheff, data_total_thick$tLpHC, 
#           c(data_total_thick$sex, data_total_thick$age, data_total_thick$tiv), method = "pearson")
# pcor.test(data_total_thick$patheff, data_total_thick$tRaHC, 
#           c(data_total_thick$sex, data_total_thick$age, data_total_thick$tiv), method = "pearson")
# pcor.test(data_total_thick$patheff, data_total_thick$tRpHC, 
#           c(data_total_thick$sex, data_total_thick$age, data_total_thick$tiv), method = "pearson")
# 
# ggplot(data = data_total_thick, aes(y = patheff, x = tLaHC)) + 
#   geom_point(color = palette[6]) + geom_smooth(method = "lm", color = palette[6]) +
#   ggtitle("Path Efficiency and Left Ant. HC Thickness") + xlab("Left Anterior HC Thickness") +
#   ylab("Path Inefficiency") + scale_y_continuous(breaks = seq(1, 10, by = 0.1))
# ggplot(data = data_total_thick, aes(y = patheff, x = tLpHC)) + 
#   geom_point(color = palette[6]) + geom_smooth(method = "lm", color = palette[6]) +
#   ggtitle("Path Efficiency and Left Post. HC Thickness") + xlab("Left Posterior HC Thickness") +
#   ylab("Path Inefficiency") + scale_y_continuous(breaks = seq(1, 10, by = 0.1))
# ggplot(data = data_total_thick, aes(y = patheff, x = tRaHC)) + 
#   geom_point(color = palette[6]) + geom_smooth(method = "lm", color = palette[6]) +
#   ggtitle("Path Efficiency and Right Ant. HC Thickness") + xlab("Right Anterior HC Thickness") +
#   ylab("Path Inefficiency") + scale_y_continuous(breaks = seq(1, 10, by = 0.1))
# ggplot(data = data_total_thick, aes(y = patheff, x = tRpHC)) + 
#   geom_point(color = palette[6]) + geom_smooth(method = "lm", color = palette[6]) +
#   ggtitle("Path Efficiency and Right Post. HC Thickness") + xlab("Right Posterior HC Thickness") +
#   ylab("Path Inefficiency") + scale_y_continuous(breaks = seq(1, 10, by = 0.1))
# 
# pcor.test(data_total_thick$patheff_acc_only, data_total_thick$tLaHC, 
#           c(data_total_thick$sex, data_total_thick$age, data_total_thick$tiv), method = "pearson")
# pcor.test(data_total_thick$patheff_acc_only, data_total_thick$tLpHC, 
#           c(data_total_thick$sex, data_total_thick$age, data_total_thick$tiv), method = "pearson")
# pcor.test(data_total_thick$patheff_acc_only, data_total_thick$tRaHC, 
#           c(data_total_thick$sex, data_total_thick$age, data_total_thick$tiv), method = "pearson")
# pcor.test(data_total_thick$patheff_acc_only, data_total_thick$tRpHC, 
#           c(data_total_thick$sex, data_total_thick$age, data_total_thick$tiv), method = "pearson")
# 
# ggplot(data = data_total_thick, aes(y = patheff_acc_only, x = tLaHC)) + 
#   geom_point(color = palette[6]) + geom_smooth(method = "lm", color = palette[6]) +
#   ggtitle("Path Efficiency and Left Ant. HC Thickness") + xlab("Left Anterior HC Thickness") +
#   ylab("Path Inefficiency (Correct Trials Only)") + scale_y_continuous(breaks = seq(1, 10, by = 0.05))
# ggplot(data = data_total_thick, aes(y = patheff_acc_only, x = tLpHC)) + 
#   geom_point(color = palette[6]) + geom_smooth(method = "lm", color = palette[6]) +
#   ggtitle("Path Efficiency and Left Post. HC Thickness") + xlab("Left Posterior HC Thickness") +
#   ylab("Path Inefficiency (Correct Trials Only)") + scale_y_continuous(breaks = seq(1, 10, by = 0.05))
# ggplot(data = data_total_thick, aes(y = patheff_acc_only, x = tRaHC)) + 
#   geom_point(color = palette[6]) + geom_smooth(method = "lm", color = palette[6]) +
#   ggtitle("Path Efficiency and Right Ant. HC Thickness") + xlab("Right Anterior HC Thickness") +
#   ylab("Path Inefficiency (Correct Trials Only)") + scale_y_continuous(breaks = seq(1, 10, by = 0.05))
# ggplot(data = data_total_thick, aes(y = patheff_acc_only, x = tRpHC)) + 
#   geom_point(color = palette[6]) + geom_smooth(method = "lm", color = palette[6]) +
#   ggtitle("Path Efficiency and Right Post. HC Thickness") + xlab("Right Posterior HC Thickness") +
#   ylab("Path Inefficiency (Correct Trials Only)") + scale_y_continuous(breaks = seq(1, 10, by = 0.05))

# Outliers removed: Partial correlation between left ant/post and right ant/post HC thickness and patheff + patheff_acc_only (n=95)
pcor.test(data_total_thick_rm$patheff, data_total_thick_rm$tLaHC, 
          c(data_total_thick_rm$sex, data_total_thick_rm$age, data_total_thick_rm$tiv), method = "pearson")
pcor.test(data_total_thick_rm$patheff, data_total_thick_rm$tLpHC, 
          c(data_total_thick_rm$sex, data_total_thick_rm$age, data_total_thick_rm$tiv), method = "pearson")
pcor.test(data_total_thick_rm$patheff, data_total_thick_rm$tRaHC, 
          c(data_total_thick_rm$sex, data_total_thick_rm$age, data_total_thick_rm$tiv), method = "pearson")
pcor.test(data_total_thick_rm$patheff, data_total_thick_rm$tRpHC, 
          c(data_total_thick_rm$sex, data_total_thick_rm$age, data_total_thick_rm$tiv), method = "pearson")

ggplot(data = data_total_thick_rm, aes(y = patheff, x = tLaHC)) + 
  geom_point(color = palette[9]) + geom_smooth(method = "lm", color = palette[6]) +
  ggtitle("Path Efficiency and Left Ant. HC Thickness") + xlab("Left Anterior HC Thickness") +
  ylab("Path Inefficiency") + scale_y_continuous(breaks = seq(1, 10, by = 0.1))
ggplot(data = data_total_thick_rm, aes(y = patheff, x = tLpHC)) + 
  geom_point(color = palette[9]) + geom_smooth(method = "lm", color = palette[6]) +
  ggtitle("Path Efficiency and Left Post. HC Thickness") + xlab("Left Posterior HC Thickness") +
  ylab("Path Inefficiency") + scale_y_continuous(breaks = seq(1, 10, by = 0.1))
ggplot(data = data_total_thick_rm, aes(y = patheff, x = tRaHC)) + 
  geom_point(color = palette[9]) + geom_smooth(method = "lm", color = palette[6]) +
  ggtitle("Path Efficiency and Right Ant. HC Thickness") + xlab("Right Anterior HC Thickness") +
  ylab("Path Inefficiency") + scale_y_continuous(breaks = seq(1, 10, by = 0.1))
ggplot(data = data_total_thick_rm, aes(y = patheff, x = tRpHC)) + 
  geom_point(color = palette[9]) + geom_smooth(method = "lm", color = palette[6]) +
  ggtitle("Path Efficiency and Right Post. HC Thickness") + xlab("Right Posterior HC Thickness") +
  ylab("Path Inefficiency") + scale_y_continuous(breaks = seq(1, 10, by = 0.1))

pcor.test(data_total_thick_rm$patheff_acc_only, data_total_thick_rm$tLaHC, 
          c(data_total_thick_rm$sex, data_total_thick_rm$age, data_total_thick_rm$tiv), method = "pearson")
pcor.test(data_total_thick_rm$patheff_acc_only, data_total_thick_rm$tLpHC, 
          c(data_total_thick_rm$sex, data_total_thick_rm$age, data_total_thick_rm$tiv), method = "pearson")
pcor.test(data_total_thick_rm$patheff_acc_only, data_total_thick_rm$tRaHC, 
          c(data_total_thick_rm$sex, data_total_thick_rm$age, data_total_thick_rm$tiv), method = "pearson")
pcor.test(data_total_thick_rm$patheff_acc_only, data_total_thick_rm$tRpHC, 
          c(data_total_thick_rm$sex, data_total_thick_rm$age, data_total_thick_rm$tiv), method = "pearson")

ggplot(data = data_total_thick_rm, aes(y = patheff_acc_only, x = tLaHC)) + 
  geom_point(color = palette[9]) + geom_smooth(method = "lm", color = palette[6]) +
  ggtitle("Path Efficiency and Left Ant. HC Thickness") + xlab("Left Anterior HC Thickness") +
  ylab("Path Inefficiency (Correct Trials Only)") + scale_y_continuous(breaks = seq(1, 10, by = 0.05))
ggplot(data = data_total_thick_rm, aes(y = patheff_acc_only, x = tLpHC)) + 
  geom_point(color = palette[9]) + geom_smooth(method = "lm", color = palette[6]) +
  ggtitle("Path Efficiency and Left Post. HC Thickness") + xlab("Left Posterior HC Thickness") +
  ylab("Path Inefficiency (Correct Trials Only)") + scale_y_continuous(breaks = seq(1, 10, by = 0.05))
ggplot(data = data_total_thick_rm, aes(y = patheff_acc_only, x = tRaHC)) + 
  geom_point(color = palette[9]) + geom_smooth(method = "lm", color = palette[6]) +
  ggtitle("Path Efficiency and Right Ant. HC Thickness") + xlab("Right Anterior HC Thickness") +
  ylab("Path Inefficiency (Correct Trials Only)") + scale_y_continuous(breaks = seq(1, 10, by = 0.05))
ggplot(data = data_total_thick_rm, aes(y = patheff_acc_only, x = tRpHC)) + 
  geom_point(color = palette[9]) + geom_smooth(method = "lm", color = palette[6]) +
  ggtitle("Path Efficiency and Right Post. HC Thickness") + xlab("Right Posterior HC Thickness") +
  ylab("Path Inefficiency (Correct Trials Only)") + scale_y_continuous(breaks = seq(1, 10, by = 0.05))

#### -------------- Section 5A: *No cut-off* HR-T2 Subfield Volumes + Propcorr or Patheff (n=14) --------------- ####
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
ggplot(data = data_cut, aes(y = patheff, x = LSUB)) +
  geom_point(color = palette_sbfd[4]) + geom_smooth(method = "lm", color = palette_sbfd[4]) +
  ggtitle("Path Efficiency and Left SUB Volume") +
  xlab("Left SUB Volume (mm^3)") + ylab("Path Inefficiency") + scale_y_continuous(breaks = seq(1, 10, by = 0.1)) # indiv subfield
ggplot(data = data_cut, aes(y = patheff_acc_only, x = LCA23)) +
  geom_point(color = palette_sbfd[2]) + geom_smooth(method = "lm", color = palette_sbfd[2]) +
  ggtitle("Path Efficiency and Left CA2/3 Volume") +
  xlab("Left CA2/3 Volume (mm^3)") + ylab("Path Inefficiency (Correct Trials Only)") + 
  scale_y_continuous(breaks = seq(1, 10, by = 0.1)) # indiv subfield

#### -------------- Section 5AA: *No cut-off* HR-T2 Subfield Volumes + Propcorr or Patheff (n=14) -------------- ####
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

#### ------------- Section 5B: *No cut-off* HR-T2 Subfield Thickness + Propcorr or Patheff (n=26) -------------- ####
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

#### -------------- Section 6A: HR-T2 *Body-only* Subfield Volumes + Propcorr or Patheff (n=17) ---------------- ####
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
  ggtitle("Path Efficiency and Right CA1 Volume") +
  xlab("Right CA1 Volume (mm^3)") + ylab("Path Inefficiency (Correct Trials Only)") + 
  scale_y_continuous(breaks = seq(1, 10, by = 0.05)) # indiv subfield

#### ---------------- Section 6B: HR-T2 *Body-only* Subfield Thick + Propcorr or Patheff (n=17) ---------------- ####
subfields_tLR6 <- subfields_tHC_LR[-c(9:10)]

# Partial correlation tables between body-only subfield thickness and propcorrect, patheff, patheff_acc_only (n=17)
nav_dv <- c("propcorrect", "patheff", "patheff_acc_only")
for (dv in nav_dv) {
  r <- NULL;
  
  for (subfd_name in subfields_tLR6) { 
    subfield <- paste(subfd_name, "_pcor", sep = "")
    PC_subfield_pcor <- pcor.test(pull(data_tbody[subfd_name]), data_tbody[dv], 
                                  c(data_tbody$sex, data_tbody$age, data_tbody$tiv), method = "pearson")
    assign(subfd_name, PC_subfield_pcor)
    r <- rbind(r, round(PC_subfield_pcor$estimate, digits = 2), round(PC_subfield_pcor$p.value, digits = 4))
    # print(paste(i, "estimate is:", round(PC_subfield_pcor$estimate, digits = 2), 
    #             "and p-value is:", round(PC_subfield_pcor$p.value, digits = 3))) 
  } # ex. LCA1_pcor
  
  print(dv)
  print(corr_mat_6sub(r))
}

ggplot(data = data_tbody, aes(y = patheff_acc_only, x = tLCA1)) +
  geom_point(color = palette_sbfd[1]) + geom_smooth(method = "lm", color = palette_sbfd[1])+
  ggtitle("Path Efficiency and Left CA1 Thickness") +
  xlab("Normalized Thickness Ratio") + ylab("Path Inefficiency (Correct Trials Only)") + 
  scale_y_continuous(breaks = seq(1, 10, by = 0.05)) # indiv subfield
ggplot(data = data_tbody, aes(y = patheff_acc_only, x = tLPHC)) +
  geom_point(color = palette_sbfd[7]) + geom_smooth(method = "lm", color = palette_sbfd[7])+
  ggtitle("Path Efficiency and Left PHC Thickness") +
  xlab("Normalized Thickness Ratio") + ylab("Path Inefficiency (Correct Trials Only)") + 
  scale_y_continuous(breaks = seq(1, 10, by = 0.05)) # indiv subfield

#### ------------------ Section 7A: Full Reg-T2 Subfield Volumes + Propcorr or Patheff (n=26) ------------------ ####
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
  ggtitle("Path Efficiency and Right ERC Volume") + 
  xlab("Right ERC Volume (mm^3)") + ylab("Path Inefficiency (Correct Trials Only)") + 
  scale_y_continuous(breaks = seq(1, 10, by = 0.05)) # indiv subfield
ggplot(data = data_regT2, aes(y = patheff_acc_only, x = LPHC)) + 
  geom_point(color = palette_sbfd[7]) + geom_smooth(method = "lm", color = palette_sbfd[7]) +
  ggtitle("Path Efficiency and Left PHC Volume") + 
  xlab("Left PHC Volume (mm^3)") + ylab("Path Inefficiency (Correct Trials Only)") + 
  scale_y_continuous(breaks = seq(1, 10, by = 0.05)) # indiv subfield

#### ----------------- Section 7B: Full Reg-T2 Subfield Thickness + Propcorr or Patheff (n=26) ----------------- ####
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
    PC_tsubfield_pcor <- pcor.test(pull(data_tT2w[subfd_name]), data_tT2w[dv], 
                                   c(data_tT2w$sex, data_tT2w$age, data_tT2w$tiv), method = "pearson")
    assign(subfd_name, PC_tsubfield_pcor)
    r <- rbind(r, round(PC_tsubfield_pcor$estimate, digits = 2), round(PC_tsubfield_pcor$p.value, digits = 4))
    # print(paste(i, "estimate is:", round(PC_tsubfield_pcor$estimate, digits = 2), 
    #             "and p-value is:", round(PC_tsubfield_pcor$p.value, digits = 3))) 
  } # ex. tLCA1_pcor
  
  print(dv)
  print(corr_mat_5sub(r))
}

ggplot(data = data_tT2w, aes(y = patheff_acc_only, x = tRERC)) + 
  geom_point(color = palette_sbfd[5]) + geom_smooth(method = "lm", color = palette_sbfd[5]) +
  ggtitle("Path Efficiency and Right ERC Thickness") + 
  xlab("Right ERC Thickness") + ylab("Path Inefficiency (Correct Trials Only)") + 
  scale_y_continuous(breaks = seq(1, 10, by = 0.05)) # indiv subfield


#### ---------- Section 8A: *No cut-off* HR-T2 Subfield Volumes + Paper/Pencil Task or Survey (n=13) ----------- ####
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

# Data
data_survey_cut <- merge(data_survey_scores[,1:8], data_cut, by = "sub_id") %>% na.omit()
dim(data_survey_cut)

# Partial correlation tables between no cutoff subfield volumes and survey scores (n=14)
survey_dv <- c("RM_score", "SBSOD_score", "SOT_score", "Hand_score", "VG_hr", "VG_yr", "VG_score")
for (dv in survey_dv) {
  r <- NULL;
  
  for (subfd_name in subfields_LR) { 
    subfield <- paste(subfd_name, "_pcor", sep = "")
    PC_subfield_pcor <- pcor.test(pull(data_survey_cut[subfd_name]), data_survey_cut[dv], 
                                  c(data_survey_cut$sex, data_survey_cut$age, data_survey_cut$tiv), method = "pearson")
    assign(subfd_name, PC_subfield_pcor)
    r <- rbind(r, round(PC_subfield_pcor$estimate, digits = 2), round(PC_subfield_pcor$p.value, digits = 5))
    # print(paste(i, "estimate is:", round(PC_subfield_pcor$estimate, digits = 2), 
    #             "and p-value is:", round(PC_subfield_pcor$p.value, digits = 3))) 
  } # ex. LCA1_pcor
  
  print(dv)
  print(corr_mat_allsub(r))
}

theme_set(theme2)
# Road Map
ggplot(data = data_survey_cut, aes(y = RM_score, x = LCA1)) +
  geom_point(color = palette_sbfd[1]) + geom_smooth(method = "lm", color = palette_sbfd[1]) +
  ggtitle("Road Map Score and Left CA1 Volume") +
  xlab("Left CA1 Volume (mm^3)") + ylab("Road Map Score")  # indiv subfield
ggplot(data = data_survey_cut, aes(y = RM_score, x = LCA23)) +
  geom_point(color = palette_sbfd[2]) + geom_smooth(method = "lm", color = palette_sbfd[2]) +
  ggtitle("Road Map Score and Left CA2/3 Volume") +
  xlab("Left CA2/3 Volume (mm^3)") + ylab("Road Map Score")  # indiv subfield
ggplot(data = data_survey_cut, aes(y = RM_score, x = LERC)) +
  geom_point(color = palette_sbfd[5]) + geom_smooth(method = "lm", color = palette_sbfd[5]) +
  ggtitle("Road Map Score and Left ERC Volume") +
  xlab("Left ERC Volume (mm^3)") + ylab("Road Map Score")  # indiv subfield
ggplot(data = data_survey_cut, aes(y = RM_score, x = LPRC)) +
  geom_point(color = palette_sbfd[6]) + geom_smooth(method = "lm", color = palette_sbfd[6]) +
  ggtitle("Road Map Score and Left PRC Volume") +
  xlab("Left PRC Volume (mm^3)") + ylab("Road Map Score")  # indiv subfield
# SBSOD
ggplot(data = data_survey_cut, aes(y = SBSOD_score, x = LSUB)) +
  geom_point(color = palette_sbfd[4]) + geom_smooth(method = "lm", color = palette_sbfd[4]) +
  ggtitle("SBSOD and Left SUB Volume") +
  xlab("Left SUB Volume (mm^3)") + ylab("SBSOD")  # indiv subfield
# SOT
ggplot(data = data_survey_cut, aes(y = SOT_score, x = LCA1)) +
  geom_point(color = palette_sbfd[1]) + geom_smooth(method = "lm", color = palette_sbfd[1]) +
  ggtitle("SOT Score and Left CA1 Volume") +
  xlab("Left CA1 Volume (mm^3)") + ylab("SOT Score")  # indiv subfield
ggplot(data = data_survey_cut, aes(y = SOT_score, x = LDG)) +
  geom_point(color = palette_sbfd[3]) + geom_smooth(method = "lm", color = palette_sbfd[3]) +
  ggtitle("SOT Score and Left DG Volume") +
  xlab("Left DG Volume (mm^3)") + ylab("SOT Score")  # indiv subfield
ggplot(data = data_survey_cut, aes(y = SOT_score, x = LERC)) +
  geom_point(color = palette_sbfd[5]) + geom_smooth(method = "lm", color = palette_sbfd[5]) +
  ggtitle("SOT Score and Left ERC Volume") +
  xlab("Left ERC Volume (mm^3)") + ylab("SOT Score")  # indiv subfield
ggplot(data = data_survey_cut, aes(y = SOT_score, x = LPRC)) +
  geom_point(color = palette_sbfd[6]) + geom_smooth(method = "lm", color = palette_sbfd[6]) +
  ggtitle("SOT Score and Left PRC Volume") +
  xlab("Left PRC Volume (mm^3)") + ylab("SOT Score")  # indiv subfield
ggplot(data = data_survey_cut, aes(y = SOT_score, x = RDG)) +
  geom_point(color = palette_sbfd[3]) + geom_smooth(method = "lm", color = palette_sbfd[3]) +
  ggtitle("SOT Score and Right DG Volume") +
  xlab("Right DG Volume (mm^3)") + ylab("SOT Score")  # indiv subfield
# Hand
ggplot(data = data_survey_cut, aes(y = Hand_score, x = LCA1)) +
  geom_point(color = palette_sbfd[1]) + geom_smooth(method = "lm", color = palette_sbfd[1]) +
  ggtitle("Handedness Score and Left CA1 Volume") +
  xlab("Left CA1 Volume (mm^3)") + ylab("Handedness Score")  # indiv subfield
# VG
ggplot(data = data_survey_cut, aes(y = VG_score, x = RSUB)) +
  geom_point(color = palette_sbfd[4]) + geom_smooth(method = "lm", color = palette_sbfd[4]) +
  ggtitle("Video Game Score and Right SUB Volume") +
  xlab("Right SUB Volume (mm^3)") + ylab("Video Game Score")  # indiv subfield

#### --------- Section 8B: *No cut-off* HR-T2 Subfield Thickness + Paper/Pencil Task or Survey (n=24) ---------- ####
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

# Subfield thickness
subfields_tHC_LR <- NULL;

for (subfd_name in subfields_LR) {
  subfields_tHC_LR <- rbind(subfields_tHC_LR, paste("t", subfd_name, sep = ""))
}
subfields_tHC_LR <- subfields_tHC_LR[,1]

# Data
data_survey_tdata <- merge(data_survey_scores[,1:8], data_tdata, by = "sub_id") %>% na.omit()
dim(data_survey_tdata)

# Partial correlation tables between no cutoff subfield thickness and survey scores (n=24)
survey_dv <- c("RM_score", "SBSOD_score", "SOT_score", "Hand_score", "VG_hr", "VG_yr", "VG_score")
for (dv in survey_dv) {
  r <- NULL;
  
  for (subfd_name in subfields_tHC_LR) { 
    subfield <- paste(subfd_name, "_pcor", sep = "")
    PC_subfield_pcor <- pcor.test(pull(data_survey_tdata[subfd_name]), data_survey_tdata[dv], 
                                  c(data_survey_tdata$sex, data_survey_tdata$age, data_survey_tdata$tiv), method = "pearson")
    assign(subfd_name, PC_subfield_pcor)
    r <- rbind(r, round(PC_subfield_pcor$estimate, digits = 2), round(PC_subfield_pcor$p.value, digits = 5))
    # print(paste(i, "estimate is:", round(PC_subfield_pcor$estimate, digits = 2), 
    #             "and p-value is:", round(PC_subfield_pcor$p.value, digits = 3))) 
  } # ex. LCA1_pcor
  
  print(dv)
  print(corr_mat_allsub(r))
}

# SBSOD
ggplot(data = data_survey_tdata, aes(y = SBSOD_score, x = tLSUB)) +
  geom_point(color = palette_sbfd[4]) + geom_smooth(method = "lm", color = palette_sbfd[4]) +
  ggtitle("SBSOD and Left SUB Thickness") +
  xlab("Normalized Thickness Ratio") + ylab("SBSOD")  # indiv subfield
ggplot(data = data_survey_tdata, aes(y = SBSOD_score, x = tLERC)) +
  geom_point(color = palette_sbfd[5]) + geom_smooth(method = "lm", color = palette_sbfd[5]) +
  ggtitle("SBSOD and Left ERC Thickness") +
  xlab("Normalized Thickness Ratio") + ylab("SBSOD")  # indiv subfield
ggplot(data = data_survey_tdata, aes(y = SBSOD_score, x = tRPHC)) +
  geom_point(color = palette_sbfd[7]) + geom_smooth(method = "lm", color = palette_sbfd[7]) +
  ggtitle("SBSOD and Right PHC Thickness") +
  xlab("Normalized Thickness Ratio") + ylab("SBSOD")  # indiv subfield
# SOT
ggplot(data = data_survey_tdata, aes(y = SOT_score, x = tLDG)) +
  geom_point(color = palette_sbfd[3]) + geom_smooth(method = "lm", color = palette_sbfd[3]) +
  ggtitle("SOT Score and Left DG Thickness") +
  xlab("Normalized Thickness Ratio") + ylab("SOT Score")  # indiv subfield
ggplot(data = data_survey_tdata, aes(y = SOT_score, x = tRERC)) +
  geom_point(color = palette_sbfd[5]) + geom_smooth(method = "lm", color = palette_sbfd[5]) +
  ggtitle("SOT Score and Right ERC Thickness") +
  xlab("Normalized Thickness Ratio") + ylab("SOT Score")  # indiv subfield
# VG
ggplot(data = data_survey_tdata, aes(y = VG_score, x = tLDG)) +
  geom_point(color = palette_sbfd[3]) + geom_smooth(method = "lm", color = palette_sbfd[3]) +
  ggtitle("Video Game Score and Left DG Volume") +
  xlab("Normalized Thickness Ratio") + ylab("Video Game Score")  # indiv subfield

#### ---------- Section 8C: HR-T2 *Body-only* Subfield Volumes + Paper/Pencil Task or Survey (n=16) ------------ ####
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

# Data
data_survey_HCbodycut <- merge(data_survey_scores[,1:8], data_HCbody_cut, by = "sub_id") %>% na.omit()
dim(data_survey_HCbodycut)

# Partial correlation tables between body-only subfield volumes and survey scores (n=16)
survey_dv <- c("RM_score", "SBSOD_score", "SOT_score", "Hand_score", "VG_hr", "VG_yr", "VG_score")
for (dv in survey_dv) {
  r <- NULL;
  
  for (subfd_name in subfields_LR6) { 
    subfield <- paste(subfd_name, "_pcor", sep = "")
    PC_subfield_pcor <- pcor.test(pull(data_survey_HCbodycut[subfd_name]), data_survey_HCbodycut[dv], 
                                  c(data_survey_HCbodycut$sex, data_survey_HCbodycut$age, data_survey_HCbodycut$tiv), method = "pearson")
    assign(subfd_name, PC_subfield_pcor)
    r <- rbind(r, round(PC_subfield_pcor$estimate, digits = 2), round(PC_subfield_pcor$p.value, digits = 5))
    # print(paste(i, "estimate is:", round(PC_subfield_pcor$estimate, digits = 2), 
    #             "and p-value is:", round(PC_subfield_pcor$p.value, digits = 3))) 
  } # ex. LCA1_pcor
  
  print(dv)
  print(corr_mat_6sub(r))
}

# SBSOD
ggplot(data = data_survey_HCbodycut, aes(y = SBSOD_score, x = LSUB)) +
  geom_point(color = palette_sbfd[4]) + geom_smooth(method = "lm", color = palette_sbfd[4]) +
  ggtitle("SBSOD and Left SUB Volume") +
  xlab("Left SUB Volume (mm^3)") + ylab("SBSOD")  # indiv subfield
ggplot(data = data_survey_HCbodycut, aes(y = SBSOD_score, x = RCA1)) +
  geom_point(color = palette_sbfd[1]) + geom_smooth(method = "lm", color = palette_sbfd[1]) +
  ggtitle("SBSOD and Right CA1 Volume") +
  xlab("Right CA1 Volume (mm^3)") + ylab("SBSOD")  # indiv subfield
# SOT
ggplot(data = data_survey_HCbodycut, aes(y = SOT_score, x = LPRC)) +
  geom_point(color = palette_sbfd[6]) + geom_smooth(method = "lm", color = palette_sbfd[6]) +
  ggtitle("SOT Score and Left PRC Volume") +
  xlab("Left PRC Volume (mm^3)") + ylab("SOT Score")  # indiv subfield
ggplot(data = data_survey_HCbodycut, aes(y = SOT_score, x = RSUB)) +
  geom_point(color = palette_sbfd[4]) + geom_smooth(method = "lm", color = palette_sbfd[4]) +
  ggtitle("SOT Score and Right SUB Volume") +
  xlab("Right SUB Volume (mm^3)") + ylab("SOT Score")  # indiv subfield
# VG
ggplot(data = data_survey_HCbodycut, aes(y = VG_score, x = LCA23)) +
  geom_point(color = palette_sbfd[2]) + geom_smooth(method = "lm", color = palette_sbfd[2]) +
  ggtitle("Video Game Score and Left CA2/3 Volume") +
  xlab("Left CA2/3 Volume (mm^3)") + ylab("Video Game Score")  # indiv subfield
ggplot(data = data_survey_HCbodycut, aes(y = VG_score, x = RSUB)) +
  geom_point(color = palette_sbfd[4]) + geom_smooth(method = "lm", color = palette_sbfd[4]) +
  ggtitle("Video Game Score and Right SUB Volume") +
  xlab("Right SUB Volume (mm^3)") + ylab("Video Game Score")  # indiv subfield

#### --------- Section 8D: HR-T2 *Body-only* Subfield Thickness + Paper/Pencil Task or Survey (n=16) ----------- ####
subfields_tLR6 <- subfields_tHC_LR[-c(9:10)]

# Data
data_survey_tbodycut <- merge(data_survey_scores[,1:8], data_tbody, by = "sub_id") %>% na.omit()
dim(data_survey_tbodycut)

# Partial correlation tables between body-only subfield thickness and survey scores (n=16)
survey_dv <- c("RM_score", "SBSOD_score", "SOT_score", "Hand_score", "VG_hr", "VG_yr", "VG_score")
for (dv in survey_dv) {
  r <- NULL;
  
  for (subfd_name in subfields_tLR6) { 
    subfield <- paste(subfd_name, "_pcor", sep = "")
    PC_subfield_pcor <- pcor.test(pull(data_survey_tbodycut[subfd_name]), data_survey_tbodycut[dv], 
                                  c(data_survey_tbodycut$sex, data_survey_tbodycut$age, data_survey_tbodycut$tiv), method = "pearson")
    assign(subfd_name, PC_subfield_pcor)
    r <- rbind(r, round(PC_subfield_pcor$estimate, digits = 2), round(PC_subfield_pcor$p.value, digits = 5))
    # print(paste(i, "estimate is:", round(PC_subfield_pcor$estimate, digits = 2), 
    #             "and p-value is:", round(PC_subfield_pcor$p.value, digits = 3))) 
  } # ex. LCA1_pcor
  
  print(dv)
  print(corr_mat_6sub(r))
}

# SBSOD
ggplot(data = data_survey_tbodycut, aes(y = SBSOD_score, x = tLSUB)) +
  geom_point(color = palette_sbfd[4]) + geom_smooth(method = "lm", color = palette_sbfd[4]) +
  ggtitle("SBSOD and Left SUB Thickness") +
  xlab("Normalized Thickness Ratio") + ylab("SBSOD")  # indiv subfield
ggplot(data = data_survey_tbodycut, aes(y = SBSOD_score, x = tRCA1)) +
  geom_point(color = palette_sbfd[1]) + geom_smooth(method = "lm", color = palette_sbfd[1]) +
  ggtitle("SBSOD and Right CA1 Thickness") +
  xlab("Normalized Thickness Ratio") + ylab("SBSOD")  # indiv subfield
# SOT
ggplot(data = data_survey_tbodycut, aes(y = SOT_score, x = tLPRC)) +
  geom_point(color = palette_sbfd[6]) + geom_smooth(method = "lm", color = palette_sbfd[6]) +
  ggtitle("SOT Score and Left PRC Thickness") +
  xlab("Normalized Thickness Ratio") + ylab("SOT Score")  # indiv subfield
# Hand
ggplot(data = data_survey_tbodycut, aes(y = Hand_score, x = tLCA1)) +
  geom_point(color = palette_sbfd[1]) + geom_smooth(method = "lm", color = palette_sbfd[1]) +
  ggtitle("SOT Score and Left CA1 Thickness") +
  xlab("Normalized Thickness Ratio") + ylab("Handedness Score")  # indiv subfield
# VG
ggplot(data = data_survey_tbodycut, aes(y = VG_score, x = tLCA23)) +
  geom_point(color = palette_sbfd[2]) + geom_smooth(method = "lm", color = palette_sbfd[2]) +
  ggtitle("Video Game Score and Left CA2/3 Thickness") +
  xlab("Normalized Thickness Ratio") + ylab("Video Game Score")  # indiv subfield
ggplot(data = data_survey_tbodycut, aes(y = VG_score, x = tRSUB)) +
  geom_point(color = palette_sbfd[4]) + geom_smooth(method = "lm", color = palette_sbfd[4]) +
  ggtitle("Video Game Score and Right SUB Volume") +
  xlab("Normalized Thickness Ratio") + ylab("Video Game Score")  # indiv subfield

#### -------------- Section 8E: Full Reg-T2 Subfield Volumes + Paper/Pencil Task or Survey (n=24) -------------- ####
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

# Data
data_survey_regT2 <- merge(data_survey_scores[,1:8], data_regT2[,c(1, 6:10, 15:19, 24:28, 33:44)], by = "sub_id") %>% na.omit()
dim(data_survey_regT2)

# Partial correlation tables between manual regT2 subfield volumes and survey scores (n=24)
survey_dv <- c("RM_score", "SBSOD_score", "SOT_score", "Hand_score", "VG_hr", "VG_yr", "VG_score")
for (dv in survey_dv) {
  r <- NULL;
  
  for (subfd_name in subfields_LRregT2) { 
    subfield <- paste(subfd_name, "_pcor", sep = "")
    PC_subfield_pcor <- pcor.test(pull(data_survey_regT2[subfd_name]), data_survey_regT2[dv], 
                                  c(data_survey_regT2$sex, data_survey_regT2$age, data_survey_regT2$tiv), method = "pearson")
    assign(subfd_name, PC_subfield_pcor)
    r <- rbind(r, round(PC_subfield_pcor$estimate, digits = 2), round(PC_subfield_pcor$p.value, digits = 5))
    # print(paste(i, "estimate is:", round(PC_subfield_pcor$estimate, digits = 2), 
    #             "and p-value is:", round(PC_subfield_pcor$p.value, digits = 3))) 
  } # ex. LCA1_pcor
  
  print(dv)
  print(corr_mat_5sub(r))
}

# Road Map
ggplot(data = data_survey_regT2, aes(y = RM_score, x = LaHC)) +
  geom_point(color = palette_sbfd[8]) + geom_smooth(method = "lm", color = palette_sbfd[8]) +
  ggtitle("Road Map Score and Left Ant. HC Volume") +
  xlab("Left Anterior HC Volume (mm^3)") + ylab("Road Map Score")  # indiv subfield
# SBSOD
ggplot(data = data_survey_regT2, aes(y = SBSOD_score, x = LERC)) +
  geom_point(color = palette_sbfd[5]) + geom_smooth(method = "lm", color = palette_sbfd[5]) +
  ggtitle("SBSOD and Left ERC Volume") +
  xlab("Left ERC Volume (mm^3)") + ylab("SBSOD")  # indiv subfield
ggplot(data = data_survey_regT2, aes(y = SBSOD_score, x = RERC)) +
  geom_point(color = palette_sbfd[5]) + geom_smooth(method = "lm", color = palette_sbfd[5]) +
  ggtitle("SBSOD and Right ERC Volume") +
  xlab("Right ERC Volume (mm^3)") + ylab("SBSOD")  # indiv subfield
# SOT
ggplot(data = data_survey_regT2, aes(y = SOT_score, x = LPRC)) +
  geom_point(color = palette_sbfd[6]) + geom_smooth(method = "lm", color = palette_sbfd[6]) +
  ggtitle("SOT Score and Left PRC Volume") +
  xlab("Left PRC Volume (mm^3)") + ylab("SOT Score")  # indiv subfield
# Hand
ggplot(data = data_survey_regT2, aes(y = Hand_score, x = LERC)) +
  geom_point(color = palette_sbfd[5]) + geom_smooth(method = "lm", color = palette_sbfd[5]) +
  ggtitle("Handedness Score and Left ERC Volume") +
  xlab("Left ERC Volume (mm^3)") + ylab("Handedness Score")  # indiv subfield
ggplot(data = data_survey_regT2, aes(y = Hand_score, x = RpHC)) +
  geom_point(color = palette_sbfd[9]) + geom_smooth(method = "lm", color = palette_sbfd[9]) +
  ggtitle("Handedness Score and Right Post. HC Volume") +
  xlab("Right Posterior HC Volume (mm^3)") + ylab("Handedness Score")  # indiv subfield
# VG
ggplot(data = data_survey_regT2, aes(y = VG_score, x = LPRC)) +
  geom_point(color = palette_sbfd[6]) + geom_smooth(method = "lm", color = palette_sbfd[6]) +
  ggtitle("Video Game Score and Left PRC Volume") +
  xlab("Left PRC Volume (mm^3)") + ylab("Video Game Score")  # indiv subfield
ggplot(data = data_survey_regT2, aes(y = VG_score, x = RpHC)) +
  geom_point(color = palette_sbfd[9]) + geom_smooth(method = "lm", color = palette_sbfd[9]) +
  ggtitle("Video Game Score and Right Post. HC Volume") +
  xlab("Right Posterior HC Volume (mm^3)") + ylab("Video Game Score")  # indiv subfield
ggplot(data = data_survey_regT2, aes(y = VG_score, x = RPRC)) +
  geom_point(color = palette_sbfd[6]) + geom_smooth(method = "lm", color = palette_sbfd[6]) +
  ggtitle("Video Game Score and Right PRC Volume") +
  xlab("Right PRC Volume (mm^3)") + ylab("Video Game Score")  # indiv subfield

#### ------------- Section 8F: Full Reg-T2 Subfield Thickness + Paper/Pencil Task or Survey (n=24) ------------- ####
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

# Data
data_survey_tT2w <- merge(data_survey_scores[,1:8], data_tT2w, by = "sub_id") %>% na.omit()
dim(data_survey_tT2w)

# Partial correlation tables between manual regT2 subfield thickness and survey scores (n=24)
survey_dv <- c("RM_score", "SBSOD_score", "SOT_score", "Hand_score", "VG_hr", "VG_yr", "VG_score")
for (dv in survey_dv) {
  r <- NULL;
  
  for (subfd_name in subfields_tLRregT2) { 
    subfield <- paste(subfd_name, "_pcor", sep = "")
    PC_tsubfield_pcor <- pcor.test(pull(data_survey_tT2w[subfd_name]), data_survey_tT2w[dv], 
                                   c(data_survey_tT2w$sex, data_survey_tT2w$age, data_survey_tT2w$tiv), method = "pearson")
    assign(subfd_name, PC_tsubfield_pcor)
    r <- rbind(r, round(PC_tsubfield_pcor$estimate, digits = 2), round(PC_tsubfield_pcor$p.value, digits = 5))
    # print(paste(i, "estimate is:", round(PC_tsubfield_pcor$estimate, digits = 2), 
    #             "and p-value is:", round(PC_tsubfield_pcor$p.value, digits = 3))) 
  } # ex. tLCA1_pcor
  
  print(dv)
  print(corr_mat_5sub(r))
}

# SBSOD
ggplot(data = data_survey_tT2w, aes(y = SBSOD_score, x = tLERC)) +
  geom_point(color = palette_sbfd[5]) + geom_smooth(method = "lm", color = palette_sbfd[5]) +
  ggtitle("SBSOD and Left ERC Thickness") +
  xlab("Normalized Thickness Ratio") + ylab("SBSOD")  # indiv subfield
ggplot(data = data_survey_tT2w, aes(y = SBSOD_score, x = tRERC)) +
  geom_point(color = palette_sbfd[5]) + geom_smooth(method = "lm", color = palette_sbfd[5]) +
  ggtitle("SBSOD and Right ERC Thickness") +
  xlab("Normalized Thickness Ratio") + ylab("SBSOD")  # indiv subfield
# SOT
ggplot(data = data_survey_tT2w, aes(y = SOT_score, x = tLPRC)) +
  geom_point(color = palette_sbfd[6]) + geom_smooth(method = "lm", color = palette_sbfd[6]) +
  ggtitle("SOT Score and Left PRC Thickness") +
  xlab("Normalized Thickness Ratio") + ylab("SOT Score")  # indiv subfield
ggplot(data = data_survey_tT2w, aes(y = SOT_score, x = tRPRC)) +
  geom_point(color = palette_sbfd[6]) + geom_smooth(method = "lm", color = palette_sbfd[6]) +
  ggtitle("SOT Score and Right PRC Thickness") +
  xlab("Normalized Thickness Ratio") + ylab("SOT Score")  # indiv subfield
# Hand
ggplot(data = data_survey_tT2w, aes(y = Hand_score, x = tLpHC)) +
  geom_point(color = palette_sbfd[9]) + geom_smooth(method = "lm", color = palette_sbfd[9]) +
  ggtitle("Handedness Score and Left Post. HC Thickness") +
  xlab("Normalized Thickness Ratio") + ylab("Handedness Score")  # indiv subfield
ggplot(data = data_survey_tT2w, aes(y = Hand_score, x = tLERC)) +
  geom_point(color = palette_sbfd[5]) + geom_smooth(method = "lm", color = palette_sbfd[5]) +
  ggtitle("Handedness Score and Left ERC Thickness") +
  xlab("Normalized Thickness Ratio") + ylab("Handedness Score")  # indiv subfield
ggplot(data = data_survey_tT2w, aes(y = Hand_score, x = tRPRC)) +
  geom_point(color = palette_sbfd[6]) + geom_smooth(method = "lm", color = palette_sbfd[6]) +
  ggtitle("Handedness Score and Right PRC Thickness") +
  xlab("Normalized Thickness Ratio") + ylab("Handedness Score")  # indiv subfield
# VG
ggplot(data = data_survey_tT2w, aes(y = VG_score, x = tLPHC)) +
  geom_point(color = palette_sbfd[7]) + geom_smooth(method = "lm", color = palette_sbfd[7]) +
  ggtitle("Video Game Score and Left PHC Volume") +
  xlab("Normalized Thickness Ratio") + ylab("Video Game Score")  # indiv subfield
ggplot(data = data_survey_tT2w, aes(y = VG_score, x = tRPRC)) +
  geom_point(color = palette_sbfd[6]) + geom_smooth(method = "lm", color = palette_sbfd[6]) +
  ggtitle("Video Game Score and Right PRC Volume") +
  xlab("Normalized Thickness Ratio") + ylab("Video Game Score")  # indiv subfield
