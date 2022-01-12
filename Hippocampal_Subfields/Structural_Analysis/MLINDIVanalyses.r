# Author: Alina Tu
# Contact: alinat2@uci.edu
# Last Updated: 1/12/2022
# About: This script is the structural analysis of the Maze Learning Individual Differences (MLINDIV) project,
#        specifically looking at young adult hippocampal subfield volumes/thicknesses and their relationship
#        with navigation ability.

# Before running analyses in the following sections, run everything in the RUN FIRST section.

# Note: I recommend collapsing/expanding sections using the small arrows on the left pane next to the line numbers.

#### ------------------------------------------------ RUN FIRST ------------------------------------------------ ####
# Getting started [PACKAGES AND FUNCTIONS]
install.packages("readxl")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("ggcorrplot")
install.packages("stringr")
install.packages("ppcor", repos='http://cran.us.r-project.org')
install.packages("Hmisc")

library(readxl)
library(ggplot2)
library(dplyr)
library(corrplot)
library(ggcorrplot)
library(stringr)
library(ppcor)
library(Hmisc)
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
data <- read_excel("ASHS_volumes.xlsx", sheet = 2)
  hres_sub_id <- c(81:83, 85, 86, 88, 89, 92:96, 98:103, 106:113) # includes sub-092 (no propcorrect)
  data$`Spatial Neuro ID` <- hres_sub_id
  nrow(data) == 26
data_regT2 <- read_excel("ASHS_volumes.xlsx", sheet = 1)
  data_regT2$`Spatial Neuro ID` <- hres_sub_id # includes sub-092
  nrow(data_regT2) == 26
data_UCSB <- read_excel("YoungHipp_SubfieldVolumes.xlsx")
data_full <- read_excel("MLINDIV_Subject_Run_Info.xlsx") # full subject log
data_survey <- read_excel_allsheets("RA_mlindiv_surveys.xlsx") # n=113 for questionnaires

# Define custom data
data_full_clean <- data_full[-c(114:125),] # n=113 with demographics/propcorrect

full_propcorrect <- data_full_clean$`Proportion Correct (new version)`
full_sex <- data_full_clean$Sex
data_survey_scores <- data_survey$`Summary Scores`
data_survey_scores <- data_survey_scores[,1:8]
data_survey_scores$propcorrect <- full_propcorrect
data_survey_scores$sex <- full_sex

post_cutoff <- c(which(data$post_cutoff %in% "TRUE")) # index of sub with posterior cutoffs
data_cut <- data[-c(8, post_cutoff),] # n=13, 7 subfield volumes with no posterior cutoffs & removed sub-092
  nrow(data_cut) == 13

  # Define new numeric sex variable
  sex_to_numeric <- function(df) {
    sex <- as.character((strsplit(gsub("[{}]","",df$`sex`), split = ",")))
    sex[sex == "M"] <- 1
    sex[sex == "F"] <- 0
    df$sex <- as.numeric(sex) #25
  }
  
  data$sex = sex_to_numeric(data) #25
  data_cut$sex = sex_to_numeric(data_cut) #13
  data_regT2$sex = sex_to_numeric(data_regT2) #12
  
data_tHC_25 <- data[-c(8),] %>% 
  mutate(tLCA1 = LCA1/sLCA1, tLCA23 = LCA23/sLCA23, tLDG = LDG/sLDG, tLSUB = LSUB/sLSUB,
         tRCA1 = RCA1/sRCA1, tRCA23 = RCA23/sRCA23, tRDG = RDG/sRDG, tRSUB = RSUB/sRSUB) %>%
  dplyr::select("Spatial Neuro ID","sex","age","propcorrect","tm_pathefficiencies","tLCA1","tLCA23","tLDG",
                "tLSUB","tRCA1","tRCA23","tRDG","tRSUB","tiv") # n=25, 4 HC subfield thickness

data_tHC_26 <- data %>% 
  mutate(tLCA1 = LCA1/sLCA1, tLCA23 = LCA23/sLCA23, tLDG = LDG/sLDG, tLSUB = LSUB/sLSUB,
         tRCA1 = RCA1/sRCA1, tRCA23 = RCA23/sRCA23, tRDG = RDG/sRDG, tRSUB = RSUB/sRSUB) %>%
  dplyr::select("Spatial Neuro ID","sex","age","propcorrect","tm_pathefficiencies","tLCA1","tLCA23","tLDG",
                "tLSUB","tRCA1","tRCA23","tRDG","tRSUB","tiv") # n=25, 4 HC subfield thickness

data_tMTL <- data_cut %>% mutate(tLERC = LERC/sLERC, tLPRC = LPRC/sLPRC, tLPHC = LPHC/sLPHC, 
                                 tRERC = RERC/sRERC, tRPRC = RPRC/sRPRC, tRPHC = RPHC/sRPHC) %>%
  dplyr::select("Spatial Neuro ID","sex","age","propcorrect","tm_pathefficiencies","tLERC","tLPRC","tLPHC",
                "tRERC","tRPRC","tRPHC","tiv") # n=13, 3 other MTL subfield thickness

data_regT2 <- data_regT2 %>% tidyr::drop_na(LaHC, LpHC, LERC, LPRC, LPHC, RaHC, RpHC, RERC, RPRC, RPHC) # n=12

data_tT2w <- data_regT2 %>%
  mutate(tLaHC = LaHC/sLaHC, tLpHC = LpHC/sLpHC, tLERC = LERC/sLERC, tLPRC = LPRC/sLPRC, tLPHC = LPHC/sLPHC, 
         tRaHC = RaHC/sRaHC, tRpHC = RpHC/sRpHC, tRERC = RERC/sRERC, tRPRC = RPRC/sRPRC, tRPHC = RPHC/sRPHC) %>%
  dplyr::select("Spatial Neuro ID","sex","age","propcorrect","tm_pathefficiencies","tLaHC","tLpHC",
                "tLERC","tLPRC","tLPHC","tRaHC","tRpHC","tRERC","tRPRC","tRPHC","tiv") # n=12, all 5 thickness

data_tMTL_tT2w <- merge(data_tMTL, data_tT2w, all=TRUE) %>% 
  dplyr::select("Spatial Neuro ID","sex","age","propcorrect","tLERC","tLPRC","tLPHC","tRERC","tRPRC","tRPHC","tiv")

data_MTL_T2w <- merge(data_cut, data_regT2, all=TRUE) %>%
  dplyr::select("Spatial Neuro ID","sex","age","propcorrect","LERC","LPRC","LPHC","RERC","RPRC","RPHC","tiv")

# Define relevant variables
subfields_LR <- c("LCA1", "RCA1", "LCA23", "RCA23", "LDG", "RDG", "LSUB", "RSUB",
                  "LERC", "RERC", "LPRC", "RPRC", "LPHC", "RPHC")
subfields_LR_apHC <- c("LaHC", "RaHC", "LpHC", "RpHC", "LERC", "RERC", "LPRC", "RPRC", "LPHC", "RPHC")

# Compare TIV values between UCI and UCSB
ucsb <- filter(data_UCSB, ...1 >= 81 & ...1 <= 113)
ucsb <- ucsb[-c(4,9,10,16,23),]
cor.test(data$tiv, ucsb$TIV) # r=.9863713, p<2.2e-16

# Color palettes + graph themes
library(tidyr)
# Colorblind-friendly palette (https://venngage.com/blog/color-blind-friendly-palette/)
palette <- c("#a1caec", "#fdbc7a", "#cfcfcf", "#5e9ed0", "#ff800e", "#ababab", "#0469a6", "#c45400", "#898989",
             "#595959") # light blue/orange/gray, medium blue/orange/gray, dark blue/orange/gray, darker gray
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

#### ------------------------- Section 1: Questionnaires + Proportion Correct (n~100) -------------------------- ####
## Function that plots sex differences for correlation between X questionnaire and proportion correct
ggplot_survey_MF <- function(data, score, propcorrect, title, xlab) {
  ggplot(data = data, aes(x = score, y = propcorrect, # color by sex
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

## Video Game
data_VG <- data_survey_scores %>% 
  dplyr::select("sub_id", "VG_hr", "VG_yr", "VG_score", "propcorrect", "sex")

cor.test(data_VG[,2], data_VG[,5], na.action = na.omit)
cor.test(data_VG[,3], data_VG[,5], na.action = na.omit) 
cor.test(data_VG[,4], data_VG[,5], na.action = na.omit) 

theme_set(theme1)
ggplot(data = data_VG, aes(x = data_VG[,2], y = data_VG[,5])) + 
  geom_point(color = palette[10]) + geom_smooth(method = "lm", se = FALSE, color = palette[10]) +
  ggtitle("Maze Accuracy and Video Game Experience") +
  xlab("Video Game Experience (hr/wk score)") + ylab("Proportion Correct") + ylim(0, 1)
ggplot(data = data_VG, aes(x = data_VG[,3], y = data_VG[,5])) + 
  geom_point(color = palette[10]) + geom_smooth(method = "lm", se = FALSE, color = palette[10]) +
  ggtitle("Maze Accuracy and Video Game Experience") +
  xlab("Video Game Experience (years)") + ylab("Proportion Correct") + ylim(0, 1)
ggplot(data = data_VG, aes(x = data_VG[,4], y = data_VG[,5])) + 
  geom_point(color = palette[10]) + geom_smooth(method = "lm", color = palette[10]) +
  ggtitle("Maze Accuracy and Video Game Experience") +
  xlab("Overall Video Game Experience Score") + ylab("Proportion Correct") + ylim(0, 1)

ggplot_survey_MF(data_VG, data_VG[,4], data_VG[,5], 
                 "Sex Differences in Maze Accuracy and Video Game Experience", "Overall Video Game Experience Score")

data_VG_rm <- data_VG
data_VG_rm <- data_VG_rm[-c(34),] # remove sub-034: age 25 and 25 yrs of playing video games

cor.test(data_VG_rm[,3], data_VG_rm[,5], na.action = na.omit) # r=.5314872, p=5.005e-08, n=90 -> no difference

## Road Map
data_RM <- data_survey_scores %>% 
  dplyr::select("sub_id", "RM_score", "propcorrect", "sex")

cor.test(data_RM[,2], data_RM[,3], na.action = na.omit) 

ggplot(data = data_RM, aes(x = data_RM[,2], y = data_RM[,3])) + 
  geom_point(color = palette[10]) + geom_smooth(method = "lm", color = palette[10]) +
  ggtitle("Maze Accuracy and Road Map Task") +
  xlab("Total Correct Turns in Road Map Task") + ylab("Proportion Correct") + ylim(0, 1)

ggplot_survey_MF(data_RM, data_RM[,2], data_RM[,3], 
                 "Sex Differences in Maze Accuracy and Road Map Task", "Total Correct Turns in Road Map Task")

## SBSOD
data_SBSOD <- data_survey_scores %>% 
  dplyr::select("sub_id", "SBSOD_score", "propcorrect", "sex")

cor.test(data_SBSOD[,2], data_SBSOD[,3], na.action = na.omit) 

ggplot(data = data_SBSOD, aes(x = data_SBSOD[,2], y = data_SBSOD[,3])) + 
  geom_point(color = palette[10]) + geom_smooth(method = "lm", color = palette[10]) +
  ggtitle("Maze Accuracy and SB Sense of Direction Scale (SBSOD)") +
  xlab("SBSOD Score") + ylab("Proportion Correct") + ylim(0, 1)

ggplot_survey_MF(data_SBSOD, data_SBSOD[,2], data_SBSOD[,3], 
                 "Sex Differences in Maze Accuracy and SBSOD", "SBSOD Score")

## Handedness
data_hand <- data_survey_scores %>% 
  dplyr::select("sub_id", "Hand_score", "propcorrect", "sex")

cor.test(data_hand[,2], data_hand[,3], na.action = na.omit) 

## Spatial Orientation
data_SOT <- data_survey_scores %>% 
  dplyr::select("sub_id", "SOT_score", "propcorrect", "sex")

cor.test(data_SOT[,2], data_SOT[,3], na.action = na.omit) 

ggplot(data = data_SOT, aes(x = data_SOT[,2], y = data_SOT[,3])) +
  geom_point(color = palette[10]) + geom_smooth(method = "lm", color = palette[10]) +
  ggtitle("Maze Accuracy and Spatial Orientation Task (SOT)") +
  xlab("SOT Angular Error (degrees)") + ylab("Proportion Correct") + ylim(0, 1)

ggplot_survey_MF(data_SOT, data_SOT[,2], data_SOT[,3], 
                 "Sex Differences in Maze Accuracy and SOT", "SOT Angular Error (degrees)")

#### ------------------------- Section 2: Questionnaires Summary Demographics (n~100) -------------------------- ####
## Function that plots sex differences for correlation between questionnaires and proportion correct
ggplot_survey_MF_bar <- function(data, sex, score, title, ylab) {
  data %>%
    ggplot(aes(x = sex, y = score, col = sex, fill = sex, group = sex)) + # color by sex
    geom_violin(show.legend = FALSE) + # geom_boxplot
    scale_x_discrete(na.translate = FALSE) +
    scale_color_manual(values = c(palette[4], palette[5]), na.translate = F) +
    scale_fill_manual(values = c(palette[4], palette[5])) +
    ggtitle(title) +
    xlab("Biological Sex") + ylab(ylab) +
    labs(color = "Biological Sex") +
    theme(legend.position = "bottom",
          legend.direction = "horizontal",
          legend.box = "horizontal")
}

## Video Game ###### NEED TO FIGUREE OUTTT
summary(data_VG[,2])
ggplot_survey_MF_bar(data_VG, sex, data_VG[,2], 
                     "Sex Differences in Hours/Week Playing Video Games", "Video Game Experience (hr/wk score)")
ggplot_survey_MF_bar(data_VG, sex, data_VG[,3], 
                     "Sex Differences in Years Playing Video Games", "Video Game Experience (years)")
ggplot_survey_MF_bar(data_VG, sex, data_VG[,4], 
                     "Sex Differences in Video Game Experience", "Overall Video Game Experience Score")

## Road Map
ggplot_survey_MF_bar(data_RM, sex, data_RM[,2], 
                     "Sex Differences in Road Map Task", "Total Correct Turns in Road Map Task")

## SBSOD
ggplot_survey_MF_bar(data_SBSOD, sex, data_SBSOD[,2], 
                     "Sex Differences in Santa Barbara Sense of Direction Scale (SBSOD)", "SBSOD Score")

## Hand
ggplot_survey_MF_bar(data_hand, sex, data_hand[,2], 
                     "Sex Differences in Handedness", "Handedness Score")

## SOT
ggplot_survey_MF_bar(data_SOT, sex, data_SOT[,2], 
                     "Sex Differences in Spatial Orientation Task (SOT)", "SOT Angular Error (degrees)")

#### ---------------------- Section 3: Total Volumes (summed) + Proportion Correct (n=25) ---------------------- ####
# Testing null hypotheses
# Define custom data
data_total_vol <- merge(data_cut, data_regT2, all=TRUE) %>% 
  dplyr::select("Spatial Neuro ID", "LaHC", "RaHC", "LpHC", "RpHC", "LCA1", "RCA1", "LCA23", "RCA23", "LDG", "RDG",  
                "LSUB", "RSUB", "LERC", "RERC", "LPHC", "RPHC", "LPRC", "RPRC", "sex", "age", "propcorrect", "tiv")
data_total_vol[is.na(data_total_vol)] <- 0
data_total_vol <- data_total_vol %>%
  mutate(HC = LaHC + RaHC + LpHC + RpHC + LCA1 + RCA1 + LCA23 + RCA23 + LDG + RDG + LSUB + RSUB, 
         MTL = HC + LERC + RERC + LPRC + RPRC + LPHC + RPHC) %>%
  mutate(tiv_mm3 = tiv*1000000) %>%
  mutate(HC_tiv = HC/tiv_mm3, MTL_tiv = MTL/tiv_mm3) %>% 
  dplyr::select("Spatial Neuro ID", "sex", "age", "propcorrect", "tiv", "HC", "MTL", "HC_tiv", "MTL_tiv")

# Partial correlation between total (HC and MTL) volumes and propcorrect (n=25)
pcor.test(data_total_vol$propcorrect, data_total_vol$HC, 
          c(data_total_vol$sex, data_total_vol$age, data_total_vol$tiv), method = "pearson") 
pcor.test(data_total_vol$propcorrect, data_total_vol$MTL, 
          c(data_total_vol$sex, data_total_vol$age, data_total_vol$tiv), method = "pearson")

ggplot(data = data_total_vol, aes(y = propcorrect, x = HC)) + 
  geom_point(color = palette[4]) + geom_smooth(method = "lm", color = palette[4]) +
  ggtitle("Maze Accuracy and Total HC Volume") + xlab("Total Hippocampal (HC) Volume (mm^3)") +
  ylab("Proportion Correct") + ylim(0, 1)
ggplot(data = data_total_vol, aes(y = propcorrect, x = MTL)) + 
  geom_point(color = palette[7]) + geom_smooth(method = "lm", color = palette[7]) +
  ggtitle("Maze Accuracy and Total MTL Volume") + xlab("Total Medial Temporal Lobe (MTL) Volume (mm^3)") +
  ylab("Proportion Correct") + ylim(0, 1)

# Partial correlation between ratio of total volumes and propcorrect (n=25)
pcor.test(data_total_vol$propcorrect, data_total_vol$HC_tiv, 
          c(data_total_vol$sex, data_total_vol$age), method = "pearson") 
pcor.test(data_total_vol$propcorrect, data_total_vol$MTL_tiv, 
          c(data_total_vol$sex, data_total_vol$age), method = "pearson")

ggplot(data = data_total_vol, aes(y = propcorrect, x = HC_tiv)) +
  geom_point(color = palette[4]) + geom_smooth(method = "lm", color = palette[4])+
  ggtitle("Maze Accuracy and Ratio between HC Volumes and TIV") +
  xlab("Ratio between HC Volume and Total Hippocampal Volume (TIV)") + ylab("Proportion Correct") + ylim(0, 1)
ggplot(data = data_total_vol, aes(y = propcorrect, x = MTL_tiv)) +
  geom_point(color = palette[7]) + geom_smooth(method = "lm", color = palette[7])+
  ggtitle("Maze Accuracy and Ratio between MTL Volumes and TIV") +
  xlab("Ratio between MTL Volume and Total Hippocampal Volume (TIV)") + ylab("Proportion Correct") + ylim(0, 1)

#### --------------- Section 4: *No cut-off* HR-T2 Subfield Volumes + Proportion Correct (n=13) ---------------- ####
# Create function: correlation table for all 7 subfields
subfields_all <- c("CA1", "CA2/3", "DG", "SUB", "ERC", "PRC", "PHC")
corr_mat_allsub <- function(r_mat) {
  corr_mat_allsub <- matrix(unlist(t(r_mat)), byrow=T, 7, 4)
  colnames(corr_mat_allsub) <- c("Left (r)", "p", "Right (r)", "p")
  rownames(corr_mat_allsub) <- subfields_all
  return(corr_mat_allsub)
}

# Partial correlation table between no cutoff subfield volumes and propcorrect
r <- NULL;

for (i in subfields_LR) { 
  subfield_name <- paste(i, "_pcor", sep = "")
  PC_subfield_pcor <- pcor.test(pull(data_cut[i]), data_cut$propcorrect, 
                                c(data_cut$sex, data_cut$age, data_cut$tiv), method = "pearson")
  assign(subfield_name, PC_subfield_pcor)
  r <- rbind(r, round(PC_subfield_pcor$estimate, digits = 2), round(PC_subfield_pcor$p.value, digits = 3))
  # print(paste(i, "estimate is:", round(PC_subfield_pcor$estimate, digits = 2), 
  #             "and p-value is:", round(PC_subfield_pcor$p.value, digits = 3))) 
} # ex. LCA1_pcor

corr_mat_allsub(r)

# data_cut in long format
col_order <- c("Spatial Neuro ID", "sex", "age", "propcorrect",
               "LCA1", "LCA23", "LDG", "LSUB", "LERC", "LPRC", "LPHC",
               "RCA1", "RCA23", "RDG", "RSUB", "RERC", "RPRC", "RPHC", "tiv")
data_cut_long <- data_cut[, col_order] %>%
  pivot_longer(cols = LCA1:RPHC, names_to = "roi", values_to = "roi_volume") %>%
  mutate(sex = as.factor(sex)) # raw subfield volume in mm^3

# Plots
theme_set(theme2)
subfields_facet <- c("L CA1", "L CA2/3", "L DG", "L SUB", "L ERC", "L PRC", "L PHC",
                     "R CA1", "R CA2/3", "R DG", "R SUB", "R ERC", "R PRC", "R PHC")

library(tidyr)
data_cut_long$roi <- label_value(rep(subfields_facet, 13))
data_cut_plot <- data_cut_long %>%
  mutate(roi_f = factor(roi, levels=subfields_facet)) %>%
  ggplot(aes(y = propcorrect, 
             x = roi_volume, 
             group = factor(roi_f))) +
  geom_smooth(method = "lm", color = palette[9]) + 
  geom_point(color = palette[9]) +
  facet_wrap(~factor(roi_f), 
             nrow = 2, scales = "free_x", drop = TRUE) +
  scale_y_continuous(name = "Proportion Correct",
                     breaks = seq(0, 1, 0.25), limits = c(0, 1.25), sec.axis = dup_axis()) + 
  scale_x_continuous(name = "Hippocampal Subfield Volume (mm^3)") +
  theme(panel.spacing.x = unit(0.2, units = "inches"),
        axis.text.x = element_text(size = 8))
data_cut_plot # proportion correct and subfield volume

data_cut_plot_MF <- data_cut_long %>%
  mutate(roi_f = factor(roi, levels=subfields_facet)) %>%
  ggplot(aes(y = propcorrect, 
             x = roi_volume, 
             color = factor(sex, 
                            levels = c("0", "1"), 
                            labels = c("Female", "Male")), 
             group = factor(roi_f))) +
  geom_point() +
  geom_smooth(method = "lm", aes(group = sex, fill = factor(sex,
                                                            levels = c("0", "1")))) + 
  scale_color_manual(values = c(palette[4], palette[5])) +
  scale_fill_manual(values = c(palette[4], palette[5]), 
                    name = "Biological Sex",
                    labels = c("Female", "Male")) +
  facet_wrap(~factor(roi_f), nrow = 2, scales = "free_x") +
  scale_y_continuous(name = "Proportion Correct", 
                     breaks = seq(0, 1, 0.25), limits = c(0, 1.25), sec.axis = dup_axis()) + 
  scale_x_continuous(name = "Hippocampal Subfield Volume (mm^3)") +
  labs(color = "Biological Sex", group = "roi") + 
  # theme(legend.position = "bottom",
  #       legend.direction = "horizontal",
  #       legend.box = "horizontal")
  theme(legend.position = "right",
        legend.direction = "vertical",
        legend.box = "vertical")
data_cut_plot_MF # sex differences in proportion correct and subfield volume

theme_set(theme1)
ggplot(data = data_cut, aes(y = propcorrect, x = LCA1)) + 
  geom_point(color = palette[9]) + geom_smooth(method = "lm", color = palette[9])+
  ggtitle("Maze Accuracy and Left CA1 Volume") + 
  xlab("Left CA1 Volumes") + ylab("Proportion Correct") + ylim(0,1) # indiv subfield

#### ----------------- Section 5: *No cut-off* HR-T2 Subfield Volumes + Questionnaires (n=13) ------------------ ####
## Video Game
data_VG <- data_survey_scores[hres_sub_id,] %>%          # Note: using the same variable name from Section 1 + 2
  dplyr::select("sub_id", "VG_hr", "VG_yr", "VG_score")
VG_na <- c(which(is.na(data_VG$VG_hr) %in% "TRUE"))

r_VG_hr <- NULL;
r_VG_yr <- NULL;
r_VG_score <- NULL;

for (i in subfields_LR) { 
  data_ques <- data.frame(data_cut[i], data_cut[,c("sex","age","tiv")], data_VG[,c("VG_hr","VG_yr","VG_score")])
  data_ques <- data_ques[-(VG_na),] #12
  colnames(data_ques) <- c(i, "sex", "age", "tiv", "VG_hr", "VG_yr", "VG_score")

  VG_hr_sub_pcor <- pcor.test(data_ques[i], data_ques$VG_hr, data_ques[c("sex","age","tiv")], method = "pearson")
  VG_yr_sub_pcor <- pcor.test(data_ques[i], data_ques$VG_yr, data_ques[c("sex","age","tiv")], method = "pearson")
  VG_score_sub_pcor <- pcor.test(data_ques[i], data_ques$VG_score, data_ques[c("sex","age","tiv")], method = "pearson")
  
  r_VG_hr <- rbind(r_VG_hr, round(VG_hr_sub_pcor$estimate, digits = 2), round(VG_hr_sub_pcor$p.value, digits = 3))
  r_VG_yr <- rbind(r_VG_yr, round(VG_yr_sub_pcor$estimate, digits = 2), round(VG_yr_sub_pcor$p.value, digits = 3))
  r_VG_score <- rbind(r_VG_score, round(VG_score_sub_pcor$estimate, digits = 2), round(VG_score_sub_pcor$p.value, digits = 3))
}

corr_mat_allsub(r_VG_hr)
corr_mat_allsub(r_VG_yr)
corr_mat_allsub(r_VG_score)

## Road Map
data_RM <- data_survey_scores[hres_sub_id,] %>% 
  dplyr::select("sub_id", "RM_score")
RM_na <- c(which(is.na(data_RM$RM_score) %in% "TRUE"))

r_RM <- NULL;

for (i in subfields_LR) { 
  data_ques <- data.frame(data_cut[i], data_cut[,c("sex","age","tiv")], data_RM$RM_score)
  # data_ques <- data_ques[-(RM_na),] #12
  colnames(data_ques) <- c(i, "sex", "age", "tiv", "RM_score")
  
  RM_sub_pcor <- pcor.test(data_ques[i], data_ques$RM_score, data_ques[c("sex","age","tiv")], method = "pearson")
  r_RM <- rbind(r_RM, round(RM_sub_pcor$estimate, digits = 2), round(RM_sub_pcor$p.value, digits = 3))
}

corr_mat_allsub(r_RM)

## SBSOD
data_SBSOD <- data_survey_scores[hres_sub_id,] %>% 
  dplyr::select("sub_id", "SBSOD_score")
SBSOD_na <- c(which(is.na(data_SBSOD$SBSOD_score) %in% "TRUE"))

r_SBSOD <- NULL;

for (i in subfields_LR) {
  data_ques <- data.frame(data_cut[i], data_cut[,c("sex","age","tiv")], data_SBSOD$SBSOD_score)
  data_ques <- data_ques[-(SBSOD_na),] #11
  colnames(data_ques) <- c(i, "sex", "age", "tiv", "SBSOD_score")
  
  SBSOD_sub_pcor <- pcor.test(data_ques[i], data_ques$SBSOD_score, data_ques[c("sex","age","tiv")], method = "pearson")
  r_SBSOD <- rbind(r_SBSOD, round(SBSOD_sub_pcor$estimate, digits = 2), round(SBSOD_sub_pcor$p.value, digits = 3))
}

corr_mat_allsub(r_SBSOD)

## Handedness
data_hand <- data_survey_scores[hres_sub_id,] %>% 
  dplyr::select("sub_id", "Hand_score")
hand_na <- c(which(is.na(data_hand$Hand_score) %in% "TRUE"))

r_hand <- NULL;

for (i in subfields_LR) { 
  data_ques <- data.frame(data_cut[i], data_cut[,c("sex","age","tiv")], data_hand$Hand_score)
  data_ques <- data_ques[-(hand_na),] #11
  colnames(data_ques) <- c(i, "sex", "age", "tiv", "hand_score")
  
  hand_sub_pcor <- pcor.test(data_ques[i], data_ques$hand_score, data_ques[c("sex","age","tiv")], method = "pearson")
  r_hand <- rbind(r_hand, round(hand_sub_pcor$estimate, digits = 2), round(hand_sub_pcor$p.value, digits = 3))
}

corr_mat_allsub(r_hand)

## Spatial Orientation
data_SOT <- data_survey_scores[hres_sub_id,] %>% 
  dplyr::select("sub_id", "SOT_score")
SOT_na <- c(which(is.na(data_SOT$SOT_score) %in% "TRUE"))

r_SOT <- NULL;

for (i in subfields_LR) { 
  data_ques <- data.frame(data_cut[i], data_cut[,c("sex","age","tiv")], data_SOT$SOT_score)
  # data_ques <- data_ques[-(SOT_na),] #12
  colnames(data_ques) <- c(i, "sex", "age", "tiv", "SOT_score")
  
  SOT_sub_pcor <- pcor.test(data_ques[i], data_ques$SOT_score, data_ques[c("sex","age","tiv")], method = "pearson")
  r_SOT <- rbind(r_SOT, round(SOT_sub_pcor$estimate, digits = 2), round(SOT_sub_pcor$p.value, digits = 3))
}

corr_mat_allsub(r_SOT)

#### ------- Section 6: *No cut-off* Subfield Volumes (as a ratio over tiv) + Proportion Correct (n=13) -------- ####
# Define custom data (tiv) for 13 subjects without cutoffs
data_cut_tiv <- data_cut[, col_order] %>%
  mutate(tiv = tiv * 1000000) %>%
  mutate(LCA1_tiv = LCA1/tiv, RCA1_tiv = RCA1/tiv,
         LCA23_tiv = LCA23/tiv, RCA23_tiv = RCA23/tiv,
         LDG_tiv = LDG/tiv, RDG_tiv = RDG/tiv,
         LSUB_tiv = LSUB/tiv, RSUB_tiv = RSUB/tiv,
         LERC_tiv = LERC/tiv, RERC_tiv = RERC/tiv,
         LPRC_tiv = LPRC/tiv, RPRC_tiv = RPRC/tiv,
         LPHC_tiv = LPHC/tiv, RPHC_tiv = RPHC/tiv,) %>%
  dplyr::select('Spatial Neuro ID', sex, age, propcorrect, LCA1_tiv:RPHC_tiv)
# Partial correlation table between no cutoff subfield volumes (RATIO) and propcorrect
subfields_LR_tiv <- NULL;

for (i in subfields_LR) {
  subfields_LR_tiv <- rbind(subfields_LR_tiv, paste(i, "_tiv", sep = ""))
}
subfields_LR_tiv <- subfields_LR_tiv[,1]

r <- NULL;

for (i in subfields_LR_tiv) { 
  subfield_name <- paste(i, "_pcor", sep = "")
  PC_subfield_tiv_pcor <- pcor.test(pull(data_cut_tiv[i]), data_cut_tiv$propcorrect, 
                                    c(data_cut_tiv$sex, data_cut_tiv$age), method = "pearson")
  assign(subfield_name, PC_subfield_tiv_pcor)
  r <- rbind(r, round(PC_subfield_tiv_pcor$estimate, digits = 2), round(PC_subfield_tiv_pcor$p.value, digits = 3))
  # print(paste(i, "estimate is:", round(PC_subfield_tiv_pcor$estimate, digits = 2), 
  #             "and p-value is:", round(PC_subfield_tiv_pcor$p.value, digits = 3))) 
} # ex. LCA1_tiv_pcor

corr_mat_allsub(r)

# data_cut in long format
col_order <- c("Spatial Neuro ID", "sex", "age", "propcorrect",
               "LCA1", "LCA23", "LDG", "LSUB", "LERC", "LPRC", "LPHC",
               "RCA1", "RCA23", "RDG", "RSUB", "RERC", "RPRC", "RPHC", "tiv")
data_cut_long_tiv <- data_cut_tiv %>%
  pivot_longer(cols = LCA1_tiv:RPHC_tiv, names_to = "roi_tiv", values_to = "roi_volume_tiv") %>%
  mutate(tiv_norm = normalize(roi_volume_tiv)) %>%
  mutate(sex = as.factor(sex)) # ratio between subfield volume and tiv

# Plots
theme_set(theme2)
data_cut_long_tiv$roi_tiv <- label_value(rep(subfields_facet, 13))
data_cut_plot_tiv <- data_cut_long_tiv %>%
  mutate(roi_f = factor(roi_tiv, levels = subfields_facet)) %>%
  ggplot(aes(y = propcorrect, 
             x = roi_volume_tiv, 
             group = factor(roi_f))) +
  geom_smooth(method = "lm", color=palette[9]) + 
  geom_point(color = palette[9]) +
  facet_wrap(~factor(roi_f), 
             nrow = 2, scales = "free_x", drop = TRUE) +
  scale_y_continuous(name = "Proportion Correct",
                     breaks = seq(0, 1, 0.25), limits = c(0, 1.25), sec.axis = dup_axis()) + 
  scale_x_continuous(name = "Ratio between Subfield Volume and TIV") +
  theme(panel.spacing.x = unit(0.2, units = "inches"),
        axis.text.x = element_text(size = 8))
data_cut_plot_tiv # proportion correct and ratio between subfield volume and tiv

data_cut_plot_tiv_norm <- data_cut_long_tiv %>%
  mutate(roi_f = factor(roi_tiv, levels = subfields_facet)) %>%
  ggplot(aes(y = propcorrect, 
             x = tiv_norm, 
             group = factor(roi_f))) +
  geom_smooth(method = "lm", color=palette[3]) + 
  geom_point(color = palette[4]) +
  facet_wrap(~factor(roi_f), 
             nrow = 2, scales = "free_x", drop = TRUE) +
  scale_y_continuous(name = "Proportion Correct",
                     breaks = seq(0, 1, 0.25), limits = c(0, 1.25), sec.axis = dup_axis()) + 
  scale_x_continuous(name = "Normalized Subfield Volume") +
  theme(panel.spacing.x = unit(0.2, units = "inches"),
        axis.text.x = element_text(size = 8))
data_cut_plot_tiv_norm # proportion correct and normalized ratio

data_cut_plot_tiv_MF <- data_cut_long_tiv %>%
  mutate(roi_f = factor(roi_tiv, levels = subfields_facet)) %>%
  ggplot(aes(y = propcorrect, 
             x = roi_volume_tiv, 
             color = factor(sex, 
                            levels = c("0", "1"), 
                            labels = c("Female", "Male")), 
             group = factor(roi_f))) +
  geom_point() +
  geom_smooth(method = "lm", aes(group = sex, fill = factor(sex,
                                                            levels = c("0", "1")))) + 
  scale_color_manual(values = c(palette[4], palette[5])) +
  scale_fill_manual(values = c(palette[4], palette[5]), 
                    name = "Biological Sex",
                    labels = c("Female", "Male")) +
  facet_wrap(~factor(roi_f), nrow = 2, scales = "free_x") +
  scale_y_continuous(name = "Proportion Correct", 
                     breaks = seq(0, 1, 0.25), limits = c(0, 1.25), sec.axis = dup_axis()) + 
  scale_x_continuous(name = "Ratio between Subfield Volume and TIV") +
  labs(color = "Biological Sex", group = "roi") + 
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        legend.box = "horizontal")
data_cut_plot_tiv_MF # sex differences in proportion correct and ratio between subfield volume and tiv

data_cut_plot_tiv_norm_MF <- data_cut_long_tiv %>%
  mutate(roi_f = factor(roi_tiv, levels = subfields_facet)) %>%
  ggplot(aes(y = propcorrect, 
             x = tiv_norm, 
             color = factor(sex, 
                            levels = c("0", "1"), 
                            labels = c("Female", "Male")), 
             group = factor(roi_f))) +
  geom_point() +
  geom_smooth(method = "lm", aes(group = sex, fill = factor(sex,
                                                            levels = c("0", "1")))) + 
  scale_color_manual(values = c(palette[4], palette[5])) +
  scale_fill_manual(values = c(palette[4], palette[5]), 
                    name = "Biological Sex",
                    labels = c("Female", "Male")) +
  facet_wrap(~factor(roi_f), nrow = 2, scales = "free_x") +
  scale_y_continuous(name = "Proportion Correct", 
                     breaks = seq(0, 1, 0.25), limits = c(0, 1.25), sec.axis = dup_axis()) + 
  scale_x_continuous(name = "Normalized Subfield Volume") +
  labs(color = "Biological Sex", group = "roi") + 
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        legend.box = "horizontal")
data_cut_plot_tiv_norm_MF # sex differences in proportion correct and normalized ratio

#### ----------------------- Section 7: Reg-T2 Subfield Volumes + Proportion Correct (n=12) -------------------- ####
# Define custom data for 12 subjects with regT2 subfield volumes
data_T2w <- data_regT2 %>% tidyr::drop_na(LaHC, LpHC, LERC, LPRC, LPHC, RaHC, RpHC, RERC, RPRC, RPHC) %>%
  dplyr::select("Spatial Neuro ID","sex","age","propcorrect","tm_pathefficiencies","LaHC","LpHC",
                "LERC","LPRC","LPHC","RaHC","RpHC","RERC","RPRC","RPHC","tiv")

# Create function: correlation table for 5 subfields
subfields_T2w <- c("aHC", "pHC", "ERC", "PRC", "PHC")
corr_mat_T2wsub <- function(r_mat) {
  corr_mat_T2wsub <- matrix(unlist(t(r_mat)), byrow=T, 5, 4)
  colnames(corr_mat_T2wsub) <- c("Left (r)", "p", "Right (r)", "p")
  rownames(corr_mat_T2wsub) <- subfields_T2w
  return(corr_mat_T2wsub)
}

# Partial correlation table between subfield volume and propcorrect
subfields_T2w_LR <- c("LaHC", "RaHC", "LpHC", "RpHC", "LERC", "RERC", "LPRC", "RPRC", "LPHC", "RPHC")

r <- NULL;

for (i in subfields_T2w_LR) { 
  subfield_name <- paste(i, "_pcor", sep = "")
  PC_subfield_pcor <- pcor.test(pull(data_T2w[i]), data_T2w$propcorrect, 
                                c(data_T2w$sex, data_T2w$age, data_T2w$tiv), method = "pearson")
  assign(subfield_name, PC_subfield_pcor)
  r <- rbind(r, round(PC_subfield_pcor$estimate, digits = 2), round(PC_subfield_pcor$p.value, digits = 3))
  # print(paste(i, "estimate is:", round(PC_subfield_pcor$estimate, digits = 2),
  #             "and p-value is:", round(PC_subfield_pcor$p.value, digits = 3)))
} # ex. LaHC_pcor

corr_mat_T2wsub(r)

# data_tMTL in long format
data_T2w_long <- data_T2w %>%
  pivot_longer(cols = LaHC:RPHC, names_to = "roi", values_to = "roi_thickness") %>%
  mutate(sex = as.factor(sex))

# Plots
subfields_T2w_facet <- c("L aHC", "L pHC", "L ERC", "L PRC", "L PHC",
                         "R aHC", "R pHC", "R ERC", "R PRC", "R PHC")
data_T2w_long$roi <- label_value(rep(subfields_T2w_facet, 12))
data_T2w_plot <- data_T2w_long %>%
  mutate(roi_f = factor(roi, levels = subfields_T2w_facet)) %>%
  ggplot(aes(y = propcorrect, 
             x = roi_thickness, 
             group = factor(roi_f))) +
  geom_smooth(method = "lm", color=palette[9]) + 
  geom_point(color = palette[9]) +
  facet_wrap(~factor(roi_f), 
             nrow = 2, scales = "free_x", drop = TRUE) +
  scale_y_continuous(name = "Proportion Correct",
                     breaks = seq(0, 1, 0.25), limits = c(0, 1.25), sec.axis = dup_axis()) + 
  scale_x_continuous(name = "Subfield Volume (mm^3)") +
  theme(panel.spacing.x = unit(0.2, units = "inches"),
        axis.text.x = element_text(size = 8))
data_T2w_plot

data_T2w_plot_MF <- data_T2w_long %>%
  mutate(roi_f = factor(roi, levels = subfields_T2w_facet)) %>%
  ggplot(aes(y = propcorrect, 
             x = roi_thickness,
             color = factor(sex, 
                            levels = c("0", "1"), 
                            labels = c("Female", "Male")), 
             group = factor(roi_f))) +
  geom_point() +
  geom_smooth(method = "lm", aes(group = sex, fill = factor(sex,
                                                            levels = c("0", "1")))) + 
  scale_color_manual(values = c(palette[4], palette[5])) +
  scale_fill_manual(values = c(palette[4], palette[5]), 
                    name = "Biological Sex",
                    labels = c("Female", "Male")) +
  facet_wrap(~factor(roi_f), nrow = 2, scales = "free_x") +
  scale_y_continuous(name = "Proportion Correct", 
                     breaks = seq(0, 1, 0.25), limits = c(0, 1.25), sec.axis = dup_axis()) + 
  scale_x_continuous(name = "Subfield Volume (mm^3)") +
  labs(color = "Biological Sex", group = "roi") + 
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        legend.box = "horizontal") +
  theme(panel.spacing.x = unit(0.2, units = "inches"),
        axis.text.x = element_text(size = 8))
data_T2w_plot_MF

ggplot(data = data_regT2, aes(y = propcorrect, x = RaHC)) + 
  geom_point() + geom_smooth(method= "lm", color='#e080de') +
  ggtitle("Right aHC Thickness and Proportion Correct") + 
  xlab("Right aHC Thicknesses") + ylab("Proportion Correct") + ylim(0, 1) #indiv subfield
ggplot(data = data_regT2, aes(y = propcorrect, x = LPRC)) + 
  geom_point() + geom_smooth(method= "lm", color='#e080de') +
  ggtitle("Left PRC Thickness and Proportion Correct") + 
  xlab("Left PRC Thicknesses") + ylab("Proportion Correct") + ylim(0, 1) #indiv subfield
ggplot(data = data_regT2, aes(y = propcorrect, x = RaHC, 
                              color = factor(sex, 
                                             levels = c("0", "1"), 
                                             labels = c("Female", "Male")))) +
  geom_point() + geom_smooth(method = "lm", se = FALSE) +
  ggtitle("Right aHC Thickness and Proportion Correct") +
  xlab("Right aHC Thicknesses") + ylab("Proportion Correct") + ylim(0, 1) +
  labs(color = "Biological Sex") #color by sex
ggplot(data = data_regT2, aes(y = propcorrect, x = LPHC, 
                              color = factor(sex, 
                                             levels = c("0", "1"), 
                                             labels = c("Female", "Male")))) +
  geom_point() + geom_smooth(method = "lm", se = FALSE) +
  ggtitle("Left PRC Thickness and Proportion Correct") +
  xlab("Left PRC Thicknesses") + ylab("Proportion Correct") + ylim(0, 1) +
  labs(color = "Biological Sex") #color by sex

#### ----------------- Section 8: Other MTL Volumes (hr-T2 + regT2) + Proportion Correct (n=25) ---------------- ####
# Create function: correlation table for 3 other MTL subfields
subfields_MTL <- c("ERC", "PRC", "PHC")
corr_mat_MTLsub <- function(r_mat) {
  corr_mat_MTLsub <- matrix(unlist(t(r_mat)), byrow=T, 3, 4)
  colnames(corr_mat_MTLsub) <- c("Left (r)", "p", "Right (r)", "p")
  rownames(corr_mat_MTLsub) <- subfields_MTL
  return(corr_mat_MTLsub)
}

# Partial correlation table between other MTL volumes (hr-T2 + regT2) and propcorrect
subfields_MTL_LR <- c("LERC", "RERC", "LPRC", "RPRC", "LPHC", "RPHC")

r <- NULL;

for (i in subfields_MTL_LR) { 
  subfield_name <- paste(i, "_pcor", sep = "")
  PC_subfield_pcor <- pcor.test(pull(data_MTL_T2w[i]), data_MTL_T2w$propcorrect, 
                                 c(data_MTL_T2w$sex, data_MTL_T2w$age, data_MTL_T2w$tiv), method = "pearson")
  assign(subfield_name, PC_subfield_pcor)
  r <- rbind(r, round(PC_subfield_pcor$estimate, digits = 2), round(PC_subfield_pcor$p.value, digits = 3))
  # print(paste(i, "estimate is:", round(PC_subfield_pcor$estimate, digits = 2),
  #             "and p-value is:", round(PC_subfield_pcor$p.value, digits = 3)))
} # ex. tLaHC_pcor

corr_mat_MTLsub(r)

# data_tMTL in long format
data_MTL_T2w_long <- data_MTL_T2w %>%
  pivot_longer(cols = LERC:RPHC, names_to = "roi", values_to = "roi_volume") %>%
  mutate(sex = as.factor(sex))

# Plots
subfields_MTL_facet <- c("L ERC", "L PRC", "L PHC",
                         "R ERC", "R PRC", "R PHC")
data_MTL_T2w_long$roi <- label_value(rep(subfields_MTL_facet, 25))
data_MTL_T2w_plot <- data_MTL_T2w_long %>%
  mutate(roi_f = factor(roi, levels = subfields_MTL_facet)) %>%
  ggplot(aes(y = propcorrect, 
             x = roi_volume, 
             group = factor(roi_f))) +
  geom_smooth(method = "lm", color=palette[9]) + 
  geom_point(color = palette[9]) +
  facet_wrap(~factor(roi_f), 
             nrow = 2, scales = "free_x", drop = TRUE) +
  scale_y_continuous(name = "Proportion Correct",
                     breaks = seq(0, 1, 0.25), limits = c(0, 1.25), sec.axis = dup_axis()) + 
  scale_x_continuous(name = "Other MTL Subfield Volume (mm^3)") +
  theme(panel.spacing.x = unit(0.2, units = "inches"),
        axis.text.x = element_text(size = 8))
data_MTL_T2w_plot

data_MTL_T2w_plot_MF <- data_MTL_T2w_long %>%
  mutate(roi_f = factor(roi, levels = subfields_MTL_facet)) %>%
  ggplot(aes(y = propcorrect, 
             x = roi_volume,
             color = factor(sex, 
                            levels = c("0", "1"), 
                            labels = c("Female", "Male")), 
             group = factor(roi_f))) +
  geom_point() +
  geom_smooth(method = "lm", aes(group = sex, fill = factor(sex,
                                                            levels = c("0", "1")))) + 
  scale_color_manual(values = c(palette[4], palette[5])) +
  scale_fill_manual(values = c(palette[4], palette[5]), 
                    name = "Biological Sex",
                    labels = c("Female", "Male")) +
  facet_wrap(~factor(roi_f), nrow = 2, scales = "free_x") +
  scale_y_continuous(name = "Proportion Correct", 
                     breaks = seq(0, 1, 0.25), limits = c(0, 1.25), sec.axis = dup_axis()) + 
  scale_x_continuous(name = "Other MTL Subfield Volume (mm^3)") +
  labs(color = "Biological Sex", group = "roi") +
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        legend.box = "horizontal")
  # theme(legend.position = "right",
  #       legend.direction = "vertical",
  #       legend.box = "vertical")
  # theme(panel.spacing.x = unit(0.2, units = "inches"),
  #       axis.text.x = element_text(size = 8))
data_MTL_T2w_plot_MF

#### --------------------------- Section 9: HC Thickness + Proportion Correct (n=25) --------------------------- ####
# Create function: correlation table for 4 HC subfields
subfields_HC <- c("CA1", "CA2/3", "DG", "SUB")
corr_mat_HCsub <- function(r_mat) {
  corr_mat_HCsub <- matrix(unlist(t(r_mat)), byrow=T, 4, 4)
  colnames(corr_mat_HCsub) <- c("Left (r)", "p", "Right (r)", "p")
  rownames(corr_mat_HCsub) <- subfields_HC
  return(corr_mat_HCsub)
}

# Partial correlation table between subfield thickness and propcorrect
subfields_HC_LR <- c("LCA1", "RCA1", "LCA23", "RCA23", "LDG", "RDG", "LSUB", "RSUB")

subfields_tHC_LR <- NULL;

for (i in subfields_HC_LR) {
  subfields_tHC_LR <- rbind(subfields_tHC_LR, paste("t", i, sep = ""))
}
subfields_tHC_LR <- subfields_tHC_LR[,1]

r <- NULL;

for (i in subfields_tHC_LR) { 
  subfield_name <- paste(i, "_pcor", sep = "")
  PC_tsubfield_pcor <- pcor.test(pull(data_tHC_25[i]), data_tHC_25$propcorrect, 
                                    c(data_tHC_25$sex, data_tHC_25$age, data_tHC_25$tiv), method = "pearson")
  assign(subfield_name, PC_tsubfield_pcor)
  r <- rbind(r, round(PC_tsubfield_pcor$estimate, digits = 2), round(PC_tsubfield_pcor$p.value, digits = 3))
  # print(paste(i, "estimate is:", round(PC_tsubfield_pcor$estimate, digits = 2), 
  #             "and p-value is:", round(PC_tsubfield_pcor$p.value, digits = 3))) 
} # ex. tLCA1_pcor

corr_mat_HCsub(r)

# data_tHC_25 in long format
data_tHC_long <- data_tHC_25 %>%
  pivot_longer(cols = tLCA1:tRSUB, names_to = "roi", values_to = "roi_thickness") %>%
  mutate(sex = as.factor(sex))

# Plots
subfields_HC_facet <- c("L CA1", "L CA2/3", "L DG", "L SUB",
                        "R CA1", "R CA2/3", "R DG", "R SUB")
data_tHC_long$roi <- label_value(rep(subfields_HC_facet, 25))
data_tHC_plot <- data_tHC_long %>%
  mutate(roi_f = factor(roi, levels = subfields_HC_facet)) %>%
  ggplot(aes(y = propcorrect, 
             x = roi_thickness, 
             group = factor(roi_f))) +
  geom_smooth(method = "lm", color=palette[9]) + 
  geom_point(color = palette[9]) +
  facet_wrap(~factor(roi_f), 
             nrow = 2, scales = "free_x", drop = TRUE) +
  scale_y_continuous(name = "Proportion Correct",
                     breaks = seq(0, 1, 0.25), limits = c(0, 1.25), sec.axis = dup_axis()) + 
  scale_x_continuous(name = "Hippocampal Subfield Thickness (mm^3)") +
  theme(panel.spacing.x = unit(0.2, units = "inches"),
        axis.text.x = element_text(size = 8))
data_tHC_plot

data_tHC_plot_MF <- data_tHC_long %>%
  mutate(roi_f = factor(roi, levels = subfields_HC_facet)) %>%
  ggplot(aes(y = propcorrect, 
             x = roi_thickness,
             color = factor(sex, 
                            levels = c("0", "1"), 
                            labels = c("Female", "Male")), 
             group = factor(roi_f))) +
  geom_point() +
  geom_smooth(method = "lm", aes(group = sex, fill = factor(sex,
                                                            levels = c("0", "1")))) + 
  scale_color_manual(values = c(palette[6], palette[2])) +
  scale_fill_manual(values = c(palette[6], palette[2]), 
                    name = "Biological Sex",
                    labels = c("Female", "Male")) +
  facet_wrap(~factor(roi_f), nrow = 2, scales = "free_x") +
  scale_y_continuous(name = "Proportion Correct", 
                     breaks = seq(0, 1, 0.25), limits = c(0, 1.25), sec.axis = dup_axis()) + 
  scale_x_continuous(name = "Hippocampal Subfield Thickness (mm^3)") +
  labs(color = "Biological Sex", group = "roi") + 
  # theme(legend.position = "bottom",
  #       legend.direction = "horizontal",
  #       legend.box = "horizontal")
  theme(legend.position = "right",
        legend.direction = "vertical",
        legend.box = "vertical")
  # theme(panel.spacing.x = unit(0.2, units = "inches"),
  #       axis.text.x = element_text(size = 8))
data_tHC_plot_MF

ggplot(data = data_tHC_25, aes(y = propcorrect, x = tLDG)) + 
  geom_point() + geom_smooth(method= "lm", color='#5a7cb3') +
  ggtitle("Left DG Thickness and Proportion Correct") + 
  xlab("Left DG Thicknesses") + ylab("Proportion Correct") + ylim(0, 1) #indiv subfield
ggplot(data = data_tHC_25, aes(y = propcorrect, x = tLDG, 
                            color = factor(sex, 
                                           levels = c("0", "1"), 
                                           labels = c("Female", "Male")))) +
  geom_point() + geom_smooth(method = "lm", se = FALSE) +
  ggtitle("Left DG Thicknesses and Proportion Correct") +
  xlab("Left DG Thicknesses") + ylab("Proportion Correct") + ylim(0, 1) +
  labs(color = "Biological Sex") #color by sex

#### ---------------------------- Section 10: HC Thickness + Questionnaires (n=25) ----------------------------- ####
## !!! Note: currently missing tiv for sub-092 !!!
## Video Game
data_VG <- data_survey_scores[hres_sub_id,] %>% 
  dplyr::select("sub_id", "VG_hr", "VG_yr", "VG_score")

VG_na <- c(which(is.na(data_VG$VG_hr) %in% "TRUE"))

r_VG_hr <- NULL;
r_VG_yr <- NULL;
r_VG_score <- NULL;

for (i in subfields_tHC_LR) { 
  data_ques <- data.frame(data_tHC_26[i], data_tHC_26[,c("sex","age","tiv")], data_VG[,c("VG_hr","VG_yr","VG_score")])
  data_ques <- data_ques[-(VG_na),] #22
  colnames(data_ques) <- c(i, "sex", "age", "tiv", "VG_hr", "VG_yr", "VG_score")
  
  # VG_hr_sub_pcor <- pcor.test(data_ques[i], data_ques$VG_hr, data_ques[c("sex","age","tiv")], method = "pearson")
  # VG_yr_sub_pcor <- pcor.test(data_ques[i], data_ques$VG_yr, data_ques[c("sex","age","tiv")], method = "pearson")
  # VG_score_sub_pcor <- pcor.test(data_ques[i], data_ques$VG_score, data_ques[c("sex","age","tiv")], method = "pearson")
  # 
  # r_VG_hr <- rbind(r_VG_hr, round(VG_hr_sub_pcor$estimate, digits = 2), round(VG_hr_sub_pcor$p.value, digits = 3))
  # r_VG_yr <- rbind(r_VG_yr, round(VG_yr_sub_pcor$estimate, digits = 2), round(VG_yr_sub_pcor$p.value, digits = 3))
  # r_VG_score <- rbind(r_VG_score, round(VG_score_sub_pcor$estimate, digits = 2), round(VG_score_sub_pcor$p.value, digits = 3))
}

corr_mat_HCsub(r_VG_hr)
corr_mat_HCsub(r_VG_yr)
corr_mat_HCsub(r_VG_score)

## Road Map
data_RM <- data_survey_scores[hres_sub_id,] %>% 
  dplyr::select("sub_id", "RM_score")

RM_na <- c(which(is.na(data_RM$RM_score) %in% "TRUE"))

r_RM <- NULL;

for (i in subfields_tHC_LR) { 
  data_ques <- data.frame(data_tHC_26[i], data_tHC_26[,c("sex","age","tiv")], data_RM$RM_score)
  data_ques <- data_ques[-(RM_na),] #24
  colnames(data_ques) <- c(i, "sex", "age", "tiv", "RM_score")
  
  RM_sub_pcor <- pcor.test(data_ques[i], data_ques$RM_score, data_ques[c("sex","age","tiv")], method = "pearson")
  r_RM <- rbind(r_RM, round(RM_sub_pcor$estimate, digits = 2), round(RM_sub_pcor$p.value, digits = 3))
}

corr_mat_HCsub(r_RM)

## SBSOD
data_SBSOD <- data_survey_scores[hres_sub_id,] %>% 
  dplyr::select("sub_id", "SBSOD_score")

SBSOD_na <- c(which(is.na(data_SBSOD$SBSOD_score) %in% "TRUE"))

r_SBSOD <- NULL;

for (i in subfields_tHC_LR) { 
  data_ques <- data.frame(data_tHC_26[i], data_tHC_26[,c("sex","age","tiv")], data_SBSOD$SBSOD_score)
  data_ques <- data_ques[-(SBSOD_na),] #24
  colnames(data_ques) <- c(i, "sex", "age", "tiv", "SBSOD_score")
  
  SBSOD_sub_pcor <- pcor.test(data_ques[i], data_ques$SBSOD_score, data_ques[c("sex","age","tiv")], method = "pearson")
  r_SBSOD <- rbind(r_SBSOD, round(SBSOD_sub_pcor$estimate, digits = 2), round(SBSOD_sub_pcor$p.value, digits = 3))
}

corr_mat_HCsub(r_SBSOD)

## Handedness
data_hand <- data_survey_scores[hres_sub_id,] %>% 
  dplyr::select("sub_id", "Hand_score")

hand_na <- c(which(is.na(data_hand$Hand_score) %in% "TRUE"))

r_hand <- NULL;

for (i in subfields_tHC_LR) { 
  data_ques <- data.frame(data_tHC_26[i], data_tHC_26[,c("sex","age","tiv")], data_hand$Hand_score)
  data_ques <- data_ques[-(hand_na),] #23
  colnames(data_ques) <- c(i, "sex", "age", "tiv", "hand_score")
  
  hand_sub_pcor <- pcor.test(data_ques[i], data_ques$hand_score, data_ques[c("sex","age","tiv")], method = "pearson")
  r_hand <- rbind(r_hand, round(hand_sub_pcor$estimate, digits = 2), round(hand_sub_pcor$p.value, digits = 3))
}

corr_mat_HCsub(r_hand)

## Spatial Orientation
data_SOT <- data_survey_scores[hres_sub_id,] %>% 
  dplyr::select("sub_id", "SOT_score")

SOT_na <- c(which(is.na(data_SOT$SOT_score) %in% "TRUE"))

r_SOT <- NULL;

for (i in subfields_tHC_LR) { 
  data_ques <- data.frame(data_tHC_26[i], data_tHC_26[,c("sex","age","tiv")], data_SOT$SOT_score)
  data_ques <- data_ques[-(SOT_na),] #23
  colnames(data_ques) <- c(i, "sex", "age", "tiv", "SOT_score")
  
  SOT_sub_pcor <- pcor.test(data_ques[i], data_ques$SOT_score, data_ques[c("sex","age","tiv")], method = "pearson")
  r_SOT <- rbind(r_SOT, round(SOT_sub_pcor$estimate, digits = 2), round(SOT_sub_pcor$p.value, digits = 3))
}

corr_mat_HCsub(r_SOT)

#### ----------------------- Section 11: Other MTL Thickness + Proportion Correct (n=13) ----------------------- ####
# Create function: correlation table for 3 other MTL subfields
subfields_MTL <- c("ERC", "PRC", "PHC")
corr_mat_MTLsub <- function(r_mat) {
  corr_mat_MTLsub <- matrix(unlist(t(r_mat)), byrow=T, 3, 4)
  colnames(corr_mat_MTLsub) <- c("Left (r)", "p", "Right (r)", "p")
  rownames(corr_mat_MTLsub) <- subfields_MTL
  return(corr_mat_MTLsub)
}

# Partial correlation table between subfield thickness and propcorrect
subfields_MTL_LR <- c("LERC", "RERC", "LPRC", "RPRC", "LPHC", "RPHC")

subfields_tMTL_LR <- NULL;

for (i in subfields_MTL_LR) {
  subfields_tMTL_LR <- rbind(subfields_tMTL_LR, paste("t", i, sep = ""))
}
subfields_tMTL_LR <- subfields_tMTL_LR[,1]

r <- NULL;

for (i in subfields_tMTL_LR) { 
  subfield_name <- paste(i, "_pcor", sep = "")
  PC_tsubfield_pcor <- pcor.test(pull(data_tMTL[i]), data_tMTL$propcorrect, 
                                 c(data_tMTL$sex,data_tMTL$age,data_tMTL$tiv), method = "pearson")
  assign(subfield_name, PC_tsubfield_pcor)
  r <- rbind(r, round(PC_tsubfield_pcor$estimate, digits = 2), round(PC_tsubfield_pcor$p.value, digits = 3))
  # print(paste(i, "estimate is:", round(PC_tsubfield_pcor$estimate, digits = 2), 
  #             "and p-value is:", round(PC_tsubfield_pcor$p.value, digits = 3))) 
} # ex. tLERC_pcor

corr_mat_MTLsub(r)

# data_tMTL in long format
data_tMTL_long <- data_tMTL %>%
  pivot_longer(cols = tLERC:tRPHC, names_to = "roi", values_to = "roi_thickness") %>%
  mutate(sex = as.factor(sex))

# Plots
subfields_MTL_facet <- c("L ERC", "L PRC", "L PHC",
                         "R ERC", "R PRC", "R PHC")
data_tMTL_long$roi <- label_value(rep(subfields_MTL_facet, 13))
data_tMTL_plot <- data_tMTL_long %>%
  mutate(roi_f = factor(roi, levels = subfields_MTL_facet)) %>%
  ggplot(aes(y = propcorrect, 
             x = roi_thickness, 
             group = factor(roi_f))) +
  geom_smooth(method = "lm", color=palette[9]) + 
  geom_point(color = palette[9]) +
  facet_wrap(~factor(roi_f), 
             nrow = 2, scales = "free_x", drop = TRUE) +
  scale_y_continuous(name = "Proportion Correct",
                     breaks = seq(0, 1, 0.25), limits = c(0, 1.25), sec.axis = dup_axis()) + 
  scale_x_continuous(name = "Other MTL Subfield Thickness (mm^3)") +
  theme(panel.spacing.x = unit(0.2, units = "inches"),
        axis.text.x = element_text(size = 8))
data_tMTL_plot

data_tMTL_plot_MF <- data_tMTL_long %>%
  mutate(roi_f = factor(roi, levels = subfields_MTL_facet)) %>%
  ggplot(aes(y = propcorrect, 
             x = roi_thickness,
             color = factor(sex, 
                            levels = c("0", "1"), 
                            labels = c("Female", "Male")), 
             group = factor(roi_f))) +
  geom_point() +
  geom_smooth(method = "lm", aes(group = sex, fill = factor(sex,
                                                            levels = c("0", "1")))) + 
  scale_color_manual(values = c(palette[4], palette[5])) +
  scale_fill_manual(values = c(palette[4], palette[5]), 
                    name = "Biological Sex",
                    labels = c("Female", "Male")) +
  facet_wrap(~factor(roi_f), nrow = 2, scales = "free_x") +
  scale_y_continuous(name = "Proportion Correct", 
                     breaks = seq(0, 1, 0.25), limits = c(0, 1.25), sec.axis = dup_axis()) + 
  scale_x_continuous(name = "Other MTL Subfield Thickness (mm^3)") +
  labs(color = "Biological Sex", group = "roi") + 
  # theme(legend.position = "bottom",
  #       legend.direction = "horizontal",
  #       legend.box = "horizontal") +
  theme(legend.position = "right",
        legend.direction = "vertical",
        legend.box = "vertical")
  # theme(panel.spacing.x = unit(0.2, units = "inches"),
  #       axis.text.x = element_text(size = 8))
data_tMTL_plot_MF

ggplot(data = data_tMTL, aes(y = propcorrect, x = tRPRC)) + 
  geom_point() + geom_smooth(method= "lm", color='#e080de') +
  ggtitle("Right PRC Thickness and Proportion Correct") + 
  xlab("Right PRC Thicknesses") + ylab("Proportion Correct") + ylim(0, 1) #indiv subfield
ggplot(data = data_tMTL, aes(y = propcorrect, x = tRPRC, 
                            color = factor(sex, 
                                           levels = c("0", "1"), 
                                           labels = c("Female", "Male")))) +
  geom_point() + geom_smooth(method = "lm", se = FALSE) +
  ggtitle("Right PRC Thickness and Proportion Correct") +
  xlab("Right PRC Thicknesses") + ylab("Proportion Correct") + ylim(0, 1) +
  labs(color = "Biological Sex") #color by sex

#### ------------------------- Section 12: Other MTL Thickness + Questionnaires (n=13) ------------------------- ####
## Video Game
data_VG <- data_survey_scores[hres_sub_id,] %>% 
  dplyr::select("sub_id", "VG_hr", "VG_yr", "VG_score")

VG_na <- c(which(is.na(data_VG$VG_hr) %in% "TRUE"))

r_VG_hr <- NULL;
r_VG_yr <- NULL;
r_VG_score <- NULL;

for (i in subfields_tMTL_LR) { 
  data_ques <- data.frame(data_tMTL[i], data_tMTL[,c("sex","age","tiv")], data_VG[,c("VG_hr","VG_yr","VG_score")])
  data_ques <- data_ques[-(VG_na),] #12
  colnames(data_ques) <- c(i, "sex", "age", "tiv", "VG_hr", "VG_yr", "VG_score")
  
  VG_hr_sub_pcor <- pcor.test(data_ques[i], data_ques$VG_hr, data_ques[c("sex","age","tiv")], method = "pearson")
  VG_yr_sub_pcor <- pcor.test(data_ques[i], data_ques$VG_yr, data_ques[c("sex","age","tiv")], method = "pearson")
  VG_score_sub_pcor <- pcor.test(data_ques[i], data_ques$VG_score, data_ques[c("sex","age","tiv")], method = "pearson")
  
  r_VG_hr <- rbind(r_VG_hr, round(VG_hr_sub_pcor$estimate, digits = 2), round(VG_hr_sub_pcor$p.value, digits = 3))
  r_VG_yr <- rbind(r_VG_yr, round(VG_yr_sub_pcor$estimate, digits = 2), round(VG_yr_sub_pcor$p.value, digits = 3))
  r_VG_score <- rbind(r_VG_score, round(VG_score_sub_pcor$estimate, digits = 2), round(VG_score_sub_pcor$p.value, digits = 3))
}

corr_mat_MTLsub(r_VG_hr)
corr_mat_MTLsub(r_VG_yr)
corr_mat_MTLsub(r_VG_score)

## Road Map
data_RM <- data_survey_scores[hres_sub_id,] %>% 
  dplyr::select("sub_id", "RM_score")

RM_na <- c(which(is.na(data_RM$RM_score) %in% "TRUE"))

r_RM <- NULL;

for (i in subfields_tMTL_LR) { 
  data_ques <- data.frame(data_tMTL[i], data_tMTL[,c("sex","age","tiv")], data_RM$RM_score)
  # data_ques <- data_ques[-(RM_na),] #12
  colnames(data_ques) <- c(i, "sex", "age", "tiv", "RM_score")
  
  RM_sub_pcor <- pcor.test(data_ques[i], data_ques$RM_score, data_ques[c("sex","age","tiv")], method = "pearson")
  r_RM <- rbind(r_RM, round(RM_sub_pcor$estimate, digits = 2), round(RM_sub_pcor$p.value, digits = 3))
}

corr_mat_MTLsub(r_RM)

## SBSOD
data_SBSOD <- data_survey_scores[hres_sub_id,] %>% 
  dplyr::select("sub_id", "SBSOD_score")

SBSOD_na <- c(which(is.na(data_SBSOD$SBSOD_score) %in% "TRUE"))

r_SBSOD <- NULL;

for (i in subfields_tMTL_LR) { 
  data_ques <- data.frame(data_tMTL[i], data_tMTL[,c("sex","age","tiv")], data_SBSOD$SBSOD_score)
  data_ques <- data_ques[-(SBSOD_na),] #11
  colnames(data_ques) <- c(i, "sex", "age", "tiv", "SBSOD_score")
  
  SBSOD_sub_pcor <- pcor.test(data_ques[i], data_ques$SBSOD_score, data_ques[c("sex","age","tiv")], method = "pearson")
  r_SBSOD <- rbind(r_SBSOD, round(SBSOD_sub_pcor$estimate, digits = 2), round(SBSOD_sub_pcor$p.value, digits = 3))
}

corr_mat_MTLsub(r_SBSOD)

## Handedness
data_hand <- data_survey_scores[hres_sub_id,] %>% 
  dplyr::select("sub_id", "Hand_score")

hand_na <- c(which(is.na(data_hand$Hand_score) %in% "TRUE"))

r_hand <- NULL;

for (i in subfields_tMTL_LR) { 
  data_ques <- data.frame(data_tMTL[i], data_tMTL[,c("sex","age","tiv")], data_hand$Hand_score)
  data_ques <- data_ques[-(hand_na),] #11
  colnames(data_ques) <- c(i, "sex", "age", "tiv", "hand_score")
  
  hand_sub_pcor <- pcor.test(data_ques[i], data_ques$hand_score, data_ques[c("sex","age","tiv")], method = "pearson")
  r_hand <- rbind(r_hand, round(hand_sub_pcor$estimate, digits = 2), round(hand_sub_pcor$p.value, digits = 3))
}

corr_mat_MTLsub(r_hand)

## Spatial Orientation
data_SOT <- data_survey_scores[hres_sub_id,] %>% 
  dplyr::select("sub_id", "SOT_score")

SOT_na <- c(which(is.na(data_SOT$SOT_score) %in% "TRUE"))

r_SOT <- NULL;

for (i in subfields_tMTL_LR) { 
  data_ques <- data.frame(data_tMTL[i], data_tMTL[,c("sex","age","tiv")], data_SOT$SOT_score)
  # data_ques <- data_ques[-(SOT_na),] #12
  colnames(data_ques) <- c(i, "sex", "age", "tiv", "SOT_score")
  
  SOT_sub_pcor <- pcor.test(data_ques[i], data_ques$SOT_score, data_ques[c("sex","age","tiv")], method = "pearson")
  r_SOT <- rbind(r_SOT, round(SOT_sub_pcor$estimate, digits = 2), round(SOT_sub_pcor$p.value, digits = 3))
}

corr_mat_MTLsub(r_SOT)

#### ------------------------ Section 13: Subfield Thickness + Proportion Correct (n=12) ----------------------- ####
# Create function: correlation table for 5 subfields
subfields_T2w <- c("aHC", "pHC", "ERC", "PRC", "PHC")
corr_mat_T2wsub <- function(r_mat) {
  corr_mat_T2wsub <- matrix(unlist(t(r_mat)), byrow=T, 5, 4)
  colnames(corr_mat_T2wsub) <- c("Left (r)", "p", "Right (r)", "p")
  rownames(corr_mat_T2wsub) <- subfields_T2w
  return(corr_mat_T2wsub)
}

# Partial correlation table between subfield thickness and propcorrect
subfields_T2w_LR <- c("LaHC", "RaHC", "LpHC", "RpHC", "LERC", "RERC", "LPRC", "RPRC", "LPHC", "RPHC")

subfields_tT2w_LR <- NULL;

for (i in subfields_T2w_LR) {
  subfields_tT2w_LR <- rbind(subfields_tT2w_LR, paste("t", i, sep = ""))
}
subfields_tT2w_LR <- subfields_tT2w_LR[,1]

r <- NULL;

for (i in subfields_tT2w_LR) { 
  subfield_name <- paste(i, "_pcor", sep = "")
  PC_tsubfield_pcor <- pcor.test(pull(data_tT2w[i]), data_tT2w$propcorrect, 
                                 c(data_tT2w$sex,data_tT2w$age,data_tT2w$tiv), method = "pearson")
  assign(subfield_name, PC_tsubfield_pcor)
  r <- rbind(r, round(PC_tsubfield_pcor$estimate, digits = 2), round(PC_tsubfield_pcor$p.value, digits = 3))
  # print(paste(i, "estimate is:", round(PC_tsubfield_pcor$estimate, digits = 2),
  #             "and p-value is:", round(PC_tsubfield_pcor$p.value, digits = 3)))
} # ex. tLaHC_pcor

corr_mat_T2wsub(r)

# data_tMTL in long format
data_tT2w_long <- data_tT2w %>%
  pivot_longer(cols = tLaHC:tRPHC, names_to = "roi", values_to = "roi_thickness") %>%
  mutate(sex = as.factor(sex))

# Plots
theme_set(theme2)
subfields_T2w_facet <- c("L aHC", "L pHC", "L ERC", "L PRC", "L PHC",
                         "R aHC", "R pHC", "R ERC", "R PRC", "R PHC")
data_tT2w_long$roi <- label_value(rep(subfields_T2w_facet, 12))
data_tT2w_plot <- data_tT2w_long %>%
  mutate(roi_f = factor(roi, levels=subfields_T2w_facet)) %>%
  ggplot(aes(y = propcorrect, 
             x = roi_thickness, 
             group = factor(roi_f))) +
  geom_smooth(method = "lm", color=palette[9]) + 
  geom_point(color = palette[9]) +
  facet_wrap(~factor(roi_f), 
             nrow = 2, scales = "free_x", drop = TRUE) +
  scale_y_continuous(name = "Proportion Correct",
                     breaks = seq(0, 1, 0.25), limits = c(0, 1.25), sec.axis = dup_axis()) + 
  scale_x_continuous(name = "Subfield Thickness (mm^3)") +
  theme(panel.spacing.x = unit(0.2, units = "inches"),
        axis.text.x = element_text(size = 8))
data_tT2w_plot

data_tT2w_plot_MF <- data_tT2w_long %>%
  mutate(roi_f = factor(roi, levels=subfields_T2w_facet)) %>%
  ggplot(aes(y = propcorrect, 
             x = roi_thickness,
             color = factor(sex, 
                            levels = c("0", "1"), 
                            labels = c("Female", "Male")), 
             group = factor(roi_f))) +
  geom_point() +
  geom_smooth(method = "lm", aes(group = sex, fill = factor(sex,
                                                            levels = c("0", "1")))) + 
  scale_color_manual(values = c(palette[4], palette[5])) +
  scale_fill_manual(values = c(palette[4], palette[5]), 
                    name = "Biological Sex",
                    labels = c("Female", "Male")) +
  facet_wrap(~factor(roi_f), nrow = 2, scales = "free_x") +
  scale_y_continuous(name = "Proportion Correct", 
                     breaks = seq(0, 1, 0.25), limits = c(0, 1.25), sec.axis = dup_axis()) + 
  scale_x_continuous(name = "Subfield Thickness (mm^3)") +
  labs(color = "Biological Sex", group = "roi") + 
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        legend.box = "horizontal") +
  theme(panel.spacing.x = unit(0.2, units = "inches"),
        axis.text.x = element_text(size = 8))
data_tT2w_plot_MF

ggplot(data = data_tT2w, aes(y = propcorrect, x = tRaHC)) + 
  geom_point() + geom_smooth(method= "lm", color='#e080de') +
  ggtitle("Right aHC Thickness and Proportion Correct") + 
  xlab("Right aHC Thicknesses") + ylab("Proportion Correct") + ylim(0, 1) #indiv subfield
ggplot(data = data_tT2w, aes(y = propcorrect, x = tLPRC)) + 
  geom_point() + geom_smooth(method= "lm", color='#e080de') +
  ggtitle("Left PRC Thickness and Proportion Correct") + 
  xlab("Left PRC Thicknesses") + ylab("Proportion Correct") + ylim(0, 1) #indiv subfield
ggplot(data = data_tT2w, aes(y = propcorrect, x = tRaHC, 
                             color = factor(sex, 
                                            levels = c("0", "1"), 
                                            labels = c("Female", "Male")))) +
  geom_point() + geom_smooth(method = "lm", se = FALSE) +
  ggtitle("Right aHC Thickness and Proportion Correct") +
  xlab("Right aHC Thicknesses") + ylab("Proportion Correct") + ylim(0, 1) +
  labs(color = "Biological Sex") #color by sex
ggplot(data = data_tT2w, aes(y = propcorrect, x = tLPHC, 
                             color = factor(sex, 
                                            levels = c("0", "1"), 
                                            labels = c("Female", "Male")))) +
  geom_point() + geom_smooth(method = "lm", se = FALSE) +
  ggtitle("Left PRC Thickness and Proportion Correct") +
  xlab("Left PRC Thicknesses") + ylab("Proportion Correct") + ylim(0, 1) +
  labs(color = "Biological Sex") #color by sex

#### --------------- Section 14: Other MTL Thickness (hr-T2 + regT2) + Proportion Correct (n=25) --------------- ####
# Partial correlation table between other MTL thickness (hr-T2 + regT2) and propcorrect
subfields_tMTL_LR

r <- NULL;

for (i in subfields_tMTL_LR) { 
  subfield_name <- paste(i, "_pcor", sep = "")
  PC_tsubfield_pcor <- pcor.test(pull(data_tMTL_tT2w[i]), data_tMTL_tT2w$propcorrect, 
                                 c(data_tMTL_tT2w$sex,data_tMTL_tT2w$age,data_tMTL_tT2w$tiv), method = "pearson")
  assign(subfield_name, PC_tsubfield_pcor)
  r <- rbind(r, round(PC_tsubfield_pcor$estimate, digits = 2), round(PC_tsubfield_pcor$p.value, digits = 3))
  # print(paste(i, "estimate is:", round(PC_tsubfield_pcor$estimate, digits = 2),
  #             "and p-value is:", round(PC_tsubfield_pcor$p.value, digits = 3)))
} # ex. tLaHC_pcor

corr_mat_MTLsub(r)

# data_tMTL in long format
data_tMTL_tT2w_long <- data_tMTL_tT2w %>%
  pivot_longer(cols = tLERC:tRPHC, names_to = "roi", values_to = "roi_thickness") %>%
  mutate(sex = as.factor(sex))

# Plots
subfields_MTL_facet <- c("L ERC", "L PRC", "L PHC",
                         "R ERC", "R PRC", "R PHC")
data_tMTL_tT2w_long$roi <- label_value(rep(subfields_MTL_facet, 25))
data_tMTL_tT2w_plot <- data_tMTL_tT2w_long %>%
  mutate(roi_f = factor(roi, levels=subfields_MTL_facet)) %>%
  ggplot(aes(y = propcorrect, 
             x = roi_thickness, 
             group = factor(roi_f))) +
  geom_smooth(method = "lm", color=palette[9]) + 
  geom_point(color = palette[9]) +
  facet_wrap(~factor(roi_f), 
             nrow = 2, scales = "free_x", drop = TRUE) +
  scale_y_continuous(name = "Proportion Correct",
                     breaks = seq(0, 1, 0.25), limits = c(0, 1.25), sec.axis = dup_axis()) + 
  scale_x_continuous(name = "Other MTL Subfield Thickness (mm^3)") +
  theme(panel.spacing.x = unit(0.2, units = "inches"),
        axis.text.x = element_text(size = 8))
data_tMTL_tT2w_plot

data_tMTL_tT2w_plot_MF <- data_tMTL_tT2w_long %>%
  mutate(roi_f = factor(roi, levels=subfields_T2w_facet)) %>%
  ggplot(aes(y = propcorrect, 
             x = roi_thickness,
             color = factor(sex, 
                            levels = c("0", "1"), 
                            labels = c("Female", "Male")), 
             group = factor(roi_f))) +
  geom_point() +
  geom_smooth(method = "lm", aes(group = sex, fill = factor(sex,
                                                            levels = c("0", "1")))) + 
  scale_color_manual(values = c(palette[4], palette[5])) +
  scale_fill_manual(values = c(palette[4], palette[5]), 
                    name = "Biological Sex",
                    labels = c("Female", "Male")) +
  facet_wrap(~factor(roi_f), nrow = 2, scales = "free_x") +
  scale_y_continuous(name = "Proportion Correct", 
                     breaks = seq(0, 1, 0.25), limits = c(0, 1.25), sec.axis = dup_axis()) + 
  scale_x_continuous(name = "Other MTL Subfield Thickness (mm^3)") +
  labs(color = "Biological Sex", group = "roi") + 
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        legend.box = "horizontal") +
  theme(panel.spacing.x = unit(0.2, units = "inches"),
        axis.text.x = element_text(size = 8))
data_tMTL_tT2w_plot_MF

ggplot(data = data_tMTL_tT2w, aes(y = propcorrect, x = tRaHC)) + 
  geom_point() + geom_smooth(method= "lm", color='#e080de') +
  ggtitle("Right aHC Thickness and Proportion Correct") + 
  xlab("Right aHC Thicknesses") + ylab("Proportion Correct") + ylim(0, 1) #indiv subfield
ggplot(data = data_tMTL_tT2w, aes(y = propcorrect, x = tLPRC)) + 
  geom_point() + geom_smooth(method= "lm", color='#e080de') +
  ggtitle("Left PRC Thickness and Proportion Correct") + 
  xlab("Left PRC Thicknesses") + ylab("Proportion Correct") + ylim(0, 1) #indiv subfield
ggplot(data = data_tMTL_tT2w, aes(y = propcorrect, x = tRaHC, 
                             color = factor(sex, 
                                            levels = c("0", "1"), 
                                            labels = c("Female", "Male")))) +
  geom_point() + geom_smooth(method = "lm", se = FALSE) +
  ggtitle("Right aHC Thickness and Proportion Correct") +
  xlab("Right aHC Thicknesses") + ylab("Proportion Correct") + ylim(0, 1) +
  labs(color = "Biological Sex") #color by sex
ggplot(data = data_tMTL_tT2w, aes(y = propcorrect, x = tLPHC, 
                             color = factor(sex, 
                                            levels = c("0", "1"), 
                                            labels = c("Female", "Male")))) +
  geom_point() + geom_smooth(method = "lm", se = FALSE) +
  ggtitle("Left PRC Thickness and Proportion Correct") +
  xlab("Left PRC Thicknesses") + ylab("Proportion Correct") + ylim(0, 1) +
  labs(color = "Biological Sex") #color by sex

#### --------------------------------------- Section 15: Sex Differences --------------------------------------- ####
# Behavioral/navigational data analysis for HIGH RES VOLUMES (n=25)
data_M <- data %>% filter(sex == "1")
data_F <- data %>% filter(sex == "0")
sd(data_M$propcorrect, na.rm = TRUE)
sd(data_F$propcorrect, na.rm = TRUE)

t.test(data_M$propcorrect, data_F$propcorrect)
t.test(data_M$tm_pathefficiencies, data_F$tm_pathefficiencies)

# Propcorrect data analysis for full data set (n=112)
data_full_M <- data_full_clean %>% filter(Sex == "M") # data for M
data_full_F <- data_full_clean %>% filter(Sex == "F") # data for F

t.test(data_full_M[6], data_full_F[6], na.rm=TRUE)

# Questionnaires
## Video Game
data_VG <- data_survey_scores %>% 
  dplyr::select("sub_id", "sex", "VG_hr", "VG_yr", "VG_score")

data_VG_M <- data_VG %>% filter(sex == "M") # data for M, n=59
data_VG_F <- data_VG %>% filter(sex == "F") # data for F, n=53

t.test(data_VG_M$VG_score, data_VG_F$VG_score, na.rm=TRUE)

## Road Map
data_RM <- data_survey_scores %>% 
  dplyr::select("sub_id", "sex", "RM_score")

data_RM_M <- data_RM %>% filter(sex == "M") # data for M
data_RM_F <- data_RM %>% filter(sex == "F") # data for F

t.test(data_RM_M$RM_score, data_RM_F$RM_score, na.rm=TRUE) 

## SBSOD
data_SBSOD <- data_survey_scores %>% 
  dplyr::select("sub_id", "sex", "SBSOD_score")

data_SBSOD_M <- data_SBSOD %>% filter(sex == "M") # data for M
data_SBSOD_F <- data_SBSOD %>% filter(sex == "F") # data for F

t.test(data_SBSOD_M$SBSOD_score, data_SBSOD_F$SBSOD_score, na.rm=TRUE) # t=-3.889, p=.0001924, df=90.026

## Handedness
data_hand <- data_survey_scores %>% 
  dplyr::select("sub_id", "sex", "Hand_score")

data_hand_M <- data_hand %>% filter(sex == "M") # data for M
data_hand_F <- data_hand %>% filter(sex == "F") # data for F

t.test(data_hand_M$Hand_score, data_hand_F$Hand_score, na.rm=TRUE) # t=-1.9803, p=0.05057, df=94.948

## SOT
data_SOT <- data_survey_scores %>% 
  dplyr::select("sub_id", "sex", "SOT_score")

data_SOT_M <- data_SOT %>% filter(sex == "M") # data for M
data_SOT_F <- data_SOT %>% filter(sex == "F") # data for F

t.test(data_SOT_M$SOT_score, data_SOT_F$SOT_score, na.rm=TRUE) # t=-4.5286, p=1.961e-05, df=83.322

# Proportion correct of specific subfields
data_cut_M <- data_cut %>% filter(sex == "1")
data_cut_F <- data_cut %>% filter(sex == "0")
# LCA23
pcor.test(data_cut_M$LCA23, data_cut_M$propcorrect, c(data_cut_M$age,data_cut_M$tiv), method = "pearson") 
  # M: r=.1121404, p=.7153045, n=14
pcor.test(data_cut_F$LCA23, data_cut_F$propcorrect, c(data_cut_F$age,data_cut_F$tiv), method = "pearson") 
  # F: r=.2837486, p=.3977876, n=12
  # vassarstats signif of diff b/w 2 corre coeff: z=, p= (two-tailed)
# LDG
pcor.test(data_cut_M$LDG, data_cut_M$propcorrect, c(data_cut_M$age,data_cut_M$tiv), method = "pearson") 
  # M: r=.3875189, p=.1907798, n=14
pcor.test(data_cut_F$LDG, data_cut_F$propcorrect, c(data_cut_F$age,data_cut_F$tiv), method = "pearson") 
  # F: r=-.1670075, p=.623562, n=12
  # vassarstats signif of diff b/w 2 corre coeff: z=, p= (two-tailed)

# Thickness?
#LDG
pcor.test(data_M$tLDG, data_M$propcorrect, c(data_M$age,data_M$tiv), method = "pearson")
# males: r=-.148, p=.502, n=12, df=21
pcor.test(data_F$tLDG, data_F$propcorrect, c(data_F$age,data_F$tiv), method = "pearson")
# females: r=.444, p=.026, n=13, df=23
# vassarstats signif of diff b/w 2 corre coeff: z=.09, p=.9283 (two-tailed)
