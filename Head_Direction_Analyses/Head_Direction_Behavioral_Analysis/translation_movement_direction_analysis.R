# path: ~/Desktop/MLINDIV/mvpa/results/translational_movement_direction

library(tidyverse)
library(ggplot2)
library(RColorBrewer)
# Step 1. load data

path = '~/Desktop/MLINDIV/mvpa/results/translational_movement_direction'

exploration <- read_csv(paste0(path,'/translation_movement_explore.csv'))
#exploration_2 <- read_csv('translation_movement_explore_sub5.csv')
#exploration_3 <- read_csv('translation_movement_explore_sub14.csv')

#exploration <- rbind(exploration_1, exploration_2, exploration_3)

exploration_rand <- read_csv(paste0(path,'/translation_movement_explore_rand.csv'))
#exploration_2_rand <- read_csv('translation_movement_explore_rand_sub5.csv')
#exploration_3_rand <- read_csv('translation_movement_explore_rand_sub14.csv')

test <- read_csv(paste0(path,'/translation_movement_test.csv'))
test_rand <- read_csv(paste0(path,'/translation_movement_test_rand.csv'))
# Step 2. calculate mean and standard error for each phase

exploration_rand <-as.data.frame(colMeans(exploration_rand))

exp_mean <- as.data.frame(apply(exploration, 2, mean))
exp_sd <- apply(exploration, 2, sd)
exp_se <- as.data.frame(exp_sd/sqrt(nrow(exploration)))
#write_csv(exploration, paste0(path,'/exploration.csv'))

test_rand <-as.data.frame(colMeans(test_rand))

test_mean <- as.data.frame(apply(test, 2, mean))
test_sd <- apply(test, 2, sd)
test_se <- as.data.frame(test_sd/sqrt(nrow(test)))

mean20 <- cbind(exp_mean, test_mean, exp_se, test_se, exploration_rand, test_rand)
colnames(mean20) <- c('Exploration','Test', 'Explore_se', 'Test_se', 'Explore_rand', 'Test_rand')


# Step 3. T-tests

#exp lore
ex_tha <- t.test(exploration$thalamus, mu = mean20$Explore_rand[2]); ex_tha
ex_rsc <- t.test(exploration$retrosplenial, mu = mean20$Explore_rand[3]); ex_rsc
ex_prc <- t.test(exploration$precuneus, mu = mean20$Explore_rand[4]); ex_prc
ex_ext <- t.test(exploration$extrastriate, mu = mean20$Explore_rand[5]); ex_ext
ex_str <- t.test(exploration$early_visual, mu = mean20$Explore_rand[6]); ex_str
ex_aud <- t.test(exploration$auditory, mu = mean20$Explore_rand[7]); ex_aud

# test
test_tha <- t.test(test$thalamus, mu = mean20$Test_rand[2]); test_tha
test_rsc <- t.test(test$retrosplenial, mu = mean20$Test_rand[3]); test_rsc
test_prc <- t.test(test$precuneus, mu = mean20$Test_rand[4]); test_prc
test_ext <- t.test(test$extrastriate, mu = mean20$Test_rand[5]); test_ext
test_str <- t.test(test$early_visual, mu = mean20$Test_rand[6]); test_str
test_aud <- t.test(test$auditory, mu = mean20$Test_rand[7]); test_aud



# step 4. scatter plots

# add navigation performance to exploration and test dataframes
performance <- read_csv('~/Desktop/MLINDIV/behavioral/MLINDIV_participant_master.csv')

exploration <- merge(exploration, performance, by.x = 'sub', by.y = 'Subject') %>% 
  select('sub', 'thalamus', 'retrosplenial', 'precuneus', 'extrastriate', 'early_visual', 'auditory', 'tm_accuracy')
names(exploration) <- c('sub', 'thalamus', 'retrosplenial', 'precuneus', 'extrastriate', 'early_visual', 'auditory', 'acc')

test <- merge(test, performance, by.x = 'sub', by.y = 'Subject') %>% 
  select('sub', 'thalamus', 'retrosplenial', 'precuneus', 'extrastriate', 'early_visual', 'auditory', 'tm_accuracy')
names(test) <- c('sub', 'thalamus', 'retrosplenial', 'precuneus', 'extrastriate', 'early_visual', 'auditory', 'acc')
# correlation function
corr_eqn <- function(x,y, digits1 = 2, digits2 = 3) {
  result <- cor.test(x, y)
  corr_coef <- round(result$estimate, digits = digits1)
  corr_p <- round(result$p.value, digits = digits2)
  label1 <- paste("italic(r) == ", corr_coef) 
  label2 <- paste("italic(p) == ", corr_p)
  return(c(label1, label2))
}



label_tha_e = corr_eqn(exploration$thalamus, exploration$acc)
label_rsc_e = corr_eqn(exploration$retrosplenial, exploration$acc)
label_prc_e = corr_eqn(exploration$precuneus, exploration$acc)
label_ext_e = corr_eqn(exploration$extrastriate, exploration$acc)
label_str_e = corr_eqn(exploration$early_visual, exploration$acc)
label_aud_e = corr_eqn(exploration$auditory, exploration$acc)

label_tha_t = corr_eqn(test$thalamus, test$acc)
label_rsc_t = corr_eqn(test$retrosplenial, test$acc)
label_prc_t = corr_eqn(test$precuneus, test$acc)
label_ext_t = corr_eqn(test$extrastriate, test$acc)
label_str_t = corr_eqn(test$early_visual, test$acc)
label_aud_t = corr_eqn(test$auditory, test$acc)

colors <- brewer.pal(n = 5, name = "Set2")


tha_e <- ggplot(exploration, aes(acc, thalamus)) + 
  geom_point(color = colors[1]) +
  geom_smooth(method = "lm", color = colors[1], size = 0.5) +
  theme(legend.position=c(0,1), legend.justification=c(0,1))+
  ylim(0,0.4) + ylab('') +  xlab('') +
  labs(title="Thalamus") +
  geom_text(data = exploration, aes(x = 0.8, y = 0.1,
                                label = label_tha_e[1]), parse = TRUE)+ 
  geom_text(data = exploration, aes(x = 0.8, y = 0.05,
                                    label = label_tha_e[2]), parse = TRUE)+ 
  theme(panel.background = element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=1.5), axis.text=element_text(size=12))

rsc_e <- ggplot( exploration, aes(acc, retrosplenial)) + 
  geom_point(color = colors[2]) +
  geom_smooth(method = "lm", color = colors[2], size = 0.5) +
  theme(legend.position=c(0,1), legend.justification=c(0,1))+
  ylim(0,0.4) + ylab('') + xlab('') +
  labs(title="Retrosplenial Cortex") +
  geom_text(data =  exploration, aes(x = 0.8, y = 0.1,
                                label = label_rsc_e[1]), parse = TRUE)+ 
  geom_text(data =  exploration, aes(x = 0.8, y = 0.05,
                                     label = label_rsc_e[2]), parse = TRUE)+ 
  theme(panel.background = element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=1.5), axis.text=element_text(size=12) )

prc_e <- ggplot( exploration, aes( acc, precuneus)) + 
  geom_point(color = colors[3]) +
  geom_smooth(method = "lm", color = colors[3], size = 0.5) +
  theme(legend.position=c(0,1), legend.justification=c(0,1))+
  ylim(0,0.4) + ylab('') +  xlab('') +
  labs(title="Precuneus") +
  geom_text(data =  exploration, aes(x = 0.8, y = 0.1,
                                label = label_prc_e[1]), parse = TRUE)+
  geom_text(data =  exploration, aes(x = 0.8, y = 0.05,
                                     label = label_prc_e[2]), parse = TRUE)+
  theme(panel.background = element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=1.5) , axis.text=element_text(size=12))

ext_e <- ggplot( exploration, aes(acc, extrastriate)) + 
  geom_point(color = colors[4]) +
  geom_smooth(method = "lm", color = colors[4], size = 0.5) +
  theme(legend.position=c(0,1), legend.justification=c(0,1))+
  ylim(0,0.4) + ylab('') +  xlab('') +
  labs(title="Extrastriate Cortex") +
  geom_text(data =  exploration, aes(x = 0.8, y = 0.1,
                                label = label_ext_e[1]), parse = TRUE)+ 
  geom_text(data =  exploration, aes(x = 0.8, y = 0.05,
                                     label = label_ext_e[2]), parse = TRUE)+ 
  theme(panel.background = element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=1.5) , axis.text=element_text(size=12))

str_e <- ggplot( exploration, aes(acc, early_visual)) + 
  geom_point(color = colors[5]) +
  geom_smooth(method = "lm", color = colors[5], size = 0.5) +
  theme(legend.position=c(0,1), legend.justification=c(0,1))+
  ylim(0,0.4) + ylab('') +  xlab('') +
  labs(title="Striate Cortex") +
  geom_text(data =  exploration, aes(x = 0.8, y = 0.1,
                                     label = label_str_e[1]), parse = TRUE)+ 
  geom_text(data =  exploration, aes(x = 0.8, y = 0.05,
                                     label = label_str_e[2]), parse = TRUE)+ 
  theme(panel.background = element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=1.5) , axis.text=element_text(size=12))

# not used
aud_e <- ggplot( exploration, aes(acc, auditory)) + 
  geom_point(color = colors[6]) +
  geom_smooth(method = "lm", color = colors[6], size = 0.5) +
  theme(legend.position=c(0,1), legend.justification=c(0,1))+
  ylim(0,0.4) + ylab('') +  xlab('') +
  labs(title="Auditory Cortex") +
  geom_text(data =  exploration, aes(x = 0.8, y = 0.1,
                                     label = label_aud_e), parse = TRUE)+ 
  theme(panel.background = element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=1.5) , axis.text=element_text(size=12))

#########

tha_t <- ggplot(test, aes(acc, thalamus)) + 
  geom_point(color = colors[1]) +
  geom_smooth(method = "lm", color = colors[1], size = 0.5) +
  theme(legend.position=c(0,1), legend.justification=c(0,1))+
  ylim(0,0.4) + ylab('') +  xlab('') +
  labs(title="Thalamus") +
  geom_text(data = test, aes(x = 0.8, y = 0.1,
                             label = label_tha_t[1]), parse = TRUE)+ 
  geom_text(data = test, aes(x = 0.8, y = 0.05,
                             label = label_tha_t[2]), parse = TRUE)+ 
  theme(panel.background = element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=1.5) , axis.text=element_text(size=12))

rsc_t <- ggplot(test, aes(acc, retrosplenial)) + 
  geom_point(color = colors[2]) +
  geom_smooth(method = "lm", color = colors[2], size = 0.5) +
  theme(legend.position=c(0,1), legend.justification=c(0,1))+
  ylim(0,0.4) + ylab('') +  xlab('') +
  labs(title="Retrosplenial Cortex") +
  geom_text(data = test, aes(x = 0.8, y = 0.1,
                             label = label_rsc_t[1]), parse = TRUE)+
  geom_text(data = test, aes(x = 0.8, y = 0.05,
                             label = label_rsc_t[2]), parse = TRUE)+
  theme(panel.background = element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=1.5), axis.text=element_text(size=12) )

prc_t <- ggplot(test, aes(acc, precuneus)) + 
  geom_point(color = colors[3]) +
  geom_smooth(method = "lm", color = colors[3], size = 0.5) +
  theme(legend.position=c(0,1), legend.justification=c(0,1))+
  ylim(0,0.4) + ylab('') +  xlab('') +
  labs(title="Precuneus") +
  geom_text(data = test, aes(x = 0.8, y = 0.1,
                             label = label_prc_t[1]), parse = TRUE)+ 
  geom_text(data = test, aes(x = 0.8, y = 0.05,
                             label = label_prc_t[2]), parse = TRUE)+ 
  theme(panel.background = element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=1.5), axis.text=element_text(size=12) )

ext_t <- ggplot(test, aes(acc, extrastriate)) + 
  geom_point(color = colors[4]) +
  geom_smooth(method = "lm", color = colors[4] , size = 0.5) +
  theme(legend.position=c(0,1), legend.justification=c(0,1))+
  ylim(0,0.4) + ylab('') +  xlab('') +
  labs(title="Extrastriate Cortex") +
  geom_text(data = test, aes(x = 0.8, y = 0.1,
                             label = label_ext_t[1]), parse = TRUE)+ 
  geom_text(data = test, aes(x = 0.8, y = 0.05,
                             label = label_ext_t[2]), parse = TRUE)+ 
  theme(panel.background = element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=1.5) , axis.text=element_text(size=12))

str_t <- ggplot(test, aes(acc, early_visual)) + 
  geom_point(color = colors[5]) +
  geom_smooth(method = "lm", color = colors[5], size = 0.5) +
  theme(legend.position=c(0,1), legend.justification=c(0,1))+
  ylim(0,0.4) + ylab('') +  xlab('') +
  labs(title="Striate Cortex") +
  geom_text(data = test, aes(x = 0.8, y = 0.1,
                             label = label_str_t[1]), parse = TRUE)+ 
  geom_text(data = test, aes(x = 0.8, y = 0.05,
                             label = label_str_t[2]), parse = TRUE)+ 
  theme(panel.background = element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=1.5), axis.text=element_text(size=12) )

# not used
aud_t <- ggplot(test, aes(acc, auditory)) + 
  geom_point(color = colors[6]) +
  geom_smooth(method = "lm", color = colors[6], size = 0.5) +
  theme(legend.position=c(0,1), legend.justification=c(0,1))+
  ylim(0,0.4) + ylab('') +  xlab('') +
  labs(title="Auditory Cortex") +
  geom_text(data = test, aes(x = 0.8, y = 0.1,
                             label = label_aud_t), parse = TRUE)+ 
  theme(panel.background = element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=1.5) )

# fit together


library("gridExtra")
library(gtable)
library(grid)
grid.arrange(arrangeGrob(tha_e, rsc_e, prc_e, ext_e, str_e, ncol = 5, top = textGrob("Exploration Phase", vjust = 1, gp = gpar(fontface = "bold", cex = 1.5))), 
             arrangeGrob(tha_t, rsc_t, prc_t, ext_t, str_t, ncol = 5, top = textGrob("Test Phase", vjust = 1, gp = gpar(fontface = "bold", cex = 1.5))), 
             ncol=1, 
             left = textGrob("Classification Strength", rot = 90, vjust = 1, gp = gpar(fontface = "bold", cex = 1.8)),
             bottom = textGrob("Navigation Performance", vjust = 0, gp = gpar(fontface = "bold", cex = 1.8)))

