library(lcmm)
library(RColorBrewer)
library(gridExtra)
library(ggplot2)
library(ggrepel)
library(mgcv)
library(itsadug)
library(mosaic)
library(reshape2)
library(fitdistrplus)
library(viridis)
library(sjPlot)
library(lme4)
library(gamlss)
library(reporttools)
library(colorspace)
library(ggpubr)
library(multcompView)
library(cowplot)

##### IMPORT DATA #####
setwd("/Users/Kathryn/Desktop/DATASETS/New Excel Sheets3/Output")
move_data_all <- read.csv("MOVE_DATA_ALL.csv", sep=",", header=TRUE)
move_data_daily <- read.csv("MOVE_DATA_DAILY.csv", sep=",", header=TRUE)
MASTERFILE_DIAGNOSIS7 <- read.csv("MASTERFILE_DIAGNOSIS7.csv", sep=",", header=TRUE) 



###### ###### ###### ###### ###### ###### ###### ###### ###### ###### ###### ###### ###### ######
###### ###### ###### ###### ######  ALL DAILY DATA AVERAGED TOGETHER  ###### ###### ###### ######
###### ###### ###### ###### ###### ###### ###### ###### ###### ###### ###### ###### ###### ###### ###### 
move_data_30_daily <- droplevels(move_data_all)
move_data_30_daily <- droplevels(move_data_30_daily[-c(which(move_data_30_daily$day %in% c(-1,0,10,11,12,13,14) & 
                                           !(is.na(move_data_30_daily$total_locations)))),])
move_data_30_daily <- droplevels(move_data_30_daily[-c(which(move_data_30_daily$day %in% c(-1,0,10,11,12,13,14))),])

move_data_30_daily$day_short <- character(length=length(move_data_30_daily$part))
move_data_30_daily$day_short[which(move_data_30_daily$day == "30")] <- "30"
move_data_30_daily$day_short[which(move_data_30_daily$day == -15)] <- "0"
move_data_30_daily$day_short[which(move_data_30_daily$day_short == "")] <- "daily"

### create dataset with only one row per "day level", i.e., 0,daily,30
parts_list <- levels(as.factor(move_data_30_daily$part))
move_data_30_daily2 <- data.frame(cbind(rep(1:length(parts_list), each=3), rep(c("0","daily","30"), length(parts_list))))
names(move_data_30_daily2) <- c("part", "day")
move_data_30_daily2$part <- as.factor(as.numeric(as.character(move_data_30_daily2$part)))
move_data_30_daily2$locations <- character(length=length(move_data_30_daily2$part))
move_data_30_daily2$houses <- character(length=length(move_data_30_daily2$part))
move_data_30_daily2$hours <- character(length=length(move_data_30_daily2$part))

parts_list <- levels(move_data_30_daily2$part)
for(i in 1:(length(parts_list))){
  part1 <- parts_list[i]
  index <- which(move_data_30_daily2$part == part1)
  ## locations
  temp_0_loc <- move_data_30_daily$total_locations[which(move_data_30_daily$part ==part1 & move_data_30_daily$day_short == "0")]
  temp_30_loc <- move_data_30_daily$total_locations[which(move_data_30_daily$part ==part1 & move_data_30_daily$day_short == "30")]
  daily_temp_loc <- as.numeric(sum(move_data_30_daily$total_locations[which(move_data_30_daily$part ==part1 & move_data_30_daily$day_short == "daily")], na.rm=TRUE)/
                             sum(!(is.na(move_data_30_daily$total_locations[which(move_data_30_daily$part ==part1 & move_data_30_daily$day_short == "daily")]))))
  move_data_30_daily2$locations[which(move_data_30_daily2$part == part1 & move_data_30_daily2$day == "0")] <- temp_0_loc
  move_data_30_daily2$locations[which(move_data_30_daily2$part == part1 & move_data_30_daily2$day == "daily")] <- daily_temp_loc
  move_data_30_daily2$locations[which(move_data_30_daily2$part == part1 & move_data_30_daily2$day == "30")] <- temp_30_loc
  
  ## houses
  temp_0_house <- move_data_30_daily$total_houses[which(move_data_30_daily$part ==part1 & move_data_30_daily$day_short == "0")]
  temp_30_house <- move_data_30_daily$total_houses[which(move_data_30_daily$part ==part1 & move_data_30_daily$day_short == "30")]
  daily_temp_house <- as.numeric(sum(move_data_30_daily$total_houses[which(move_data_30_daily$part ==part1 & move_data_30_daily$day_short == "daily")], na.rm=TRUE)/
                                 sum(!(is.na(move_data_30_daily$total_houses[which(move_data_30_daily$part ==part1 & move_data_30_daily$day_short == "daily")]))))
  move_data_30_daily2$houses[which(move_data_30_daily2$part == part1 & move_data_30_daily2$day == "0")] <- temp_0_house
  move_data_30_daily2$houses[which(move_data_30_daily2$part == part1 & move_data_30_daily2$day == "daily")] <- daily_temp_house
  move_data_30_daily2$houses[which(move_data_30_daily2$part == part1 & move_data_30_daily2$day == "30")] <- temp_30_house
  
  ## hours
  temp_0_hours <- move_data_30_daily$hours_home[which(move_data_30_daily$part ==part1 & move_data_30_daily$day_short == "0")]
  temp_30_hours <- move_data_30_daily$hours_home[which(move_data_30_daily$part ==part1 & move_data_30_daily$day_short == "30")]
  daily_temp_hours <- as.numeric(sum(move_data_30_daily$hours_home[which(move_data_30_daily$part ==part1 & move_data_30_daily$day_short == "daily")], na.rm=TRUE)/
                                 sum(!(is.na(move_data_30_daily$hours_home[which(move_data_30_daily$part ==part1 & move_data_30_daily$day_short == "daily")]))))
  move_data_30_daily2$hours[which(move_data_30_daily2$part == part1 & move_data_30_daily2$day == "0")] <- temp_0_hours
  move_data_30_daily2$hours[which(move_data_30_daily2$part == part1 & move_data_30_daily2$day == "daily")] <- daily_temp_hours
  move_data_30_daily2$hours[which(move_data_30_daily2$part == part1 & move_data_30_daily2$day == "30")] <- temp_30_hours
}
move_data_30_daily2$locations <- as.numeric(as.character(move_data_30_daily2$locations))
move_data_30_daily2$houses <- as.numeric(as.character(move_data_30_daily2$houses))
move_data_30_daily2$hours <- as.numeric(as.character(move_data_30_daily2$hours))
move_data_30_daily2$day <- factor(move_data_30_daily2$day, levels=c("0","daily","30"))



######################################################################################
#                                       TOTAL LOCATIONS
######################################################################################
day_0_vec_loc <- move_data_30_daily2$locations[which(move_data_30_daily2$day == "0")]
daily_vec_loc <- move_data_30_daily2$locations[which(move_data_30_daily2$day == "daily")]
day_30_vec_loc <- move_data_30_daily2$locations[which(move_data_30_daily2$day == "30")]
day_30_vec_loc2 <- day_30_vec_loc[which(!(is.na(day_30_vec_loc)))]

## test data for normality
shapiro.test(move_data_30_daily2$locations) # p < 0.001 --> NOT NORMAL --> NON-PARAMETRIC

#### NON-PARAMETRIC Kruskall Wallis Rank Sum Test ####
kruskal.test(move_data_30_daily2$locations, move_data_30_daily2$day)
# p < 0.001

## WILCOXON PAIRWISE TESTS --> USE BONFERONNI CORRECTION --> 3 comparisons --> multiply all p-values by 3
pairwise.wilcox.test(move_data_30_daily2$locations, move_data_30_daily2$day, paired=TRUE, p.adjust.method = "bonf")
#          0        daily
# daily   2e-06***   -    
#   30    1.00      0.01*

plot1 <- ggplot(data=move_data_30_daily2, aes(x=day, y=locations)) +
  geom_boxplot() +
  scale_y_continuous(limits=c(0,4.5),breaks=seq(from=0, to=4.5, by=1)) +
  scale_x_discrete(labels=c("Day 0\nRetrospective","Averaged Daily\n(Days 1-9)","Post-illness")) +
  labs(x= "\nDay of Movement Survey", y="Average number of locations visited") +
  annotate("text", x=c(1,2,3), y=c(3.5,2.7,2.55), label= c("a","b","a"), 
           size=7, color="blue") +
  theme_classic(base_size = 13) +
  theme(panel.background = element_rect(color="black"), 
        axis.text.x = element_text(size=14, color="black"), axis.text.y = element_text(size=15)) +
  theme(plot.margin = unit(c(0.75,0.75,0.15,0.5), "cm"))



#### MCNEMAR'S TEST for 0 vs 1+ places for pairs of day groups ####
day_0_vec_loc[which(day_0_vec_loc > 0)] <- 1
daily_vec_loc[which(daily_vec_loc > 0)] <- 1
day_30_vec_loc[which(day_30_vec_loc > 0)] <- 1
day_0_vec_loc <- factor(day_0_vec_loc, levels=c(0,1))
daily_vec_loc <- factor(daily_vec_loc, levels=c(0,1))
day_30_vec_loc <- factor(day_30_vec_loc, levels=c(0,1))

mcnemar.test(table(day_0_vec_loc, day_30_vec_loc)) # p = 0.48
mcnemar.test(table(day_0_vec_loc, daily_vec_loc)) # p = 0.016 *
mcnemar.test(table(day_30_vec_loc, daily_vec_loc)) # p = 0.074




######################################################################################
#                                       TOTAL HOUSES
######################################################################################
day_0_vec_house <- move_data_30_daily2$houses[which(move_data_30_daily2$day == "0")]
daily_vec_house <- move_data_30_daily2$houses[which(move_data_30_daily2$day == "daily")]
day_30_vec_house <- move_data_30_daily2$houses[which(move_data_30_daily2$day == "30")]
day_30_vec_house2 <- day_30_vec_house[which(!(is.na(day_30_vec_house)))]

## test data for normality
shapiro.test(move_data_30_daily2$houses) # p < 0.001 --> NOT NORMAL --> NON-PARAMETRIC

#### NON-PARAMETRIC Kruskall Wallis Rank Sum Test ####
kruskal.test(move_data_30_daily2$houses, move_data_30_daily2$day)
## p < 0.001

## WILCOXON PAIRWISE TESTS --> USE BONFERONNI CORRECTION --> 3 comparisons --> multiply all p-values by 3
pairwise.wilcox.test(move_data_30_daily2$houses, move_data_30_daily2$day, paired=TRUE, p.adjust.method = "bonf")
#          0        daily
# daily   0.00095***   -    
#   30    1.00      0.09

ggplot(data=move_data_30_daily2, aes(x=day, y=houses)) +
  geom_boxplot() +
  scale_y_continuous(limits=c(0,4.5),breaks=seq(from=0, to=4.5, by=1)) +
  scale_x_discrete(labels=c("Day 0\nRetrospective","Averaged Daily\n(Days 1-9)","Post-illness")) +
  labs(x= "\nDay of Movement Survey", y="Average number of houses visited") +
  annotate("text", x=c(1,2,3), y=c(3.5,2.7,2.55), label= c("a","b","ab"), 
           size=7, color="blue") +
  theme_classic(base_size = 13) +
  theme(panel.background = element_rect(color="black"), 
        axis.text.x = element_text(size=14, color="black"), axis.text.y = element_text(size=15)) +
  theme(plot.margin = unit(c(0.75,0.75,0.15,0.5), "cm"))



#### MCNEMAR'S TEST for 0 vs 1+ places for pairs of day groups ####
day_0_vec_house[which(day_0_vec_house > 0)] <- 1
daily_vec_house[which(daily_vec_house > 0)] <- 1
day_30_vec_house[which(day_30_vec_house > 0)] <- 1
day_0_vec_house <- factor(day_0_vec_house, levels=c(0,1))
daily_vec_house <- factor(daily_vec_house, levels=c(0,1))
day_30_vec_house <- factor(day_30_vec_house, levels=c(0,1))

mcnemar.test(table(day_0_vec_house, day_30_vec_house)) # p = 0.75
mcnemar.test(table(day_0_vec_house, daily_vec_house)) # p < 0.001 ***
mcnemar.test(table(day_30_vec_house, daily_vec_house)) # p = 0.043 *








######################################################################################
#                                       HOURS HOME
######################################################################################
day_0_vec_hours <- move_data_30_daily2$hours[which(move_data_30_daily2$day == "0")]
daily_vec_hours <- move_data_30_daily2$hours[which(move_data_30_daily2$day == "daily")]
day_30_vec_hours <- move_data_30_daily2$hours[which(move_data_30_daily2$day == "30")]
day_30_vec_hours2 <- day_30_vec_hours[which(!(is.na(day_30_vec_hours)))]



move_data_30_daily2$hours_prop <- move_data_30_daily2$hours/17
# test data for normality
shapiro.test(move_data_30_daily2$hours) # p = 0.046 --> NOT NORMAL --> NON-PARAMETRIC
shapiro.test(move_data_30_daily2$hours_prop) # p = 0.46 --> NOT NORMAL --> NON-PARAMETRIC

#### NON-PARAMETRIC Kruskall Wallis Rank Sum Test ####
kruskal.test(move_data_30_daily2$hours, move_data_30_daily2$day)
## p < 0.001

## WILCOXON PAIRWISE TESTS --> USE BONFERONNI CORRECTION --> 3 comparisons --> multiply all p-values by 3
pairwise.wilcox.test(move_data_30_daily2$hours, move_data_30_daily2$day, paired=TRUE, p.adjust.method = "bonf")
#          0        daily
# daily   2e-05***   -    
#   30    1.00      9e-06***

ggplot(data=move_data_30_daily2, aes(x=day, y=houses)) +
  geom_boxplot() +
  scale_y_continuous(limits=c(0,4.5),breaks=seq(from=0, to=4.5, by=1)) +
  scale_x_discrete(labels=c("Day 0\nRetrospective","Averaged Daily\n(Days 1-9)","Post-illness")) +
  labs(x= "\nDay of Movement Survey", y="Average number of houses visited") +
  annotate("text", x=c(1,2,3), y=c(3.5,2.7,2.55), label= c("a","b","ab"), 
           size=7, color="blue") +
  theme_classic(base_size = 13) +
  theme(panel.background = element_rect(color="black"), 
        axis.text.x = element_text(size=14, color="black"), axis.text.y = element_text(size=15)) +
  theme(plot.margin = unit(c(0.75,0.75,0.15,0.5), "cm"))


plot2 <- ggplot(data=move_data_30_daily2, aes(x=day, y=hours_prop)) +
  geom_boxplot() +
  scale_y_continuous(limits=c(0.1,1.1),breaks=seq(from=0.2, to=1.1, by=0.2)) +
  scale_x_discrete(labels=c("Day 0\nRetrospective","Averaged Daily\n(Days 1-9)","Post-illness")) +
  labs(x= "\nDay of Movement Survey", y="Average time spent at home") +
  annotate("text", x=c(1,2,3), y=c(1.05,1.05,0.88), label= c("a","b","a"), 
           size=7, color="blue") +
  theme_classic(base_size = 13) +
  theme(panel.background = element_rect(color="black"), 
        axis.text.x = element_text(size=14, color = "black"), axis.text.y = element_text(size=15)) +
  theme(plot.margin = unit(c(0.75,0.5,0.15,0.75), "cm"))
#





#### MCNEMAR'S TEST for 0 vs 1+ places for pairs of day groups ####
day_0_vec_hours[which(day_0_vec_hours <= 16)] <- 0
daily_vec_hours[which(daily_vec_hours <= 16)] <- 0
day_30_vec_hours[which(day_30_vec_hours <= 16)] <- 0
day_0_vec_hours[which(day_0_vec_hours > 16)] <- 1
daily_vec_hours[which(daily_vec_hours > 16)] <- 1
day_30_vec_hours[which(day_30_vec_hours > 16)] <- 1
day_0_vec_hours <- factor(day_0_vec_hours, levels=c(0,1))
daily_vec_hours <- factor(daily_vec_hours, levels=c(0,1))
day_30_vec_hours <- factor(day_30_vec_hours, levels=c(0,1))

mcnemar.test(table(day_0_vec_hours, day_30_vec_hours)) # p = 0.75
mcnemar.test(table(day_0_vec_hours, daily_vec_hours)) # p = 0.016 *
mcnemar.test(table(day_30_vec_hours, daily_vec_hours)) # p = 0.074


######################################################################################
#                                 GRAPHS EXPORTED AS PDF
######################################################################################
plot_temp1 <- arrangeGrob(plot1, plot2, ncol = 2, nrow = 1)
# Add labels to the arranged plots
plot_temp2 <- as_ggplot(plot_temp1) +   # transform to a ggplot
  draw_plot_label(label = c("A", "B"), size = 20, x = c(0, 0.5), y = c(1, 1)) # Add labels
setwd("/Users/Kathryn/Desktop/Aim 1/CURRENT MANUSCRIPT/Figures 3/")
pdf(file = 'Fig1.pdf', width = 11, height = 6)
plot_temp2
dev.off()









###### ###### ###### ###### ###### ###### ###### ###### ###### ###### ###### ###### ###### ######
###### ###### ###### ###### ###### ######   GROUPS OF 3 DAYS   ###### ###### ###### ###### ######
###### ###### ###### ###### ###### ###### ###### ###### ###### ###### ###### ###### ###### ######
move_data_groups2 <- droplevels(move_data_all[-c(which(move_data_all$day %in% c(-15,0,10,11,12,13,14))),])

move_data_groups2$day_3gp <- as.character(move_data_groups2$day)
move_data_groups2$day_3gp[which(move_data_groups2$day %in% c("1","2","3"))] <- "1-3"
move_data_groups2$day_3gp[which(move_data_groups2$day %in% c("4","5","6"))] <- "4-6"
move_data_groups2$day_3gp[which(move_data_groups2$day %in% c("7","8","9"))] <- "7-9"
move_data_groups2$day_3gp <- as.factor(move_data_groups2$day_3gp)

move_data_groups2$day_3gp.1 <- as.numeric(move_data_groups2$day)
move_data_groups2$day_3gp.1[which(move_data_groups2$day %in% c(1,2,3))] <- 2
move_data_groups2$day_3gp.1[which(move_data_groups2$day %in% c(4,5,6))] <- 5
move_data_groups2$day_3gp.1[which(move_data_groups2$day %in% c(7,8,9))] <- 8

move_data_groups2a <- droplevels(move_data_groups2[!duplicated(move_data_groups2[,c(1,14)]),])
days <- levels(as.factor(move_data_groups2$day_3gp.1))
parts_list <- levels(as.factor(move_data_groups2$part))
for(i in 1:length(parts_list)){
  part <- parts_list[i]
  for(j in 1:(length(days))){
    day <- days[j]
    if(length(move_data_groups2$total_locations[which(move_data_groups2$part == part & move_data_groups2$day_3gp.1 == day)]) == 
       length(which(is.na(move_data_groups2$total_locations[which(move_data_groups2$part == part & move_data_groups2$day_3gp.1 == day)])))){
      move_data_groups2a$total_locations[which(move_data_groups2a$part == part & 
                                               move_data_groups2a$day_3gp.1 == day)] <- NA
    }
    else{
      move_data_groups2a$total_locations[which(move_data_groups2a$part == part & 
                                               move_data_groups2a$day_3gp.1 == day)] <- 
        sum(move_data_groups2$total_locations[which(move_data_groups2$part == part & 
                                                    move_data_groups2$day_3gp.1 == day)], na.rm=TRUE)
    }
    if(length(move_data_groups2$total_houses[which(move_data_groups2$part == part & move_data_groups2$day_3gp.1 == day)]) == 
       length(which(is.na(move_data_groups2$total_houses[which(move_data_groups2$part == part & move_data_groups2$day_3gp.1 == day)])))){
      move_data_groups2a$total_houses[which(move_data_groups2a$part == part & 
                                            move_data_groups2a$day_3gp.1 == day)] <- NA
    }
    else{
      move_data_groups2a$total_houses[which(move_data_groups2a$part == part & 
                                            move_data_groups2a$day_3gp.1 == day)] <- 
        sum(move_data_groups2$total_houses[which(move_data_groups2$part == part & 
                                                 move_data_groups2$day_3gp.1 == day)], na.rm=TRUE)
    }
    if(day %in% c("2","5","8")){
      if(length(which(is.na(move_data_groups2$hours_home[which(move_data_groups2$part == part & 
                                                             move_data_groups2$day_3gp.1 == day)]))) == 3){
        move_data_groups2a$hours_home[which(move_data_groups2a$part == part & 
                                            move_data_groups2a$day_3gp.1 == day)] <- NA
      }
      else{
        hour_check  <- move_data_groups2$hours_home[which(move_data_groups2$part == part & move_data_groups2$day_3gp.1 == day)]
        div <- length(which(!(is.na(hour_check))))
        move_data_groups2a$hours_home[which(move_data_groups2a$part == part & 
                                            move_data_groups2a$day_3gp.1 == day)] <- 
          round(sum(move_data_groups2$hours_home[which(move_data_groups2$part == part & 
                                                       move_data_groups2$day_3gp.1 == day)], na.rm=TRUE)/div)
      }
    }
    else{
      move_data_groups2a$hours_home[which(move_data_groups2a$part == part & move_data_groups2a$day_3gp.1 == day)] <- 
        move_data_groups2$hours_home[which(move_data_groups2$part == part & move_data_groups2$day_3gp.1 == day)]
    }
  }
}
################## DAY 30 RE-SCALING 
## Grouped day totals are for 3 day interval --> day 30 for average of 1 day ==> change
## For all total___ on day=30, multiply by 3
move_data_groups2a$total_locations[which(move_data_groups2a$day == "30" & !(is.na(move_data_groups2a$total_locations)))] <- 
  move_data_groups2a$total_locations[which(move_data_groups2a$day == "30" & !(is.na(move_data_groups2a$total_locations)))] * 3
move_data_groups2a$total_houses[which(move_data_groups2a$day == "30" & !(is.na(move_data_groups2a$total_houses)))] <- 
  move_data_groups2a$total_houses[which(move_data_groups2a$day == "30" & !(is.na(move_data_groups2a$total_houses)))] * 3

move_data_groups2a$total_locations <- as.integer(round(move_data_groups2a$total_locations))
move_data_groups2a$total_houses <- as.integer(round(move_data_groups2a$total_houses))
move_data_groups2a$hours_home <- as.integer(round(move_data_groups2a$hours_home))



######################################################################################
#                                       TOTAL LOCATIONS
######################################################################################
## test data for normality
shapiro.test(move_data_groups2a$total_locations) # p < 0.001 --> NOT NORMAL --> NON-PARAMETRIC

#### NON-PARAMETRIC Kruskall Wallis Rank Sum Test ####
kruskal.test(move_data_groups2a$total_locations, move_data_groups2a$day)
# p < 0.001

## WILCOXON PAIRWISE TESTS --> USE BONFERONNI CORRECTION --> 3 comparisons --> multiply all p-values by 3
pairwise.wilcox.test(move_data_groups2a$total_locations, move_data_groups2a$day_3gp.1, paired=TRUE, p.adjust.method = "bonf")
#         2       5               8    
#   5  1.000      -               -    
#   8  0.0474*    0.87            -    
#   30 0.0168*    0.00096***    0.126

## get significance letters
a3 <- matrix(c(NA, 1, 0.047, 0.017,
               1, NA, 0.87, 0.00096,
               0.047, 0.87, NA, 0.123,
               0.017, 0.00096, 0.123, NA),
             nrow=4,ncol=4, dimnames=list(c("2","5","8","30"), c("2","5","8","30")))
multcompLetters(a3) ## a, ab, bc,c 

move_data_groups2a$day_3gp.2 <- factor((move_data_groups2a$day_3gp), levels=c("1-3","4-6","7-9","Post-Illness"))
move_data_groups2a$day_3gp.2[which(move_data_groups2a$day_3gp == "30")] <- "Post-Illness"

plot3 <- ggplot(data=move_data_groups2a, aes(x=day_3gp.2, y=total_locations)) +
  geom_boxplot() +
  scale_y_continuous(breaks=seq(from=0, to=12, by=2)) +
  labs(x= "Days post symptom onset", y="Number of locations visited") +
  geom_vline(xintercept = 3.5, color="red", size=2, linetype=2) + 
  annotate("text", x=c(1,2,3,4), y=c(5.5,5.5,5.5,7.5), label= c("a","ab","bc","c"), 
           size=7, color="blue") +
  theme_classic(base_size = 13) +
  theme(panel.background = element_rect(color="black"), 
        axis.text.y = element_text(size=15)) +
  theme(plot.margin = unit(c(0.75,0.75,0.15,0.5), "cm"))


#### MCNEMAR'S TEST for 0 vs 1+ places for pairs of day groups ####
day2_loc <- move_data_groups2a$total_locations[which(move_data_groups2a$day_3gp.1 == "2")]
day5_loc <- move_data_groups2a$total_locations[which(move_data_groups2a$day_3gp.1 == "5")]
day8_loc <- move_data_groups2a$total_locations[which(move_data_groups2a$day_3gp.1 == "8")]
day30_loc <- move_data_groups2a$total_locations[which(move_data_groups2a$day_3gp.1 == "30")]
day2_loc[which(day2_loc > 1)] <- 1
day5_loc[which(day5_loc > 1)] <- 1
day8_loc[which(day8_loc > 1)] <- 1
day30_loc[which(day30_loc > 1)] <- 1
day2_loc <- factor(day2_loc, levels=c(0,1))
day5_loc <- factor(day5_loc, levels=c(0,1))
day8_loc <- factor(day8_loc, levels=c(0,1))
day30_loc <- factor(day30_loc, levels=c(0,1))

mcnemar.test(table(day2_loc, day30_loc)) # p = 0.0015**
mcnemar.test(table(day5_loc, day30_loc)) # p = 0.0009 ***
mcnemar.test(table(day8_loc, day30_loc)) # p = 0.008 **
mcnemar.test(table(day2_loc, day5_loc)) # p=1
mcnemar.test(table(day2_loc, day8_loc)) #p=0.08
mcnemar.test(table(day5_loc, day8_loc)) # p=0.15




######################################################################################
#                                       TOTAL HOUSES
######################################################################################
## test data for normality
shapiro.test(move_data_groups2a$total_houses) # p < 0.001 --> NOT NORMAL --> NON-PARAMETRIC

#### NON-PARAMETRIC Kruskall Wallis Rank Sum Test ####
kruskal.test(move_data_groups2a$total_houses, move_data_groups2a$day)
# p = 0.31 --> NOT SIGNIFICANT

## WILCOXON PAIRWISE TESTS --> USE BONFERONNI CORRECTION --> 3 comparisons --> multiply all p-values by 3
pairwise.wilcox.test(move_data_groups2a$total_houses, move_data_groups2a$day_3gp.1, paired=TRUE, p.adjust.method = "bonf")
#         2       5     8    
#   5  1.000      -     -    
#   8  0.29    1.00     -    
#   30 0.63    0.15    0.72


move_data_groups2a$day_3gp.2 <- factor((move_data_groups2a$day_3gp), levels=c("1-3","4-6","7-9","Post-Illness"))
move_data_groups2a$day_3gp.2[which(move_data_groups2a$day_3gp == "30")] <- "Post-Illness"

ggplot(data=move_data_groups2a, aes(x=day_3gp.2, y=total_houses)) +
  geom_boxplot() +
  scale_y_continuous(breaks=seq(from=0, to=6, by=1)) +
  labs(x= "Days post symptom onset", y="Number of locations visited") +
  geom_vline(xintercept = 3.5, color="red", size=2, linetype=2) + 
  annotate("text", x=c(1,2,3,4), y=c(0.5,0.5,0.5,1.5), label= c("a","a","a","a"), 
           size=7, color="blue") +
  theme_classic(base_size = 13) +
  theme(panel.background = element_rect(color="black"), 
        axis.text.y = element_text(size=15)) +
  theme(plot.margin = unit(c(0.75,0.75,0.15,0.5), "cm"))



#### MCNEMAR'S TEST for 0 vs 1+ places for pairs of day groups ####
day2_house <- move_data_groups2a$total_houses[which(move_data_groups2a$day_3gp.1 == "2")]
day5_house <- move_data_groups2a$total_houses[which(move_data_groups2a$day_3gp.1 == "5")]
day8_house <- move_data_groups2a$total_houses[which(move_data_groups2a$day_3gp.1 == "8")]
day30_house <- move_data_groups2a$total_houses[which(move_data_groups2a$day_3gp.1 == "30")]
day2_house[which(day2_house > 1)] <- 1
day5_house[which(day5_house > 1)] <- 1
day8_house[which(day8_house > 1)] <- 1
day30_house[which(day30_house > 1)] <- 1
day2_house <- factor(day2_house, levels=c(0,1))
day5_house <- factor(day5_house, levels=c(0,1))
day8_house <- factor(day8_house, levels=c(0,1))
day30_house <- factor(day30_house, levels=c(0,1))

mcnemar.test(table(day2_house, day30_house)) # p = 0.13
mcnemar.test(table(day5_house, day30_house)) # p = 0.11
mcnemar.test(table(day8_house, day30_house)) # p = 0.51
mcnemar.test(table(day2_house, day5_house)) # p=1
mcnemar.test(table(day2_house, day8_house)) #p=0.023*
mcnemar.test(table(day5_house, day8_house)) # p=0.074






######################################################################################
#                                       HOURS HOME
######################################################################################
move_data_groups2a$hours_prop <- move_data_groups2a$hours_home/17

## test data for normality
shapiro.test(move_data_groups2a$hours_home) # p < 0.001 --> NOT NORMAL --> NON-PARAMETRIC
shapiro.test(move_data_groups2a$hours_prop) # p < 0.001 --> NOT NORMAL --> NON-PARAMETRIC

#### NON-PARAMETRIC Kruskall Wallis Rank Sum Test ####
kruskal.test(move_data_groups2a$hours_prop, move_data_groups2a$day)
# p < 0.001

## WILCOXON PAIRWISE TESTS --> USE BONFERONNI CORRECTION --> 3 comparisons --> multiply all p-values by 3
pairwise.wilcox.test(move_data_groups2a$hours_prop, move_data_groups2a$day_3gp.1, paired=TRUE, p.adjust.method = "bonf")
#         2       5             8    
#   5  1.000      -             -    
#   8  0.014*    0.008**        -    
#   30 0.005**    1e-04***    0.308

move_data_groups2a$day_3gp.2 <- factor((move_data_groups2a$day_3gp), levels=c("1-3","4-6","7-9","Post-Illness"))
move_data_groups2a$day_3gp.2[which(move_data_groups2a$day_3gp == "30")] <- "Post-Illness"

plot4 <- ggplot(data=move_data_groups2a, aes(x=day_3gp.2, y=hours_prop)) +
  geom_boxplot() +
  scale_y_continuous(breaks=seq(from=0, to=1.1, by=0.2)) +
  labs(x= "Days post symptom onset", y="Number of locations visited") +
  geom_vline(xintercept = 3.5, color="red", size=2, linetype=2) + 
  annotate("text", x=c(1,2,3,4), y=c(1.1,1.1,1.1,0.9), label= c("a","a","b","b"), 
           size=7, color="blue") +
  theme_classic(base_size = 13) +
  theme(panel.background = element_rect(color="black"), 
        axis.text.y = element_text(size=15, color="black")) +
  theme(plot.margin = unit(c(0.75,0.75,0.15,0.5), "cm"))


#### MCNEMAR'S TEST for 0 vs 1+ places for pairs of day groups ####
day2_hours <- move_data_groups2a$hours_home[which(move_data_groups2a$day_3gp.1 == "2")]
day5_hours <- move_data_groups2a$hours_home[which(move_data_groups2a$day_3gp.1 == "5")]
day8_hours <- move_data_groups2a$hours_home[which(move_data_groups2a$day_3gp.1 == "8")]
day30_hours <- move_data_groups2a$hours_home[which(move_data_groups2a$day_3gp.1 == "30")]

day2_hours[which(day2_hours <= 16)] <- 0
day2_hours[which(day2_hours > 16)] <- 1
day5_hours[which(day5_hours <= 16)] <- 0
day5_hours[which(day5_hours > 16)] <- 1
day8_hours[which(day8_hours <= 16)] <- 0
day8_hours[which(day8_hours > 16)] <- 1
day30_hours[which(day30_hours <= 16)] <- 0
day30_hours[which(day30_hours > 16)] <- 1
day2_hours <- factor(day2_hours, levels=c(0,1))
day5_hours <- factor(day5_hours, levels=c(0,1))
day8_hours <- factor(day8_hours, levels=c(0,1))
day30_hours <- factor(day30_hours, levels=c(0,1))

mcnemar.test(table(day2_hours, day30_hours)) # p = 0.0077**
mcnemar.test(table(day5_hours, day30_hours)) # p = 0.0077 **
mcnemar.test(table(day8_hours, day30_hours)) # p = 0.074 
mcnemar.test(table(day2_hours, day5_hours)) # p=1
mcnemar.test(table(day2_hours, day8_hours)) #p=0.046*
mcnemar.test(table(day5_hours, day8_hours)) # p=0.11





######################################################################################
#                                 GRAPHS EXPORTED AS PDF
######################################################################################
plot_temp3 <- arrangeGrob(plot3, plot4, ncol = 2, nrow = 1)
# Add labels to the arranged plots
plot_temp4 <- as_ggplot(plot_temp3) +   # transform to a ggplot
  draw_plot_label(label = c("A", "B"), size = 20, x = c(0, 0.5), y = c(1, 1)) # Add labels
setwd("/Users/Kathryn/Desktop/Aim 1/CURRENT MANUSCRIPT/Figures 3/")
pdf(file = 'Fig3.pdf', width = 11, height = 6)
plot_temp4
dev.off()









##################### PLACE TYPE FOR GROUPED DAYS ##################### ####################
place_type_temp <- MASTERFILE_DIAGNOSIS7
#### Now try grouping into fewer categories
## House, Health, Work/Study/education, cemetery/church/elsewhere/ports/internet; markets/restaurants, recreation
place_type_temp$place_type_new <- as.character(place_type_temp$place_type)
place_type_temp$place_type_new[which(place_type_temp$place_type %in% c("study", "education","work"))] <- "education/work"
place_type_temp$place_type_new[which(place_type_temp$place_type %in% c("cemetery","church","elsewhere","ports",
                                                   "internet", "recreation", "restaurant","market"))] <- "other"
place_type_temp$place_type_new <- factor(place_type_temp$place_type_new, levels=c("house","health", "education/work","other"))
place_type_temp$times_visited <- factor(as.numeric(as.character(place_type_temp$times_visited)), 
                              levels=c(1:15))

place_type_temp2 <- droplevels(place_type_temp[-c(which(place_type_temp$day_of_survey == "0")),])
place_type_temp2$day_new <- character(length=length(place_type_temp2$part_number))
place_type_temp2$day_new[which(place_type_temp2$day_interest_by_first_symp2 %in% c(1:3))] <- "1-3"
place_type_temp2$day_new[which(place_type_temp2$day_interest_by_first_symp2 %in% c(4:6))] <- "4-6"
place_type_temp2$day_new[which(place_type_temp2$day_interest_by_first_symp2 %in% c(7:9))] <- "7-9"
place_type_temp2$day_new[which(place_type_temp2$day_interest_by_first_symp2 %in% c(30))] <- "30"
place_type_temp2$day_new <- factor(place_type_temp2$day_new, levels= c("1-3","4-6","7-9","30"))
place_type_temp3 <- place_type_temp2[-c(which(is.na(place_type_temp2$day_new))),]
place_type_temp4 <- place_type_temp3[,c(1,11,10,7)]

place_type_temp4$house_bin <- numeric(length=length(place_type_temp4$part_number))
place_type_temp4$house_bin[which(place_type_temp4$place_type_new == "house")] <- 1
place_type_temp4$health_bin <- numeric(length=length(place_type_temp4$part_number))
place_type_temp4$health_bin[which(place_type_temp4$place_type_new == "health")] <- 1
place_type_temp4$ed_bin <- numeric(length=length(place_type_temp4$part_number))
place_type_temp4$ed_bin[which(place_type_temp4$place_type_new == "education/work")] <- 1
place_type_temp4$other_bin <- numeric(length=length(place_type_temp4$part_number))
place_type_temp4$other_bin[which(place_type_temp4$place_type_new == "other")] <- 1

#### day 30 --> need "times visited" to be considered #####
place_type_temp4a <- place_type_temp4
rows <- which(place_type_temp4$day_new == "30" & place_type_temp4$house_bin == "1")
times <- as.numeric(as.character(place_type_temp4$times_visited[which(place_type_temp4$day_new == "30" & place_type_temp4$house_bin == "1")]))
for(i in 1:(length(rows))){
  row <- rows[i]
  time <- times[i]
  new_rows <- place_type_temp4[rep(row, times=time),]
  place_type_temp4a <- rbind(place_type_temp4a, new_rows)
}
rows <- which(place_type_temp4$day_new == "30" & place_type_temp4$health_bin == "1")
times <- as.numeric(as.character(place_type_temp4$times_visited[which(place_type_temp4$day_new == "30" & place_type_temp4$health_bin == "1")]))
for(i in 1:(length(rows))){
  row <- rows[i]
  time <- times[i]
  new_rows <- place_type_temp4[rep(row, times=time),]
  place_type_temp4a <- rbind(place_type_temp4a, new_rows)
}
rows <- which(place_type_temp4$day_new == "30" & place_type_temp4$ed_bin == "1")
times <- as.numeric(as.character(place_type_temp4$times_visited[which(place_type_temp4$day_new == "30" & place_type_temp4$ed_bin == "1")]))
for(i in 1:(length(rows))){
  row <- rows[i]
  time <- times[i]
  new_rows <- place_type_temp4[rep(row, times=time),]
  place_type_temp4a <- rbind(place_type_temp4a, new_rows)
}
rows <- which(place_type_temp4$day_new == "30" & place_type_temp4$other_bin == "1")
times <- as.numeric(as.character(place_type_temp4$times_visited[which(place_type_temp4$day_new == "30" & place_type_temp4$other_bin == "1")]))
for(i in 1:(length(rows))){
  row <- rows[i]
  time <- times[i]
  new_rows <- place_type_temp4[rep(row, times=time),]
  place_type_temp4a <- rbind(place_type_temp4a, new_rows)
}

#### fit linear mixed effect models ####
place_type_temp4a$house_bin <- as.factor(place_type_temp4a$house_bin)
place_type_temp4a$health_bin <- as.factor(place_type_temp4a$health_bin)
place_type_temp4a$ed_bin <- as.factor(place_type_temp4a$ed_bin)
place_type_temp4a$other_bin <- as.factor(place_type_temp4a$other_bin)

place_type_temp4a$day_new <- factor(place_type_temp4a$day_new, levels=c("30","7-9","4-6","1-3"))
house_log <- glmer(house_bin ~ day_new + (1|part_number), data=place_type_temp4a, family = "binomial")
exp(fixed.effects(house_log))
summary(house_log) ## no signif. differences
#predict(house_log, type="response") 
resp <- data.frame(day_new=place_type_temp4a$day_new, pred=predict(house_log, type="response"))
mean(resp$pred~resp$day_new)
cc <- confint(house_log,parm="beta_")
ctab <- cbind(est=fixef(house_log),cc)
rtab <- exp(ctab)
print(rtab,digits=5)

health_log <- glmer(health_bin ~ day_new + (1|part_number), data=place_type_temp4a, family = "binomial")
exp(fixed.effects(health_log)) 
summary(health_log) ## all signif.
#predict(health_log, type="response")
resp <- data.frame(day_new=place_type_temp4a$day_new, pred=predict(health_log, type="response"))
mean(resp$pred~resp$day_new)
cc <- confint(health_log,parm="beta_")
ctab <- cbind(est=fixef(health_log),cc)
rtab <- exp(ctab)
print(rtab,digits=5)

ed_log <- glmer(ed_bin ~ day_new + (1|part_number), data=place_type_temp4a, family = "binomial", control = glmerControl(optimizer ="bobyqa"))
exp(fixed.effects(ed_log)) 
summary(ed_log) ## all significant
#predict(ed_log, type="response")
resp <- data.frame(day_new=place_type_temp4a$day_new, pred=predict(ed_log, type="response"))
mean(resp$pred~resp$day_new)
cc <- confint(ed_log,parm="beta_")
ctab <- cbind(est=fixef(ed_log),cc)
rtab <- exp(ctab)
print(rtab,digits=5)

other_log <- glmer(other_bin ~ day_new + (1|part_number), data=place_type_temp4a, family = "binomial")
exp(fixed.effects(other_log)) 
summary(other_log) ## 4-6 < 0.001; 1-3 = 0.083
resp <- data.frame(day_new=place_type_temp4a$day_new, pred=predict(other_log, type="response"))
mean(resp$pred~resp$day_new)
cc <- confint(other_log,parm="beta_")
ctab <- cbind(est=fixef(other_log),cc)
rtab <- exp(ctab)
print(rtab,digits=5)

temp5a$day_new <- factor(temp5a$day_new, levels=c("1-3","4-6","7-9","30"))











################## GRAPH OF PLACE TYPE BY GROUPED DAYS ###################  ####################
place_table <- table(place_type_temp3$place_type_new, place_type_temp3$day_new)
## Number of places listed each day
counts_daily <- colSums(place_table)
## consider times visited for day 30
place_type_temp3$times_visited <- factor(as.numeric(as.character(place_type_temp3$times_visited)),
                              levels=c(1:15))
count_30 <- as.matrix(table(place_type_temp3$place_type_new[which(place_type_temp3$day_new == "30")],
                            place_type_temp3$times_visited[which(place_type_temp3$day_new == "30")]))
count_30_new <- matrix(nrow=4, ncol=15, dimnames=list(c(row.names(count_30)),c(1:15)))
count_30_new[1,] <- count_30[1,]*c(1:15)
count_30_new[2,] <- count_30[2,]*c(1:15)
count_30_new[3,] <- count_30[3,]*c(1:15)
count_30_new[4,] <- count_30[4,]*c(1:15)
day_30_total <- rowSums(count_30_new) #totalls for 15 days
place_table[,4] <- day_30_total

### number of parts with movement on each set of day
a <- table(place_type_temp3$part_number, place_type_temp3$day_new)
a[which(a!=0)] <- 1
colSums(a)
# n=27, n=32, n=34, n=34

place_table2 <- prop.table(place_table, margin=2) * 100
mycol2 <- c("mediumorchid3","yellow","darkblue","green2")
par(mfrow=c(1,1), mar=c(5.1, 4.6, 2.1, 11.3), xpd=TRUE)
barplot(place_table2, col=mycol2[c(1:4)], main="",
        xlab="Days post first symptom", ylab="Frequency of place type visitation",
        cex.names=1.4, cex.lab=1.7, cex.main=1.5, cex.axis=1.5,beside=FALSE, xaxt="n")
axis(1, c(seq(0.5,4,1)), labels = FALSE, tick=FALSE)
text(x=c(seq(0.9,5, 1.2)), 102 , adj = 1,
     labels = c("n=27","n=32","n=34","n=34"), xpd = TRUE, cex=1.25)
legend("topright",bty="n", x.intersp = 0.5, inset=c(-0.49,0), 
       fill=rev(mycol2[c(1:4)]), legend=rev(rownames(place_table2)), cex=1.6)
axis(side=1, at=c(seq(0.7,5,1.2)), labels=c("1-3","4-6","7-9","   Post Illness"), tick=FALSE, cex.axis=1.45)
segments(x0=3.7,x1=3.7,y0=-12,y1=108, lwd=3, lty=2, col="red")








########## AVERAGE ALL DAILY TOGETHER FOR PLACE TYPE ANALYSIS ############
place_type_temp_avg <- droplevels(place_type_temp)
place_type_temp_avg$day_new <- character(length=length(place_type_temp_avg$part_number))
place_type_temp_avg$day_new[which(place_type_temp_avg$day_interest_by_first_symp2 %in% c(1:9))] <- "daily"
place_type_temp_avg$day_new[which(place_type_temp_avg$day_of_survey == 0)] <- "0"
place_type_temp_avg$day_new[which(place_type_temp_avg$day_of_survey ==30)] <- "30"
place_type_temp_avg$day_new <- factor(place_type_temp_avg$day_new, levels= c("0","daily","30"))
place_type_temp_avg2 <- place_type_temp_avg[-c(which(is.na(place_type_temp_avg$day_new))),]
place_type_temp_avg3 <- place_type_temp_avg2[,c(1,11,10,7)]

place_type_temp_avg3$house_bin <- numeric(length=length(place_type_temp_avg3$part_number))
place_type_temp_avg3$house_bin[which(place_type_temp_avg3$place_type_new == "house")] <- 1
place_type_temp_avg3$health_bin <- numeric(length=length(place_type_temp_avg3$part_number))
place_type_temp_avg3$health_bin[which(place_type_temp_avg3$place_type_new == "health")] <- 1
place_type_temp_avg3$ed_bin <- numeric(length=length(place_type_temp_avg3$part_number))
place_type_temp_avg3$ed_bin[which(place_type_temp_avg3$place_type_new == "education/work")] <- 1
place_type_temp_avg3$other_bin <- numeric(length=length(place_type_temp_avg3$part_number))
place_type_temp_avg3$other_bin[which(place_type_temp_avg3$place_type_new == "other")] <- 1

#### day 0 and 30 --> need "times visited" to be considered #####
place_type_temp_avg3$times_visited[which(is.na(place_type_temp_avg3$times_visited))] <- 1
place_type_temp_avg3a <- place_type_temp_avg3
rows <- which(place_type_temp_avg3$day_new %in% c("0","30") & place_type_temp_avg3$house_bin == "1")
times <- as.numeric(as.character(place_type_temp_avg3$times_visited[which(place_type_temp_avg3$day_new %in% c("0","30") & place_type_temp_avg3$house_bin == "1")]))
for(i in 1:(length(rows))){
  row <- rows[i]
  time <- times[i] - 1
  new_rows <- place_type_temp_avg3[rep(row, times=time),]
  place_type_temp_avg3a <- rbind(place_type_temp_avg3a, new_rows)
}
rows <- which(place_type_temp_avg3$day_new %in% c("0","30") & place_type_temp_avg3$health_bin == "1")
times <- as.numeric(as.character(place_type_temp_avg3$times_visited[which(place_type_temp_avg3$day_new %in% c("0","30") & place_type_temp_avg3$health_bin == "1")]))
for(i in 1:(length(rows))){
  row <- rows[i]
  time <- times[i] - 1
  new_rows <- place_type_temp_avg3[rep(row, times=time),]
  place_type_temp_avg3a <- rbind(place_type_temp_avg3a, new_rows)
}
rows <- which(place_type_temp_avg3$day_new  %in% c("0","30") & place_type_temp_avg3$ed_bin == "1")
times <- as.numeric(as.character(place_type_temp_avg3$times_visited[which(place_type_temp_avg3$day_new  %in% c("0","30") & place_type_temp_avg3$ed_bin == "1")]))
for(i in 1:(length(rows))){
  row <- rows[i]
  time <- times[i] - 1
  new_rows <- place_type_temp_avg3[rep(row, times=time),]
  place_type_temp_avg3a <- rbind(place_type_temp_avg3a, new_rows)
}
rows <- which(place_type_temp_avg3$day_new  %in% c("0","30") & place_type_temp_avg3$other_bin == "1")
times <- as.numeric(as.character(place_type_temp_avg3$times_visited[which(place_type_temp_avg3$day_new  %in% c("0","30") & place_type_temp_avg3$other_bin == "1")]))
for(i in 1:(length(rows))){
  row <- rows[i]
  time <- times[i] - 1
  new_rows <- place_type_temp_avg3[rep(row, times=time),]
  place_type_temp_avg3a <- rbind(place_type_temp_avg3a, new_rows)
}

place_type_temp_avg3a$house_bin <- as.factor(place_type_temp_avg3a$house_bin)
place_type_temp_avg3a$health_bin <- as.factor(place_type_temp_avg3a$health_bin)
place_type_temp_avg3a$ed_bin <- as.factor(place_type_temp_avg3a$ed_bin)
place_type_temp_avg3a$other_bin <- as.factor(place_type_temp_avg3a$other_bin)

#### fit linear mixed effect models ####
place_type_temp_avg3a$day_new <- factor(place_type_temp_avg3a$day_new, levels=c("daily","0","30"))

house_log <- glmer(house_bin ~ day_new + (1|part_number), data=place_type_temp_avg3a, family = "binomial")
exp(fixed.effects(house_log))
summary(house_log) 
#predict(house_log, type="response") 
resp <- data.frame(day_new=place_type_temp_avg3a$day_new, pred=predict(house_log, type="response"))
mean(resp$pred~resp$day_new)
cc <- confint(house_log,parm="beta_")
ctab <- cbind(est=fixef(house_log),cc)
rtab <- exp(ctab)
print(rtab,digits=5)

health_log <- glmer(health_bin ~ day_new + (1|part_number), data=place_type_temp_avg3a, family = "binomial")
exp(fixed.effects(health_log))
summary(health_log) 
#predict(health_log, type="response") 
resp <- data.frame(day_new=place_type_temp_avg3a$day_new, pred=predict(health_log, type="response"))
mean(resp$pred~resp$day_new)
cc <- confint(health_log,parm="beta_")
ctab <- cbind(est=fixef(health_log),cc)
rtab <- exp(ctab)
print(rtab,digits=5)

ed_log <- glmer(ed_bin ~ day_new + (1|part_number), data=place_type_temp_avg3a, family = "binomial")
exp(fixed.effects(ed_log))
summary(ed_log) 
#predict(ed_log, type="response") 
resp <- data.frame(day_new=place_type_temp_avg3a$day_new, pred=predict(ed_log, type="response"))
mean(resp$pred~resp$day_new)
cc <- confint(ed_log,parm="beta_")
ctab <- cbind(est=fixef(ed_log),cc)
rtab <- exp(ctab)
print(rtab,digits=5)

other_log <- glmer(other_bin ~ day_new + (1|part_number), data=place_type_temp_avg3a, family = "binomial")
exp(fixed.effects(other_log))
summary(other_log) 
#predict(other_log, type="response") 
resp <- data.frame(day_new=place_type_temp_avg3a$day_new, pred=predict(other_log, type="response"))
mean(resp$pred~resp$day_new)
cc <- confint(other_log,parm="beta_")
ctab <- cbind(est=fixef(other_log),cc)
rtab <- exp(ctab)
print(rtab,digits=5)










###################### GRAPH OF PLACE TYPE BY AVERAGED DAYS #################################
place_table_avg <- table(place_type_temp_avg$place_type_new, place_type_temp_avg$day_new)
## Number of places listed each day
counts_daily <- colSums(place_table_avg)

place_type_temp_avg$times_visited <- factor(as.numeric(as.character(place_type_temp_avg$times_visited)),
                                levels=c(1:15))
count_30 <- as.matrix(table(place_type_temp_avg$place_type_new[which(place_type_temp_avg$day_new == "30")],
                            place_type_temp_avg$times_visited[which(place_type_temp_avg$day_new == "30")]))
count_30_new <- matrix(nrow=4, ncol=15, dimnames=list(c(row.names(count_30)),c(1:15)))
count_30_new[1,] <- count_30[1,]*c(1:15)
count_30_new[2,] <- count_30[2,]*c(1:15)
count_30_new[3,] <- count_30[3,]*c(1:15)
count_30_new[4,] <- count_30[4,]*c(1:15)
day_30_total <- rowSums(count_30_new) #totalls for 15 days
place_table_avg[,3] <- day_30_total

###
count_0 <- as.matrix(table(place_type_temp_avg$place_type_new[which(place_type_temp_avg$day_new == "0")],
                           place_type_temp_avg$times_visited[which(place_type_temp_avg$day_new == "0")]))
count_0_new <- matrix(nrow=4, ncol=15, dimnames=list(c(row.names(count_0)),c(1:15)))
count_0_new[1,] <- count_0[1,]*c(1:15)
count_0_new[2,] <- count_0[2,]*c(1:15)
count_0_new[3,] <- count_0[3,]*c(1:15)
count_0_new[4,] <- count_0[4,]*c(1:15)
day_0_total <- rowSums(count_0_new) #totalls for 15 days
place_table_avg[,1] <- day_0_total

### number of people
a <- table(place_type_temp_avg$part_number, place_type_temp_avg$day_new)
a[which(a!=0)] <- 1
colSums(a)
# n=63, n=52, n=34

place_table_avg2 <- prop.table(place_table_avg, margin=2) * 100
mycol2 <- c("mediumorchid3","yellow","darkblue","green2")
par(mfrow=c(1,1), mar=c(5.1, 4.6, 2.1, 10), xpd=TRUE)
barplot(place_table_avg2, col=mycol2[c(1:4)], main="",
        xlab="Day of Movement Survey", ylab="Frequency of place type visitation",
        cex.names=1.4, cex.lab=1.7, cex.main=1.5, cex.axis=1.5,beside=FALSE, xaxt="n")
axis(1, c(seq(0.5,4,1)), labels = FALSE, tick=FALSE)
text(x=c(seq(0.9,4, 1.2)), 102 , adj = 1,
     labels = c("n=63","n=52","n=35"), xpd = TRUE, cex=1.35)
legend("topright",inset=c(-0.44,0), bty="n", x.intersp = 0.5,
       fill=rev(mycol2[c(1:4)]), legend=rev(rownames(place_table_avg)), cex=1.5)
axis(side=1, at=c(seq(0.7,4,1.2)), 
     labels=c("Day 0\nRetrosective","Averaged Daily\n(Days 1-9)","Post-illness\n"), 
     tick=FALSE, cex.axis=1.3, line=0.25)






##################### HOUSE TYPE FOR GROUPED DAYS ##################### ####################
temp_house_type <- MASTERFILE_DIAGNOSIS7
parts_list <- levels(temp_house_type$part_number)
temp_house_type$place_name3 <- character(length=length(temp_house_type$part_number))
temp_house_type$place_name3[which(temp_house_type$place_name2 %in% c("brother_in_law_house","house_brother","house_child",
                                               "house_cousin","house_grandparents","house_nephew",
                                               "house_uncle","parents_house", "House_law"))] <- "house family" 
temp_house_type$place_name3[which(temp_house_type$place_name2 %in% c("house_friend", "house_of_love"))] <- "house friend" 
temp_house_type$place_name3[which(temp_house_type$place_name2 %in% c("Someone_else_home"))] <- "house other" 
temp_house_type$place_name3 <- as.factor(as.character(temp_house_type$place_name3))
temp_house_type$place_name3[which(temp_house_type$place_name3 == "")] <- NA
temp_house_type$place_type <- as.character(temp_house_type$place_type)
temp_house_type$place_name3 <- as.character(temp_house_type$place_name3)
temp_house_type$place_type[which(temp_house_type$place_type == "house")] <- temp_house_type$place_name3[which(temp_house_type$place_type == "house")]
temp_house_type$place_type <- as.factor(as.character(temp_house_type$place_type))

temp_house_type2 <- droplevels(temp_house_type[-c(which(temp_house_type$day_interest_by_first_symp2 %in% c("0","10","11","12","13","14"))),])
temp_house_type2 <- droplevels(temp_house_type2[-c(which(temp_house_type2$day_of_survey == "0")),])
temp_house_type2$day.gp <- character(length=length(temp_house_type2$part_number))
temp_house_type2$day.gp[which(temp_house_type2$day_interest_by_first_symp2 %in% c(1,2,3))] <- "1-3"
temp_house_type2$day.gp[which(temp_house_type2$day_interest_by_first_symp2 %in% c(4,5,6))] <- "4-6"
temp_house_type2$day.gp[which(temp_house_type2$day_interest_by_first_symp2 %in% c(7,8,9))] <- "7-9"
temp_house_type2$day.gp[which(temp_house_type2$day_interest_by_first_symp2 %in% c(30))] <- "30"
temp_house_type2$day.gp[which(temp_house_type2$day.gp == "")] <- NA
temp_house_type2$day.gp <- factor(temp_house_type2$day.gp, levels=c("1-3","4-6","7-9","30"))
temp_house_type3 <- temp_house_type2[-c(which(is.na(temp_house_type2$day.gp))),]
temp_house_type3a <- temp_house_type3[-c(which(is.na(temp_house_type3$place_name3))),]
temp_house_type4a <- temp_house_type3a[,c(1,11,10,7)]
temp_house_type4a$place_name3 <- as.factor(temp_house_type4a$place_name3)

temp_house_type4a$fam_bin <- numeric(length=length(temp_house_type4a$part_number))
temp_house_type4a$fam_bin[which(temp_house_type4a$place_name3 == "house family")] <- 1

#### day 30 --> need "times visited" to be considered #####
temp_house_type5 <- temp_house_type4a

rows <- which(temp_house_type4a$day.gp == "30" & temp_house_type4a$fam_bin == "1")
times <- as.numeric(as.character(temp_house_type4a$times_visited[which(temp_house_type4a$day.gp == "30" & temp_house_type4a$fam_bin == "1")]))
for(i in 1:(length(rows))){
  row <- rows[i]
  time <- times[i] - 1
  new_rows <- temp_house_type4a[rep(row, times=time),]
  temp_house_type5 <- rbind(temp_house_type5, new_rows)
}
rows <- which(temp_house_type4a$day.gp == "30" & temp_house_type4a$fam_bin == "0")
times <- as.numeric(as.character(temp_house_type4a$times_visited[which(temp_house_type4a$day.gp == "30" & temp_house_type4a$fam_bin == "0")]))
for(i in 1:(length(rows))){
  row <- rows[i]
  time <- times[i] - 1
  new_rows <- temp_house_type4a[rep(row, times=time),]
  temp_house_type5 <- rbind(temp_house_type5, new_rows)
}
temp_house_type5$fam_bin <- as.factor(temp_house_type5$fam_bin)
temp_house_type5$day.gp <- factor(temp_house_type5$day.gp, levels=c("30","1-3","4-6","7-9"))

#### fit linear mixed effect models ####
fam_log <- glmer(fam_bin ~ day.gp + (1|part_number), data=temp_house_type5, family = "binomial", control = glmerControl(optimizer ="bobyqa"))
exp(fixed.effects(fam_log))
summary(fam_log) 
#predict(fam_log, type="response")
resp <- data.frame(day_new=temp_house_type5$day.gp, pred=predict(fam_log, type="response"))
mean(resp$pred~resp$day_new)
cc <- confint(fam_log,parm="beta_")
ctab <- cbind(est=fixef(fam_log),cc)
rtab <- exp(ctab)
print(rtab,digits=5)





########## AVERAGE ALL DAILY TOGETHER FOR HOUSE TYPE ANALYSIS ############
temp_house_type_avg <- droplevels(temp_house_type)
temp_house_type_avg$day_new <- character(length=length(temp_house_type_avg$part_number))
temp_house_type_avg$day_new[which(temp_house_type_avg$day_interest_by_first_symp2 %in% c(1:9))] <- "daily"
temp_house_type_avg$day_new[which(temp_house_type_avg$day_of_survey == 0)] <- "0"
temp_house_type_avg$day_new[which(temp_house_type_avg$day_of_survey ==30)] <- "30"
temp_house_type_avg$day_new <- factor(temp_house_type_avg$day_new, levels= c("0","daily","30"))
temp_house_type_avg <- temp_house_type_avg[-c(which(is.na(temp_house_type_avg$day_new))),]
temp_house_type_avg2 <- temp_house_type_avg[-c(which(is.na(temp_house_type_avg$place_name3))),]
temp_house_type_avg3 <- temp_house_type_avg2[,c(1,11,10,7)]

temp_house_type_avg3$fam_bin <- numeric(length=length(temp_house_type_avg3$part_number))
temp_house_type_avg3$fam_bin[which(temp_house_type_avg3$place_name3 == "house family")] <- 1

#### day 0 and 30 --> need "times visited" to be considered #####
temp_house_type_avg3$times_visited[which(is.na(temp_house_type_avg3$times_visited))] <- 1
temp_house_type_avg3$times_visited[which(temp_house_type_avg3$day_new == "daily" & temp_house_type_avg3$times_visited > 1)] <- 1
temp_house_type_avg3a <- temp_house_type_avg3
rows <- which(temp_house_type_avg3$day_new %in% c("0","30") & temp_house_type_avg3$fam_bin == "1")
times <- as.numeric(as.character(temp_house_type_avg3$times_visited[which(temp_house_type_avg3$day_new %in% c("0","30") & temp_house_type_avg3$fam_bin == "1")]))
for(i in 1:(length(rows))){
  row <- rows[i]
  time <- times[i] - 1
  new_rows <- temp_house_type_avg3[rep(row, times=time),]
  temp_house_type_avg3a <- rbind(temp_house_type_avg3a, new_rows)
}
rows <- which(temp_house_type_avg3$day_new %in% c("0","30") & temp_house_type_avg3$fam_bin == "0")
times <- as.numeric(as.character(temp_house_type_avg3$times_visited[which(temp_house_type_avg3$day_new %in% c("0","30") & temp_house_type_avg3$fam_bin == "0")]))
for(i in 1:(length(rows))){
  row <- rows[i]
  time <- times[i] - 1
  new_rows <- temp_house_type_avg3[rep(row, times=time),]
  temp_house_type_avg3a <- rbind(temp_house_type_avg3a, new_rows)
}
temp_house_type_avg3a$fam_bin <- as.factor(temp_house_type_avg3a$fam_bin)

#### fit linear mixed effect models ####
temp_house_type_avg3a$day_new <- factor(temp_house_type_avg3a$day_new, levels=c("daily","0","30"))

fam_log <- glmer(fam_bin ~ day_new + (1|part_number), data=temp_house_type_avg3a, family = "binomial")
exp(fixed.effects(fam_log))
summary(fam_log) 
#predict(fam_log, type="response") 
resp <- data.frame(day_new=temp_house_type_avg3a$day_new, pred=predict(fam_log, type="response"))
mean(resp$pred~resp$day_new)
cc <- confint(fam_log,parm="beta_")
ctab <- cbind(est=fixef(fam_log),cc)
rtab <- exp(ctab)
print(rtab,digits=5)

