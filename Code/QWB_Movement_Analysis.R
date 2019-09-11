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
library(gamlss.dist)
library(gamlss)
library(Hmisc)
library(corrplot)
library(car)
library(reshape2)

######## IMPORT FILES ########
setwd()
temp_qwb <- read.csv("QWB_Movement_Data.csv", sep=",", header=T)

####### look at 35 participants with all QWB surveys matched up ########
parts_3 <- row.names(as.data.frame(which(summary(as.factor(temp_qwb$part[which(!(is.na(temp_qwb$qwb_score)))])) == 3)))
temp_qwb_3 <- droplevels(temp_qwb[which(temp_qwb$part %in% parts_3),])
########## For days of interest for each part, QWB score is assigned:
## ==> day(2_3)-3 ---> day(2_3)-1 <- score(2_3)
## ==> day(7)-3 ---> day(7)-1 <- score(7)
## ==> day(30) <- score(30)
##### For 2_3 and week QWB scores, need to put score on other days its discussing (not just survey day)
# A part's 2_3 QWB score should be made QWB score on 3 days before current, but not current day
# A part's week QWB score should be made QWB score on 3 days before current, but not current
for(i in 1:(length(parts_3))){
  part <- parts_3[i]
  rows <- which(temp_qwb_3$part == part)
  rows_qwb <- which(temp_qwb_3$part == part & !(is.na(temp_qwb_3$qwb_score)))
  positions <- which(rows %in% rows_qwb)
  if(rows_qwb[1]-3 > rows[1]){  ## from 2nd row since first is day "-15"
    rows_2_3_new <- (rows_qwb[1]-3):(rows_qwb[1] -1)
  }
  if (rows_qwb[1]-3 == rows[1]){
    rows_2_3_new <- (rows_qwb[1]-2):(rows_qwb[1] -1)
  }
  if(rows_qwb[1]-2 == rows[1]){
    rows_2_3_new <- rows_qwb[1] -1
  }
  rows_week_new <- (rows_qwb[2]-3):(rows_qwb[2] -1)
  temp_qwb_3$qwb_score[rows_2_3_new] <- temp_qwb_3$qwb_score[rows_qwb[1]]
  temp_qwb_3$qwb_score[rows_week_new] <- temp_qwb_3$qwb_score[rows_qwb[2]]
  if(!(rows_qwb[1] %in% rows_week_new)){
    temp_qwb_3$qwb_score[rows_qwb[1]] <- ""
    temp_qwb_3$qwb_score[rows_qwb[2]] <- ""
  }
  else if (rows_qwb[1] %in% rows_week_new){
    temp_qwb_3$qwb_score[rows_qwb[2]] <- ""
  }
}
temp_qwb_3$qwb_score[which(temp_qwb_3$qwb_score == "")] <- NA
temp_qwb_3$part <- as.factor(temp_qwb_3$part)



################## Get 38 parts with both 2/3 and week ##################
temp_qwb_short <- temp_qwb[which(temp_qwb$day != 30),]
temp_qwb_short <- temp_qwb_short[which(temp_qwb_short$day != -15),]
parts_2_3 <- row.names(as.data.frame(which(summary(as.factor(temp_qwb_short$part[which(!(is.na(temp_qwb_short$qwb_score)))])) == 2)))
temp_qwb_short1 <- droplevels(temp_qwb_short[which(temp_qwb_short$part %in% parts_2_3),])
########## For days of interest for each part, QWB score is assigned:
## ==> day(2_3)-3 ---> day(2_3)-1 <- score(2_3)
## ==> day(7)-3 ---> day(7)-1 <- score(7)
##### For 2_3 and week QWB scores, need to put score on other days its discussing (not just survey day)
# A part's 2_3 QWB score should be made QWB score on 3 days before current, but not current day
# A part's week QWB score should be made QWB score on 3 days before current, but not current
for(i in 1:(length(parts_2_3))){
  part <- parts_2_3[i]
  rows <- which(temp_qwb_short1$part == part)
  rows_qwb <- which(temp_qwb_short1$part == part & !(is.na(temp_qwb_short1$qwb_score)))
  positions <- which(rows %in% rows_qwb)
  if(rows_qwb[1]-3 != rows[1]){  ## from 2nd row since first is day "-15"
    rows_2_3_new <- (rows_qwb[1]-3):(rows_qwb[1] -1)
  }
  if (rows_qwb[1]-3 == rows[1]){
    rows_2_3_new <- (rows_qwb[1]-2):(rows_qwb[1] -1)
  }
  rows_week_new <- (rows_qwb[2]-3):(rows_qwb[2] -1)
  temp_qwb_short1$qwb_score[rows_2_3_new] <- temp_qwb_short1$qwb_score[rows_qwb[1]]
  temp_qwb_short1$qwb_score[rows_week_new] <- temp_qwb_short1$qwb_score[rows_qwb[2]]
  if(!(rows_qwb[1] %in% rows_week_new)){
    temp_qwb_short1$qwb_score[rows_qwb[1]] <- ""
    temp_qwb_short1$qwb_score[rows_qwb[2]] <- ""
  }
  else if (rows_qwb[1] %in% rows_week_new){
    temp_qwb_short1$qwb_score[rows_qwb[2]] <- ""
  }
}
temp_qwb_short1$qwb_score[which(temp_qwb_short1$qwb_score == "")] <- NA
temp_qwb_short1$part <- as.factor(temp_qwb_short1$part)
temp_qwb_short2 <- droplevels(temp_qwb_short1)

#########################################################
##################### GLMM/GAMM/GAMLSSs ##################
temp_qwb_short3 <- temp_qwb_short2
temp_qwb_short3$prop_hours <- temp_qwb_short3$hours_home/17
temp_qwb_short3 <- temp_qwb_short3[which(temp_qwb_short3$day %in% c(1:9)),]
temp_qwb_short3_new <- droplevels(temp_qwb_short3[,c(1:5,7:14)])
temp_qwb_short3_new2 <- temp_qwb_short3_new[which(!(is.na(temp_qwb_short3_new$total_locations))),]
temp_qwb_short3_new2$qwb_score <- as.numeric(temp_qwb_short3_new2$qwb_score)
temp_qwb_short3_new2$day <- as.numeric(as.character(temp_qwb_short3_new2$day))

######### ######### ######### ABSOLUTE TOTAL LOCATIONS ######### ######### ##########
######### Non-linearity?
gamm_non_lin_test <- gamm(total_locations ~ s(day, bs="ts", k=3) + day, random=list(part=~1), 
                          data=temp_qwb_short3_new2[which(!(is.na(temp_qwb_short3_new2$qwb_score))),], family=poisson()) ## ***
gamm_0_test <- gamm(total_locations ~ s(day, bs="ts", k=3), random=list(part=~1), 
                    data=temp_qwb_short3_new2[which(!(is.na(temp_qwb_short3_new2$qwb_score))),], family=poisson())
glmm_0_test <- glmer(total_locations ~ day + (1|part), 
                     data=temp_qwb_short3_new2[which(!(is.na(temp_qwb_short3_new2$qwb_score))),], family=poisson()) ## ***
#### GLMM tests
glmm_0_test <- glmer(total_locations ~ day + (1|part), data=temp_qwb_short3_new2[which(!(is.na(temp_qwb_short3_new2$qwb_score))),], family=poisson()) ## ***
glmm_1_test <- glmer(total_locations ~ qwb_score + (1|part), data=temp_qwb_short3_new2[which(!(is.na(temp_qwb_short3_new2$qwb_score))),], family=poisson()) ## ***
glmm_3_test <- glmer(total_locations ~ male3 + (1|part), data=temp_qwb_short3_new2[which(!(is.na(temp_qwb_short3_new2$qwb_score))),], family=poisson()) ## ***
glmm_4_test <- glmer(total_locations ~ age_18_up2 + (1|part), data=temp_qwb_short3_new2[which(!(is.na(temp_qwb_short3_new2$qwb_score))),], family=poisson()) ## ***
glmm_5_test <- glmer(total_locations ~ socio_comb + (1|part), data=temp_qwb_short3_new2[which(!(is.na(temp_qwb_short3_new2$qwb_score))),], family=poisson()) ## ***
glmm_6_test <- glmer(total_locations ~ (1|part), data=temp_qwb_short3_new2[which(!(is.na(temp_qwb_short3_new2$qwb_score))),], family=poisson()) ## ***
anova(glmm_0_test, glmm_1_test)
anova(glmm_0_test, glmm_2_test)
anova(glmm_0_test, glmm_3_test)
anova(glmm_0_test, glmm_4_test)
anova(glmm_0_test, glmm_5_test)
anova(glmm_0_test, glmm_6_test)
anova(glmm_0_test, glmm_1_test, glmm_2_test, glmm_3_test)

glmm_7_test <- glmer(total_locations ~ day + male3 + (1|part), data=temp_qwb_short3_new2[which(!(is.na(temp_qwb_short3_new2$qwb_score))),], family=poisson()) ## ***
glmm_8_test <- glmer(total_locations ~ day + age_18_up2 + (1|part), data=temp_qwb_short3_new2[which(!(is.na(temp_qwb_short3_new2$qwb_score))),], family=poisson()) ## ***
glmm_9_test <- glmer(total_locations ~ day + qwb_score + (1|part), data=temp_qwb_short3_new2[which(!(is.na(temp_qwb_short3_new2$qwb_score))),], family=poisson()) ## ***
glmm_10_test <- glmer(total_locations ~ day*qwb_score + (1|part), data=temp_qwb_short3_new2[which(!(is.na(temp_qwb_short3_new2$qwb_score))),], family=poisson()) ## ***
glmm_11_test <- glmer(total_locations ~ day*age_18_up2 + (1|part), data=temp_qwb_short3_new2[which(!(is.na(temp_qwb_short3_new2$qwb_score))),], family=poisson()) ## ***
glmm_12_test <- glmer(total_locations ~ day*male3 + (1|part), data=temp_qwb_short3_new2[which(!(is.na(temp_qwb_short3_new2$qwb_score))),], family=poisson()) ## ***
anova(glmm_0_test, glmm_7_test)
anova(glmm_0_test, glmm_8_test)
anova(glmm_0_test, glmm_9_test)
anova(glmm_0_test, glmm_10_test)
anova(glmm_0_test, glmm_11_test)
anova(glmm_0_test, glmm_12_test)
anova(glmm_0_test, glmm_1_test, glmm_9_test, glmm_10_test)
anova(glmm_0_test, glmm_1_test)
anova(glmm_0_test, glmm_9_test)
anova(glmm_0_test, glmm_10_test)

#### GAMM tests
gamm_0a_test <- gamm(total_locations ~ day, random=list(part=~1), 
                     data=temp_qwb_short3_new2[which(!(is.na(temp_qwb_short3_new2$qwb_score))),], family=poisson())
gamm_0b_test <- gamm(total_locations ~ s(day, k=3), random=list(part=~1), 
                     data=temp_qwb_short3_new2[which(!(is.na(temp_qwb_short3_new2$qwb_score))),], family=poisson())
gamm_0c_test <- gamm(total_locations ~ qwb_score, random=list(part=~1), 
                     data=temp_qwb_short3_new2[which(!(is.na(temp_qwb_short3_new2$qwb_score))),], family=poisson())
gamm_0d_test <- gamm(total_locations ~ s(qwb_score, bs="ts", k=3), random=list(part=~1), 
                     data=temp_qwb_short3_new2[which(!(is.na(temp_qwb_short3_new2$qwb_score))),], family=poisson())
AIC(gamm_0a_test$lme, gamm_0b_test$lme, gamm_0c_test$lme, gamm_0d_test$lme)
(1-(gamm_0a_test$gam$df.residual/gamm_0a_test$gam$df.null))*100
(1-(gamm_0b_test$gam$df.residual/gamm_0b_test$gam$df.null))*100
(1-(gamm_0c_test$gam$df.residual/gamm_0c_test$gam$df.null))*100
(1-(gamm_0d_test$gam$df.residual/gamm_0d_test$gam$df.null))*100

gamm_1z_test <- gamm(total_locations ~ day + qwb_score, random=list(part=~1), 
                     data=temp_qwb_short3_new2[which(!(is.na(temp_qwb_short3_new2$qwb_score))),], family=poisson())
gamm_1a_test <- gamm(total_locations ~ day + s(qwb_score), random=list(part=~1), 
                     data=temp_qwb_short3_new2[which(!(is.na(temp_qwb_short3_new2$qwb_score))),], family=poisson())
gamm_1b_test <- gamm(total_locations ~ s(day, bs="ts", k=3) + qwb_score, random=list(part=~1), 
                     data=temp_qwb_short3_new2[which(!(is.na(temp_qwb_short3_new2$qwb_score))),], family=poisson())
gamm_2_test <- gamm(total_locations ~ s(day, bs="ts", k=3) + s(qwb_score), random=list(part=~1), 
                    data=temp_qwb_short3_new2[which(!(is.na(temp_qwb_short3_new2$qwb_score))),], family=poisson())
gamm_3_test <- gamm(total_locations ~ s(day, bs="ts", k=3) + s(qwb_score, bs="ts", k=3) + ti(qwb_score, day), random=list(part=~1), 
                    data=temp_qwb_short3_new2[which(!(is.na(temp_qwb_short3_new2$qwb_score))),], family=poisson())
gamm_4_test <- gamm(total_locations ~ te(qwb_score, day), random=list(part=~1), 
                    data=temp_qwb_short3_new2[which(!(is.na(temp_qwb_short3_new2$qwb_score))),], family=poisson())
(1-(gamm_3_test$gam$df.residual/gamm_3_test$gam$df.null))*100
(1-(gamm_4_test$gam$df.residual/gamm_4_test$gam$df.null))*100
AIC(gamm_1z_test$lme, gamm_0a_test$lme, gamm_1a_test$lme, gamm_1b_test$lme, gamm_2_test$lme, gamm_3_test$lme, gamm_4_test$lme)
AIC(gamm_0a_test$lme, gamm_3_test$lme, gamm_4_test$lme)


gamm_0a1_test <- gamm(total_locations ~ day + age_18_up2, random=list(part=~1), 
                      data=temp_qwb_short3_new2[which(!(is.na(temp_qwb_short3_new2$qwb_score))),], family=poisson())
gamm_0a2_test <- gamm(total_locations ~ day + male3, random=list(part=~1), 
                      data=temp_qwb_short3_new2[which(!(is.na(temp_qwb_short3_new2$qwb_score))),], family=poisson())
gamm_0a3_test <- gamm(total_locations ~ day + socio_comb, random=list(part=~1), 
                      data=temp_qwb_short3_new2[which(!(is.na(temp_qwb_short3_new2$qwb_score))),], family=poisson())
gamm_0a4_test <- gamm(total_locations ~ day*qwb_score, random=list(part=~1), 
                      data=temp_qwb_short3_new2[which(!(is.na(temp_qwb_short3_new2$qwb_score))),], family=poisson())
AIC(gamm_0a_test$lme, gamm_0a1_test$lme, gamm_0a2_test$lme, gamm_0a3_test$lme, gamm_0a4_test$lme)
summary(gamm_0a_test$gam)




######### ######### ######### RELATIVE LOCATIONS ######### #########
descdist(temp_qwb_short3_new2$rel_locations, boot=500)
## normal or log
shapiro.test(temp_qwb_short3_new2$rel_locations) ## p=0.01 --> not normal
fit_l <- fitdist(temp_qwb_short3_new2$rel_locations, "logis")
par(mfrow=c(2,2))
denscomp(fit_l)
cdfcomp(fit_l)
qqcomp(fit_l)
ppcomp(fit_l)
## logis seems like a good fit

nn <- droplevels(na.omit(temp_qwb_short3_new2))
test <- gamlss(rel_locations ~  day, data=nn, family = "LO") ## 
test1 <- gamlss(rel_locations ~  qwb_score, data=nn, family = "LO") ## 
test2 <- gamlss(rel_locations ~  male3, data=nn, family = "LO") ## 
test3 <- gamlss(rel_locations ~  age_18_up2, data=nn, family = "LO") ## 
test4 <- gamlss(rel_locations ~  socio_comb, data=nn, family = "LO") ##
test5 <- gamlss(rel_locations ~  re(random=~1|part), data=nn, family = "LO") ##
AIC(test,test1,test2,test3,test4, test5)
##          df        AIC
## test5   29.9      383.0      --> AIC is 41 less, df is 27 higher
## test1   3.0       423.9

test5 <- gamlss(rel_locations ~  re(random=~1|part), data=nn, family = "LO") ##
test5a <- gamlss(rel_locations ~  re(random=~1|part) + day, data=nn, family = "LO") ## 
test5b <- gamlss(rel_locations ~  re(random=~1|part) + age_18_up2, data=nn, family = "LO") ## 
test5c <- gamlss(rel_locations ~  re(random=~1|part) + male3, data=nn, family = "LO") ## 
test5d <- gamlss(rel_locations ~  re(random=~1|part) + socio_comb, data=nn, family = "LO") ## 
test5e <- gamlss(rel_locations ~  re(random=~1|part) + qwb_score, data=nn, family = "LO") ## 
AIC(test5,test5a,test5b,test5c,test5d,test5e)
##           df        AIC
## test5a   31.3      378.3      --> AIC is 5 less, df is 1.4 higher
## test5    29.9      383.0 

test5a <- gamlss(rel_locations ~  re(random=~1|part) + day, data=nn, family = "LO") ## 
test5a1 <- gamlss(rel_locations ~  re(random=~1|part) + day + qwb_score, data=nn, family = "LO") ## 
test5a2 <- gamlss(rel_locations ~  re(random=~1|part) + day + age_18_up2, data=nn, family = "LO") ## 
test5a3 <- gamlss(rel_locations ~  re(random=~1|part) + day + male3, data=nn, family = "LO") ## 
test5a4 <- gamlss(rel_locations ~  re(random=~1|part) + day + socio_comb, data=nn, family = "LO") ## 
AIC(test5a,test5a1,test5a2,test5a3,test5a4)
##           df        AIC
## test5a   31.3      378.3      --> AIC is 1.4 less, df is 0.9 less
## test5a2  32.2      379.7 

test5aa <- gamlss(rel_locations ~  re(random=~1|part) + day, sigma.formula =~ day, data=nn, family = "LO") ## 
test5ab <- gamlss(rel_locations ~  re(random=~1|part) + day, sigma.formula =~ qwb_score, data=nn, family = "LO") ## 
test5ac <- gamlss(rel_locations ~  re(random=~1|part) + day, sigma.formula =~ socio_comb, data=nn, family = "LO") ## 
test5ad <- gamlss(rel_locations ~  re(random=~1|part) + day, sigma.formula =~ age_18_up2, data=nn, family = "LO") ## 
test5ae <- gamlss(rel_locations ~  re(random=~1|part) + day, sigma.formula =~ male3, data=nn, family = "LO") ## 
#test5af <- gamlss(rel_locations ~  re(random=~1|part) + day, sigma.formula =~ re(random=~1|part), data=nn, family = "LO") ## 
AIC(test5a,test5aa,test5ab,test5ac,test5ad, test5ae)
##           df        AIC
## test5aa  36.6      360.3      --> AIC is 18 less, df is 5.3 higher
## test5a   31.3      378.3 

test5aa <- gamlss(rel_locations ~  re(random=~1|part) + day, sigma.formula =~ day, data=nn, family = "LO") ## 
test5aa1 <- gamlss(rel_locations ~  re(random=~1|part) + day, sigma.formula =~ day + qwb_score, data=nn, family = "LO", control=gamlss.control(n.cyc=60)) ## 
test5aa2 <- gamlss(rel_locations ~  re(random=~1|part) + day, sigma.formula =~ day + socio_comb, data=nn, family = "LO", control=gamlss.control(n.cyc=60)) ## 
test5aa3 <- gamlss(rel_locations ~  re(random=~1|part) + day, sigma.formula =~ day + age_18_up2, data=nn, family = "LO", control=gamlss.control(n.cyc=60)) ## 
test5aa4 <- gamlss(rel_locations ~  re(random=~1|part) + day, sigma.formula =~ day + male3, data=nn, family = "LO", control=gamlss.control(n.cyc=60)) ## 
#test5aa5 <- gamlss(rel_locations ~  re(random=~1|part) + day, sigma.formula =~ day + re(random=~1|part), data=nn, family = "LO", control=gamlss.control(n.cyc=60)) ## 
AIC(test5aa,test5aa1,test5aa2,test5aa3,test5aa4, test5aa3a, test5aa2a)
##           df        AIC
## test5aa  36.6      360.3      --> AIC is ~2 less, df is 1 less
## test5aa3   37.7      362.1 
test5aa2a <- gamlss(rel_locations ~  re(random=~1|part) + day, sigma.formula =~ day*qwb_score, data=nn, family = "LO", control=gamlss.control(n.cyc=60)) ## 
test5aa3a <- gamlss(rel_locations ~  re(random=~1|part) + day, sigma.formula =~ day*age_18_up2, data=nn, family = "LO", control=gamlss.control(n.cyc=60)) ## 
##              df        AIC
## test5aa2a   39.5      339.4      --> vs 5aa --> AIC is 21 less, df is 2.9 higher **
## test5aa3a   39.0      358.9      --> vs 5aa --> AIC is ~2 less, df is 2.4 higher
## test5aa     36.6      360.3      

test5aa2a <- gamlss(rel_locations ~  re(random=~1|part) + day, sigma.formula =~ day*qwb_score, data=nn, family = "LO", control=gamlss.control(n.cyc=60)) ## 
test5aa2a1 <- gamlss(rel_locations ~  re(random=~1|part) + day, sigma.formula =~ day*qwb_score + age_18_up2, data=nn, family = "LO", control=gamlss.control(n.cyc=60)) ## 
test5aa2a2 <- gamlss(rel_locations ~  re(random=~1|part) + day, sigma.formula =~ day*qwb_score + male3, data=nn, family = "LO", control=gamlss.control(n.cyc=60)) ## 
test5aa2a3 <- gamlss(rel_locations ~  re(random=~1|part) + day, sigma.formula =~ day*qwb_score + socio_comb, data=nn, family = "LO", control=gamlss.control(n.cyc=60)) ## 
#test5aa2a4 <- gamlss(rel_locations ~  re(random=~1|part) + day, sigma.formula =~ day*qwb_score + re(random=~1|part), data=nn, family = "LO", control=gamlss.control(n.cyc=60)) ## 
AIC(test5aa2a, test5aa2a1, test5aa2a2, test5aa2a3)
##              df        AIC
## test5aa2a   39.5      339.4      --> AIC is 1 less, df is 2.5 less 
## test5aa2a3  42.0      340.5     

test5aa2a <- gamlss(rel_locations ~  re(random=~1|part) + day, sigma.formula =~ day*qwb_score, data=nn, family = "LO", control=gamlss.control(n.cyc=60)) ## 
test5aa2aa <- gamlss(rel_locations ~  day, sigma.formula =~ day*qwb_score, data=nn, family = "LO", control=gamlss.control(n.cyc=60)) ## 
test5aa2ab <- gamlss(rel_locations ~  re(random=~1|part), sigma.formula =~ day*qwb_score, data=nn, family = "LO", control=gamlss.control(n.cyc=60)) ## 
AIC(test5aa2a, test5aa2aa, test5aa2ab)
##              df        AIC
## test5aa2a   39.5      339.4      --> AIC is 7 less, df is 0.9 higher 
## test5aa2ab  38.6      346.6     
test5aa2ac <- gamlss(rel_locations ~  re(random=~1|part) + day+qwb_score, sigma.formula =~ day*qwb_score, data=nn, family = "LO", control=gamlss.control(n.cyc=60)) ## 
test5aa2ad <- gamlss(rel_locations ~  re(random=~1|part) + day+age_18_up2, sigma.formula =~ day*qwb_score, data=nn, family = "LO", control=gamlss.control(n.cyc=60)) ## 
test5aa2ae <- gamlss(rel_locations ~  re(random=~1|part) + day+male3, sigma.formula =~ day*qwb_score, data=nn, family = "LO", control=gamlss.control(n.cyc=60)) ## 
test5aa2af <- gamlss(rel_locations ~  re(random=~1|part) + day+socio_comb, sigma.formula =~ day*qwb_score, data=nn, family = "LO", control=gamlss.control(n.cyc=60)) ## 
AIC(test5aa2a, test5aa2ac, test5aa2ad, test5aa2af)
##              df        AIC
## test5aa2a   39.5      339.4      --> AIC is 0.8 less, df is 1 less 
## test5aa2ac  40.5      340.6     
test5aa2ag <- gamlss(rel_locations ~  re(random=~1|part) + day*qwb_score, sigma.formula =~ day*qwb_score, data=nn, family = "LO", control=gamlss.control(n.cyc=60)) ## 
AIC(test5aa2a, test5aa2ac, test5aa2ag)


############## OUTPUTS FOR WINNING MODEL ##############
# test5aa2a <- gamlss(rel_locations ~  re(random=~1|part) + day, sigma.formula =~ day*qwb_score, data=nn, family = "LO", control=gamlss.control(n.cyc=60)) ## 
## data == nn <- na.omit(temp_qwb_short3_new2)
## need --> day, qwb, part,
temp1 <- nn[!duplicated(nn$part),]
temp1 <- temp1[order(temp1$part),]
parts <- temp1$part
nd1 <- rep(c(rep(seq(from=0.35, to=1, by=0.01), 9)),36)
nd <- data.frame(day=rep(c(rep(1:9,each=66)),36), qwb_score=c(nd1), 
                 part=rep(parts, each=594))
nd$part <- as.factor(as.character(nd$part))
a <- predictAll(test5aa2a, newdata = nd, type="response") # fitted mu
a$exp_val =a$mu
a$sd <- a$sigma*pi/sqrt(3)

aaa <- as.data.frame(cbind(nd,a))
aa2 <- aaa[order(aaa$day, aaa$qwb_score),]

######## SIGMA 
ggplot(data = aa2, aes(x = as.numeric(qwb_score), y = sigma, colour = as.factor(day))) +       
  geom_line(aes(group = as.factor(day))) +
  theme_classic() +
  theme(legend.position = "top")

##### FITTED
## gets fitted values with SEs for each parameter
b123 <- as.data.frame(predictAll(test5aa2a, data=nn, se.fit=TRUE))
fitted_exp_val <- b123$mu.fit
b <- as.data.frame(cbind(b123, fitted_exp_val))
bbb <- as.data.frame(cbind(nn,b))
bb2 <- bbb[order(bbb$day, bbb$qwb_score),]




######### ######### ######### ABSOLUTE TOTAL HOUSES ######### ######### ##########
######### Non-linearity? 
gamm_non_lin_test <- gamm(total_houses ~ s(day, bs="ts", k=3) + day, random=list(part=~1), 
                          data=temp_qwb_short3_new2[which(!(is.na(temp_qwb_short3_new2$qwb_score))),], family=poisson()) ## ***
gamm_0_test <- gamm(total_houses ~ s(day, bs="ts", k=3), random=list(part=~1), 
                    data=temp_qwb_short3_new2[which(!(is.na(temp_qwb_short3_new2$qwb_score))),], family=poisson())
glmm_0_test <- glmer(total_houses ~ day + (1|part), 
                     data=temp_qwb_short3_new2[which(!(is.na(temp_qwb_short3_new2$qwb_score))),], family=poisson()) ## ***

gamm_non_lin_test1 <- gamm(total_houses ~ s(qwb_score, bs="ts", k=3) + day, random=list(part=~1), 
                           data=temp_qwb_short3_new2[which(!(is.na(temp_qwb_short3_new2$qwb_score))),], family=poisson()) ## ***
gamm_0_test1 <- gamm(total_houses ~ s(qwb_score, bs="ts", k=3), random=list(part=~1), 
                     data=temp_qwb_short3_new2[which(!(is.na(temp_qwb_short3_new2$qwb_score))),], family=poisson())
glmm_0_test1 <- glmer(total_houses ~ qwb_score + (1|part), 
                      data=temp_qwb_short3_new2[which(!(is.na(temp_qwb_short3_new2$qwb_score))),], family=poisson()) ## ***

#### GLMM tests 
glmm_0_test <- glmer(total_houses ~ day + (1|part), data=temp_qwb_short3_new2[which(!(is.na(temp_qwb_short3_new2$qwb_score))),], family=poisson()) ## ***
glmm_1_test <- glmer(total_houses ~ qwb_score + (1|part), data=temp_qwb_short3_new2[which(!(is.na(temp_qwb_short3_new2$qwb_score))),], family=poisson()) ## ***
glmm_2_test <- glmer(total_houses ~ day + qwb_score + (1|part), data=temp_qwb_short3_new2[which(!(is.na(temp_qwb_short3_new2$qwb_score))),], family=poisson()) ## ***
glmm_3_test <- glmer(total_houses ~ day*qwb_score + (1|part), data=temp_qwb_short3_new2[which(!(is.na(temp_qwb_short3_new2$qwb_score))),], family=poisson()) ## ***
anova(glmm_0_test, glmm_1_test)
anova(glmm_0_test, glmm_2_test)
anova(glmm_0_test, glmm_3_test)

glmm_4_test <- glmer(total_houses ~ day + male3 + (1|part), data=temp_qwb_short3_new2[which(!(is.na(temp_qwb_short3_new2$qwb_score))),], family=poisson()) ## ***
glmm_5_test <- glmer(total_houses ~ day + age_18_up2 + (1|part), data=temp_qwb_short3_new2[which(!(is.na(temp_qwb_short3_new2$qwb_score))),], family=poisson()) ## ***
anova(glmm_0_test, glmm_4_test)
anova(glmm_0_test, glmm_5_test)

#### GAMM tests 
gamm_0a_test <- gamm(total_houses ~ day, random=list(part=~1), 
                     data=temp_qwb_short3_new2[which(!(is.na(temp_qwb_short3_new2$qwb_score))),], family=poisson())
gamm_0b_test <- gamm(total_houses ~ s(day, k=3), random=list(part=~1), 
                     data=temp_qwb_short3_new2[which(!(is.na(temp_qwb_short3_new2$qwb_score))),], family=poisson())
gamm_0c_test <- gamm(total_houses ~ qwb_score, random=list(part=~1), 
                     data=temp_qwb_short3_new2[which(!(is.na(temp_qwb_short3_new2$qwb_score))),], family=poisson())
gamm_0d_test <- gamm(total_houses ~ qwb_score + s(qwb_score, bs="ts", k=3), random=list(part=~1), 
                     data=temp_qwb_short3_new2[which(!(is.na(temp_qwb_short3_new2$qwb_score))),], family=poisson())
AIC(gamm_0a_test$lme, gamm_0b_test$lme, gamm_0c_test$lme, gamm_0d_test$lme)
(1-(gamm_0a_test$gam$df.residual/gamm_0a_test$gam$df.null))*100
(1-(gamm_0b_test$gam$df.residual/gamm_0b_test$gam$df.null))*100
(1-(gamm_0c_test$gam$df.residual/gamm_0c_test$gam$df.null))*100
(1-(gamm_0d_test$gam$df.residual/gamm_0d_test$gam$df.null))*100

gamm_1a_test <- gamm(total_houses ~ day + s(qwb_score), random=list(part=~1), 
                     data=temp_qwb_short3_new2[which(!(is.na(temp_qwb_short3_new2$qwb_score))),], family=poisson())
gamm_1b_test <- gamm(total_houses ~ s(day, bs="ts", k=3) + qwb_score, random=list(part=~1), 
                     data=temp_qwb_short3_new2[which(!(is.na(temp_qwb_short3_new2$qwb_score))),], family=poisson())
gamm_2_test <- gamm(total_houses ~ s(day, bs="ts", k=3) + s(qwb_score), random=list(part=~1), 
                    data=temp_qwb_short3_new2[which(!(is.na(temp_qwb_short3_new2$qwb_score))),], family=poisson())
gamm_3_test <- gamm(total_houses ~ s(day, bs="ts", k=3) + s(qwb_score, bs="ts", k=3) + ti(qwb_score, day), random=list(part=~1), 
                    data=temp_qwb_short3_new2[which(!(is.na(temp_qwb_short3_new2$qwb_score))),], family=poisson())
gamm_4_test <- gamm(total_houses ~ te(qwb_score, day), random=list(part=~1), 
                    data=temp_qwb_short3_new2[which(!(is.na(temp_qwb_short3_new2$qwb_score))),], family=poisson())
AIC(gamm_1a_test$lme, gamm_1b_test$lme, gamm_2_test$lme, gamm_4_test$lme)
(1-(gamm_1a_test$gam$df.residual/gamm_0a_test$gam$df.null))*100
(1-(gamm_1b_test$gam$df.residual/gamm_0b_test$gam$df.null))*100
(1-(gamm_2_test$gam$df.residual/gamm_0c_test$gam$df.null))*100
(1-(gamm_4_test$gam$df.residual/gamm_0d_test$gam$df.null))*100




######### ######### ######### ABSOLUTE PROPORTION HOURS HOME ######### ######### ##########
temp_qwb_3a <- droplevels(temp_qwb_3[which(temp_qwb_3$day %in% c(1:9)),])
temp_qwb_3a$prop_hours <- temp_qwb_3a$hours_home/17

k4 <- temp_qwb_3a$prop_hours[which(!(is.na(temp_qwb_3a$prop_hours)))]
mean(k4) # 0.79
var(k4) # 0.056
descdist(k4)

temp_qwb_3a1 <- droplevels(temp_qwb_3a[which(!(is.na(temp_qwb_3a$prop_hours))),])
### Data can't have any NAs --> part 1 has NA for relative hours since no day 0 hours
## For now, remove that column from dataset since not comparing to day 0
temp_qwb_3a2 <- droplevels(temp_qwb_3a1[,c(1:11,13,14)])

## one-inflated beta
temp_qwb_3a2$prop_hours2 <- temp_qwb_3a2$prop_hours
temp_qwb_3a2$prop_hours2[which(temp_qwb_3a2$prop_hours2 == 0)] <- 0.01

###########################
## STEP AIC procedure done manually to account for df and AIC (not just AIC)
###########################
a <- gamlss(prop_hours2 ~ 1, 
            sigma.formula = ~ 1,
            nu.formula = ~ 1,
            family=BEOI, data=na.omit(temp_qwb_3a2))

a1 <- GAIC(gamlss(prop_hours2 ~ 1,  sigma.formula = ~ 1,nu.formula = ~ 1,family=BEOI,data=na.omit(temp_qwb_3a2)),
           gamlss(prop_hours2 ~ day,  sigma.formula = ~ 1,nu.formula = ~ 1,family=BEOI,data=na.omit(temp_qwb_3a2)),
           gamlss(prop_hours2 ~ pb(day),  sigma.formula = ~ 1,nu.formula = ~ 1,family=BEOI,data=na.omit(temp_qwb_3a2)),
           gamlss(prop_hours2 ~ age_18_up2,  sigma.formula = ~ 1,nu.formula = ~ 1,family=BEOI,data=na.omit(temp_qwb_3a2)),
           gamlss(prop_hours2 ~ male3,  sigma.formula = ~ 1,nu.formula = ~ 1,family=BEOI,data=na.omit(temp_qwb_3a2)),
           gamlss(prop_hours2 ~ socio_comb,  sigma.formula = ~ 1,nu.formula = ~ 1,family=BEOI,data=na.omit(temp_qwb_3a2)),
           gamlss(prop_hours2 ~ qwb_score,   sigma.formula = ~ 1,nu.formula = ~ 1,family=BEOI,data=na.omit(temp_qwb_3a2)),
           gamlss(prop_hours2 ~ re(random=~ 1|part),   sigma.formula = ~ 1,nu.formula = ~ 1,family=BEOI,data=na.omit(temp_qwb_3a2)),
           gamlss(prop_hours2 ~ re(random=~day|part),   sigma.formula = ~ 1,nu.formula = ~ 1,family=BEOI,data=na.omit(temp_qwb_3a2)))
## + male3

a2 <- GAIC(gamlss(prop_hours2 ~ male3,  sigma.formula = ~ 1,nu.formula = ~ 1,family=BEOI,data=na.omit(temp_qwb_3a2)),
           gamlss(prop_hours2 ~ male3 + day,  sigma.formula = ~ 1,nu.formula = ~ 1,family=BEOI,data=na.omit(temp_qwb_3a2)),
           gamlss(prop_hours2 ~ male3 + pb(day),  sigma.formula = ~ 1,nu.formula = ~ 1,family=BEOI,data=na.omit(temp_qwb_3a2)),
           gamlss(prop_hours2 ~ male3 + age_18_up2,  sigma.formula = ~ 1,nu.formula = ~ 1,family=BEOI,data=na.omit(temp_qwb_3a2)),
           gamlss(prop_hours2 ~ male3 + socio_comb,  sigma.formula = ~ 1,nu.formula = ~ 1,family=BEOI,data=na.omit(temp_qwb_3a2)),
           gamlss(prop_hours2 ~ male3 + qwb_score,   sigma.formula = ~ 1,nu.formula = ~ 1,family=BEOI,data=na.omit(temp_qwb_3a2)),
           gamlss(prop_hours2 ~ male3 + re(random=~ 1|part),   sigma.formula = ~ 1,nu.formula = ~ 1,family=BEOI,data=na.omit(temp_qwb_3a2)),
           gamlss(prop_hours2 ~ male3 + re(random=~day|part),   sigma.formula = ~ 1,nu.formula = ~ 1,family=BEOI,data=na.omit(temp_qwb_3a2)))
## male + qwb

a3 <- GAIC(gamlss(prop_hours2 ~ male3 + qwb_score,  sigma.formula = ~ 1,nu.formula = ~ 1,family=BEOI,data=na.omit(temp_qwb_3a2)),
           gamlss(prop_hours2 ~ male3 + qwb_score + day,  sigma.formula = ~ 1,nu.formula = ~ 1,family=BEOI,data=na.omit(temp_qwb_3a2)),
           gamlss(prop_hours2 ~ male3 + qwb_score + pb(day),  sigma.formula = ~ day,nu.formula = ~ 1,family=BEOI,data=na.omit(temp_qwb_3a2)),
           gamlss(prop_hours2 ~ male3 + qwb_score + socio_comb,  sigma.formula = ~ day,nu.formula = ~ 1,family=BEOI,data=na.omit(temp_qwb_3a2)),
           gamlss(prop_hours2 ~ male3 + qwb_score + age_18_up2,   sigma.formula = ~ day,nu.formula = ~ 1,family=BEOI,data=na.omit(temp_qwb_3a2)),
           gamlss(prop_hours2 ~ male3 + qwb_score + re(random=~ 1|part),   sigma.formula = ~ day,nu.formula = ~ 1,family=BEOI,data=na.omit(temp_qwb_3a2)),
           gamlss(prop_hours2 ~ male3 + qwb_score + re(random=~day|part),   sigma.formula = ~ day,nu.formula = ~ 1,family=BEOI,data=na.omit(temp_qwb_3a2)))

## male + qwb + age

a4 <- GAIC(gamlss(prop_hours2 ~ male3 + qwb_score + age_18_up2,  sigma.formula = ~ 1,nu.formula = ~ 1,family=BEOI,data=na.omit(temp_qwb_3a2)),
           gamlss(prop_hours2 ~ male3 + qwb_score + age_18_up2 + day,  sigma.formula = ~ 1,nu.formula = ~ 1,family=BEOI,data=na.omit(temp_qwb_3a2)),
           gamlss(prop_hours2 ~ male3 + qwb_score + age_18_up2 + pb(day),  sigma.formula = ~ 1,nu.formula = ~ 1,family=BEOI,data=na.omit(temp_qwb_3a2)),
           gamlss(prop_hours2 ~ male3 + qwb_score + age_18_up2 + socio_comb,  sigma.formula = ~ 1,nu.formula = ~ 1,family=BEOI,data=na.omit(temp_qwb_3a2)),
           gamlss(prop_hours2 ~ male3 + qwb_score + age_18_up2 + re(random=~ 1|part),   sigma.formula = ~ 1,nu.formula = ~ 1,family=BEOI,data=na.omit(temp_qwb_3a2)))
# gamlss(prop_hours2 ~ male3 + qwb_score + age_18_up2 + re(random=~day|part),   sigma.formula = ~ 1,nu.formula = ~ 1,family=BEOI,data=na.omit(temp_qwb_3a2))
## male + qwb + age


## SIGMA step
b1 <- GAIC(gamlss(prop_hours2 ~ male3 + qwb_score + age_18_up2,  
                  sigma.formula = ~ 1, nu.formula = ~ 1,family=BEOI,data=na.omit(temp_qwb_3a2)),
           gamlss(prop_hours2 ~ male3 + qwb_score + age_18_up2,
                  sigma.formula = ~ day, nu.formula = ~ 1,family=BEOI,data=na.omit(temp_qwb_3a2)),
           gamlss(prop_hours2 ~ male3 + qwb_score + age_18_up2,
                  sigma.formula = ~ pb(day), nu.formula = ~ 1,family=BEOI,data=na.omit(temp_qwb_3a2)),
           gamlss(prop_hours2 ~ male3 + qwb_score + age_18_up2,
                  sigma.formula = ~ age_18_up2, nu.formula = ~ 1,family=BEOI,data=na.omit(temp_qwb_3a2)),
           gamlss(prop_hours2 ~ male3 + qwb_score + age_18_up2,
                  sigma.formula = ~ male3, nu.formula = ~ 1,family=BEOI,data=na.omit(temp_qwb_3a2)),
           gamlss(prop_hours2 ~ male3 + qwb_score + age_18_up2, 
                  sigma.formula = ~ socio_comb, nu.formula = ~ 1,family=BEOI,data=na.omit(temp_qwb_3a2)),
           gamlss(prop_hours2 ~ male3 + qwb_score + age_18_up2,   
                  sigma.formula = ~ qwb_score, nu.formula = ~ 1,family=BEOI,data=na.omit(temp_qwb_3a2)),
           gamlss(prop_hours2 ~ male3 + qwb_score + age_18_up2,
                  sigma.formula = ~ re(random=~ 1|part), nu.formula = ~ 1,family=BEOI,data=na.omit(temp_qwb_3a2),control=gamlss.control(n.cyc=50)))
# gamlss(prop_hours2 ~ male3 + qwb_score + age_18_up2,
#                   sigma.formula = ~ re(random=~day|part), nu.formula = ~ 1,family=BEOI,data=na.omit(temp_qwb_3a2)))
## re(random=~1|part)

b2 <- GAIC(gamlss(prop_hours2 ~ male3 + qwb_score + age_18_up2,  
                  sigma.formula = ~ re(random=~1|part), nu.formula = ~ 1,family=BEOI,data=na.omit(temp_qwb_3a2),control=gamlss.control(n.cyc=50)),
           gamlss(prop_hours2 ~ male3 + qwb_score + age_18_up2,
                  sigma.formula = ~ re(random=~1|part) + day, nu.formula = ~ 1,family=BEOI,data=na.omit(temp_qwb_3a2),control=gamlss.control(n.cyc=50)),
           gamlss(prop_hours2 ~ male3 + qwb_score + age_18_up2,
                  sigma.formula = ~ re(random=~1|part) + pb(day), nu.formula = ~ 1,family=BEOI,data=na.omit(temp_qwb_3a2),control=gamlss.control(n.cyc=50)),
           gamlss(prop_hours2 ~ male3 + qwb_score + age_18_up2,
                  sigma.formula = ~ re(random=~1|part) + age_18_up2, nu.formula = ~ 1,family=BEOI,data=na.omit(temp_qwb_3a2),control=gamlss.control(n.cyc=50)),
           gamlss(prop_hours2 ~ male3 + qwb_score + age_18_up2,
                  sigma.formula = ~ re(random=~1|part) + male3, nu.formula = ~ 1,family=BEOI,data=na.omit(temp_qwb_3a2),control=gamlss.control(n.cyc=50)),
           gamlss(prop_hours2 ~ male3 + qwb_score + age_18_up2, 
                  sigma.formula = ~ re(random=~1|part) + socio_comb, nu.formula = ~ 1,family=BEOI,data=na.omit(temp_qwb_3a2),control=gamlss.control(n.cyc=50)),
           gamlss(prop_hours2 ~ male3 + qwb_score + age_18_up2,   
                  sigma.formula = ~ re(random=~1|part) + qwb_score, nu.formula = ~ 1,family=BEOI,data=na.omit(temp_qwb_3a2),control=gamlss.control(n.cyc=50)))
#gamlss(prop_hours2 ~ male3 + qwb_score + age_18_up2, sigma.formula = ~ re(random=~1|part) + re(random=~day|part), nu.formula = ~ 1,family=BEOI,data=na.omit(temp_qwb_3a2),control=gamlss.control(n.cyc=80))
## re(random=~1|part) + day

b3 <- GAIC(gamlss(prop_hours2 ~ male3 + qwb_score + age_18_up2,  
                  sigma.formula = ~ re(random=~1|part) + day, nu.formula = ~ 1,family=BEOI,data=na.omit(temp_qwb_3a2),control=gamlss.control(n.cyc=60)),
           gamlss(prop_hours2 ~ male3 + qwb_score + age_18_up2,
                  sigma.formula = ~ re(random=~1|part) + day + pb(day), nu.formula = ~ 1,family=BEOI,data=na.omit(temp_qwb_3a2),control=gamlss.control(n.cyc=60)),
           gamlss(prop_hours2 ~ male3 + qwb_score + age_18_up2,
                  sigma.formula = ~ re(random=~1|part) + day + age_18_up2, nu.formula = ~ 1,family=BEOI,data=na.omit(temp_qwb_3a2),control=gamlss.control(n.cyc=60)),
           gamlss(prop_hours2 ~ male3 + qwb_score + age_18_up2,
                  sigma.formula = ~ re(random=~1|part) + day + male3, nu.formula = ~ 1,family=BEOI,data=na.omit(temp_qwb_3a2),control=gamlss.control(n.cyc=60)),
           gamlss(prop_hours2 ~ male3 + qwb_score + age_18_up2, 
                  sigma.formula = ~ re(random=~1|part) + day + socio_comb, nu.formula = ~ 1,family=BEOI,data=na.omit(temp_qwb_3a2),control=gamlss.control(n.cyc=60)),
           gamlss(prop_hours2 ~ male3 + qwb_score + age_18_up2,   
                  sigma.formula = ~ re(random=~1|part) + day + qwb_score, nu.formula = ~ 1,family=BEOI,data=na.omit(temp_qwb_3a2),control=gamlss.control(n.cyc=60)))
#gamlss(prop_hours2 ~ male3 + qwb_score + age_18_up2, sigma.formula = ~ re(random=~1|part) + day + re(random=~day|part), nu.formula = ~ 1,family=BEOI,data=na.omit(temp_qwb_3a2),control=gamlss.control(n.cyc=80))
## re(random=~1|part) + day

#### NU step
c1 <- GAIC(gamlss(prop_hours2 ~ male3 + qwb_score + age_18_up2,sigma.formula = ~ re(random=~1|part) + day,
                  nu.formula = ~ 1,family=BEOI,data=na.omit(temp_qwb_3a2),control=gamlss.control(n.cyc=70)),
           gamlss(prop_hours2 ~ male3 + qwb_score + age_18_up2,sigma.formula = ~ re(random=~1|part) + day,
                  nu.formula = ~ day,family=BEOI,data=na.omit(temp_qwb_3a2),control=gamlss.control(n.cyc=70)),
           gamlss(prop_hours2 ~ male3 + qwb_score + age_18_up2,sigma.formula = ~ re(random=~1|part) + day,
                  nu.formula = ~ pb(day),family=BEOI,data=na.omit(temp_qwb_3a2),control=gamlss.control(n.cyc=70)),
           gamlss(prop_hours2 ~ male3 + qwb_score + age_18_up2,sigma.formula = ~ re(random=~1|part) + day,
                  nu.formula = ~ age_18_up2,family=BEOI,data=na.omit(temp_qwb_3a2),control=gamlss.control(n.cyc=70)),
           gamlss(prop_hours2 ~ male3 + qwb_score + age_18_up2,sigma.formula = ~ re(random=~1|part) + day,
                  nu.formula = ~ male3,family=BEOI,data=na.omit(temp_qwb_3a2),control=gamlss.control(n.cyc=70)),
           gamlss(prop_hours2 ~ male3 + qwb_score + age_18_up2,sigma.formula = ~ re(random=~1|part) + day,
                  nu.formula = ~ socio_comb,family=BEOI,data=na.omit(temp_qwb_3a2),control=gamlss.control(n.cyc=70)),
           gamlss(prop_hours2 ~ male3 + qwb_score + age_18_up2,sigma.formula = ~ re(random=~1|part) + day,
                  nu.formula = ~ qwb_score,family=BEOI,data=na.omit(temp_qwb_3a2),control=gamlss.control(n.cyc=70)),
           gamlss(prop_hours2 ~ male3 + qwb_score + age_18_up2,sigma.formula = ~ re(random=~1|part) + day,
                  nu.formula = ~ re(random=~1|part),family=BEOI,data=na.omit(temp_qwb_3a2),control=gamlss.control(n.cyc=70)),
           gamlss(prop_hours2 ~ male3 + qwb_score + age_18_up2,sigma.formula = ~ re(random=~1|part) + day,
                  nu.formula = ~ re(random=~day|part),family=BEOI,data=na.omit(temp_qwb_3a2),control=gamlss.control(n.cyc=70))) 
## re(random=~1|part)

c2 <- GAIC(gamlss(prop_hours2 ~ male3 + qwb_score + age_18_up2,sigma.formula = ~ re(random=~1|part) + day,
                  nu.formula = ~ re(random=~day|part),family=BEOI,data=na.omit(temp_qwb_3a2),control=gamlss.control(n.cyc=70)),
           gamlss(prop_hours2 ~ male3 + qwb_score + age_18_up2,sigma.formula = ~ re(random=~1|part) + day,
                  nu.formula = ~ re(random=~day|part) + day,family=BEOI,data=na.omit(temp_qwb_3a2),control=gamlss.control(n.cyc=70)),
           gamlss(prop_hours2 ~ male3 + qwb_score + age_18_up2,sigma.formula = ~ re(random=~1|part) + day,
                  nu.formula = ~ re(random=~day|part) + pb(day),family=BEOI,data=na.omit(temp_qwb_3a2),control=gamlss.control(n.cyc=70)),
           gamlss(prop_hours2 ~ male3 + qwb_score + age_18_up2,sigma.formula = ~ re(random=~1|part) + day,
                  nu.formula = ~ re(random=~day|part) + age_18_up2,family=BEOI,data=na.omit(temp_qwb_3a2),control=gamlss.control(n.cyc=70)),
           gamlss(prop_hours2 ~ male3 + qwb_score + age_18_up2,sigma.formula = ~ re(random=~1|part) + day,
                  nu.formula = ~ re(random=~day|part) + male3,family=BEOI,data=na.omit(temp_qwb_3a2),control=gamlss.control(n.cyc=70)),
           gamlss(prop_hours2 ~ male3 + qwb_score + age_18_up2,sigma.formula = ~ re(random=~1|part) + day,
                  nu.formula = ~ re(random=~day|part) + socio_comb,family=BEOI,data=na.omit(temp_qwb_3a2),control=gamlss.control(n.cyc=70)),
           gamlss(prop_hours2 ~ male3 + qwb_score + age_18_up2,sigma.formula = ~ re(random=~1|part) + day,
                  nu.formula = ~ re(random=~day|part) + qwb_score,family=BEOI,data=na.omit(temp_qwb_3a2),control=gamlss.control(n.cyc=70)))
## re(random=~1|part) + age

c3 <- GAIC(gamlss(prop_hours2 ~ male3 + qwb_score + age_18_up2,sigma.formula = ~ re(random=~1|part) + day,
                  nu.formula = ~ re(random=~day|part) + age_18_up2,family=BEOI,data=na.omit(temp_qwb_3a2),control=gamlss.control(n.cyc=70)),
           gamlss(prop_hours2 ~ male3 + qwb_score + age_18_up2,sigma.formula = ~ re(random=~1|part) + day,
                  nu.formula = ~ re(random=~day|part) + age_18_up2+ day,family=BEOI,data=na.omit(temp_qwb_3a2),control=gamlss.control(n.cyc=70)),
           gamlss(prop_hours2 ~ male3 + qwb_score + age_18_up2,sigma.formula = ~ re(random=~1|part) + day,
                  nu.formula = ~ re(random=~day|part) + age_18_up2 + pb(day),family=BEOI,data=na.omit(temp_qwb_3a2),control=gamlss.control(n.cyc=70)),
           gamlss(prop_hours2 ~ male3 + qwb_score + age_18_up2,sigma.formula = ~ re(random=~1|part) + day,
                  nu.formula = ~ re(random=~day|part) + age_18_up2 + male3,family=BEOI,data=na.omit(temp_qwb_3a2),control=gamlss.control(n.cyc=70)),
           gamlss(prop_hours2 ~ male3 + qwb_score + age_18_up2,sigma.formula = ~ re(random=~1|part) + day,
                  nu.formula = ~ re(random=~day|part) + age_18_up2 + socio_comb,family=BEOI,data=na.omit(temp_qwb_3a2),control=gamlss.control(n.cyc=70)),
           gamlss(prop_hours2 ~ male3 + qwb_score + age_18_up2,sigma.formula = ~ re(random=~1|part) + day,
                  nu.formula = ~ re(random=~day|part) + age_18_up2 + qwb_score,family=BEOI,data=na.omit(temp_qwb_3a2),control=gamlss.control(n.cyc=70)))
## re(random=~day|part) + age

#### SIGMA step down
d1 <- GAIC(gamlss(prop_hours2 ~ male3 + qwb_score + age_18_up2,
                  sigma.formula = ~ re(random=~1|part) + day, nu.formula = ~ re(random=~day|part) + age_18_up2,family=BEOI,data=na.omit(temp_qwb_3a2),control=gamlss.control(n.cyc=70)),
           gamlss(prop_hours2 ~ male3 + qwb_score + age_18_up2,
                  sigma.formula = ~ re(random=~1|part), nu.formula = ~ re(random=~day|part) + age_18_up2,family=BEOI,data=na.omit(temp_qwb_3a2),control=gamlss.control(n.cyc=70)),
           gamlss(prop_hours2 ~ male3 + qwb_score + age_18_up2,
                  sigma.formula = ~ day, nu.formula = ~ re(random=~day|part) + age_18_up2,family=BEOI,data=na.omit(temp_qwb_3a2),control=gamlss.control(n.cyc=70)))

#### MU step down
e1 <- GAIC(gamlss(prop_hours2 ~ male3 + qwb_score + age_18_up2, sigma.formula = ~ re(random=~1|part) + day,nu.formula = ~ re(random=~day|part) + age_18_up2,family=BEOI,data=na.omit(temp_qwb_3a2),control=gamlss.control(n.cyc=70)),
           gamlss(prop_hours2 ~ male3 + qwb_score, sigma.formula = ~ re(random=~1|part) + day,nu.formula = ~ re(random=~day|part) + age_18_up2,family=BEOI,data=na.omit(temp_qwb_3a2),control=gamlss.control(n.cyc=70)),
           gamlss(prop_hours2 ~ male3 + age_18_up2, sigma.formula = ~ re(random=~1|part) + day,nu.formula = ~ re(random=~day|part) + age_18_up2,family=BEOI,data=na.omit(temp_qwb_3a2),control=gamlss.control(n.cyc=70)),
           gamlss(prop_hours2 ~ qwb_score + age_18_up2, sigma.formula = ~ re(random=~1|part) + day,nu.formula = ~ re(random=~day|part) + age_18_up2,family=BEOI,data=na.omit(temp_qwb_3a2),control=gamlss.control(n.cyc=70)))
## male3 + age_18_up2

e2 <- GAIC(gamlss(prop_hours2 ~ male3 + age_18_up2, sigma.formula = ~ re(random=~1|part) + day,nu.formula = ~ re(random=~day|part) + age_18_up2,family=BEOI,data=na.omit(temp_qwb_3a2),control=gamlss.control(n.cyc=70)),
           gamlss(prop_hours2 ~ male3, sigma.formula = ~ re(random=~1|part) + day,nu.formula = ~ re(random=~day|part) + age_18_up2,family=BEOI,data=na.omit(temp_qwb_3a2),control=gamlss.control(n.cyc=70)),
           gamlss(prop_hours2 ~ age_18_up2, sigma.formula = ~ re(random=~1|part) + day,nu.formula = ~ re(random=~day|part) + age_18_up2,family=BEOI,data=na.omit(temp_qwb_3a2),control=gamlss.control(n.cyc=70)))
## male3 + age_18_up2



###### STEP WINNER
a_new <- gamlss(prop_hours2 ~ male3 + age_18_up2, 
                sigma.formula = ~ re(random=~1|part) + day,
                nu.formula = ~ re(random=~day|part) + age_18_up2,
                family=BEOI,data=na.omit(temp_qwb_3a2),control=gamlss.control(n.cyc=70))
summary(a_new)

###### consider adding parts from previous winning model to see if any better
a_new_mu_test2 <- gamlss(prop_hours2 ~ re(random=~1|part) + age_18_up2, 
                         sigma.formula = ~ re(random=~1|part) + day,
                         nu.formula = ~ re(random=~day|part) + age_18_up2,
                         family=BEOI,data=na.omit(temp_qwb_3a2),control=gamlss.control(n.cyc=250))

a_new_mu_test <- gamlss(prop_hours2 ~ re(random=~day|part),
                        sigma.formula = ~ day,
                        nu.formula = ~ re(random=~day|part) + age_18_up2,
                        family=BEOI,data=na.omit(temp_qwb_3a2),control=gamlss.control(n.cyc=250))
a_new_mu_test1 <- gamlss(prop_hours2 ~ re(random=~day|part) + male3,
                         sigma.formula = ~ day,
                         nu.formula = ~ re(random=~day|part) + age_18_up2,
                         family=BEOI,data=na.omit(temp_qwb_3a2),control=gamlss.control(n.cyc=250))
a_new_mu_testb <- gamlss(prop_hours2 ~ re(random=~1|part),
                         sigma.formula = ~ day,
                         nu.formula = ~ re(random=~day|part) + age_18_up2,
                         family=BEOI,data=na.omit(temp_qwb_3a2),control=gamlss.control(n.cyc=250))
a_new_mu_test1b <- gamlss(prop_hours2 ~ re(random=~1|part) + male3,
                          sigma.formula = ~ day,
                          nu.formula = ~ re(random=~day|part) + age_18_up2,
                          family=BEOI,data=na.omit(temp_qwb_3a2),control=gamlss.control(n.cyc=250))
a_new_mu_test2b <- gamlss(prop_hours2 ~ re(random=~1|part) + age_18_up2, 
                          sigma.formula = ~day,
                          nu.formula = ~ re(random=~day|part) + age_18_up2,
                          family=BEOI,data=na.omit(temp_qwb_3a2),control=gamlss.control(n.cyc=250))
a_new_mu_test3b <- gamlss(prop_hours2 ~ re(random=~1|part) + male3 + age_18_up2, 
                          sigma.formula = ~ day,
                          nu.formula = ~ re(random=~day|part) + age_18_up2,
                          family=BEOI,data=na.omit(temp_qwb_3a2),control=gamlss.control(n.cyc=250))
GAIC(a_new, a_new_mu_test2, a_new_mu_test, a_new_mu_test1, a_new_mu_testb, a_new_mu_test1b, a_new_mu_test2b, a_new_mu_test3b)
## a_new_mu_test2
## mu =~ re(random=~1|part) + age_18_up2

## test sigma from old mu first
a_new_sig_test0 <- gamlss(prop_hours2 ~ male3 + age_18_up2, 
                          sigma.formula = ~ re(random=~1|part) + qwb_score,
                          nu.formula = ~ re(random=~day|part) + age_18_up2,
                          family=BEOI,data=na.omit(temp_qwb_3a2),control=gamlss.control(n.cyc=250))
a_new_sig_test <- gamlss(prop_hours2 ~ male3 + age_18_up2, 
                         sigma.formula = ~ re(random=~1|part) + day + qwb_score,
                         nu.formula = ~ re(random=~day|part) + age_18_up2,
                         family=BEOI,data=na.omit(temp_qwb_3a2),control=gamlss.control(n.cyc=250))
a_new_sig_test1 <- gamlss(prop_hours2 ~ male3 + age_18_up2, 
                          sigma.formula = ~ re(random=~1|part) + pb(day) + qwb_score,
                          nu.formula = ~ re(random=~day|part) + age_18_up2,
                          family=BEOI,data=na.omit(temp_qwb_3a2),control=gamlss.control(n.cyc=250))
a_new_sig_test2 <- gamlss(prop_hours2 ~ male3 + age_18_up2, 
                          sigma.formula = ~ re(random=~1|part) + pb(day),
                          nu.formula = ~ re(random=~day|part) + age_18_up2,
                          family=BEOI,data=na.omit(temp_qwb_3a2),control=gamlss.control(n.cyc=250))
a_new_sig_test3 <- gamlss(prop_hours2 ~ male3 + age_18_up2, 
                          sigma.formula = ~ re(random=~1|part) + pb(day)*qwb_score,
                          nu.formula = ~ re(random=~day|part) + age_18_up2,
                          family=BEOI,data=na.omit(temp_qwb_3a2),control=gamlss.control(n.cyc=250))
a_new_sig_test4 <- gamlss(prop_hours2 ~ male3 + age_18_up2, 
                          sigma.formula = ~ re(random=~1|part) + day*qwb_score,
                          nu.formula = ~ re(random=~day|part) + age_18_up2,
                          family=BEOI,data=na.omit(temp_qwb_3a2),control=gamlss.control(n.cyc=250))
a_new_sig_test5 <- gamlss(prop_hours2 ~ male3 + age_18_up2, 
                          sigma.formula = ~ day*qwb_score,
                          nu.formula = ~ re(random=~day|part) + age_18_up2,
                          family=BEOI,data=na.omit(temp_qwb_3a2),control=gamlss.control(n.cyc=250))
a_new_sig_test6 <- gamlss(prop_hours2 ~ male3 + age_18_up2, 
                          sigma.formula = ~ pb(day)*qwb_score,
                          nu.formula = ~ re(random=~day|part) + age_18_up2,
                          family=BEOI,data=na.omit(temp_qwb_3a2),control=gamlss.control(n.cyc=250))
a_new_sig_test7 <- gamlss(prop_hours2 ~ male3 + age_18_up2, 
                          sigma.formula = ~ day + qwb_score,
                          nu.formula = ~ re(random=~day|part) + age_18_up2,
                          family=BEOI,data=na.omit(temp_qwb_3a2),control=gamlss.control(n.cyc=250))
a_new_sig_test8 <- gamlss(prop_hours2 ~ male3 + age_18_up2, 
                          sigma.formula = ~ qwb_score,
                          nu.formula = ~ re(random=~day|part) + age_18_up2,
                          family=BEOI,data=na.omit(temp_qwb_3a2),control=gamlss.control(n.cyc=250))
a_new_sig_test9 <- gamlss(prop_hours2 ~ male3 + age_18_up2, 
                          sigma.formula = ~ day,
                          nu.formula = ~ re(random=~day|part) + age_18_up2,
                          family=BEOI,data=na.omit(temp_qwb_3a2),control=gamlss.control(n.cyc=250))
a_new_sig_test10 <- gamlss(prop_hours2 ~ male3 + age_18_up2, 
                           sigma.formula = ~ pb(day) + qwb_score,
                           nu.formula = ~ re(random=~day|part) + age_18_up2,
                           family=BEOI,data=na.omit(temp_qwb_3a2),control=gamlss.control(n.cyc=250))
a_new_sig_test11 <- gamlss(prop_hours2 ~ male3 + age_18_up2, 
                           sigma.formula = ~ pb(day),
                           nu.formula = ~ re(random=~day|part) + age_18_up2,
                           family=BEOI,data=na.omit(temp_qwb_3a2),control=gamlss.control(n.cyc=250))
a_new_sig_test12 <- gamlss(prop_hours2 ~ age_18_up2 + re(random=~1|part), 
                           sigma.formula = ~ day,
                           nu.formula = ~ re(random=~day|part) + age_18_up2,
                           family=BEOI,data=na.omit(temp_qwb_3a2),control=gamlss.control(n.cyc=250))
a_new_sig_test13 <- gamlss(prop_hours2 ~ age_18_up2 + re(random=~1|part),
                           sigma.formula = ~ pb(day),
                           nu.formula = ~ re(random=~day|part) + age_18_up2,
                           family=BEOI,data=na.omit(temp_qwb_3a2),control=gamlss.control(n.cyc=250))
a_new_sig_test14 <- gamlss(prop_hours2 ~ age_18_up2 + re(random=~1|part),
                           sigma.formula = ~ qwb_score,
                           nu.formula = ~ re(random=~day|part) + age_18_up2,
                           family=BEOI,data=na.omit(temp_qwb_3a2),control=gamlss.control(n.cyc=250))
a_new_sig_test15 <- gamlss(prop_hours2 ~ age_18_up2 + re(random=~1|part),
                           sigma.formula = ~ day + qwb_score,
                           nu.formula = ~ re(random=~day|part) + age_18_up2,
                           family=BEOI,data=na.omit(temp_qwb_3a2),control=gamlss.control(n.cyc=250))
a_new_sig_test16 <- gamlss(prop_hours2 ~ age_18_up2 + re(random=~1|part),
                           sigma.formula = ~ pb(day) + qwb_score,
                           nu.formula = ~ re(random=~day|part) + age_18_up2,
                           family=BEOI,data=na.omit(temp_qwb_3a2),control=gamlss.control(n.cyc=250))
a_new_sig_test17 <- gamlss(prop_hours2 ~ age_18_up2 + re(random=~1|part),
                           sigma.formula = ~ day*qwb_score,
                           nu.formula = ~ re(random=~day|part) + age_18_up2,
                           family=BEOI,data=na.omit(temp_qwb_3a2),control=gamlss.control(n.cyc=250))
a_new_sig_test18 <- gamlss(prop_hours2 ~ male3 + age_18_up2 + re(random=~1|part),
                           sigma.formula = ~ pb(day)*qwb_score,
                           nu.formula = ~ re(random=~day|part) + age_18_up2,
                           family=BEOI,data=na.omit(temp_qwb_3a2),control=gamlss.control(n.cyc=250))
GAIC(a_new, a_new_sig_test0, a_new_sig_test, a_new_sig_test1, a_new_sig_test2, a_new_sig_test3, a_new_sig_test4, a_new_sig_test5,
     a_new_sig_test6, a_new_sig_test7, a_new_sig_test8, a_new_sig_test9, a_new_sig_test10, a_new_sig_test11, a_new_sig_test12,
     a_new_sig_test13, a_new_sig_test14, a_new_sig_test15, a_new_sig_test16, a_new_sig_test17, a_new_sig_test18) 
## a_new_sig_test18
## sigma =~ pb(day)*qwb_score        mu=~ male+age+ re(random=~1|day)

#### include mu from above (mu=~ re(random=~1|part) + AGE)
a_new_sig_test18b <- gamlss(prop_hours2 ~ age_18_up2 + re(random=~1|part),
                            sigma.formula = ~ pb(day)*qwb_score,
                            nu.formula = ~ re(random=~day|part) + age_18_up2,
                            family=BEOI,data=na.omit(temp_qwb_3a2),control=gamlss.control(n.cyc=500))

### do we still need re(day|part) in nu, or is re(1|part) good enough?
a_new_sig_test18c <- gamlss(prop_hours2 ~ age_18_up2 + male3 + re(random=~1|part),
                            sigma.formula = ~ pb(day)*qwb_score,
                            nu.formula = ~ re(random=~1|part) + age_18_up2,
                            family=BEOI,data=na.omit(temp_qwb_3a2),control=gamlss.control(n.cyc=500))

GAIC(a_new, a_new_sig_test18, a_new_sig_test18b, a_new_sig_test18c) 
## best model --> a_new_sig_test18
## mu =~ re(random=~1|part) + age + male
## sigma =~ pb(day)*qwb_score
## nu =~ re(random=~day|part) + age

a_new_sig_test18d <- gamlss(prop_hours2 ~ age_18_up2 + male3 + re(random=~1|part),
                            sigma.formula = ~ pb(day)*qwb_score,
                            nu.formula = ~ re(random=~1|part),
                            family=BEOI,data=na.omit(temp_qwb_3a2),control=gamlss.control(n.cyc=500))
a_new_sig_test18e <- gamlss(prop_hours2 ~ age_18_up2 + re(random=~1|part),
                            sigma.formula = ~ pb(day)*qwb_score,
                            nu.formula = ~ re(random=~1|part),
                            family=BEOI,data=na.omit(temp_qwb_3a2),control=gamlss.control(n.cyc=500))
a_new_sig_test18f <- gamlss(prop_hours2 ~ male3 + re(random=~1|part),
                            sigma.formula = ~ pb(day)*qwb_score,
                            nu.formula = ~ re(random=~1|part),
                            family=BEOI,data=na.omit(temp_qwb_3a2),control=gamlss.control(n.cyc=500))
a_new_sig_test18f1 <- gamlss(prop_hours2 ~ male3 + re(random=~1|part),
                             sigma.formula = ~ pb(day)*qwb_score,
                             nu.formula = ~ re(random=~1|part) + age_18_up2,
                             family=BEOI,data=na.omit(temp_qwb_3a2),control=gamlss.control(n.cyc=500))
a_new_sig_test18g <- gamlss(prop_hours2 ~ re(random=~1|part),
                            sigma.formula = ~ pb(day)*qwb_score,
                            nu.formula = ~ re(random=~1|part),
                            family=BEOI,data=na.omit(temp_qwb_3a2),control=gamlss.control(n.cyc=500))
a_new_sig_test18g1 <- gamlss(prop_hours2 ~ re(random=~1|part),
                             sigma.formula = ~ pb(day)*qwb_score,
                             nu.formula = ~ re(random=~1|part) + age_18_up2,
                             family=BEOI,data=na.omit(temp_qwb_3a2),control=gamlss.control(n.cyc=500))
a_new_sig_test18i <- gamlss(prop_hours2 ~ male3,
                            sigma.formula = ~ pb(day)*qwb_score,
                            nu.formula = ~ re(random=~1|part),
                            family=BEOI,data=na.omit(temp_qwb_3a2),control=gamlss.control(n.cyc=500))
a_new_sig_test18j <- gamlss(prop_hours2 ~ age_18_up2,
                            sigma.formula = ~ pb(day)*qwb_score,
                            nu.formula = ~ re(random=~1|part),
                            family=BEOI,data=na.omit(temp_qwb_3a2),control=gamlss.control(n.cyc=500))
GAIC(a_new, a_new_sig_test18, a_new_sig_test18d, a_new_sig_test18e, a_new_sig_test18f, a_new_sig_test18f1,
     a_new_sig_test18g, a_new_sig_test18g1, a_new_sig_test18h, a_new_sig_test18i, a_new_sig_test18j) 
q5 <- gamlss(prop_hours2 ~ re(random=~day|part),
             sigma.formula = ~ pb(day)+qwb_score,
             nu.formula = ~ re(random=~1|part),
             family=BEOI,data=na.omit(temp_qwb_3a2),control=gamlss.control(n.cyc=500))
q5b <- gamlss(prop_hours2 ~ re(random=~day|part),
              sigma.formula = ~ pb(day)+qwb_score,
              nu.formula = ~ re(random=~1|part) + age_18_up2,
              family=BEOI,data=na.omit(temp_qwb_3a2),control=gamlss.control(n.cyc=500))
q6 <- gamlss(prop_hours2 ~ re(random=~day|part),
             sigma.formula = ~ pb(day)+qwb_score,
             nu.formula = ~ re(random=~day|part),
             family=BEOI,data=na.omit(temp_qwb_3a2),control=gamlss.control(n.cyc=500))
q7 <- gamlss(prop_hours2 ~ re(random=~day|part),
             sigma.formula = ~ pb(day)+qwb_score,
             nu.formula = ~ re(random=~day|part) + day,
             family=BEOI,data=na.omit(temp_qwb_3a2),control=gamlss.control(n.cyc=500))
q8 <- gamlss(prop_hours2 ~ re(random=~day|part),
             sigma.formula = ~ pb(day)+qwb_score,
             nu.formula = ~ re(random=~day|part) + pb(day),
             family=BEOI,data=na.omit(temp_qwb_3a2),control=gamlss.control(n.cyc=500))
q9 <- gamlss(prop_hours2 ~ re(random=~day|part),
             sigma.formula = ~ pb(day)+qwb_score,
             nu.formula = ~ re(random=~day|part) + qwb_score,
             family=BEOI,data=na.omit(temp_qwb_3a2),control=gamlss.control(n.cyc=500))
q10 <- gamlss(prop_hours2 ~ re(random=~day|part),
              sigma.formula = ~ pb(day)+qwb_score,
              nu.formula = ~ re(random=~day|part) + age_18_up2,
              family=BEOI,data=na.omit(temp_qwb_3a2),control=gamlss.control(n.cyc=500))
q11 <- gamlss(prop_hours2 ~ re(random=~day|part),
              sigma.formula = ~ pb(day)+qwb_score,
              nu.formula = ~ re(random=~day|part) + male3,
              family=BEOI,data=na.omit(temp_qwb_3a2),control=gamlss.control(n.cyc=500))
q12 <- gamlss(prop_hours2 ~ re(random=~day|part),
              sigma.formula = ~ pb(day)+qwb_score,
              nu.formula = ~ re(random=~day|part) + socio_comb,
              family=BEOI,data=na.omit(temp_qwb_3a2),control=gamlss.control(n.cyc=500))
GAIC(a2kka, q5, q5b, q6, q7, q8, q9, q10, q11, q12)
GAIC(a2kka, q5, q5b, q6)
## df     AIC     model
#  89    -251     a2kka
#  88    -251      q6
#  68    -241      q5
#  69    -241      q5b

a2kkav.1 <- gamlss(prop_hours2 ~ (re(random = ~day | part)), 
                   sigma.formula = ~pb(day) + qwb_score, 
                   nu.formula = ~age_18_up2*day, 
                   family = BEOI, data = na.omit(temp_qwb_3a2), control=gamlss.control(n.cyc=250)) ## chosen by step function
a2kkav3.1 <- gamlss(prop_hours2 ~ (re(random = ~day | part)), 
                    sigma.formula = ~pb(day) + qwb_score, 
                    nu.formula = ~age_18_up2*day + re(random=~1|part), 
                    family = BEOI, data = na.omit(temp_qwb_3a2), control=gamlss.control(n.cyc=250)) ## chosen by step function
GAIC(a2kka, a2kkav.1, a2kkav3.1)



############## OUTPUTS FOR WINNING MODEL ##############
## ## a2kkav.1 ## ## 
temp_aa2 <- na.omit(temp_qwb_3a2)
temp1 <- temp_aa2[!duplicated(temp_aa2$part),]
temp1 <- temp1[order(temp1$part),]
ages <- temp1$age_18_up2
parts <- temp1$part
nd1 <- rep(c(rep(seq(from=0.35, to=1, by=0.01), 9)),34)
nd <- data.frame(day=rep(c(rep(1:9,each=66)),34), qwb_score=c(nd1), 
                 age_18_up2=rep(ages, each=594),
                 part=rep(parts, each=594))
nd$age_18_up2 <- as.factor(as.character(nd$age_18_up2))
nd$part <- as.factor(as.character(nd$part))
a <- predictAll(a2kkav.1, newdata = nd, type="response") # fitted mu
a$exp_val =a$nu+(1-a$nu)*a$mu
a$var <- (1-a$nu)*(a$mu*(1-a$mu))/(a$sigma+1) + a$nu*(1-a$nu)*(1-a$mu)^2
aaa <- as.data.frame(cbind(nd,a))
aa2 <- aaa[order(aaa$day, aaa$qwb_score),]
##### FITTED 
temp_aa2 <- na.omit(temp_qwb_3a2)
## gets fitted values with SEs for each parameter
b123 <- as.data.frame(predictAll(a2kkav.1, data=na.omit(temp_qwb_3a2), se.fit=TRUE))
fitted_exp_val <- b123$nu.fit+(1-b123$nu.fit)*b123$mu.fit
b <- as.data.frame(cbind(b123, fitted_exp_val))
bbb <- as.data.frame(cbind(temp_aa2,b))
bb2 <- bbb[order(bbb$day, bbb$qwb_score),]
bb2a <- droplevels(bb2[which(!(bb2$part %in% c("SA319BP13","SCA626P05"))),])






######### ######### ######### RELATIVE LOCATIONS ######### #########
nnn <- droplevels(na.omit(temp_qwb_short3_new2))
shapiro.test(nnn$rel_hours)
descdist(nnn$rel_hours, boot=500)
#fitdist(nnn$rel_hours, "beta")
plotdist(nnn$rel_hours, histo = TRUE, demp = TRUE)
fit_w  <- fitdist(nnn$rel_hours, "norm")
fit_g  <- fitdist(nnn$rel_hours, "logis")
summary(fit_g)
par(mfrow=c(2,2))
plot.legend <- c("norm", "logis")
denscomp(list(fit_w, fit_g), legendtext = plot.legend)
cdfcomp (list(fit_w, fit_g), legendtext = plot.legend)
qqcomp  (list(fit_w, fit_g), legendtext = plot.legend)
ppcomp  (list(fit_w, fit_g), legendtext = plot.legend)

nn <- droplevels(na.omit(temp_qwb_short3_new2))
test <- gamlss(rel_hours ~  day, data=nn, family = "LO") ## 
test1 <- gamlss(rel_hours ~  qwb_score, data=nn, family = "LO") ## 
test2 <- gamlss(rel_hours ~  male3, data=nn, family = "LO") ## 
test3 <- gamlss(rel_hours ~  age_18_up2, data=nn, family = "LO") ## 
test4 <- gamlss(rel_hours ~  socio_comb, data=nn, family = "LO") ##
test5 <- gamlss(rel_hours ~  re(random=~1|part), data=nn, family = "LO") ##
AIC(test,test1,test2,test3,test4, test5)

test5 <- gamlss(rel_hours ~  re(random=~1|part), data=nn, family = "LO") ##
test51 <- gamlss(rel_hours ~  re(random=~1|part) + day, data=nn, family = "LO") ## 
test52 <- gamlss(rel_hours ~  re(random=~1|part) + qwb_score, data=nn, family = "LO") ## 
test53 <- gamlss(rel_hours ~  re(random=~1|part) + male3, data=nn, family = "LO") ## 
test54 <- gamlss(rel_hours ~  re(random=~1|part) + age_18_up2, data=nn, family = "LO") ## 
test55 <- gamlss(rel_hours ~  re(random=~1|part) + socio_comb, data=nn, family = "LO") ##
AIC(test5,test51,test52,test53,test54, test55)

test51 <- gamlss(rel_hours ~  re(random=~1|part) + day, data=nn, family = "LO") ## 
test51a <- gamlss(rel_hours ~  re(random=~1|part) + day + qwb_score, data=nn, family = "LO") ## 
test51b <- gamlss(rel_hours ~  re(random=~1|part) + day + male3, data=nn, family = "LO") ## 
test51c <- gamlss(rel_hours ~  re(random=~1|part) + day + age_18_up2, data=nn, family = "LO") ## 
test51d <- gamlss(rel_hours ~  re(random=~1|part) + day + socio_comb, data=nn, family = "LO") ##
AIC(test51,test51a,test51b,test51c, test51d)

test51 <- gamlss(rel_hours ~  re(random=~1|part) + day, data=nn, family = "LO") ## 
test511 <- gamlss(rel_hours ~  re(random=~1|part) + day, sigma.formula = ~ day, data=nn, family = "LO") ## 
test512 <- gamlss(rel_hours ~  re(random=~1|part) + day, sigma.formula = ~ qwb_score, data=nn, family = "LO") ## 
#test513 <- gamlss(rel_hours ~  re(random=~1|part) + day, sigma.formula = ~ socio_comb, data=nn, family = "LO", control=gamlss.control(n.cyc=100)) ## 
test514 <- gamlss(rel_hours ~  re(random=~1|part) + day, sigma.formula = ~ male3, data=nn, family = "LO") ## 
test515 <- gamlss(rel_hours ~  re(random=~1|part) + day, sigma.formula = ~ age_18_up2, data=nn, family = "LO") ## 
#test516 <- gamlss(rel_hours ~  re(random=~1|part) + day, sigma.formula = ~ re(random=~1|part), data=nn, family = "LO") ## 
AIC(test51,test511,test512, test514, test515)

test511 <- gamlss(rel_hours ~  re(random=~1|part) + day, sigma.formula = ~ day, data=nn, family = "LO") ## 
test511a <- gamlss(rel_hours ~  re(random=~1|part) + day, sigma.formula = ~ day + qwb_score, data=nn, family = "LO") ## 
#test511b <- gamlss(rel_hours ~  re(random=~1|part) + day, sigma.formula = ~ day + socio_comb, data=nn, family = "LO") ## 
test511c <- gamlss(rel_hours ~  re(random=~1|part) + day, sigma.formula = ~ day + age_18_up2, data=nn, family = "LO") ## 
test511d <- gamlss(rel_hours ~  re(random=~1|part) + day, sigma.formula = ~ day + male3, data=nn, family = "LO") ## 
AIC(test511,test511a, test511c, test511d)

test511d1 <- gamlss(rel_hours ~  re(random=~1|part) + day, sigma.formula = ~ day*male3, data=nn, family = "LO") ## 
test511a1 <- gamlss(rel_hours ~  re(random=~1|part) + day, sigma.formula = ~ day*qwb_score, data=nn, family = "LO") ## 
AIC(test511,test511a, test511c, test511d, test511d1, test511a1)

test511 <- gamlss(rel_hours ~  re(random=~1|part) + day, sigma.formula = ~ day, data=nn, family = "LO") ## 
test5111 <- gamlss(rel_hours ~  day, sigma.formula = ~ day, data=nn, family = "LO") ## 
test5112 <- gamlss(rel_hours ~  re(random=~1|part), sigma.formula = ~ day, data=nn, family = "LO", control=gamlss.control(n.cyc=100)) ## 
AIC(test511,test5111, test5112)

test511 <- gamlss(rel_hours ~  re(random=~1|part) + day, sigma.formula = ~ day, data=nn, family = "LO") ## 
test511aa <- gamlss(rel_hours ~  re(random=~1|part) + day+qwb_score, sigma.formula = ~ day, data=nn, family = "LO") ## 
test511bb <- gamlss(rel_hours ~  re(random=~1|part) + day+male3, sigma.formula = ~ day, data=nn, family = "LO") ## 
test511cc <- gamlss(rel_hours ~  re(random=~1|part) + day+socio_comb, sigma.formula = ~ day, data=nn, family = "LO") ## 
test511dd <- gamlss(rel_hours ~  re(random=~1|part) + day+age_18_up2, sigma.formula = ~ day, data=nn, family = "LO") ## 
AIC(test511,test511aa, test511bb, test511cc, test511dd)

test511 <- gamlss(rel_hours ~  re(random=~1|part) + day, sigma.formula = ~ day, data=nn, family = "LO") ## 
test511dd <- gamlss(rel_hours ~  re(random=~1|part) + day + age_18_up2, sigma.formula = ~ day, data=nn, family = "LO") ## 

## normal dist
test <- lm(rel_hours ~  day, data=nn) ##
test1 <- lm(rel_hours ~  qwb_score, data=nn) ##
test2 <- lm(rel_hours ~  male3, data=nn) ##
test3 <- lm(rel_hours ~  age_18_up2, data=nn) ##
test4 <- lm(rel_hours ~  socio_comb, data=nn) ##
test5 <- lmer(rel_hours ~ (1|part), data=nn) ##
AIC(test,test1,test2,test3,test4, test5)

test5 <- lmer(rel_hours ~ (1|part), data=nn) ##
test55a <- lmer(rel_hours ~ (1|part) + day, data=nn) ##
test55b <- lmer(rel_hours ~ (1|part) + qwb_score, data=nn) ##
test55c <- lmer(rel_hours ~ (1|part) + age_18_up2, data=nn) ##
test55d <- lmer(rel_hours ~ (1|part) + socio_comb, data=nn) ##
test55e <- lmer(rel_hours ~ (1|part) + male3, data=nn) ##
anova(test5,test55a,test55b,test55c,test55d,test55e)

anova(test5,test55a) #55a
anova(test5,test55b) #55b
anova(test5,test55c)
anova(test5,test55d)
anova(test5,test55e)
anova(test55a,test55b) #55b

test55b <- lmer(rel_hours ~ (1|part) + qwb_score, data=nn) ##
test55b1 <- lmer(rel_hours ~ (1|part) + qwb_score + day, data=nn) ##
test55b2 <- lmer(rel_hours ~ (1|part) + qwb_score + socio_comb, data=nn) ##
test55b3 <- lmer(rel_hours ~ (1|part) + qwb_score + male3, data=nn) ##
test55b4 <- lmer(rel_hours ~ (1|part) + qwb_score + age_18_up2, data=nn) ##
anova(test55b,test55b1,test55b2,test55b3,test55b4)

anova(test55b,test55b1) 
anova(test55b,test55b2)
anova(test55b,test55b3)
anova(test55b,test55b4)
#55b

test511 <- gamlss(rel_hours ~  re(random=~day|part) + day, sigma.formula = ~ day, data=nn, family = "LO") ## 
test511dd <- gamlss(rel_hours ~  re(random=~1|part) + day + age_18_up2, sigma.formula = ~ day, data=nn, family = "LO") ## 
AIC(test55b, test511, test511dd)
#              df     AIC
# test55b     4.00   786.2    # --> AIC is 36.7 higher, but df is 29.75 lower
# test511    33.75   749.5      
# test511dd  34.59   750.7
anova(test55b,test511)  ## 511 signif better 
anova(test55b,test511dd) ## 511dd signif better




############## OUTPUTS FOR WINNING MODEL ##############
## test511 <- gamlss(rel_hours ~  re(random=~1|part) + day, sigma.formula = ~ day, data=nn, family = "LO") ## 
temp1 <- nn[!duplicated(nn$part),]
temp1 <- temp1[order(temp1$part),]
parts <- temp1$part

nd1 <- rep(c(rep(seq(from=0.35, to=1, by=0.01), 9)),36)
nd <- data.frame(day=rep(c(rep(1:9,each=66)),36), qwb_score=c(nd1), 
                 part=rep(parts, each=594))
nd$part <- as.factor(as.character(nd$part))
a <- predictAll(test511, newdata = nd, type="response") # fitted mu
a$exp_val =a$mu
a$sd <- a$sigma*pi/sqrt(3)
aaa <- as.data.frame(cbind(nd,a))
aa2 <- aaa[order(aaa$day, aaa$qwb_score),]

##### FITTED 
## gets fitted values with SEs for each parameter
b123 <- as.data.frame(predictAll(test511, data=nn, se.fit=TRUE))
fitted_exp_val <- b123$mu.fit
b <- as.data.frame(cbind(b123, fitted_exp_val))
bbb <- as.data.frame(cbind(nn,b))
bb2 <- bbb[order(bbb$day, bbb$qwb_score),]
