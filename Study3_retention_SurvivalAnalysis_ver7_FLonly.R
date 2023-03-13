library(tidyverse) # for data manipulation and plots
library(effects) #for plotting parameter effects
library(jtools) #for transformaing model summaries
library(grid)
library(xtable)
library(dplyr)
library(readr)
library(tidyr) # for transforming
library(ggplot2)
library(survival)
library(survminer)

Somerawdata <- read.csv("rawdata_ForVer6.csv", fileEncoding = 'UTF-8-BOM')
#rawdata_ver4_FLonlyQisQuizOnly = Ver6 # rawdata_ver4_FLonly
table(Somerawdata$Course)
tablel1 <- table(Somerawdata$cc, Somerawdata$CourseS)
tablel1
barplot(table1)

table2 <- prop.table(tablel1)
table2
###################################################################################################################################
rawdata <- Somerawdata 
#rawdata <- rawdata %>% filter(prc_act_count>1)
dim(rawdata)
str(rawdata)
summary(rawdata)
LD <- read.csv("FL_LDwithQuiz.csv", fileEncoding = 'UTF-8-BOM')


summary(LD)
LD_act <- LD[,3:6]
cor(LD_act)
IQR(LD_act$A)
range(LD$A)
boxplot(LD$A, LD$D, LD$V, LD$Q)#, horizontal = TRUE


#boxplot(rawdata$A, rawdata$D, rawdata$V, rawdata$Q, horizontal = TRUE)
table(rawdata$Course)
hist(rawdata$prc_act_count)

fit = lm(prc_act_count ~ poly(A, 4), data = rawdata, subset = rawdata$prc_act_count>1)
summary(fit)
coef(summary(fit))
# raw = TRUE argument in poly() function gives age, age^2, age^3 and age^4 directly
fit2 = lm(prc_act_count~poly(A, 4, raw = TRUE), data = rawdata, subset = rawdata$prc_act_count>1)
summary(fit2)
coef(summary(fit2))

Alims = rawdata %>%
  select(A) %>%
  range

Alims
# Generate a sequence of age values spanning the range
A_grid = seq(from = min(Alims), to = max(Alims))

# Predict the value of the generated ages,
# returning the standard error using se = TRUE
preds = predict(fit2, newdata = list(A = A_grid), se = TRUE)

# Compute error bands (2*SE)
se_bands = cbind("upper" = preds$fit+2*preds$se.fit, 
                 "lower" = preds$fit-2*preds$se.fit)   

ggplot() +
  geom_point(data = rawdata, aes(x = A, y = prc_act_count)) +
  geom_line(aes(x = A_grid, y = preds$fit), color = "#0000FF") +
  geom_ribbon(aes(x = A_grid, 
                  ymin = se_bands[,"lower"], 
                  ymax = se_bands[,"upper"]), 
              alpha = 0.3) +
  xlim(Alims) +
  labs(title = "4th degree Polynomial for A")

str(rawdata$cc)
# Deciding on a degree
fit_1 = lm(prc_act_count~ D, data = rawdata, subset = rawdata$prc_act_count>50)
fit_2 = lm(prc_act_count~poly(D,2), data = rawdata, subset = rawdata$prc_act_count>50)
fit_3 = lm(prc_act_count~poly(D,3), data = rawdata, subset = rawdata$prc_act_count>50)
fit_4 = lm(prc_act_count~poly(D,4), data = rawdata, subset = rawdata$prc_act_count>50)
fit_5 = lm(prc_act_count~poly(D,5), data = rawdata, subset = rawdata$prc_act_count>50)
print(anova(fit_1,fit_2,fit_3,fit_4,fit_5))

table(rawdata$Course)
rawdata$nA <- scale(rawdata$A)
rawdata$nV <- scale(rawdata$V)
rawdata$nD <- scale(rawdata$D)
rawdata$nQ <- scale(rawdata$Q)
rawdata$nT <- scale(rawdata$T)
rawdata$nlengthW <- scale(rawdata$lengthW)
rawdata$SocioEco <- ifelse(rawdata$secS ==c(1,2), "LOW", "HI")
cor.test(rawdata$duration, rawdata$prc_act_count)

baseline_model <- survfit(Surv(prc_act_count, prc_act_count >1) ~ 1 , data = rawdata)
print(baseline_model)

actInCourse<- survfit(Surv(prc_act_count, prc_act_count >1) ~ Course , data = rawdata)
# Table2.
actInCourse # n= Joiners (n), events = engaged learners who have expereinced an event, i.e., have accessed more than 1% activities,# median = median number of accessed activities for engaged learners (those who have crossed the 1% threshold) in respective course 
actInLD<- survfit(Surv(prc_act_count, prc_act_count >1) ~ A , data = rawdata)
actInLD

ccInCourse<- survfit(Surv(prc_act_count, prc_act_count>1) ~ cc , data = rawdata)
ccInCourse
secInCourse<- survfit(Surv(prc_act_count, prc_act_count>1) ~ sec , data = rawdata)
secInCourse
socioInCourse<- survfit(Surv(prc_act_count, prc_act_count>1) ~ SocioEco , data = rawdata)
socioInCourse
cc_socioInCourse<- survfit(Surv(prc_act_count, prc_act_count > 1) ~  cc + SocioEco , data = rawdata)
cc_socioInCourse

###################################################################################################################################

#### KM curves and logrank test #########

ggsurvplot(secInCourse,#fun = "cumhaz",#,"cloglog"
           #facet.by = "sec",
           pval = TRUE,
           pval.coord = c(50,0.50),
           pval.size = 5,
           #conf.int = TRUE,
           risk.table = "abs_pct", # Add risk table "percentage"# abs_pct#Shows the absolute number and the percentage of subjects at risk by time, respectively.
           risk.table.col = "strata", # Change risk table color by groups
           risk.table.height = 0.30, # default 0.25
           risk.table.fontsize =4,
           break.time.by = 10, # 7 for duration
           #linetype = "strata", # Change line type by groups
           size = 1.1,
           surv.median.line = "hv", # Specify median survival
           font.legend = c(13,"plain", "black"), 
           #cumevents = TRUE,
           ggtheme = theme_bw(), # Change ggplot2 theme
           xlab="Activities accesses (%)" #Learners lifespan (in days)" # "Activities accesses (%)" 
)
# risk set table shows the number of patients that were under observation in the specific period of time.(at the start of specific time period)
glist <- list(
  ggsurvplot(secInCourse, fun = "event", main = "Cumulative proportion"),
  ggsurvplot(secInCourse, fun = "cumhaz",  main = "Cumulative Hazard"),
  ggsurvplot(secInCourse, fun = "cloglog", main = "Complementary log???log")
)
arrange_ggsurvplots(glist, print = TRUE, ncol = 3, nrow = 1)

###################################################################################################################################

####### using log rank function to see if there are differences in survivalS ##########

# using log rank test to see if there are sig. stat. differences between survival in different Courses?
survdiff(Surv(prc_act_count, prc_act_count >1) ~ Course, data = rawdata) # duration
# using log rank test to see if there are sig. stat. differences between survival in different cc?
survdiff(Surv(prc_act_count, prc_act_count >1) ~ cc, data = rawdata) # duration
# using log rank test to see if there are sig. stat. differences between survival in different sec?
survdiff(Surv(prc_act_count, prc_act_count >1) ~ sec, data = rawdata) # duration
# using log rank test to see if there are sig. stat. differences between survival in different learning activities?
#survdiff(Surv(duration) ~ A, data = rawdata) # A, V , lengthW

###################################################################################################################################

############### Cox regression #####################
colnames(rawdata)
table(rawdata$Course)

#baseline #prc_act_count,
cox_act_baseline <- coxph(Surv(prc_act_count, prc_act_count >1) ~ 1 , data = rawdata)
summary(cox_act_baseline)
extractAIC(cox_act_baseline)
cox.zph(cox_act_baseline)

# how far they progress (prc_act_count) as a function of var1, var2, var3
prop.table(table(rawdata$ccS))
table(rawdata$ccS)
table(rawdata$sec)
cox_act_all <- coxph(Surv(prc_act_count, prc_act_count >1) ~ nA + nV + nD + nQ , data = rawdata)#, subset = sec =="LMI")#, subset = ccS =="SA")#, #subset = secS =="4" )# subset = SocioEco=="HI")# subset = secS =="4" )# ,subset = ccS=="GE") #, subset = SocioEco=="LOW")
#cox_act_all_interact <- coxph(Surv(prc_act_count, prc_act_count >1) ~ nA*sec + nV*sec + nD*sec + nQ*sec , data = rawdata)#, subset = ccS =="SA")#, subset = ccS =="AF") #subset = secS =="4" )# subset = SocioEco=="HI")# subset = secS =="4" )# ,subset = ccS=="GE") #, subset = SocioEco=="LOW")
extractAIC(cox_act_all) # A+V+D+Q had minimum AIC therefore the best model 
summary(cox_act_all)

summary(cox_act_all_interact)
cox.zph(cox_act_all, transform="identity") # identity means no transformation on time; time=time (default was KM transformation, another was rank which ranks the time and then measure propotional hazard)
cox_act_all$loglik
cox_act_all$coefficients
extractAIC(cox_act_all) # A+V+D+Q had minimum AIC therefore the best model 
ggforest(cox_act_all, fontsize = 1.2, main = "Hazard ratio for Socio-economic subgroup: LMI")

###################################################################################################################################
table(rawdata$Course)
CCinCourses<- table( rawdata$Course, rawdata$cc)
CCinCourses
round(prop.table(CCinCourses,1)*100,digits=1)
#write.csv(CCinCourses, file= "Table&Fig/CCinCourses.csv")
ccprop <- round(prop.table(CCinCourses,1)*100,digits=1)
#write.csv(ccprop, file = "Table&Fig/CCpropinCourses.csv")
SECinCourses<- table(rawdata$Course, rawdata$sec)
SECinCourses
barplot(SECinCourses)
round(prop.table(SECinCourses,1)*100,digits=1)
#write.csv(SECinCourses, file= "Table&Fig/SECinCourses.csv")
#write.csv(round(prop.table(SECinCourses,1)*100,digits=1), file = "Table&Fig/SECpropinCourses.csv")
table(rawdata$sec)
SECinCC<- table( rawdata$cc, rawdata$sec)
SECinCC
round(prop.table(SECinCC,1)*100,digits=1)
#write.csv(round(prop.table(SECinCC,1)*100,digits=1), file = "table&fig/SECinCC.csv")
table(rawdata$cc, rawdata$sec)
barplot(table(rawdata$sec), xlab='Socioeconomic group',ylab='Learners',main="Number of learners in each SEC", col=c("darkblue"))

CCinSEC<- table( rawdata$sec, rawdata$cc)
barplot(CCinSEC, xlab='SEC',ylab='Frequency',main="Frequency of Socioeconomic groups in each CC",
        col=c("darkblue", "red", "orange","lightblue")
        ,legend=rownames(CCinSEC), args.legend = list(x = "topright"))

prop.table(table(rawdata$region))
table(rawdata$Course, rawdata$cc, rawdata$sec)
#write.csv(table(rawdata$Course, rawdata$cc, rawdata$sec), file = "table&fig/CC&SECinCourses.csv")



rawdata$sec
################################ Interaction terms #############################
cox_interact <- coxph(Surv(prc_act_count, prc_act_count >1) ~  nA*cc*sec + nV*cc*sec + nD*cc*sec + nQ*cc*sec, data = rawdata)#, subset = ccS =="SA")#, subset = ccS =="AF") #subset = secS =="4" )# subset = SocioEco=="HI")# subset = secS =="4" )# ,subset = ccS=="GE") #, subset = SocioEco=="LOW")
summary(cox_interact)
AIC(cox_interact)
# nA*cc + nV*cc + nD*cc + nQ*cc + nA*sec + nV*sec + nD*sec + nQ*sec
#nA:cc:sec + nV:cc:sec + nD:cc:sec + nQ:cc:sec # nA*cc*sec + nV*cc*sec + nD*cc*sec + nQ*cc*sec
#nA:ccS:secS + nV:ccS:secS + nD:ccS:secS + nQ:ccS:secS # nA*ccS*secS + nV*ccS*secS + nD*ccS*secS + nQ*ccS*secS
cox.zph(cox_interact, transform="identity") # identity means no transformation on time; time=time (default was KM transformation, another was rank which ranks the time and then measure propotional hazard)
cox_interact$coefficients
# 
# ###################### using glmnet ############################################
# 
# data <- as.data.frame(rawdata)
# 
# library(glmnet)
# load("CoxExample.RData")
# CoxData <- get(load("CoxExample.RData"))
# write.csv(CoxData, "CoxData.csv", row.names = FALSE)
# y[1:5,]
# x[1:5,]
# fit = glmnet(x, y, family = "cox")# , alpha=1)
# plot(fit)
# # we can extract the coefficients at certain values of ??.
# coef(fit, s = 0.05)
# cvfit = cv.glmnet(x, y, family = "cox")
# plot(cvfit)
# cvfit$lambda.min
# cvfit$lambda.1se
# # We can check the active covariates in our model and see their coefficients.
# coef.min = coef(cvfit, s = "lambda.min")
# coef.min
# active.min = which(coef.min != 0)
# index.min = coef.min[active.min]
# index.min
# 
# 
# ######################################################################################################################
# dat <- rawdata %>% dplyr::select(nA, nV, nD, nQ) # , D, lengthW, secS
# head(dat)
# X <- as.matrix(dat)
# head(X)
# rawdata$time <- rawdata %>% dplyr::select(prc_act_count) # , D, lengthW, secS
# rawdata$status <- rawdata %>% dplyr::select(event) # , D, lengthW, secS
# Y <- rawdata %>% dplyr::select(time, status)
# Y <- as.matrix(Y)
# head(Y)
# 
# cv.model <- cv.glmnet( X, Y, family = "cox", alpha = 1) # alpha = 1 for lasso that deals with multicplinearility
# summary(cv.model)
# coef(cv.model)
# cv.model$lambda.min
# coef(cv.model, cv.model$lambda.min)
# coef(cv.model, cv.model$lambda.1se) # only show that has significant p value; coeff for non-significant p will be set to . by algorithm
