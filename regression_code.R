# Test the assumptions
fatigue.mod <- lm(ave_runningTime ~ ave_fatigue + ave_soreness + ave_desire + ave_irritability +
                    ave_sleepHours + ave_sleepQuality + ave_pain + ave_illness + ave_menstruation + 
                    ave_nutrition + ave_usg + ave_trainingReadiness + avg_rpe + ave_acuteChronicRatio,data = final_dataset)
library(corrplot)
corrplot(corr=cor(final_dataset[2:16]),method = 'color',order = "AOE",addCoef.col = "black")

final2 <- final_dataset[,c(-4,-5,-11)]
fatigue.mod1 <- lm(ave_runningTime ~ ave_fatigue  + ave_sleepHours  + ave_sleepQuality  + ave_illness + ave_menstruation + 
                    ave_usg + ave_trainingReadiness + avg_rpe + ave_acuteChronicRatio,data = final2)
library(corrplot)
corrplot(corr=cor(final2[2:13]),method = 'color',order = "AOE",addCoef.col = "black")

# Test for linearity
summary(fatigue.mod1)
anova(fatigue.mod1)

residualPlots(fatigue.mod1)

# Normality of Error Terms
par(mfrow=c(1,1))
qqnorm(residuals(fatigue.mod1))
qqline(residuals(fatigue.mod1))
shapiro.test(resid(fatigue.mod1))

###########################################################################

# BOX COX Tranformation
library(MASS)
par(mfrow=c(1,1))
b <- boxcox(ave_runningTime ~ ave_fatigue  + ave_sleepHours  + ave_sleepQuality  + ave_illness + ave_menstruation + 
              ave_usg + ave_trainingReadiness + avg_rpe + ave_acuteChronicRatio,data = final2)
I <- which(b$y == max(b$y))
lambda <- b$x[I]
lambda #lambda = 2
tran_fatigue.mod <- lm((ave_runningTime)^2 ~ ave_fatigue  + ave_sleepHours  + ave_sleepQuality  + ave_illness + ave_menstruation + 
                         ave_usg + ave_trainingReadiness + avg_rpe + ave_acuteChronicRatio,data = final2)
shapiro.test(resid(tran_fatigue.mod))
summary(tran_fatigue.mod)

####################################################
# Model Selection
stepAIC(tran_fatigue.mod,direction = "both")

tran_fatigue.mod2 <- lm((ave_runningTime)^2 ~ ave_sleepQuality + ave_usg + ave_trainingReadiness 
                        + avg_rpe + ave_acuteChronicRatio,data = final2)

####################################################
# Diagnostic
# Assumptions
residualPlots(tran_fatigue.mod2)
qqnorm(resid(tran_fatigue.mod2))
qqline(resid(tran_fatigue.mod2))
shapiro.test(resid(tran_fatigue.mod2))
# VIF
sqrt(vif(tran_fatigue.mod2))>2 #Multicollinearity occurs
# Summary
summary(tran_fatigue.mod2)

#####################################################
tran_fatigue.mod3 <- lm(ave_runningTime ~ ave_sleepQuality + (ave_usg)^2 + ave_trainingReadiness 
                        + avg_rpe + ave_acuteChronicRatio,data = final2)
# Diagnostic
# Assumptions
residualPlots(tran_fatigue.mod3)
qqnorm(resid(tran_fatigue.mod3))
qqline(resid(tran_fatigue.mod3))
shapiro.test(resid(tran_fatigue.mod3))
# VIF
sqrt(vif(tran_fatigue.mod3))>2 #Multicollinearity occurs
# Summary
summary(tran_fatigue.mod3)
