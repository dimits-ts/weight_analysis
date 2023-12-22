library(nortest)
library(car)
source("lib.r")



# IMPORT DATA
data <- read.table("weightloss.txt", header=T)
data$workout = factor(data$workout)
data$diet = factor(data$diet)


# Create dataset with dummy variables for factor levels

# Create dummy variables for the 'diet' and 'workout' factors
dummy_diet <- model.matrix(~ diet - 1, data=data)
dummy_workout <- model.matrix(~ workout - 1, data = data)

colnames(dummy_interactions) <- gsub("diet","",colnames(dummy_interactions))
colnames(dummy_interactions) <- gsub("workout","",colnames(dummy_interactions))
colnames(dummy_interactions) <- gsub(":","_",colnames(dummy_interactions))

# Create dummy variables for the interactions
dummy_interactions <- model.matrix(~ diet:workout - 1, data = data)
colnames(dummy_interactions) <- gsub("diet","",colnames(dummy_interactions))
colnames(dummy_interactions) <- gsub("workout","",colnames(dummy_interactions))
colnames(dummy_interactions) <- gsub(":","_",colnames(dummy_interactions))

# Combine all the dummy variables into a new dataframe
dummy_data <- cbind(dummy_diet, dummy_workout, dummy_interactions)
dummy_data <- cbind(dummy_data,data$loss)

# Rename the columns to have better names
colnames(dummy_data) <- c(levels(data$diet),
                          levels(data$workout), 
                          colnames(dummy_interactions),
                          "loss")

dummy_data <- as.data.frame(dummy_data)

# View the resulting dataframe
head(dummy_data)


# ANALYSIS

par(mfrow=c(3,1), mar=c(7,5,1,1))
boxplot(loss ~ workout, data=data)
boxplot(loss ~ diet, data=data)
boxplot(loss ~ workout*diet, data=data, las=2)
mtext("Weight loss depending on workout and diet",
      side = 3, 
      line = - 1, 
      outer = TRUE)

anova <- aov(loss ~ workout, data=data)
summary(anova)
shapiro.test(anova$res)
lillie.test(anova$res)
leveneTest(loss ~ workout, data=data)
pairwise.t.test(data$loss, data$workout, p.adjust.method = "holm")


anova <- aov(loss ~  diet, data=data)
summary(anova)
shapiro.test(anova$res)
lillie.test(anova$res)
leveneTest(loss ~ diet, data=data)
pairwise.t.test(data$loss, data$diet, p.adjust.method = "holm")


signif_data=dummy_data
anova <- aov(loss ~  D1+D2+D3+D4, data=dummy_data)
summary(anova)
shapiro.test(anova$res)
lillie.test(anova$res)
leveneTest(loss ~ diet, data=data)
#TukeyHSD(anova)

# use LM instead of anova since they are equivalent and we need
# lm in order to compare models later on
anova_main <- lm(loss ~ W1+W2+W3+D1+D2+D3, data=dummy_data)
summary(anova_main)
shapiro.test(anova_main$res)
lillie.test(anova_main$res)

qfits <- quantcut(anova_main$fit)
# Homogeneity test.
leveneTest(anova_main$res, qfits)
bartlett.test(anova_main$res, qfits)


anova_full <- lm(loss ~ ., data=dummy_data)
summary(anova_full)
shapiro.test(anova_full$res)
lillie.test(anova_full$res)
qfits <- quantcut(anova_full$fit)
# Homogeneity test.
leveneTest(anova_full$res, qfits)
bartlett.test(anova_full$res, qfits)


fullModel = anova_full 
nullModel = lm(loss ~ 1, data = data) 
optimal_anova_model = step(
  anova_full,
  direction = 'both', 
  scope = list(upper = fullModel, 
               lower = nullModel), 
  trace = 0) 

# View optimal model stats
summary(optimal_anova_model)

# Compare BIC values
AIC(anova_full)
AIC(optimal_anova_model)

par(mfrow=c(1,1))
coefs = optimal_anova_model$coefficients
plot(x=dummy_data$loss, 
     ylab = "Weight loss per day (calories)", 
     xlab="",  
     xaxt='n')

colors = grDevices::colors()[grep('gr(a|e)y', grDevices::colors(), invert = T)]
selected_colors = colors[c(12, 34, 256, 89, 98, 122, 385, 283)]
ltypes = c("dotted", "dotted", "dotted", "longdash", "longdash", "dashed", "dashed", "solid")

abline(h=coefs["(Intercept)"] + coefs["D1"], col=selected_colors[1], lty=ltypes[1])
abline(h=coefs["(Intercept)"] + coefs["D2"], col=selected_colors[2], lty=ltypes[2])
abline(h=coefs["(Intercept)"] + coefs["D3"], col=selected_colors[3], lty=ltypes[3])
abline(h=coefs["(Intercept)"] + coefs["W1"], col=selected_colors[4], lty=ltypes[4])
abline(h=coefs["(Intercept)"] + coefs["W2"], col=selected_colors[5], lty=ltypes[5])
abline(h=coefs["(Intercept)"] + coefs["D2_W1"], col=selected_colors[6], lty=ltypes[6])
abline(h=coefs["(Intercept)"] + coefs["D2_W2"], col=selected_colors[7], lty=ltypes[7])
abline(h=coefs["(Intercept)"], col=selected_colors[8], lty=ltypes[8])

legend("bottomleft", 
       legend=c("Diet 1", "Diet 2", "Diet 3", 
                "Weight 1", "Weight 2", 
                "Diet 2 x Weight 1", "Diet 2 x Weight 2", "Overall Mean"), 
       lty=ltypes,
       col=selected_colors)
title("Weight loss by diet and weight")


anova(nullModel, anova_main)
anova(nullModel, anova_full)
