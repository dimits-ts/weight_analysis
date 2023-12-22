library(nortest)
library(car)


data <- read.table("weightloss.txt", header=T)
data$workout = factor(data$workout)
data$diet = factor(data$diet)

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


# Assuming your original dataframe is named 'data'
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

# Add the original continuous variable 'loss' to the new dataframe
dummy_data <- cbind(dummy_data,data$loss)

# Rename the columns to have more meaningful names
colnames(dummy_data) <- c(levels(data$diet),
                        levels(data$workout), 
                        colnames(dummy_interactions),
                        "loss")

dummy_data <- as.data.frame(dummy_data)

# View the resulting dataframe
head(dummy_data)


signif_data=dummy_data
anova <- aov(loss ~  D1+D2+D3+D4, data=dummy_data)
summary(anova)
shapiro.test(anova$res)
lillie.test(anova$res)
leveneTest(loss ~ diet, data=data)
#TukeyHSD(anova)


anova_main <- aov(loss ~ W1+W2+W3+D1+D2+D3, data=dummy_data)
summary(anova_main)
shapiro.test(anova_main$res)
lillie.test(anova_main$res)
leveneTest(loss ~ workout+diet, data=data)
#TukeyHSD(anova)


anova_full <- aov(loss ~ ., data=dummy_data)
summary(anova_full)
shapiro.test(anova_full$res)
lillie.test(anova_full$res)
leveneTest(loss ~ workout*diet, data=data)
#TukeyHSD(anova)


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

plot(optimal_anova_model$call, data=dummy_data)
abline(optimal_anova_model)
