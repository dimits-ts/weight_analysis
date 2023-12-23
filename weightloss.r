source("lib.r")
library(nortest)
library(car)
library(FSA)


# IMPORT DATA

data <- read.table("weightloss.txt", header=T)
data$workout = factor(data$workout)
data$diet = factor(data$diet)


# ANALYSIS

name = filepath_png("boxplot_2")
png(name)
par(mfrow=c(3,1), mar=c(7,5,1,1))
boxplot(loss ~ workout, data=data)
boxplot(loss ~ diet, data=data)
boxplot(loss ~ workout*diet, data=data, las=2)
mtext("Weight loss depending on workout and diet",
      side = 3, 
      line = - 1, 
      outer = TRUE)
dev.off()


# ANOVA on workout
anova <- aov(loss ~ workout, data=data)
summary(anova)
shapiro.test(anova$res)
lillie.test(anova$res)
leveneTest(loss ~ workout, data=data)

anova <- kruskal.test(loss ~ workout, data=data)
anova
summary(lm(loss ~ workout, data=data))

# ANOVA on diet
anova <- aov(loss ~  diet, data=data)
summary(anova)
shapiro.test(anova$res)
lillie.test(anova$res)
leveneTest(loss ~ diet, data=data)

anova <- kruskal.test(loss ~  diet, data=data)
anova
summary(lm(loss ~ diet, data=data))
dunnTest(loss ~ diet, data=data)


# re-run ANOVA on diet
signif_data <- dummy_data
anova <- aov(loss ~ D2+D3+D4, data=dummy_data)
summary(anova)
shapiro.test(anova$res)
lillie.test(anova$res)
leveneTest(loss ~ diet, data=data)

excluded_data = data[data$diet != "D1",]
anova <- kruskal.test(loss ~ diet, data=excluded_data)
anova
summary(lm(loss ~ diet, data=excluded_data))


#TWO-WAY ON MAIN EFFECTS

anova_main <- aov(loss ~ workout + diet, data=data)
summary(anova_main)

shapiro.test(anova_main$res)
lillie.test(anova_main$res)
# Homogeneity test.
qfits <- quantcut(anova_main$fit)
leveneTest(anova_main$res, qfits)
bartlett.test(anova_main$res, qfits)

vif(lm(loss ~ workout + diet, data=data))
stargazer(lm(loss ~ workout + diet, data=data),
          type="latex", 
          title="Two-Way ANOVA between weight loss and main effects", 
          ci=T, 
          label="tab::main_2",
          df=T,
          out=filepath("main_2.tex"), 
          report=('vc*p'), 
          no.space=TRUE)

TukeyHSD(anova_main)


# FIXED TWO WAY ON MAIN EFFECTS
excluded_data = data[(data$workout != "W3") & (data$diet !="D1"),]
anova_fixed <- aov(loss ~ workout + diet, data=excluded_data)
shapiro.test(anova_fixed$res)
lillie.test(anova_fixed$res)
# Homogeneity test.
qfits <- quantcut(anova_fixed$fit)
leveneTest(anova_fixed$res, qfits)
bartlett.test(anova_fixed$res, qfits)

summary.lm(anova_fixed)


#TWO-WAY WITH INTERACTIONS
full_model = aov(loss ~ diet*workout, data=data)
summary.lm(full_model)

shapiro.test(full_model$res)
lillie.test(full_model$res)
qfits <- quantcut(full_model$fit)
# Homogeneity test.
leveneTest(full_model$res, qfits)
bartlett.test(full_model$res, qfits)
vif(full_model, type = "predictor")

stargazer(lm(loss ~ diet*workout, data=data),
          type="latex", 
          title="Two-Way ANOVA between weight loss and main effects 
          including interactions", 
          ci=T, 
          label="tab::full_2",
          df=T,
          out=filepath("full_2.tex"), 
          report=('vc*p'), 
          no.space=TRUE)


# STEPWISE SELECTION

# Use lm instead of aov for model selection
# remember that ANOVA models are a special case of LM models 
# and are thus equivalent
start_model = lm(loss ~ diet*workout, data=data)
null_model = lm(loss ~ 1, data = data) 
optimal_anova_model = step(
  start_model,
        direction = 'both', 
        scope = list(upper = start_model, 
                     lower = null_model), 
        trace = 0) 

# View optimal model stats
summary(optimal_anova_model)
# same model, we do not need to check assumptions

# PLOT
name = filepath_png("interaction_2")
png(name)
interaction.plot(data$workout, 
                 data$diet, 
                 data$loss, 
                 xlab="Workout Category",
                 ylab="Average weight loss (calories per day)",
                 trace.label = "Diet Category",
                 col=1:4, 
                 lwd=3, 
                 type='l', 
                 cex=2, 
                 pch=16, 
                 lty=1:4, 
                 bty='l')
title("Weight loss by diet and weight")
dev.off()

# MODEL COMPARISONS
anova(null_model, anova_main)
anova(null_model, anova_full)
