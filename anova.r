source("lib.r")
library(nortest)
library(car)
library(stargazer)
library(sjPlot)


# IMPORT DATA
data <- read.table("data.txt", header=TRUE)
data$W <- factor(data$W)


# ONE-WAY ANOVA TESTS
filepath = filepath_png("boxplots_1")
png(filepath)

par(mfrow=c(3,2), mar=c(7,5,1,1))
boxplot(X1 ~ W, data=data)
boxplot(X2 ~ W, data=data)
boxplot(X3 ~ W, data=data)
boxplot(X4 ~ W, data=data)
boxplot(Y ~ W, data=data)
mtext("Relationship between W and X1...X5 and Y",
      side = 3, 
      line = - 2, 
      outer = TRUE)
dev.off()

anova <- aov(X1 ~ W, data=data)
summary(anova)
shapiro.test(anova$res)
lillie.test(anova$res)
leveneTest(X1 ~ W, data=data)

anova <- aov(X2 ~ W, data=data)
summary(anova)
shapiro.test(anova$res)
lillie.test(anova$res)
leveneTest(X2 ~ W, data=data)

# not homogenous
anova <- aov(X3 ~ W, data=data)
summary(anova)
shapiro.test(anova$res)
lillie.test(anova$res)
leveneTest(X3 ~ W, data=data)

anova <- aov(X4 ~ W, data=data)
summary(anova)
shapiro.test(anova$res)
lillie.test(anova$res)
leveneTest(X4 ~ W, data=data)

anova <- aov(Y ~ W, data=data)
shapiro.test(anova$res)
lillie.test(anova$res)
leveneTest(Y ~ W, data=data)

# SCATTER PLOT
filepath = filepath_png("scatterplot_1")
png(filepath)
par(mfrow=c(2,2))
plot(data$Y, data$X1, col=data$W, xlab="X1", ylab="Y", pch=17)
legend("topleft", legend=levels(data$W),pch=17, col=unique(data$W))

plot(data$Y, data$X2, col=data$W, xlab="X2", ylab="Y", pch=17)
legend("topleft", legend=levels(data$W),pch=17, col=unique(data$W))

plot(data$Y, data$X3, col=data$W, xlab="X3", ylab="Y", pch=17)
legend("topleft", legend=levels(data$W),pch=17, col=unique(data$W))

plot(data$Y, data$X4, col=data$W, xlab="X4", ylab="Y", pch=17)
legend("topleft", legend=levels(data$W),pch=17, col=unique(data$W))

mtext("Y single LS with respect to W",
      side = 3, 
      line = - 2, 
      outer = TRUE)
dev.off()

# REGRESSION
model <- lm(Y ~ X4, data=data)
summary(model)

stargazer(model,
          type="latex", 
          title="Linear Regression of Y depending on X4", 
          ci=T, 
          label="tab::simple_1",
          df=T,
          out=filepath("simple_1.tex"), 
          report=('vc*p'), 
          no.space=TRUE)

multimodel <- lm(Y ~ X1*W + X2*W + X3*W + X4*W, data=data)
summary(multimodel)

stargazer(multimodel,
          type="latex", 
          title="Mutiple Linear Regression of Y with main effects and interaction", 
          ci=T, 
          label="tab::multi_full_1",
          df=T,
          out=filepath("multi_full_1.tex"), 
          report=('vc*p'), 
          no.space=TRUE)

# Check for multi-colinearity
vif(multimodel)
vif(multimodel, type="predictor")

multimodel_fixed <- lm(Y ~ X1 + X2 + X3 + X4 + W, data=data)
vif(multimodel_fixed)
vif(multimodel_fixed, type="predictor")

stargazer(multimodel_fixed,
          type="latex", 
          title="Mutiple Linear Regression with no multicolinearity issues ", 
          ci=T, 
          label="tab::multi_fixed_1",
          df=T,
          out=filepath("multi_fixed_1.tex"), 
          report=('vc*p'), 
          no.space=TRUE)

shapiro.test(multimodel_fixed$res)
lillie.test(multimodel_fixed$res)
qqnorm(multimodel_fixed$res)
qqline(multimodel_fixed$res)


# Split the fitted values, split among the 4 quantiles
qfits <- quantcut(multimodel$fit)
# Homogeneity test.
leveneTest(multimodel$res, qfits)
bartlett.test(multimodel$res, qfits)


# Use an automated stepwise model selection routine to determine best model (by BIC)
fullModel = multimodel_fixed
nullModel = lm(Y ~ 1, data = data) 
optimal_model = step(
                multimodel_fixed,
                direction = 'both', 
                scope = list(upper = fullModel, 
                             lower = nullModel), 
                trace = 0, # do not show the step-by-step process of model selection
                ) 

# View optimal model stats
summary(optimal_model)

stargazer(optimal_model,
          type="latex", 
          title="Stepwise Mutiple Linear Regression on Y", 
          ci=T, 
          label="tab::multi_optimal_1",
          df=T,
          out=filepath("multi_optimal_1.tex"), 
          report=('vc*p'), 
          no.space=TRUE)


# Compare BIC values
AIC(model)
AIC(multimodel_fixed)
AIC(optimal_model)

# Check assumptions
vif(optimal_model)

shapiro.test(optimal_model$res)
lillie.test(optimal_model$res)
qqnorm(optimal_model$res)
qqline(optimal_model$res)


# Split the fitted values, split among the 4 quantiles
qfits <- quantcut(optimal_model$fit)
# Homogeneity test.
leveneTest(optimal_model$res, qfits)
bartlett.test(optimal_model$res, qfits)

# PREDICTION
predin <- data.frame(X1=120, X2=30, X3=10, X4=90, W="B")
predict(optimal_model, newdata=predin)
predict(optimal_model, newdata=predin, interval = 'predict', level = 0.95)

# CUT
Z <- cut(data$X4, breaks=quantile(data$X4))
#fancier table
sjt.xtab(var.row=Z, var.col=data$W, show.row.prc = T)

anova <- aov(Y ~ W * Z, data=data)
summary(anova)
shapiro.test(anova$res)
lillie.test(anova$res)
leveneTest(Y ~ W * Z, data=data)