library(nortest)
library(car)


# FUNCTIONS

# generates bins for lm model homoscedascity  
quantcut <- function(x, digits=6) { 
  cut(x, breaks=quantile(x), include.lowest=TRUE, dig.lab = digits) 
}

# IMPORT DATA
data <- read.table("data.txt", header=TRUE)
data$W <- factor(data$W)


# ONE-WAY ANOVA TESTS
boxplot(X1 ~ W, data=data)
boxplot(X2 ~ W, data=data)
boxplot(X3 ~ W, data=data)
boxplot(X4 ~ W, data=data)
boxplot(Y ~ W, data=data)

anova <- aov(X1 ~ W, data=data)
shapiro.test(anova$res)
lillie.test(anova$res)
leveneTest(X1 ~ W, data=data)

anova <- aov(X2 ~ W, data=data)
shapiro.test(anova$res)
lillie.test(anova$res)
leveneTest(X2 ~ W, data=data)

# not homogenous
anova <- aov(X3 ~ W, data=data)
shapiro.test(anova$res)
lillie.test(anova$res)
leveneTest(X3 ~ W, data=data)

anova <- aov(X4 ~ W, data=data)
shapiro.test(anova$res)
lillie.test(anova$res)
leveneTest(X4 ~ W, data=data)

anova <- aov(Y ~ W, data=data)
shapiro.test(anova$res)
lillie.test(anova$res)
leveneTest(Y ~ W, data=data)

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


# REGRESSION
model <- lm(Y ~ X4, data=data)
summary(model)

multimodel <- lm(Y ~ X1*W + X2*W + X3*W + X4*W, data=data)
summary(multimodel)

# Check for multi-colinearity
vif(multimodel)
vif(multimodel, type="predictor")

multimodel <- lm(Y ~ X1 + X2 + X3 + X4 + W, data=data)
vif(multimodel)
vif(multimodel, type="predictor")

shapiro.test(multimodel$res)
lillie.test(multimodel$res)
qqnorm(multimodel$res)
qqline(multimodel$res)


# Split the fitted values, split among the 4 quantiles
qfits <- quantcut(multimodel$fit)
# Homogeneity test.
leveneTest(multimodel$res, qfits)
bartlett.test(multimodel$res, qfits)


# Use an automated stepwise model selection routine to determine best model (by BIC)
fullModel = multimodel
nullModel = lm(Y ~ 1, data = data) 
optimal_model = step(
  multimodel,
  direction = 'both', 
  scope = list(upper = fullModel, 
               lower = nullModel), 
  trace = 0, # do not show the step-by-step process of model selection
  k=2) #choose by BIC as we want the best explanatory, not predictive model 

# View optimal model stats
summary(optimal_model)

# Compare BIC values
BIC(model)
BIC(multimodel)
BIC(optimal_model)

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

# CUT
Z <- cut(data$X4, breaks=quantile(data$X4))
table(Z, data$W)

anova <- aov(Y ~ W * Z, data=data)
summary(anova)
shapiro.test(anova$res)
lillie.test(anova$res)
leveneTest(Y ~ W * Z, data=data)

