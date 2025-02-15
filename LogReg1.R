library(readr)
library(ggplot2)
library(tidyverse)
library(lattice)
library(pROC)
library(caret)

getwd()
dat <- read.csv("./dataset/bdiag.csv")

dat['diag_'] <- as.numeric(dat[c('diagnosis')] == 'M')
#View(dat)

dat_m <- dat[c('diag_', 'radius_mean', 'texture_mean')]

p1 = ggplot(data = dat, aes(fill = diagnosis, y = radius_mean))+geom_boxplot()
p2 = ggplot(data = dat, aes(fill = diagnosis, y = texture_mean))+geom_boxplot()

p1
p2

#removing outliers
Q_r = quantile(dat$radius_mean, probs=c(.25, .75), na.rm = FALSE)
iqr_r = IQR(dat$radius_mean)

up_r <-  Q_r[2]+1.5*iqr_r # Upper Range for radius 
low_r<- Q_r[1]-1.5*iqr_r # Lower Range for radius

Q_t = quantile(dat$texture_mean, probs=c(.25, .75), na.rm = FALSE)
iqr_t = IQR(dat$texture_mean)

up_t <-  Q_t[2]+1.5*iqr_t # Upper Range for texture
low_t<- Q_t[1]-1.5*iqr_t # Lower Range for texture

dat_m<- subset(dat_m, dat$radius_mean > (low_r) & dat$radius_mean < (up_r) & dat$texture_mean > (low_t) & dat$texture_mean < (up_t))


#split data
dat_m['randu'] <- c(runif(nrow(dat_m), 0, 1))

#training data 
train_dat = dat_m %>% filter(randu <= 0.1)

#test data
test_dat = dat_m %>% filter(randu >= 0.1)

#train model
model <- glm (diag_ ~ radius_mean + texture_mean, data = train_dat, family = binomial)

#check coefficients
summary(model)
sprintf('Odds ratio : \n %s', paste(exp(coef(model)), collapse = ", ")) %>% writeLines()

#scatterplot

slope <- coef(model)[2]/(-coef(model)[3])
intercept <- coef(model)[1]/(-coef(model)[3]) 

xyplot(texture_mean ~ radius_mean , data = test_dat, groups = diag_,
       panel=function(...){
         panel.xyplot(...)
         panel.abline(intercept , slope)
         panel.grid(...)
       })

#roc curve 
pred_y = predict.glm(object = model, newdata = test_dat, type = "response") 

roc_curve <- roc(test_dat$diag_ ~ pred_y, plot = TRUE, print.auc = TRUE, 
                 legacy.axes = TRUE)

#get best threshold 
predictions = prediction(pred_y, test_dat$diag_)

sens <- data.frame(x=unlist(performance(predictions, "sens")@x.values), 
                   y=unlist(performance(predictions, "sens")@y.values))
spec <- data.frame(x=unlist(performance(predictions, "spec")@x.values), 
                   y=unlist(performance(predictions, "spec")@y.values))

sens %>% ggplot(aes(x,y)) + 
  geom_line() + 
  geom_line(data=spec, aes(x,y,col="red")) +
  scale_y_continuous(sec.axis = sec_axis(~., name = "Specificity")) +
  labs(x='Cutoff', y="Sensitivity") +
  theme(axis.title.y.right = element_text(colour = "red"), legend.position="none") 

sens = cbind(unlist(performance(predictions, "sens")@x.values), unlist(performance(predictions, "sens")@y.values))
spec = cbind(unlist(performance(predictions, "spec")@x.values), unlist(performance(predictions, "spec")@y.values))
thresh = sens[which.min(apply(sens, 1, function(x) min(colSums(abs(t(spec) - x))))), 1]
thresh

#get confusion matrix
confusionMatrix(as.factor(as.double(pred_y>thresh)),as.factor(test_dat$diag_))
