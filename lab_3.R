#relations to look at: 
#arrest vs conviction vs prison sentence
#length of prison sentence
#police per capita -- at some point there needs to be a tipping point (Mark)

#MARK QUESTIONS
#SHOULD we filter out data points where prob is greater than 1?
#Looking for causal? Only things that I think are remotely causal is wage
#PCA analysis on the data 
#stepwise regression CARET package

#Paul questions?
#cross validation with test data to see if the effect is causal?

lm_plot_routine <- function(x,y) {
  model = lm(y ~ x)
  plot(x,y)
  abline(model)
  print(summary(model)$r.squared)
}
library(dplyr)

setwd("~/Desktop/Stone/Berkeley_MIDS/Statistics/Labs/Lab_3")
full_data <- read.csv('crime_v2.csv')
data <- na.omit(full_data)

#check for duplicated data and remove
sum(duplicated(data))
data <- distinct(data, .keep_all=T)

#data type of prbarr, prbconv, prbpris
data %>% 
  select(prbarr, prbconv, prbpris) %>%
  lapply(class)

#convert prbconv factor in numeric
data$prbconv <- as.numeric(levels(data$prbconv))[data$prbconv]

#QUESTION: arrest vs conviction vs prison sentence
#we need to make sure the probabilities go from 0 to 1
#makes no sense for example for convictions:arrest to be greater than 1
#must be arrested to have a conviction

#point out that some are greater than 1
#maybe renormalize them
#data <- subset(data, prbarr <=1 & prbconv <= 1 &prbpris <= 1)

#Correlate everything
y <- data$crmrte
X <- names(data)[4:dim(data)[2]]
cor_df <- data.frame(crmrte_cor = numeric(0))
for (x in X) {
  corr <- as.data.frame(cor(y, data[x]))
  rownames(corr) <- c(x)
  colnames(corr) <- c('crmrte_cor')
  cor_df <- rbind(cor_df, corr)
}
cor_df$var <- rownames(cor_df)
cor_df <- arrange(cor_df, desc(crmrte_cor))

#wages
all_wages <- grep('^w', names(data))[-1:-1]
median_wages <- apply(data[, all_wages], 2, median)
pctymle_wages <- lm(data$pctymle ~ ., data=data[, all_wages])
crmrte_wages <- lm(data$crmrte ~ ., data=data[, all_wages])

#poor wages
poor_wages <- c('wcon', 'wtrd')
pctymle_poor_wages <- lm(data$pctymle ~ ., data=lapply(data[, poor_wages],log))
crmrte_poor_wages <- lm(data$crmrte ~ ., data=lapply(data[, poor_wages], log))

crmrte_wcon <- lm(data$crmrte ~ log(data[,'wfir']))

#arrest -- fear
#higher probability of arrest, lower the crime rate
m <- lm(crmrte ~ prbarr, data=data)
plot(data$prbarr, data$crmrte)
abline(m)
plot(m)

#rate of conviction and prison
#rate of arrest not enough! Need to convict and punish in order to prevent repeat offenders and send message
rtofconv <- data$prbconv*data$prbarr
rtofprison <- data$prbconv*data$prbarr*data$prbpris

#maybe average blue collar wage?

#young male good predictor on its own; better programs for rehabilitating young men


#stepwise AIC optimization from MASS package

y <- data$crmrte
X <- data[,!names(data) %in% c('crmrte')]
mod <- lm(y ~ ., data=X)
min.AIC <- stepAIC(mod);
summary(min.AIC)$r.squared

crime <- data
filtered <- crime[crime$polpc <=
                    (as.numeric(quantile(crime$polpc)[4]) + (IQR(crime$polpc) * 1.5)) &
                    crime$polpc >= (as.numeric(quantile(crime$polpc)[2]) - (IQR(crime$polpc) * 1.5)),]
y <- filtered$crmrte
X <- filtered[,!names(filtered) %in% c('crmrte')]
mod_polfiltered <- lm(y ~ ., data=X)
min.AIC_polfiltered <- stepAIC(mod_polfiltered);
summary(min.AIC_polfiltered)$r.squared
