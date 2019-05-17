source("code/functions.R")
source("code/multistart.R")

source("code/RBF.R")
source("code/laplace.R")
source("code/matern52.R")
source("code/matern32.R")

fet <- lapply(train, feature) #estimate hyperparameter

save(fet, file = "fetmatern32fem.Rdata")

# Plot fitted
ageseq <- seq(-3,3, length = 100)

plot(fet$Asian$trainx, fet$Asian$trainy, pch = 16, col = 1, 
     cex=.4, xlab='Age', ylab='Spinal bone mineral density')
points(fet$Black$trainx, fet$Black$trainy, pch = 16, col = 2, cex=.4)
points(fet$Hispanic$trainx, fet$Hispanic$trainy, pch = 16, col = 3, cex=.4)
points(fet$White$trainx, fet$White$trainy, pch = 16, col = 4, cex=.4)
lines(ageseq, gpsmooth(ageseq, fet$Asian), type='l', lwd = 2, col = 1)
lines(ageseq, gpsmooth(ageseq, fet$Black), type='l', lwd = 2, col = 2)
lines(ageseq, gpsmooth(ageseq, fet$Hispanic), type='l', lwd = 2, col = 3)
lines(ageseq, gpsmooth(ageseq, fet$White), type='l', lwd = 2, col = 4)
legend('bottomright', c('Asian', 'Black', 'Hispanic', 'White'), col = c(1:4), lty=1, bty = 'n')


#------------------------------


log.prob <- c()

for(i in unique(test$idnum)) {
  
  fit1 <- fit.gp(fet$Asian, testx = test[test$idnum == i, 3], testy = test[test$idnum == i, 5])
  fit2 <- fit.gp(fet$Black, testx = test[test$idnum == i, 3], testy = test[test$idnum == i, 5])
  fit3 <- fit.gp(fet$Hispanic, testx = test[test$idnum == i, 3], testy = test[test$idnum == i, 5])
  fit4 <- fit.gp(fet$White, testx = test[test$idnum == i, 3], testy = test[test$idnum == i, 5])
  log.prob <- rbind(log.prob, c(fit1, fit2, fit3, fit4))
  
}

y <- c()
for (i in unique(test$idnum)) {
  y <- rbind(y, as.character((test[test$idnum == i, 2])[1]))
}

y.pred <- apply(log.prob, 1, function(x){ which(x == max(x))})
pred <- ifelse(y.pred == 1, 'Asian', ifelse(y.pred == 2, 'Black', 
        ifelse(y.pred == 3, 'Hispanic', 'White')))
table(pred, y)
err <- mean(pred != y)
err


#Training error
#Full training
#laplace : 0.5214286, RBF : 0.5892857, matern52: 0.5964286. matern32: 0.5821429
