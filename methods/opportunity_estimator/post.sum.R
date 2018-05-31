# Descritavas da amostra a posteriori
post.sum = function(x,probs = c(0.5, 0.025,0.975)) c(mean = mean(x), quantile(x,probs, na.rm=TRUE))
