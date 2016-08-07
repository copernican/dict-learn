dat <- read.csv("sse_and_sparsity.csv")

plot(sort(dat$SSE, decreasing = T))
summary(dat$SSE)

sf <- stepfun(x = seq_along(dat$SSE) - 1, y = c(0, dat$SSE))
plot(sf, do.points = F, main = NA, cex.lab = 0.75, cex.axis = 0.75,
     xlab = "Time", ylab = "SSE", xaxt = "n")

sec <- seq_along(dat$SSE) * 2 + 10
labs <- c("00:10", 
          paste(sprintf("%02d", sec[sec %% 30 == 0] %/% 60), 
                c("30", "00"), sep = ":"))

axis(side = 1, at = c(0, sec[sec %% 30 == 0]), 
     labels = labs, cex.axis = 0.75)

eps <- quantile(x = dat$SSE, probs = c(0.75, 0.9))
abline(h = eps, col = c("red", "blue"), lty = 2:3)

points(x = which(dat$SSE > eps[1]), 
       y = dat$SSE[which(dat$SSE > eps[1])])

