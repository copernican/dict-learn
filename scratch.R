dat <- read.csv("sse_and_sparsity.csv")

plot(sort(dat$SSE, decreasing = T))
summary(dat$SSE)

sf <- stepfun(x = (seq_along(dat$SSE) - 1) * 2, y = c(0, dat$SSE))
plot(sf, do.points = F, main = NA, cex.lab = 0.75, cex.axis = 0.75,
     xlab = "Time", ylab = "SSE", xaxt = "n", frame.plot = F,
     xaxs = "i")

z <- (seq_along(dat$SSE) - 1) * 2

sec <- z + 10 
labs <- c("00:10", 
          paste(sprintf("%02d", sec[sec %% 30 == 0] %/% 60), 
                c("30", "00"), sep = ":"))

axis(side = 1, at = c(0, z[z %% 30 == 0][-1] - 10), 
     labels = labs, cex.axis = 0.75)

labs.epoch <- c("intro", "ballad", "guitar solo", "opera",
                "hard rock", "outro")
sec.epoch <- c(-10, 38, 144, 171, 235, 284)

for(i in seq_along(labs.epoch)) {
  mtext(labs.epoch[i], side = 3, line = 2, cex = 0.75, 
        at = sec.epoch[i], adj = 0)
}

labs.key <- c("B-flat maj", "A maj", "E-flat maj")
sec.key <- c(-10, 171, 192)

for(i in seq_along(labs.key)) {
  mtext(labs.key[i], side = 3, line = 1, cex = 0.75, 
        at = sec.key[i], adj = 0)
}

labs.inst <- c("vox", "piano", "bass", "set", "guitar", "vox", 
               "bass/set", "vox")
sec.inst <- c(-10, 5, 38, 73, 129, 174, 194, 243)

for(i in seq_along(labs.inst)) {
  mtext(labs.inst[i], side = 3, cex = 0.75, at = sec.inst[i], adj = 0)
}

col.qt <- c("red", "blue")
lty.qt <- 2:3

legend(x = 290, y = 500, legend = c("90th", "75th"),
       col = rev(col.qt), lty = rev(lty.qt), bty = "n", cex = 0.6,
       title = "SSE quantiles")

eps <- quantile(x = dat$SSE, probs = c(0.75, 0.9))
abline(h = eps, col = col.qt, lty = lty.qt)

events.q3 <- (which(dat$SSE > eps[1]) - 1) * 2 + 10
events.q3

events.90 <- which(dat$SSE > eps[2]) * 2 + 10
events.90
