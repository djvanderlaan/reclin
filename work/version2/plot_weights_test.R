



s


w <- s$patterns$weight
n <- s$patterns$n


r <- sqrt(n)/sqrt(max(n)) * diff(range(w)) * 0.2

xlim <- c(min(w - r), max(w + r))
ylim <- c(-max(r), +max(r))

thr <- 0

col <- ifelse(w > thr, "#F0FF0050", "#FF00F050")

par(bg = "#444444", mar = c(0,0,0,0)+0.2)
plot(w, rep(0, length(w)), xlim = xlim, ylim = ylim, asp = 1, type=  'n',
  xaxt = 'n', yaxt = 'n', bty = 'n', xlab = "", ylab = "")
abline(h = 0, lwd = 1, col ="#101010")
symbols(w, rep(0, length(w)), circles = r, inches = FALSE, add = TRUE,
  fg = col, bg = col)
labs <- round(c(min(w), 0, max(w)), 1)
points(labs, y = rep(0, length(labs)), col = "#101010", pch = "|")
text(x = labs, y = 0, labels = labs, pos = 1)




d <- density(w, weights = n, bw = 0.05)

plot(d)
d$x

par(bg = "#444444", mar = c(3,2,0,0)+0.5)
plot(d$x, log(d$y+1), type = 'n',
   xaxt = 'n', yaxt = 'n', bty = 'n', xlab = "", ylab = "", xaxs='i', yaxs = 'i')
sel <- d$x < 0
polygon(c(min(d$x[sel]), d$x[sel], max(d$x[sel])), log(c(0, d$y[sel], 0)+1), 
  col = "#FF00F0FF", border = "#FF00F0FF")
polygon(c(min(d$x[!sel]), d$x[!sel], max(d$x[!sel])), log(c(0, d$y[!sel], 0)+1), 
  col = "#F0FF00FF", border = "#F0FF00FF")
axis(1, lwd = 0, lwd.ticks = 1, line = 0.2, cex = 0.1)
mtext(side = 1, text = "Weight", line = 2.2)
mtext(side = 2, text = "log(density + 1)", line = 1)
