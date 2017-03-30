library(circlize)

neuralVis = function(Ws, height=0.025, gap=1/2.41, filter=0, globalnorm=FALSE) {
    circos.par(track.margin=c(0, 0),
               points.overflow.warning=FALSE,
               gap.degree=0,
               cell.padding=c(0, 0, 0, 0),
               start.degree=90)
    circos.initialize(factors="a", xlim=c(0, 1))

    layers = length(Ws)
    maxA = -Inf
    minA = Inf

    for(l in 1:layers) {
        curMax = max(Ws[[l]])
        curMin = min(Ws[[l]])

        if(curMax > maxA) {
            maxA = curMax
        }
        if (curMin < minA) {
            minA = curMin
        }
    }

    circos.trackPlotRegion(
        ylim=c(0, 1),
        track.height=height,
        bg.border=NA,
        panel.fun=function(x, y) {
            n = nrow(Ws[[1]])

            for(i in seq_along(1:n)) {
                color = "darkslategrey"
                circos.rect((i-1)/n, 0, i/n, 1,
                            lwd=1, border=color)
            }
        })

    for(l in 1:layers) {
        A = Ws[[l]]
        n = nrow(A)
        m = ncol(A)

        if (!globalnorm) {
            maxA = max(A)
            minA = min(A)
        }

        circ = if(l == 1) gap/l - height
               else gap/l - height/(l-1)

        circos.trackPlotRegion(
            ylim=c(0, 1),
            track.height=circ,
            bg.border=NA,
            panel.fun=function(x, y) {
                for(i in seq_along(1:n)) {
                    for(j in seq_along(1:m)) {
                        l1 = (i-1)/n
                        r1 = i/n
                        m1 = (l1+r1)/2
                        m1i = m1-1

                        l2 = (j-1)/m
                        r2 = j/m
                        m2 = (l2+r2)/2
                        m2i = m2-1

                        d1 = sqrt((m1-m2)^2)
                        d2 = sqrt((m1i-m2)^2)
                        d3 = sqrt((m1-m2i)^2)
                        d4 = sqrt((m1i-m2i)^2)
                        which = which.min(c(d1, d2, d3, d4))

                        xs = NULL
                        ys = NULL

                        if(which == 1) {
                            xs = c(m1, m2)
                            ys = c(1, 0)
                        } else if(which == 2) {
                            xs = c(m1i, m2)
                            ys = c(1, 0)
                        } else if (which == 3) {
                            xs = c(m1, m2i)
                            ys = c(1, 0)
                        } else if (which == 4) {
                            xs = c(m1i, m2i)
                            ys = c(1, 0)
                        }

                        alpha = if(A[i, j] < 0) abs(A[i, j]) / abs(minA)
                                else A[i, j] / maxA

                        if (alpha < filter)
                            next

                        alpha = round(alpha * 255)
                        color = sprintf("#%06X%02X",
                                        if(A[i, j] < 0) 0xDD1100
                                        else 0x11DD00,
                                        alpha)

                        circos.lines(xs, ys,
                                     lwd=1/l, col=color)
                    }
                }
            })

        circos.trackPlotRegion(
            ylim=c(0, 1),
            track.height=height/l,
            bg.border=NA,
            panel.fun=function(x, y) {
                for(j in seq_along(1:m)) {
                    color = "darkslategrey"
                    circos.rect((j-1)/m, 0, j/m, 1,
                                lwd=1/l, border=color)
                }
            })
    }

    circos.clear()
}

set.seed(565)

As = list()
L = 50

for (i in 1:L) {
    n = sample(1:50, 1)
    m = sample(1:50, 1)
    A = matrix(rnorm(n*m), nrow=n, ncol=m)
    As[[i]] = A
}

SILVER = 2.4142135623730950488
GOLDEN = 1.6180339887498948482

svg("Rplot.svg")
neuralVis(As, gap=1/GOLDEN, filter=0, globalnorm=FALSE)
dev.off()
