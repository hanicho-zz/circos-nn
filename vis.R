library(circlize)

neuralVis = function(Ws, height=0.025, gap=1/1.61803, globalnorm=FALSE) {
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
                circos.rect((i-1)/n, 0, i/n, 1, border=color)
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

        circos.trackPlotRegion(
            ylim=c(0, 1),
            track.height=(gap/l)-height,
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
                        alpha = round(alpha * 255)

                        color = sprintf("#%06X%02X",
                                        if(A[i, j] < 0) 0xDD1100 else 0x11DD00, alpha)

                        circos.lines(xs, ys, col=color)
                    }
                }
            })

        circos.trackPlotRegion(
            ylim=c(0, 1),
            track.height=height,
            bg.border=NA,
            panel.fun=function(x, y) {
                for(j in seq_along(1:m)) {
                    color = "darkslategrey"
                    circos.rect((j-1)/m, 0, j/m, 1, border=color)
                }
            })
    }

    circos.clear()
}

set.seed(99)

n = 19
m = 33
q = 19
w = 12
A = matrix(rnorm(n*m), nrow=n, ncol=m)
B = matrix(rnorm(m*q), nrow=m, ncol=q)
C = matrix(5*rnorm(q*w), nrow=q, ncol=w)

neuralVis(list(A, B, C), gap=1/3.14, globalnorm=FALSE)
