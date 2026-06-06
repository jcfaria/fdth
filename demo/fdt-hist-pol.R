# Frequency distribution polygon superimposed on the histogram

library(fdth)
tb <- fdt(rnorm(1e3,
                m=10,
                sd=2))

plot(tb)
brk <- with(tb,
            seq(breaks["start"],
                breaks["end"],
                breaks["h"]))

mids <- 0.5 * (brk[-1] + brk[-length(brk)])

y <- with(tb,
          table[, 2])

lines(mids,
      y,
      type = "b",
      col = 2,
      lwd = 2,
      pch = 19)

# or
plot(tb)
par(new=TRUE)
plot(tb,
     type = "fp",
#     add = T,
     col = 2,
     lwd = 2)
