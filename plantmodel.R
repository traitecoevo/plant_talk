library(grImport)
library(plant)

source("plantmodel-fun.R")
source("circlepack.R")
dir.create("fig", FALSE)

to.pdf <- to.cairo_pdf

for (i in 1:4)
  to.pdf(fig.fitness(i),
         sprintf("fig/fitness-%d.pdf", i), width=6, height=4)
for (i in 1:2)
  to.pdf(fig.lifecycle(i),
         sprintf("fig/lifecycle-%d.pdf", i), width=4, height=4)
for (i in 1:7)
  to.pdf(fig.plantmodel(i),
         sprintf("fig/plantmodel-%d.pdf", i), width=6, height=4)
for (i in 1:3)
  to.pdf(fig.lightenv(i),
         sprintf("fig/lightenv-%d.pdf", i), width=6, height=4)
to.pdf(fig.blackbox(),
       "fig/blackbox.pdf", width=4, height=4)
for (i in 1:3)
  to.pdf(fig.growth(i),
         sprintf("fig/growth-%d.pdf", i), width=6, height=4)
for (i in 1:3)
  to.pdf(fig.competition(i),
         sprintf("fig/competition-%d.pdf", i), width=6, height=4)

to.pdf(fig.blackbox(embed=TRUE),
       "fig/blackbox-small.pdf", width=4, height=4)
