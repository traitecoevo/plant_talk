simulation <- function() {
  p0 <- scm_base_parameters("FF16")
  p0$control$equilibrium_nsteps <- 30
  p0$control$equilibrium_solver_name <- "hybrid"
  p0$disturbance_mean_interval <- 30.0

  p1 <- expand_parameters(trait_matrix(0.0825, "lma"), p0, FALSE)
  # Run everything out to equilibrium
  p1_eq <- equilibrium_seed_rain(p1)
  #This collects information about the state of the system at every ODE step:
  run_scm_collect(p1_eq)
}

fig.fitness <- function(option, seed, tree, res, draft=FALSE) {
  reset()
  w <- 0.33

  da <- unit(0.1, "snpc")
  arr <- arrow(type="closed", angle=20,
               length=convertWidth(da * 0.5, "cm"))
  arr.gp <- gpar(fill="black", lwd=3)

  w2 <- (unit(1, "npc") - convertWidth(unit(w, "snpc"), "npc")) * 0.5
  dh <- 0.2
  da <- unit(.1, "npc")
  x0 <- unit(0.5, "npc")
  y0 <- unit(0.5, "npc")

  pushViewport(viewport(x=w2 * 0.5, width=w2))
  grid.picture(seed, y=0.5 + dh, height=0.15)
  grid.picture(colour.picture(seed, "grey30"), y=0.5 - dh, height=0.15)

  xtrait <- unit(0.5, "npc") - unit(1.5, "lines")
  grid.text(expression(bgroup("{", atop(x[a], y[a]), "}")),
            xtrait, 0.5 + dh, just="right")
  grid.text(expression(bgroup("{", atop(x[b], y[b]), "}")),
            xtrait, 0.5 - dh, just="right")

  if (option == 1) {
    popViewport()
    return()
  }

  xlab <- convertX(unit(0.5, "npc") + unit(1.5, "lines"), "cm")
  grid.text(expression(N[a]), xlab, 0.5 + dh, just="left")
  grid.text(expression(N[b]), xlab, 0.5 - dh, just="left")
  grid.segments(x0 + da,               y0 + unit(dh, "npc") - da,
                unit(1, "npc") - da,   x0 + da * 0.33,
                arrow=arr, gp=arr.gp)
  grid.segments(x0 + da,               y0 - unit(dh, "npc") + da,
                unit(1, "npc") - da,   x0 - da * 0.33,
                arrow=arr, gp=arr.gp)
  popViewport()

  if (option < 4) {
    grid.rect(width=unit(w, "snpc"), height=unit(w, "snpc"),
              gp=gpar(fill="black"))
  } else {
    pushViewport(viewport(width=unit(w, "snpc"),
                          height=unit(w, "snpc"),
                          gp=gpar(cex=0.25, lex=0.5)))
    fig.blackbox(tree, res, draft=draft, embed=TRUE)
    popViewport()
  }

  if (option == 2)
    return()

  pushViewport(viewport(x=unit(1, "npc") - w2 * 0.5, width=w2))

  grid.picture(seed, y=0.5 + dh, height=0.15)
  grid.picture(colour.picture(seed, "grey30"), y=0.5 - dh, height=0.15)
  grid.text(expression(N[a] * minute), xlab, 0.5 + dh, just="left",
            gp=gpar(cex=0.7))
  grid.text(expression(N[b] * minute), xlab, 0.5 - dh, just="left",
            gp=gpar(cex=1.5))

  grid.segments(unit(0, "npc") + da, x0 + da * 0.33,
                x0 - da,             y0 + unit(dh, "npc") - da,
                arrow=arr, gp=arr.gp)
  grid.segments(unit(0, "npc") + da, x0 - da * 0.33,
                x0 - da,             y0 - unit(dh, "npc") + da,
                arrow=arr, gp=arr.gp)
  popViewport()
}

fig.lifecycle <- function(option, seed, tree, res=NULL) {
  reset()

  x0 <- unit(.15, "npc")
  w <- unit(.2, "snpc")

  grid.picture(seed, x0, just=c("centre", "centre"), height=w)
  if (option == 2) {
    grid.picture(tree,  unit(1, "npc") - x0,
                 just=c("centre", "centre"), height=2*w)
  } else if (option == 3) {
    pushViewport(viewport(x=unit(1, "npc") - x0,
                          width=unit(0.3, "snpc"),
                          height=unit(0.3, "snpc"),
                          gp=gpar(cex=0.25, lex=0.5)))
    fig.blackbox(tree, res, draft=draft, embed=TRUE,
          cols = c("black", "darkorange", "darkblue"))
    popViewport()
 }

  wc <- convertWidth(unit(1, "npc") - 2 * x0, "cm")
  pushViewport(viewport(width=wc, height=wc,
                        xscale=c(-1, 1), yscale=c(-1, 1)))

  dt <- pi * 0.25
  if (option == 1) {
    theta <- seq(0, 2 * pi - dt, length=101) + pi + dt / 2
    grid.lines(cos(theta), sin(theta), default.units="native",
               gp=gpar(lwd=4, fill="black"),
               arrow=arrow(angle=20, type="closed"))
  } else {
    dt2 <- dt * 1.5
    theta1 <- seq(dt2/2 * 1.1, pi - dt/2, length=101)
    theta2 <- seq(pi + dt/2, 2*pi - dt2/2, length=101)
    grid.lines(cos(theta1), sin(theta1), default.units="native",
               gp=gpar(lwd=4, fill="black"),
               arrow=arrow(angle=20, type="closed"))
    grid.lines(cos(theta2), sin(theta2), default.units="native",
               gp=gpar(lwd=4, fill="black"),
               arrow=arrow(angle=20, type="closed"))
  }
  popViewport()
}

fig.plantmodel <- function(option, tree, res) {
  reset()

  ## 1: Space for the plant:
  p <- 0.4
  pushViewport(viewport(width=unit(p, "npc"), x=0, just="left"))

  h <- convertHeight(unit(0.5, "npc"), "cm")
  r <- aspect.ratio(tree)
  x0 <- unit(0.55, "npc")
  y0 <- unit(0.6, "npc")
  fudge <- 1.1 # border has been added from somewhere!

  grid.picture(tree, x0, y0, just=c("centre", "centre"), height=h * fudge)

  if (option > 1) {
    x <- x0 - h * (1/r) * 0.5 - unit(.5, "lines")
    grid.segments(x, y0 - h * 0.5,
                  x, y0 + h * 0.5,
                  gp=gpar(lwd=3, lineend="butt"))
    grid.text("Height", x - unit(1, "lines"), y0, rot=90)

    da <- unit(0.1, "snpc")
    arr <- arrow(type="closed", angle=20,
                 length=convertWidth(da * 0.5, "cm"))
    arr.gp <- gpar(fill="black", lwd=3)

    m.x1 <- x0 + h * fudge * (1 / r) * 0.03
    m.x2 <- m.x1 + da
    m.y1 <- y0 - h * 0.5 - unit(0.5, "lines")
    m.y2 <- m.y1 - da
    grid.segments(m.x1, m.y1, m.x2, m.y2, arrow=arr, gp=arr.gp)
    grid.text("Mortality", m.x2, m.y2 - unit(.7, "lines"))

    f.x1 <- m.x1
    f.x2 <- f.x1 + da
    f.y1 <- y0 + h * 0.5 + unit(0.5, "lines")
    f.y2 <- f.y1 + da
    grid.segments(f.x1, f.y1, f.x2, f.y2, arrow=arr, gp=arr.gp)
    grid.text("Seeds", f.x2, f.y2 + unit(.7, "lines"))
  }
  popViewport()

  if (option <= 2)
    return()

  ## Then, figures on the RHS.
  pushViewport(viewport(width=unit(1-p, "npc"), x=1, y=y0, just="right",
                        height=h))
  on.exit(popViewport())

  b <- unit(1, "lines")
  w <- (unit(1, "npc") - 4 * b) * (1/3)

  ## Hmm, I never exposed q() and Q() from the Plant object, so we have
  ## to use the reference model here.  Oh well.
  plant <- make_reference_plant() #make.reference()
  ## Tweak the Eta to match the picture better!
  assign("p.eta", 5, environment(plant$leaf.pdf))

  ## TODO: perhaps rescale the light environment a bit to make the
  ## drop-off harsher, so that the assimilation curve looks weirder.

  env <- res$light_env[[length(res$light_env)]]
  h.max <- max(env[,"height"])
  height <- seq(0, h.max, length=201)
  light.env <- splinefun(env[,"height"], env[,"canopy_openness"])

  assim.h <- function(x, h)
    cleanup(plant$leaf.pdf(x, h) * plant$Assim(plant$LeafArea(h),
                                               light.env(x)))

  info <- data.frame(height=height,
                     light=light.env(height),
                     leaf.area=cleanup(plant$leaf.pdf(height, h.max)),
                     assim=assim.h(height, h.max))

  pushViewport(viewport(x=b, width=w, just="left",
                        xscale=range(info$leaf.area), yscale=c(0, h.max)))
  grid.text("Leaf area", y=unit(-1, "lines"))
  simple.frame()
  grid.lines(info$leaf.area, info$height,
             gp=gpar(lwd=3, lineend="butt"), default.units="native")
  popViewport()

  if (option == 3)
    return()

  pushViewport(viewport(x=2 * b + w, width=w, just="left",
                        xscale=range(info$light), yscale=c(0, h.max)))
  grid.text("Light", y=unit(-1, "lines"))
  simple.frame()
  grid.lines(info$light, info$height,
             gp=gpar(lwd=3, lineend="butt"), default.units="native")
  popViewport()

  if (option == 4)
    return()

  pushViewport(viewport(x=3 * b + 2 * w, width=w, just="left",
                        xscale=range(info$assim), yscale=c(0, h.max)))
  grid.text("Photosynthesis", y=unit(-1, "lines"))
  if (option > 5)
    grid.polygon(info$assim, info$height,
                 gp=gpar(fill="grey", col=NA),
                 default.units="native")
  simple.frame()
  grid.lines(info$assim, info$height,
             gp=gpar(lwd=3, lineend="butt"), default.units="native")

  if (option > 6) {
    ## This one wants labels...
    a.y1 <- unit(-1.75, "lines")
    a.dx <- unit(-0.3, "snpc")
    a.dy <- unit(-0.3, "snpc")

    xx <- c(0.2, 0.5, 0.8)

    a.x2 <- unit(xx[1], "npc") + a.dx
    a.y2 <- a.y1 + a.dy
    grid.segments(xx[1], a.y1, a.x2, a.y2, arrow=arr, gp=arr.gp)
    grid.text("Maintenance", a.x2 - unit(.5, "lines"), a.y2, just="right")

    a.x3 <- unit(xx[2], "npc") + a.dx*2
    a.y3 <- a.y1 + a.dy*2
    grid.segments(unit(xx[2], "npc"), a.y1, a.x3, a.y3, arrow=arr, gp=arr.gp)
    grid.text("Growth", a.x3 - unit(.5, "lines"), a.y3, just="right")

    a.x4 <- unit(xx[3], "npc") + a.dx*3
    a.y4 <- a.y1 + a.dy*3
    grid.segments(unit(xx[3], "npc"), a.y1, a.x4, a.y4, arrow=arr, gp=arr.gp)
    grid.text("Reproduction", a.x4 - unit(.5, "lines"), a.y4, just="right")
    popViewport()
  }
}

fig.lightenv <- function(option, tree, res) {
  reset()

  plant <- make_reference_plant() #make.reference()
  assign("p.eta", 5, environment(plant$leaf.pdf))

  env <- res$light_env[[length(res$light_env)]]
  h.max <- max(env[,"height"])
  height <- seq(0, h.max, length=201)
  light.env <- splinefun(env[,"height"], env[,"canopy_openness"])

  ## Let's come up with a vector of heights to make trees at:
  set.seed(10)
  heights <- rweibull(30, 1)
  # heights <- sort(heights, decreasing=TRUE)

  ## Then, compute the widths, based on the little tree picture that we
  ## have:
  w <- aspect.ratio(tree) * heights
  w <- w / sum(w)
  x <- cumsum(w)
  r <- aspect.ratio(tree)

  hh <- seq(0, 1, length=101)
  aa <- cleanup(plant$leaf.pdf(hh, 1))
  canopy <- data.frame(height=hh, leaf=aa/max(aa))

  pushViewport(viewport(y=0.75, height=.2, just=c("bottom")))
  widths <- convertWidth(unit(w, "npc"), "cm")
  for (i in seq_along(heights)) {
    pushViewport(viewport(x[i], 0, just=c("right", "bottom"),
                          width=widths[i], height=widths[i] * r))
    grid.picture(tree)
    popViewport()
  }
  popViewport()

  if (option == 1)
    return()

  pushViewport(viewport(y=0.75, height=max(widths) * r, just=c("top")))
  for (i in seq_along(heights)) {
    pushViewport(viewport(x[i], 0, just=c("right", "bottom"),
                          width=widths[i], height=widths[i] * r))
    pushViewport(viewport(w=0.9))
    grid.polygon(canopy$leaf, canopy$height,
                 gp=gpar(col=NA, fill="black"))
    grid.segments(0, 0, 0, 1, gp=gpar(lineend="butt"))
    popViewport(2)
  }
  popViewport()

  if (option == 2)
    return()

  pushViewport(viewport(x=0.5, y=unit(2, "lines"), width=0.3, height=0.4,
                        just="bottom", xscale=c(0, 1),
                        yscale=range(env[,"height"])))
  simple.frame()
  grid.lines(env[,"canopy_openness"], env[,"height"], default.units="native",
             gp=gpar(lwd=3, lineend="butt"))
  grid.text("Height", x=unit(-1, "lines"), rot=90)
  grid.text("Light", y=unit(-1, "lines"))
  popViewport(1)
}

fig.blackbox <- function(tree, res, draft=FALSE, embed=FALSE,
  cols = c("black", "grey30")) {

  set.seed(1)

  plant <- make_reference_plant() #make.reference()
  assign("p.eta", 5, environment(plant$leaf.pdf))
  i <-  length(res$light_env)
  env <- res$light_env[[i]]
  h.max <- max(env[,"height"])
  height <- seq(0, h.max, length=201)
  light.env <- splinefun(env[,"height"], env[,"canopy_openness"])
  assim <- cleanup(plant$leaf.pdf(height, h.max))

  r <- 2/3

  pushViewport(viewport(width=unit(1, "snpc"), height=unit(1, "snpc"),
                        xscale=c(-1, 1), yscale=c(-1, 1)))
  grid.rect(gp=gpar(fill="black"))

  draw.patch(x0=unit(0.03, "npc"), y0=unit(0.5, "npc"), w0=unit(0.33, "npc"),
    tree, cols = cols,
    iterations=1000, r0=0.03, w=1.2, max.size=.15, draft=FALSE)

  dt <- 2*pi / 6
  grid.points(r * cos(dt), r*sin(dt))

  x1 <- x2 <- unit(r * cos(dt), "native") + unit(0.1, "npc")
  y1 <- unit(r * sin(dt), "native")
  y2 <- unit(r * sin(-dt), "native")

  w1 <- w2 <- 0.33

  pushViewport(viewport(x1, y1, w1, w1))
  grid.rect(gp=gpar(col="white", fill="white"))
  mar <- unit(1.2, "lines")
  pushViewport(viewport(mar, mar,
                        unit(1, "npc") - mar * 1.5,
                        unit(1, "npc") - mar * 1.5,
                        just=c("left", "bottom"),
                        xscale=c(0, 1),
                        yscale=range(env[,"height"])))
  simple.frame()
  grid.lines(env[,"canopy_openness"], env[,"height"], default.units="native",
             gp=gpar(lwd=3, lineend="butt"))
  if (!embed) {
    grid.text("Height", x=unit(-0.66, "lines"), rot=90)
    grid.text("Light", y=unit(-0.66, "lines"))
  }
  popViewport(2)

  pushViewport(viewport(x2, y2, w2, w2))
  grid.rect(gp=gpar(col="white", fill="white"))

  pushViewport(viewport(width=0.9))
  pushViewport(viewport(0, width=2/3, just="left"))
  grid.picture(tree, width=unit(1, "npc"))
  popViewport()

  pushViewport(viewport(1, width=1/3, just="right", height=0.76,
                        xscale=range(assim), yscale=range(height)))
  grid.polygon(assim, height,
               gp=gpar(fill="grey", col=NA),
               default.units="native")
  simple.frame()
  grid.lines(assim, height,
             gp=gpar(lwd=if (embed) 1.5 else 3, lineend="butt"),
             default.units="native")
  popViewport(3)

  dt <- pi * 0.03
  theta0 <- c(pi - pi / 6,   pi * 0.12, 2*pi - pi * 0.4) - dt
  theta1 <- c(pi * 0.4,    - pi * 0.12, pi + pi / 6) + dt

  theta <- lapply(1:3, function(i)
                  seq(theta0[i], theta1[i], length=101))
  da <- unit(.1, "npc")
  arr <- arrow(type="closed", angle=20,
               length=convertWidth(da * 0.5, "cm"))
  lapply(theta, function(t)
         grid.lines(unit(r * cos(t), "native"),
                    unit(r * sin(t), "native"),
                    arrow=arr, gp=gpar(col="white", lwd=4,
                                 fill="white")))
  if (!embed) {
    theta2 <- (theta0 + theta1) / 2
    grid.text("Growth,\ndeaths",
              unit(1.1*r*cos(theta2[3]), "native"),
              unit(1.1*r*sin(theta2[3]), "native"),
              gp=gpar(col="white"), just=c("right", "top"))
    grid.text("Shading",
              unit(1.1*r*cos(theta2[1]), "native"),
              unit(1.1*r*sin(theta2[1]), "native"),
              gp=gpar(col="white"), just=c("right", "bottom"))
    grid.text("Photosynthesis", unit(r * 0.9, "native"),
              just="right", gp=gpar(col="white"))
  }
  popViewport(1)
}

fig.patch <- function(option, tree, draft=FALSE) {

  set.seed(1)

  cols <- c("black")

  pushViewport(viewport(width=unit(1, "snpc"), height=unit(1, "snpc"),
                        xscale=c(-1, 1), yscale=c(-1, 1)))
  grid.rect(gp=gpar(fill=make.transparent("black", 0), col = NA))

  rect  <- rect_ladj <- matrix(c(0.5, 0.5, 0.4, 0.4), ncol=1)
  rect_ladj[1,] <- rect_ladj[1,] - 0.5* rect_ladj[3,]
  rect_tree <- rect
  rect_tree[3:4,] <- 0.2

  focal_h <- rect_tree[3,1]*3

  # draw central patch, leaving space for focal tree
  if(option > 1) {
    draw.patch(x0=unit(rect_ladj[1,1], "npc"), y0=unit(rect_ladj[2,1], "npc"), w0=unit(rect_ladj[3,1], "npc"),
      tree, cols = cols, fill = "white", border = "black",
      iterations=1000, r0=0.03, w=1.2, max.size=.15, rect=rect_tree)
   focal_h <- focal_h*0.45/3
  }

  # draw focal tree
  grid.picture(colour.picture(tree, "darkgreen"),
                 rect_tree[1,1]*0.94, rect_tree[2,1]*1.01,
                 just=c("centre", "centre"), height=focal_h)
  popViewport(1)
  }

fig.metacommunity <- function(option, tree, draft=FALSE) {

  set.seed(1)

  if(option < 2) {
    cols <- c("black")
  } else {
    cols <- c("black", "darkorange", "darkblue")
  }

  pushViewport(viewport(width=unit(1, "snpc"), height=unit(1, "snpc"),
                        xscale=c(-1, 1), yscale=c(-1, 1)))

  # add more patches around perimeter. Use circle algorithm to generate spacing
  xy <- pack.circles(iterations=300, r0=0.02, w=0.95, max.size=0.1, g0=c(0.04,0.08),
           min.sep = 0.02)

  for (i in seq_len(ncol(xy))) {
    # variation in maximum tree size across patches
    max.s <- runif(1, 0.05, 0.2)
    # vary average abundance with max tree size
    iterations <- ceiling(500/max.s*xy["r",i]^2)
    draw.patch(x0=unit(xy["x",i], "npc"), y0=unit(xy["y",i], "npc"), w0=unit(xy["r",i], "npc"),
      tree, cols = cols, border = "black",
      iterations=iterations, r0=0.3*max.s, w=1.1, max.size=max.s, draft=FALSE)
  }
  popViewport(1)
}

draw.patch <- function(x0, y0, w0, tree, cols="black", fill = "white", border = fill, xy=NULL, draft=FALSE, ...) {

  if(is.null(xy)) {
     xy <- pack.circles(...)
  }

  pushViewport(viewport(x0, y0, w0, w0, just="left"))
  grid.rect(gp=gpar(col=border, fill=fill))
  pushViewport(viewport(xscale=c(0, 1.2)))
  cols <- sample(cols, ncol(xy), replace=TRUE)
  if (draft)
    grid.circle(unit(xy["x",], "native"), xy["y",], xy["r",])
  else
  for (i in seq_len(ncol(xy)))
    grid.picture(colour.picture(tree, cols[i]),
                 unit(xy["x",i], "native"), xy["y",i],
                 just=c("centre", "centre"), height=xy["r",i] * 2)
  popViewport(2)
}

fig.competition <- function(option) {
  xx <- seq(-4, 4, length=101)
  yy <- dnorm(xx)
  yy <- yy/max(yy)
  lwd <- 4
  par(mar=c(2.5, 2.5, .5, .5))

  plot(xx, yy, type="l", lwd=lwd, xlab="", ylab="", axes=FALSE,
       col=if (option == 1) "black" else "grey")
  box(bty="l")
  abline(v=0, lty=2)
  axis(1, 0, label=TRUE, tick=FALSE, mgp=c(0, 0, 0.5))
  axis(2, 0, label=TRUE, tick=FALSE, mgp=c(0, 0, 0.5), las=1)
  mtext("Trait difference", 1, 1.5)
  mtext("Strength of competition", 2, 1.5)

  if (option == 2) {
    yy2 <- dexp(xx, 2)
    yy2 <- yy2/max(yy2)
    yy2[xx < 0] <- yy[xx < 0]
    lines(yy2 ~ xx, col="black", lwd=lwd)
  } else if (option == 3) {
    yy3 <- dnorm(xx + 1)
    yy3 <- yy3/max(yy3)
    lines(yy3 ~ xx, col="black", lwd=lwd)
  }
}


get.pic <- function(file) {
  base <- tools::file_path_sans_ext(file)
  file.xml <- sprintf("%s.xml", base)
  on.exit({
    file.remove(file.xml)
    file.remove(sprintf("capture%s", basename(file)))
  })
  PostScriptTrace(file, file.xml)
  readPicture(file.xml)
}

colour.picture <- function(picture, col) {
  for (j in seq_along(picture@paths))
    picture@paths[[j]]@rgb <- col
  picture
}

make.transparent <- function(col, opacity=0.5) {
  if (length(opacity) > 1 && any(is.na(opacity))) {
    n <- max(length(col), length(opacity))
    opacity <- rep(opacity, length.out=n)
    col <- rep(col, length.out=n)
    ok <- !is.na(opacity)
    ret <- rep(NA, length(col))
    ret[ok] <- Recall(col[ok], opacity[ok])
    ret
  } else {
    tmp <- col2rgb(col)/255
    rgb(tmp[1,], tmp[2,], tmp[3,], alpha=opacity)
  }
}

aspect.ratio <- function(pic) {
  if (!inherits(pic, "Picture"))
    stop("Object is not a 'Picture'")
  unname(diff(pic@summary@yscale) / diff(pic@summary@xscale))
}

reset <- function() {
  if (interactive()) {
    grid.newpage()
    popViewport(0)
  }
}

cleanup <- function(x) {
  x[is.na(x)] <- 0
  x
}

simple.frame <- function(...)
  grid.segments(c(0, 0), c(0, 0),
                c(0, 1), c(1, 0), default.units="npc", ...)
