##################################################
# RM01 for Mphil in Environmental Policy
# Is there a Housing Wealth Effect on Energy Consumption?
# @ University of Cambridge
# Lucas Paoli, lp485
##################################################
get.pam.cluster <- function(dist, data.correct, names){
  
  # Identify best k
  k.df <- c(NA)
  for(i in 2:10){
    k.pam <- pam(dist,diss = TRUE,k = i)
    k.df[i] <- k.pam$silinfo$avg.width
    
  }
  
  clust_fit <- pam(dist, diss = TRUE, k = which(k.df==max(k.df, na.rm=T)))
  
  tsne_obj <- Rtsne(dist, is_distance = TRUE)
  tsne_data <- tsne_obj$Y %>%
    data.frame() %>%
    setNames(c("X", "Y")) %>%
    mutate(cluster = factor(clust_fit$clustering),
           name = row.names(data.correct))
  
  print(ggplot(aes(x = X, y = Y), data = tsne_data) +
    geom_point(aes(color = cluster)) + theme_grey())
  
  pam_results <- data.correct[, names(data.correct) %in% names] %>%
    mutate(cluster = clust_fit$clustering) %>%
    group_by(cluster) %>%
    do(the_summary = summary(.))
  
  print(pam_results$the_summary)
  return(clust_fit$clustering)
}

pca_ggplot <- function (pcobj, 
                        choices = 1:2, 
                        scale = 1,
                        groups = NULL,
                        shape = NULL,
                        labels = NULL, 
                        labels.size = 3, 
                        point.size = 3,
                        obs.scale = 1 - scale, 
                        var.scale = scale, # var : arrows
                        var.axes = TRUE,
                        var.lim = 0,
                        var.col = muted("red"),
                        varname.col = "darkred",
                        varname.size = 3, 
                        varname.adjust = 1.5, 
                        varname.abbrev = FALSE,
                        polygon.groups = NULL,
                        polygon.fill = NULL,
                        polygon.alpha = .3,
                        ellipse = FALSE, 
                        ellipse.prob = 0.68, 
                        alpha = 1,
                        circle = FALSE, 
                        circle.prob = 0.69, 
                        pc.biplot = TRUE, 
                        ...) 
{
  library(ggplot2)
  library(plyr)
  library(scales)
  library(grid)
  stopifnot(length(choices) == 2)
  if (inherits(pcobj, "prcomp")) {
    nobs.factor <- sqrt(nrow(pcobj$x) - 1)
    d <- pcobj$sdev
    u <- sweep(pcobj$x, 2, 1/(d * nobs.factor), FUN = "*")
    v <- pcobj$rotation
  } else if (inherits(pcobj, "princomp")) {
    nobs.factor <- sqrt(pcobj$n.obs)
    d <- pcobj$sdev
    u <- sweep(pcobj$scores, 2, 1/(d * nobs.factor), FUN = "*")
    v <- pcobj$loadings
  } else if (inherits(pcobj, "PCA")) {
    nobs.factor <- sqrt(nrow(pcobj$call$X))
    d <- unlist(sqrt(pcobj$eig)[1])
    u <- sweep(pcobj$ind$coord, 2, 1/(d * nobs.factor), FUN = "*")
    v <- sweep(pcobj$var$coord, 2, sqrt(pcobj$eig[1:ncol(pcobj$var$coord), 
                                                  1]), FUN = "/")
  } else if (inherits(pcobj, "lda")) {
    nobs.factor <- sqrt(pcobj$N)
    d <- pcobj$svd
    u <- predict(pcobj)$x/nobs.factor
    v <- pcobj$scaling
    d.total <- sum(d^2)
  } else {
    stop("Expected a object of class prcomp, princomp, PCA, or lda")
  }
  
  choices <- pmin(choices, ncol(u))
  df.u <- as.data.frame(sweep(u[, choices], 2, d[choices]^obs.scale, FUN = "*"))
  v <- sweep(v, 2, d^var.scale, FUN = "*")
  df.v <- as.data.frame(v[, choices])
  names(df.u) <- c("xvar", "yvar")
  names(df.v) <- names(df.u)
  
  if (pc.biplot) {
    df.u <- df.u * nobs.factor
  }
  
  r <- sqrt(qchisq(circle.prob, df = 2)) * prod(colMeans(df.u^2))^(1/4)
  v.scale <- rowSums(v^2)
  df.v <- r * df.v/sqrt(max(v.scale))
  
  
  if (obs.scale == 0) {
    u.axis.labs <- paste("Standardized PC", choices, sep = "")
  }
  else {
    u.axis.labs <- paste("PC", choices, sep = "")
  }
  u.axis.labs <- paste(u.axis.labs, sprintf("(%0.1f%% explained var.)", 
                                            100 * pcobj$sdev[choices]^2/sum(pcobj$sdev^2)))
  # Adds labels and groups if needed
  if (!is.null(labels)) {
    df.u$labels <- labels
  }
  if (!is.null(groups)) {
    df.u$groups <- groups
  }
  # Reduce variable names
  if (varname.abbrev) {
    df.v$varname <- abbreviate(rownames(v))
  }
  else {
    df.v$varname <- rownames(v)
  }
  # Positional parameters for varnames
  df.v$angle <- with(df.v, (180/pi) * atan(yvar/xvar))
  df.v$hjust = with(df.v, (1 - varname.adjust * sign(xvar))/2)
  
  # Initialize plot
  g <- ggplot(data = df.u, aes(x = xvar, y = yvar)) + 
    xlab(u.axis.labs[1]) + 
    ylab(u.axis.labs[2]) + 
    coord_equal()
  
  # Circle
  if (circle) { # Add circle ?
    theta <- c(seq(-pi, pi, length = 50), seq(pi, -pi, length = 50))
    circle <- data.frame(xvar = r * cos(theta), yvar = r * sin(theta))
    # Add circle to plot
    g <- g + geom_path(data = circle, color = muted("white"), 
                       size = 1/2, alpha = 1/3)
  }
  
  # Polygon
  if (!is.null(df.u$groups) && !is.null(polygon.groups)) {
    df.u$polygon.groups = polygon.groups
    if (is.null(polygon.fill)) {
      df.u$polygon.fill = polygon.groups
    }
    g <- g + geom_polygon(aes(col = groups, fill = groups, group = polygon.groups), alpha = polygon.alpha)
  }
  
  # Labels / Points
  if (!is.null(df.u$labels)) {
    if (!is.null(df.u$groups)) {
      g <- g + geom_text(aes(label = labels, color = groups), size = labels.size)
    }
    else {
      g <- g + geom_text(aes(label = labels), size = labels.size)
    }
  }
  else {
    if (!is.null(df.u$groups)) {
      g <- g + geom_point(aes(color = groups, shape = shape), alpha = alpha, size = point.size)
    }
    else {
      g <- g + geom_point(aes(shape = shape), alpha = alpha, size = point.size)
    }
  }
  
  # Ellipse
  if (!is.null(df.u$groups) && ellipse) {
    theta <- c(seq(-pi, pi, length = 50), seq(pi, -pi, length = 50))
    circle <- cbind(cos(theta), sin(theta))
    ell <- ddply(df.u, "groups", function(x) {
      if (nrow(x) <= 2) {
        return(NULL)
      }
      sigma <- var(cbind(x$xvar, x$yvar))
      mu <- c(mean(x$xvar), mean(x$yvar))
      ed <- sqrt(qchisq(ellipse.prob, df = 2))
      data.frame(sweep(circle %*% chol(sigma) * ed, 2, mu, FUN = "+"), groups = x$groups[1])
    })
    names(ell)[1:2] <- c("xvar", "yvar")
    g <- g + geom_path(data = ell, aes(color = groups, group = groups))
  }
  
  # Arrows
  if (var.axes) {
    # Filter arrows
    df.v = df.v[sqrt(df.v$xvar^2+df.v$yvar^2)>var.lim,]
    # Add arrows to plot
    g <- g + geom_segment(data = df.v, aes(x = 0, y = 0, 
                                           xend = xvar, yend = yvar), 
                          arrow = arrow(length = unit(1/2, "picas")), 
                          color = var.col)
  }
  
  # Arrows labels
  if (var.axes) { # Add arrows labels ?
    g <- g + geom_text(data = df.v, aes(label = varname, 
                                        x = xvar, y = yvar, angle = angle, hjust = hjust), 
                       color = varname.col, size = varname.size)
  }
  return(g)
}