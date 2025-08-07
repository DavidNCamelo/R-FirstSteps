# Import data
library(statsr)
data(mlb11)

ggplot2::ggplot(mlb11, ggplot2::aes(x=at_bats, y=runs)) +
  ggplot2::geom_point() +    
  ggplot2::geom_smooth(method=lm)

mlb11 |>
  dplyr::summarise(stats::cor(runs, at_bats))

plot_ss(x = at_bats, y = runs, data = mlb11)

m1 <- lm(runs ~ at_bats, data = mlb11)
summary(m1)

lm.homeruns <- lm(runs ~ homeruns, data = mlb11)
summary(lm.homeruns)

ggplot2::ggplot(data = mlb11, ggplot2::aes(x = at_bats, y = runs)) +
  ggplot2::geom_point() +
  ggplot2::stat_smooth(method = "lm", se = FALSE)

y.data <- mlb11 |>
  dplyr::filter(at_bats == 5579) |>
  dplyr::select(runs)

y.pred <- -2789.2429 + (0.6305 * 5579)

y.data - y.pred

##{r residuals}
ggplot2::ggplot(data = m1, ggplot2::aes(x = .fitted, y = .resid)) +
  ggplot2::geom_point() +
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
  ggplot2::xlab("Fitted values") +
  ggplot2::ylab("Residuals")

##{r hist-res}
ggplot2::ggplot(data = m1, ggplot2::aes(x = .resid)) +
  ggplot2::geom_histogram(binwidth = 25) +
  ggplot2::xlab("Residuals")

##{r qq-res}
ggplot2::ggplot(data = m1, ggplot2::aes(sample = .resid)) +
  ggplot2::stat_qq()

lm.at_bats <- lm(runs ~ at_bats, data = mlb11)
lm.hits <- lm(runs ~ hits, data = mlb11)
lm.wins <- lm(runs ~ wins, data = mlb11)
lm.bat_avg <- lm(runs ~ bat_avg, data = mlb11)

summary(lm.at_bats)
summary(lm.hits)
summary(lm.wins)
summary(lm.bat_avg)


multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

p1 <- ggplot2::ggplot(data = lm.at_bats, ggplot2::aes(sample = .resid)) +
  ggplot2::ggtitle("lm.at_bats") + 
  ggplot2::stat_qq()

p2 <- ggplot2::ggplot(data = lm.hits, ggplot2::aes(sample = .resid)) +
  ggplot2::ggtitle("lm.hits") + 
  ggplot2::stat_qq()

p3 <- ggplot2::ggplot(data = lm.wins, ggplot2::aes(sample = .resid)) +
  ggplot2::ggtitle("lm.wins") + 
  ggplot2::stat_qq()

p4 <- ggplot2::ggplot(data = lm.bat_avg, ggplot2::aes(sample = .resid)) +
  ggplot2::ggtitle("lm.bat_avg") + 
  ggplot2::stat_qq()



multiplot(p1, p2, p3, p4, cols=2)


##{r runs-vs-new-vars}
lm.new_obs <- lm(runs ~ new_obs, data = mlb11)
lm.new_slug <- lm(runs ~ new_slug, data = mlb11)
lm.new_onbase <- lm(runs ~ new_onbase, data = mlb11)

summary(lm.new_obs)
summary(lm.new_slug)
summary(lm.new_onbase)

lm.new_onbase$coefficients
p1 <- ggplot2::ggplot(data = lm.new_obs, ggplot2::aes(sample = .resid)) +
  ggplot2::ggtitle("lm.new_obs") + 
  ggplot2::stat_qq()

p2 <- ggplot2::ggplot(data = lm.new_slug, ggplot2::aes(sample = .resid)) +
  ggplot2::ggtitle("lm.new_slug") + 
  ggplot2::stat_qq()

p3 <- ggplot2::ggplot(data = lm.new_onbase, ggplot2::aes(sample = .resid)) +
  ggplot2::ggtitle("lm.new_onbase") + 
  ggplot2::stat_qq()


multiplot(p1, p2, p3, cols=2)