library(devtools)
library(statsr)
library(dplyr)
library(ggplot2)

data(mlb11)

ggplot(mlb11, aes(x=at_bats, y=runs)) +
  geom_point() +    
  geom_smooth(method=lm)

mlb11 %>%
  summarise(cor(runs, at_bats))

plot_ss(x = at_bats, y = runs, data = mlb11)

m1 <- lm(runs ~ at_bats, data = mlb11)
instañlsummary(m1)

lm.homeruns <- lm(runs ~ homeruns, data = mlb11)
summary(lm.homeruns)

ggplot(data = mlb11, aes(x = at_bats, y = runs)) +
  geom_point() +
  stat_smooth(method = "lm", se = FALSE)

y.data <- mlb11 %>%
  filter(at_bats == 5579) %>%
  select(runs)

y.pred <- -2789.2429 + (0.6305 * 5579)

y.data - y.pred

##{r residuals}
ggplot(data = m1, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  xlab("Fitted values") +
  ylab("Residuals")

##{r hist-res}
ggplot(data = m1, aes(x = .resid)) +
  geom_histogram(binwidth = 25) +
  xlab("Residuals")

##{r qq-res}
ggplot(data = m1, aes(sample = .resid)) +
  stat_qq()

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

p1 <- ggplot(data = lm.at_bats, aes(sample = .resid)) +
  ggtitle("lm.at_bats") + 
  stat_qq()

p2 <- ggplot(data = lm.hits, aes(sample = .resid)) +
  ggtitle("lm.hits") + 
  stat_qq()

p3 <- ggplot(data = lm.wins, aes(sample = .resid)) +
  ggtitle("lm.wins") + 
  stat_qq()

p4 <- ggplot(data = lm.bat_avg, aes(sample = .resid)) +
  ggtitle("lm.bat_avg") + 
  stat_qq()



multiplot(p1, p2, p3, p4, cols=2)


##{r runs-vs-new-vars}
lm.new_obs <- lm(runs ~ new_obs, data = mlb11)
lm.new_slug <- lm(runs ~ new_slug, data = mlb11)
lm.new_onbase <- lm(runs ~ new_onbase, data = mlb11)

summary(lm.new_obs)
summary(lm.new_slug)
summary(lm.new_onbase)

lm.new_onbase$coefficients
p1 <- ggplot(data = lm.new_obs, aes(sample = .resid)) +
  ggtitle("lm.new_obs") + 
  stat_qq()

p2 <- ggplot(data = lm.new_slug, aes(sample = .resid)) +
  ggtitle("lm.new_slug") + 
  stat_qq()

p3 <- ggplot(data = lm.new_onbase, aes(sample = .resid)) +
  ggtitle("lm.new_onbase") + 
  stat_qq()


multiplot(p1, p2, p3, cols=2)