#http://www.gapminder.org/data/

# The Gapminder website contains over 500 data sets with information about
# the world's population. Your task is to download a data set of your choice
# and create 2-5 plots that make use of the techniques from Lesson 3.

# You might use a simple histogram, a boxplot split over a categorical variable,
# or a frequency polygon. The choice is yours!

# You can find a link to the Gapminder website in the Instructor Notes.

# Once you've completed your investigation, create a post in the discussions that includes:
#       1. any questions you answered, your observations, and summary statistics
#       2. snippets of code that created the plots
#       3. links to the images of your plots

# You can save images by using the ggsave() command.
# ggsave() will save the last plot created.
# For example...
#                  qplot(x = price, data = diamonds)
#                  ggsave('priceHistogram.png')

# ggsave currently recognises the extensions eps/ps, tex (pictex),
# pdf, jpeg, tiff, png, bmp, svg and wmf (windows only).

# Copy and paste all of the code that you used for
# your investigation, and submit it when you are ready.
# ====================================================================================

library(ggplot2)
library(gridExtra)
library(XLConnect)

#Average age of dollar billionaires (years) - Forbes
#df <- readWorksheet(loadWorkbook("Indicator_Average age.xlsx"),sheet=1)
df <- read.csv("Indicator_Average age.csv")
df <- df[,1:5] #subset the first 5 columns which actually have data
df <- subset(df,X2004 > 0|X2005 > 0|X2006 > 0|X2007 > 0) #only look at countries with at least 1 billionaire in at least 1 year
#must have added x as a variable name cannot start with a number
df2004 <- subset(df, X2004 > 0)
df2004 <- df2004[,1:2]
df2005 <- subset(df, X2005 > 0)
df2005 <- df2005[,c(1,3)]
df2006 <- subset(df, X2006 > 0)
df2006 <- df2006[,c(1,4)]
df2007 <- subset(df, X2007 > 0)
df2007 <- df2007[,c(1,5)]

t <- mean(df2004$X2004)

num_countries_billionaires = c(nrow(df2004),nrow(df2005),nrow(df2006),nrow(df2007))
average_age = c(mean(df2004$X2004),mean(df2005$X2005),mean(df2006$X2006),mean(df2007$X2007))
years = c("2004","2005","2006","2007") 

df = data.frame(years, num_countries_billionaires, average_age)       # df is a data frame

library(ggplot2)
g1 <- ggplot(aes(x=years, y = average_age), data = df ) +
  geom_point(stat = "identity", aes(colour = years), size = 10) +
  scale_y_continuous(limits=c(60,65), breaks = c(60:65)) + 
  xlab("Year") +
  ylab("Average Age of Billionaires (Years)") +
  ggtitle("Average Age of Billionaires Across the World") +
  theme(legend.position="none", panel.grid.major = element_line(colour = "black"), text = element_text(size=20))
g1 
g2 <- ggplot(aes(x=years, y = num_countries_billionaires), data = df ) +
  geom_point(stat = "identity", aes(colour = years), size = 10) +
  scale_y_continuous(limits=c(40,55), breaks = c(40:55)) + 
  xlab("Year") +
  ylab("Number of Countries with Billionaires") +
  ggtitle("Total Number of Countries with Billionaires") +
  theme(panel.grid.major = element_line(colour = "black"),text = element_text(size=20))
g2
grid.arrange(g1,g2,ncol=2)
ggsave(file="billionaires_progress.png")