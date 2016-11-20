# Charts for one variable
require("datasets")

?plot

# Bar plots
?chickwts
chickwts
data(chickwts)
plot(chickwts$feed)
# data needs to be prepared into a summary table for advanced plots
feeds <- table(chickwts$feed)
feeds
barplot(feeds)
?barplot
barplot(feeds[order(feeds, decreasing = TRUE)])
# changing plotting parameters for all upcoming plots
?par
par(oma = c(1, 1, 1, 1)) # outside margins: bottom, left, top, right
par(mar = c(4, 5, 2, 1)) # plot margins
barplot(feeds[order(feeds)],
        horiz = TRUE,
        las = 1, # orientation of axis labels
        col = c("beige", "blanchedalmond"), 
        border = NA, # borders on the bars
        main = "This is \n Ze Title", 
        xlab = "X Axix Label"
)
#----------------------------------------------------------------------------



# Pie charts
data(chickwts)
feeds <- table(chickwts$feed)
feeds
pie(feeds)
?pie
# AVOID USING PIE CHARTS BECAUSE THEY INDUCE ERRORS
#----------------------------------------------------------------------------




# Histograms
?lynx
data(lynx)
hist(lynx)
h <- hist(lynx, 
          breaks = 11,
          freq = FALSE, 
          col = "thistle1", 
          main = "Zis iz a title", 
          xlab = "Zis iz a label"
          )
?curve

#----------------------------------------------------------------------------




# Box Plots
?USJudgeRatings
data(USJudgeRatings)
boxplot(USJudgeRatings$RTEN)
?boxplot
boxplot(USJudgeRatings, 
        horizontal = TRUE,
        xlab = "Walo", 
        las = 1, 
        notch = TRUE,
        ylim = c(0, 10), 
        boxwex = 0.5 # width of box as proportion of original
        whisklty = 1, # Whisker line type; 1 = solid line
        staplelty = 0, # Staple (line at end) type; 0 = none
        outpch = 16 # Symbols for outliers; 16 = filled circle
)
# call it again with $RTEN
#----------------------------------------------------------------------------




# Overlaying plots
# use parameter: add = TRUE
#----------------------------------------------------------------------------


# Saving images: export from R-Studio or method below
# Run the code for all steps at once
# 1. open device
png(filename = "./Images/graph.png", width = 640, height = 480) # pixel-based
pdf(filename = "./Images/graph.pdf", width = 9, height = 6) # vector-based 
# 2. plot graph
# 3. close device
dev.off()
#----------------------------------------------------------------------------










