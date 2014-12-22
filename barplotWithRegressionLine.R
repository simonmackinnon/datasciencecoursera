# set this directory to working directory
this.dir <- dirname(parent.frame(2)$ofile) 
setwd(this.dir) 

year <- c(1999, 2002, 2005, 2008)
vals <- c(1000, 1100, 1300, 1200)

df <- cbind(year, vals)

print(df)

png(filename = "barplot.png")

mp <- barplot(df[,"vals"], names.arg = df[,"year"])

dev.off()


print(mp)

png(filename = "barplotRegLine.png")

mp <- barplot(df[,"vals"], names.arg = df[,"year"])
abline(lm(df[,"vals"] ~ mp))

dev.off()

png(filename = "barplotRegLineLabel.png")

mp <- barplot(df[,"vals"], names.arg = df[,"year"])
abline(lm(df[,"vals"] ~ mp))
regEq <- lm(df[,"vals"] ~ df[,"year"])

text(x = 1.9,
     y = 1200,  
     labels = c(paste ("y = ", as.character(round(regEq$coefficients[2], digits = 2)), "x ", 
                       "+ ", as.character(round(regEq$coefficients[1]), digits = 2), 
                       sep = "")),
     col = "red")

dev.off()