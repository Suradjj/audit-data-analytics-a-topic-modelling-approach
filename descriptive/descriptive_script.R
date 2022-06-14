#The Stargazer package was used to generate a table with descriptive statistics
library(stargazer)

#The relevant dataframe was constructed.
df <- data.frame(titles,pages)
#Removing the row names
rownames(df) <- NULL

#The descriptive statistics were calculated and visualized in a table with the stargazer package.
stargazer(df, type="text")

#The total number of pages was calculated.
sum(df$pages)
