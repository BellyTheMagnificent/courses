suppressPackageStartupMessages(library(googleVis))
G <- gvisGeoChart(Exports, locationvar = "Country", colorvar = "Profit", options = list(width = 600, height = 400))
## HTML Output
print(G, "chart")
plot(G)

G2 <- gvisGeoChart(Exports, locationvar = "Country", colorvar = "Profit", options = list(width = 600,   height = 400, region = "150"))
print(G2, "chart")

##https://developers.google.com/chart/interactive/docs/gallery/geochart
