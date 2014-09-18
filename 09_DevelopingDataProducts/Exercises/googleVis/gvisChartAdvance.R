df <- data.frame(label = c("US", "GB", "BR"), val1 = c(1, 3, 4), val2 = c(23, 12, 32))
Line <- gvisLineChart(df, xvar = "label", yvar = c("val1", "val2"), options = list(title = "Hello World", 
                                                                                   legend = "bottom", titleTextStyle = "{color:'red', fontSize:18}", vAxis = "{gridlines:{color:'red', count:3}}", 
                                                                                   hAxis = "{title:'My Label', titleTextStyle:{color:'blue'}}", series = "[{color:'green', targetAxisIndex: 0}, \n                         {color: 'blue',targetAxisIndex:1}]", 
                                                                                   vAxes = "[{title:'Value 1 (%)', format:'##,######%'}, \n                                  {title:'Value 2 (??)'}]", 
                                                                                   curveType = "function", width = 500, height = 300))

plot(Line)


## Merging
G <- gvisGeoChart(Exports, "Country", "Profit", options = list(width = 200,  height = 100))
plot(G)
T1 <- gvisTable(Exports, options = list(width = 200, height = 270))
M <- gvisMotionChart(Fruits, "Fruit", "Year", options = list(width = 400, height = 370))
GT <- gvisMerge(G, T1, horizontal = FALSE)
plot(GT)
GTM <- gvisMerge(GT, M, horizontal = TRUE, tableOptions = "bgcolor=\"#CCCCCC\" cellspacing=10")
plot(GTM)
