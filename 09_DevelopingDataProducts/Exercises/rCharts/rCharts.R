require(rCharts)
haireye = as.data.frame(HairEyeColor)
n1 <- nPlot(Freq ~ Hair, group = 'Eye', type = 'multiBarChart',
            data = subset(haireye, Sex == 'Male')
)
n1$save('fig/n1.html', cdn = TRUE)
cat('<iframe src="fig/n1.html" width=100%, height=600></iframe>')

## Example 1 Facetted Scatterplot
names(iris) = gsub("\\.", "", names(iris))
r1 <- rPlot(SepalLength ~ SepalWidth | Species, data = iris, color = 'Species', type = 'point')
r1$save('fig/r1.html', cdn = TRUE)
cat('<iframe src="fig/r1.html" width=100%, height=600></iframe>')

## Multiple bar chart
hair_eye = as.data.frame(HairEyeColor)
r2 <- rPlot(Freq ~ Hair | Eye, color = 'Eye', data = hair_eye, type = 'bar')
r2$save('fig/r2.html', cdn = TRUE)
cat('<iframe src="fig/r2.html" width=100%, height=600></iframe>')

# mtcars : publishong
r3 <- rPlot(mpg ~ wt | am + vs, data = mtcars, type = "point", color = "gear")
r3$print("chart1") # print out the js 
r3$save('myPlot.html') #save as html file
r3$publish('myPlot', host = 'gist') # save to gist, rjson required
r3$publish('myPlot', host = 'rpubs') # save to rpubs
