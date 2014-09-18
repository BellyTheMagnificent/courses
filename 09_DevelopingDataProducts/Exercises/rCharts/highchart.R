h1 <- hPlot(x = "Wr.Hnd", y = "NW.Hnd", data = MASS::survey, type = c("line", 
                                                                      "bubble", "scatter"), group = "Clap", size = "Age")
h1$save('fig/h1.html', cdn = TRUE)
cat('<iframe src="fig/h1.html" width=100%, height=600></iframe>')