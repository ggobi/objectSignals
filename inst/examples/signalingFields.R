Brush.gen <- setRefClass("Brush",
                       fields = signalingFields(list(color = "character",
                          size = "numeric"), signalName = "brushChanged"))
brush <- Brush.gen$new(color = "red", size = 2)
brush$brushChanged$connect(function(x){print(paste(x,
"changed, Global signal emited"))})
brush$colorChanged$connect(function() print("color-changed individual signal emited"))
brush$size <- 3
brush$color <- "blue"
