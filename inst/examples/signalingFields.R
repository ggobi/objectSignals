Brush.gen <- setRefClass("Brush",
                       fields = signalingFields(list(color = "character",
                          age = "numeric"), signalName = "brushChanged"))
brush <- Brush.gen$new(color = "red", age = 2)
brush$brushChanged$connect(function(x){print(paste(x,
"changed, so emit any-changed signal"))})
brush$colorChanged$connect(function() print("colorChanged signal emited"))
brush$age <- 3
brush$color <- "blue"
