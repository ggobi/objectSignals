Brush.gen <- setRefClass("Brush",
fields = signalingField("color", "character"))
brush <- Brush.gen$new(color = "blue")
brush$colorChanged$connect(function() print(brush$color))
brush$color <- "red"
