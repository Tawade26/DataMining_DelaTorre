color <- "red"

result = switch(color,
                "red" = "Stop",
                "yellow" = "Ready",
                "green" = "Go",
                "Invalid signal"
)
print(result)