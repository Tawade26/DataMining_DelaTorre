grade <- "A"

result <- switch(grade,
                 "A" = "Excellent",
                 "B" = "Good",
                 "C" = "Average",
                 "D" = "Poor",
                 "F" = "Failure",
                 "Invalid grade"
)
print(result)