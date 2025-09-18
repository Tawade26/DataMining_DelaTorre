a <- 10
b <- 2
operation <- "1"

result <- switch(operation,
                 "1" = a + b,
                 "2" = a - b,
                 "3" = a * b, 
                 "4" = a / b,
                 "Invalid value"
)
print(result)