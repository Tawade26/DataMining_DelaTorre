nums <- c(2, 4, 5, 8)

if (all(nums %% 2 == 0)) {
  print("All numbers are even")
}
if (any(nums %% 2 == 0)) {
  print("At least one number is even")
}
