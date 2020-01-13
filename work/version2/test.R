
x <- read.table("../../data/linkexample1.txt", header = TRUE)
y <- read.table("../../data/linkexample2.txt", header = TRUE)




library(data.table)

files <- list.files("R", "*.R", full.names = TRUE)
for (file in files) source(file)

pair_blocking(x, y, blocking_var = "postcode")
