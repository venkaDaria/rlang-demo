# Task 3-2-8
funs <- c("print","summary","plot")
meths <- lapply(funs, methods)
grepl("matrix", meths)
grepl("function", meths)
grepl("default", meths)