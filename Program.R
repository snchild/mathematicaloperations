#goals:
#   Use at least 5 different R datatypes in your program:
#            character, numeric, integer, complex, logical
#   Incorporate at least one loop that works with lists or arrays

#stretch goal:
#   Use the case of coding block

add <- function(val1, val2) {
    return(val1 + val2)
}

sub <- function(val1, val2) {
    return(val1 - val2)
}

mult <- function(val1, val2) {
    return(val1 * val2)
    }

div <- function(val1, val2) {
    return(val1 / val2)
}

exp <- function(val1, val2) {
    return(val1^val2)
}

# prompt the user for pairs of numbers
# check to see if any numbers are complex
# stretch goal: prompt the user for a power function

my_data_frame <- data.frame(
    # perform artithmetic
    # assign values to columns
    Addition = c(add(1, 2), add(2, 3), add(3, 4)),
    Subtraction = c(sub(1, 2), sub(2, 3), sub(3, 4)),
    Multiplication = c(mult(1, 2), mult(2, 3), mult(3, 4)),
    Division = c(div(1, 2), div(2, 3), div(3, 4)),
    Exponential = c(exp(1, 2), exp(2, 3), exp(3, 4))
    )

print(my_data_frame)

#stretch goal: perform derivative