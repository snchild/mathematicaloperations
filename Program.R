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
data_converstion <- function(value) {
    # if converting the type leaves the actual value unchanged,
    # it's safe to assume it should be that data type

    if (as.character(as.complex(value)) == value) {
        converted_value <- as.complex(value)
    } else if (as.character(as.numeric(value)) == value) {
       converted_value <- as.numeric(value)
    } else {
        converted_value <- as.integer(value)
    }

    return(converted_value)
}

# create all the lists
input_list <- list()
add_list <- list()
sub_list <- list()
mult_list <- list()
div_list <- list()
exp_list <- list()

keep_prompting <- TRUE
print("Please enter a pair of numbers, separated by line.")
print("When you are done, press enter instead of entering numbers.")

while (keep_prompting) {
    # prompt the user for pairs of numbers
    # starting with just one pair - later add a loop
    i_1 <- readline("\n>> ")
    i_2 <- readline(">> ")

    if (i_1 == "") {
        keep_prompting <- FALSE
        break
    }

    # convert data types
    v_1 <- data_converstion(i_1)
    v_2 <- data_converstion(i_2)

    # stretch goal: prompt the user for a power function
    input_list <- append(input_list, paste(c(i_1, i_2),
                        sep = ", ", collapse = ", "))
    add_list <- append(add_list, add(v_1, v_2))
    sub_list <- append(sub_list, sub(v_1, v_2))
    mult_list <- append(mult_list, mult(v_1, v_2))
    div_list <- append(div_list, div(v_1, v_2))
    exp_list <- append(exp_list, exp(v_1, v_2))
}


my_nested_list <- list(Inputs = input_list,
    Addition = add_list,
    Subtraction = sub_list,
    Multiplication = mult_list,
    Division = div_list,
    Exponential = exp_list)

# Convert nested list to the dataframe by columns
# This line of code is originally from Sparks By Examples
my_data_frame <- as.data.frame(do.call(cbind, my_nested_list))

print(my_data_frame)

#stretch goal: perform derivative