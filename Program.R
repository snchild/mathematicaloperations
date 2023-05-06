# mathematical operations
# note that order matters for sub, div, and exp
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
    
    if (is.na(suppressWarnings(as.complex(value)))) {
        print("At least one of the inputs is not a number.")
        print("Please enter complex numbers or real numbers only.")
        converted_value <- FALSE
    } else if (as.character(as.complex(value)) == value) {
        converted_value <- as.complex(value)
    } else if (as.character(as.numeric(value)) == value) {
       converted_value <- as.numeric(value)
    } else if (as.character(as.integer(value)) == value) {
        converted_value <- as.integer(value)
    }

    # a case_when function cannot be used here because of the documentation
    #   which says that all right hand sides of the function have to be the
    #   same type of vector. That means that I cannot use a case_when function
    #   to convert the value into different types of vectors.

    return(converted_value)
}

# create all the lists
input_list <- list()
add_list <- list()
sub_list <- list()
mult_list <- list()
div_list <- list()
exp_list <- list()

print("Please enter a pair of numbers, one at a time.")
print("If using complex numbers, use the format a+bi without any spaces.")
print("When you are done, press enter instead of entering numbers.")

while (TRUE) {
    # prompt the user for pairs of numbers
    i_1 <- readline("\n>> ")
    i_2 <- readline(">> ")

    if (i_1 == "" || i_2 == "") {
        break
    }

    # convert data types
    v_1 <- data_converstion(i_1)
    v_2 <- data_converstion(i_2)

    # as long as the inputs are numbers...
    if (v_1 != FALSE && v_2 != FALSE) {
        # ... append the results of the operations to the corresponding lists
        input_list <- append(input_list, paste(c(i_1, i_2),
                        sep = ", ", collapse = ", "))
        add_list <- append(add_list, add(v_1, v_2))
        sub_list <- append(sub_list, sub(v_1, v_2))
        mult_list <- append(mult_list, mult(v_1, v_2))
        div_list <- append(div_list, div(v_1, v_2))
        exp_list <- append(exp_list, exp(v_1, v_2))
    }

}

# create nested list that includes each operation list
my_nested_list <- list(Inputs = input_list,
    Addition = add_list,
    Subtraction = sub_list,
    Multiplication = mult_list,
    Division = div_list,
    Exponential = exp_list)

# convert nested list to the columns of a dataframe
# the syntax for this line of code is originally from Sparks By Examples
my_data_frame <- as.data.frame(do.call(cbind, my_nested_list))

print("The results of the mathematical operations are as follows:")

print(my_data_frame)