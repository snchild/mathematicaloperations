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

    # a case_when function cannot be used here because of the documentation
    #   which says that all RHS have to be the same type of vector. That
    #   means that I cannot use a case_when function to convert the value
    #   into different types of vectors.

    return(converted_value)
}
read_equation <- function(my_equation, my_variable) {
    my_coefficients <- list()
    my_powers <- list()
    current_object <- list()
    current_type <- "c" #might not use
    #loop through each character in equation
    for (i in 1:nchar(my_equation)){
        char <- substr(my_equation, i, i)
        #come up with a different way to check if it's an int
        if (!is.na(as.integer(char)) || char == ".") {
            current_object <- append(current_object, char)
        } else if (char == my_variable) {
            if (length(current_object) == 0) {
                my_coefficients <- append(my_coefficients, "1")
            } else {
                my_coefficients <- append(my_coefficients,
                                        paste(current_object, collapse = ""))
                current_object <- list()
            }
        } else if (char == "+" || char == "-") {
            if (length(current_object) == 0) {
                my_powers <- append(my_powers, "1")
            } else {
                my_powers <- append(my_powers,
                                        paste(current_object, collapse = ""))
                current_object <- list()
            }
        }
        #how to addess constants in middle?
    }

    if (length(current_object) != 0 && current_type == "c") {
        my_coefficients <- append(my_coefficients,
                                    paste(current_object, collapse = ""))
        my_powers <- append(my_powers, "0")
    } else if (length(current_object) != 0 && current_type == "p") {
        my_powers <- append(my_powers,
                                    paste(current_object, collapse = ""))
    }
    return(list(my_coefficients, my_powers))
}
perform_derivative <- function(my_coefficients, my_powers) {
    #loop though an index
    #double check that neither the coefficient nor the power is 0
    #make the new coefficient into coeff*power
    # make the power into power - 1
    return() #list of new coefficients and list of new powers
}

# create all the lists
input_list <- list()
add_list <- list()
sub_list <- list()
mult_list <- list()
div_list <- list()
exp_list <- list()

keep_prompting <- TRUE
print("Please enter a pair of numbers, with each number on separate lines.")
print("If using complex numbers, use the format a+bi.")
print("When you are done, press enter instead of entering numbers.")

while (keep_prompting) {
    # prompt the user for pairs of numbers
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

# prompt the user for a power series
print("Enter a power series, using only one variable. Use the ^ symbol to denote exponents.")
my_eq <- readline("\n>> ")
print("Enter the symbol that you used for your variable.")
my_var <- readline("\n>> ")

# converts the input to coefficients and powers
# the notation %<-% supposedly lets me assign multiple things at once
coeff_and_pow <- read_equation(my_eq, my_var)
coeff_list <- coeff_and_pow[1]
powers_list <- coeff_and_pow[2]
print("coefficients: ")
print(coeff_list)
print("powers: ")
print(powers_list)
# performs derivative
# displays the derivative

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