# R Style Guide{- #RStyleGuide}

In order to make the code in this book as clear, precise, and easy to read as possible, we have created a "Style Guide" for code based in part on the [tidyverse R style guide](https://style.tidyverse.org/) and [Google's R style guide](https://google.github.io/styleguide/Rguide.html) with a few additional clarifications and modifications. We generally follow this style guide as closely as possible throughout the project with the exception of a few areas where we are explicitly demonstrating a feature or call that violates this.

***
## Naming Conventions{-}

### File names{-}

Script file names should be concise, meaningful and end in **.R**.


```r
# Good
 fit_models.R
 utility_functions.R

# Bad
 foo.r
 stuff.r

```

### Object names{-}

Object and function names should only include lowercase numbers and use underscore (_) to separate words (snake_case).  Where possible, **object** names should be **nouns** and **function** names should be **verbs**. Keep object and function names as concise as possible while still remaining descriptive.


```r
# Good
day_one
day_1

# Bad
first_day_of_the_month
dayone
```


## Syntax{-}

### Spacing {-}

Place spaces around all infix operators (=, +, -, <-, etc.). The same rule applies when using = in function calls. Always put a space after a comma, and never before (just like in regular English).


```r
# Good
average <- mean(feet / 12 + inches, na.rm = TRUE)

# Bad
average<-mean(feet/12+inches,na.rm=TRUE)

```

There's a small exception to this rule: :, :: and ::: don't need spaces around them.


```r
# Good
x <- 1:10
base::get

# Bad
x <- 1 : 10
base :: get

```

Place a space before left parentheses, except in a function call.


```r
# Good
if (debug) do(x)
plot(x, y)

# Bad
if(debug)do(x)
plot (x, y)

```

Extra spacing (i.e., more than one space in a row) is ok if it improves alignment of equal signs or assignments (<-).


```r
list(
  total = a + b + c, 
  mean  = (a + b + c) / n
)
```

Do not place spaces around code in parentheses or square brackets (unless there's a comma, in which case see above).


```r
# Good
if (debug) do(x)
diamonds[5, ]

# Bad
if ( debug ) do(x)  # No spaces around debug
x[1,]   # Needs a space after the comma
x[1 ,]  # Space goes after comma not before
```

###Curly braces

An opening curly brace should never go on its own line and should always be followed by a new line. A closing curly brace should always go on its own line, unless it's followed by else.

Always indent the code inside curly braces.


```r

# Good

if (y < 0 && debug) {
  message("Y is negative")
}

if (y == 0) {
  log(x)
} else {
  y ^ x
}

# Bad

if (y < 0 && debug)
message("Y is negative")

if (y == 0) {
  log(x)
} 
  else {
  y ^ x
}

```

It's ok to leave very short statements on the same line:


```r
if (y < 0 && debug) message("Y is negative")
```

###Line length

Strive to limit your code to **80 characters per line**. This fits comfortably on a printed page with a reasonably sized font. If you find yourself running out of room, this is a good indication that you should encapsulate some of the work in a separate function.

###Indentation

When indenting your code, use **two spaces**. Never use tabs or mix tabs and spaces.

The only exception is if a function definition runs over multiple lines. In that case, indent the second line to where the definition starts:


```r

long_function_name <- function(a = "a long argument", 
                               b = "another argument",
                               c = "another long argument") {
  # As usual code is indented by two spaces.
}

```

###Assignment

Use <-, not =, for assignment.


```r
# Good
x <- 5
# Bad
x = 5

```

***
##Organisation
***
###Commenting guidelines

Comment your code. Each line of a comment should begin with the comment symbol and a single space: #. **Comments should explain the why, not the what**. Short comments can be placed after code preceded by two spaces, #, and then one space.

Use commented lines of - and = to break up your file into easily readable chunks.


```r

# Load data ---------------------------

# Plot data ---------------------------
```

###Function Definitions and Calls

Function definitions should first list arguments without default values, followed by those with default values.

In both function definitions and function calls, multiple arguments per line are allowed; line breaks are only allowed between assignments. 


```r

# Good
PredictCTR <- function(query, property, num.days,
                       show.plot = TRUE)
# Bad
PredictCTR <- function(query, property, num.days, show.plot =
                       TRUE)
```

###Function Documentation

Functions should contain a comments section immediately below the function definition line. These comments should consist of a one-sentence description of the function; a list of the function's arguments, denoted by Args:, with a description of each (including the data type); and a description of the return value, denoted by Returns:. The comments should be descriptive enough that a caller can use the function without reading any of the function's code.


```r


CalculateSampleCovariance <- function(x, y, verbose = TRUE) {
  # Computes the sample covariance between two vectors.
  #
  # Args:
  #   x: One of two vectors whose sample covariance is to be calculated.
  #   y: The other vector. x and y must have the same length, greater than one,
  #      with no missing values.
  #   verbose: If TRUE, prints sample covariance; if not, not. Default is TRUE.
  #
  # Returns:
  #   The sample covariance between x and y.
  n <- length(x)
  # Error handling
  if (n <= 1 || n != length(y)) {
    stop("Arguments x and y have different lengths: ",
         length(x),
         " and ",
         length(y),
         ".")
  }
  if (TRUE %in% is.na(x) || TRUE %in% is.na(y)) {
    stop(" Arguments x and y must not have missing values.")
  }
  covariance <- var(x, y)
  if (verbose)
    cat("Covariance = ", round(covariance, 4), ".\n", sep = "")
  return(covariance)
}
```

Enjoy R!
