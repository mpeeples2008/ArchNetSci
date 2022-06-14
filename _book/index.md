---
title: "Online Companion to *Archaeological Network Science*"
author: "Matthew A. Peeples and Tom Brughmans"
date: "2022-06-14"
site: bookdown::bookdown_site
documentclass: book
bibliography: references_all.bib
biblio-style: apalike
url: https://book.archnetworks.net
description: |
  This online bookdown document accompanies the Cambridge Manuals in Archaeology book
  *Archaeological Network Science* by Tom Brughmans and Matthew A. Peeples.
link-citations: yes
cover-image: images/cover.png
---

# Welcome{- #Welcome}




This project serves as a companion to the Cambridge Manuals in Archaeology book *Archaeological Network Science* by Tom Brughmans and Matthew A. Peeples (2022). 

<a href="https://book.archnetworks.net"><img src="images/cover.png" width="250" height="375" alt="Brughmans and Peeples Book Cover" align="right" style="margin: 0 1em 0 1em" /></a>

This document contains a series of tutorials that outline methods for managing, analyzing, and visualizing network data, primarily using the R programming language. We provide code and examples to replicate the analyses presented in the book as well as many other additional useful tools. This Online Companion is designed to expand upon topics covered in the book and you may find it useful to follow along with these examples as you read the text. Sections 1 through 6 in this document correspond to the topics and information covered in Chapters 2 through 7 of the Brughmans and Peeples book. Section 7 of this document includes topics beyond the scope of the book (exponential random graph models and spatial interaction models) and topics we hope to expand in the future. You can use the table of contents on the left-hand side of your screen to jump directly to a particular section and the table and contents on the right to navigate within each section. We have also created a [quick TOC reference](#TableOfContents) if you are seeking something in particular. 

For more information on the book and the authors check out the project website here: [archnetworks.net](https://archnetworks.net).

**Cite this document as:**

> Peeples, Matthew A. and Tom Brughmans (2022). Online Companion to Archaeological Network Science by Brughmans and Peeples. <https://archnetworks.net>, Accessed 2022-06-14.

**The associated book can be cited as**

> Brughmans, Tom and Matthew A. Peeples (in press). *Archaeological Network Science.* Cambridge Manuals in Archaeology. Cambridge University Press, Cambridge, UK.

***
<div class="warning" style='padding:0.1em; background-color:#E9D8FD; color:#69337A'>
<span>
<p style='margin-top:1em; text-align:center'>
<b>NOTICE</b></p>
<p style='margin-left:1em;'>
Note that this is a pre-release version of this document. Be aware that this document is still being updated and edited. Please check back here for updates in the coming months for the 1.0 release version of this document.
</p>
</span>
</div>
***

## How Should I Use This Online Companion?{- #HowTo}

The tutorials here are designed to complement the text of the associated book (Brughmans and Peeples 2022) but can also stand alone as a guide to implementation of network analyses in R if you have a basic background in network methods and terminology. Although each section of this guide builds upon the previous sections in terms of network concepts and R methods, the sections are each independent in terms of data, examples, and code and can be run out of order if you choose.

A few suggestions on where to start:

* If you are new to network analysis and R, we would suggest going through each section of this document, starting with "Getting Started with R" and then going through the numbered sections in order as you following along in the book.
* If you are already familiar with R but new to network analysis, we suggest you read Section 1 to set up your data and work space, and then follow along with the remaining numbered sections and associated book chapters as you read.
* If you are already a network analyst and confident R user and are just looking for code chunks to implement something in particular, feel free to skip around. We have tried to make each section as independent as possible so that you can pick and choose what you want to work on. Use the [Table of Contents](#TableOfContents) to find topics quickly.
* If you're a real pro and are designing your own network analyses or visualizations, we would love it if you contributed to the project to help this document grow.

Throughout this document we use a few icons to call-out special information or concerns. Keep an eye out for the symbols below:

<div class="rmdnote">
<p>We use this icon to highlight our discussions of R packages used in
this project. Check here for brief overviews and instructions on how to
use and configure these packages.</p>
</div>

<div class="rmdwarning">
<p>We use this icon to highlight particular areas of concern in our
discussion of network methods and R code. In particular, we use this
icon to warn you of common errors or pitfalls for particular functions
or network methods.</p>
</div>

<div class="rmdtip">
<p>We use this icon to highlight helpful tips for using particular R
functions or R-Studio procedures.</p>
</div>


## Reproducibility{- #Repro}

The most recent version of this document was built with R version 4.2.0 (2022-04-22 ucrt). We suggest you use a recent version of R when attempting to use the code in this document. 

![Github last-commit](https://img.shields.io/github/last-commit/mpeeples2008/ArchNetSci)&nbsp; &nbsp;
[![Docker Image CI](https://github.com/mpeeples2008/ArchNetSci/actions/workflows/docker-image.yml/badge.svg)](https://github.com/mpeeples2008/ArchNetSci/actions/workflows/docker-image.yml) 

The content of this document is designed to be as accessible and reproducible as possible. The source code used to produce this document along with all of the data used in analyses are available on [GitHub](https://github.com/mpeeples2008/ArchNetSci). This GitHub repository allows users to open issues, contribute to the document, or help fix typos or other errors (see information about [contributing](#Contributing) below). We have also opened a GitHub discussion board with this repository where users can ask questions about any data or code in the repository without making edits or issue requests directly. 

The easiest way to reproduce this document is to launch the project directly in your browser using [Binder](https://mybinder.org/). When you click on the link below it will open a browser based instance of R studio with all of the required packages and files. From there you can test and evaluate the code directly.

[![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/mpeeples2008/ArchNetSci/main)

When you open Binder you will see a window with the logo and a spinning progress wheel. It will typically only take a minute or so to get up and running and then you will see the screen below. Click on the "R-Studio" link under "Notebook" and it will open a new window with an R-Studio instance that you can use just like you would on your own computer. If you click on the Binder link and it is taking a long time, click to "show" the build logs. If you are "lucky" enough to be the first to initialize Binder after a new build of the GitHub project it will take quite a bit longer to get started. Grab a coffee, tea, Dr. Pepper, or other beverage of choice as it will be approximately 30-35 minutes before R-Studio loads.

![Binder Notebook Page](images/binder.jpg){width=100%}

You can also install this repository as an R package directly from GitHub using:


```r
if (!require("devtools")) install.packages("devtools")
devtools::install_github("mpeeples2008/ArchNetSci")
```

This package installation includes all of the dependencies required to run the code in this document and will create folders called "data" and "scripts" in the installation directory with all of the required files to replicate the analyses in this document.

Finally, you can run the code and generate documents locally using R and R Studio by downloading the entire R repository here: [main.zip](https://github.com/mpeeples2008/ArchNetSci/archive/refs/heads/main.zip). Unzip the files and then:

* Open the "ArchNetSci.Rproj" file in R studio.
* Use the `renv::restore()` command to install the required packages and dependencies. Note that this is a large document that uses many packages so this may take some time.
* You will then be able to browse the files and execute all of the code in the repository on your own computer.

This online bookdown document has been deployed using the Netlify platform and the badge below shows the current status of the build hosted at [https://book.archnetworks.net](book.archnetworks.net).

[![Netlify Status](https://api.netlify.com/api/v1/badges/266d5736-f13a-4de4-b812-141c023f3a09/deploy-status)](https://app.netlify.com/sites/archnetworks/deploys)

## Computational Archaeology Discord Community{- #Discord}

We have created an [Archaeological Network Science Channel on the Computational Archaeology Discord Server](https://discord.gg/Z9UXwjASM5), which we hope will provide an additional venue for archaeological network practitioners to collaborate, interact, and ask for help with this document or with archaeological networks (and other computational methods) in general. We invite you to use this as a place to ask questions of the authors and the community at large. Note that this Discord is subject to the same [code of conduct](https://github.com/mpeeples2008/ArchNetSci/blob/main/CODE_OF_CONDUCT.md) we use for the GitHub repository and you must abide by that agreement to participate. We require that you have a Discord account with a verified email address.

<a href="https://discord.gg/Z9UXwjASM5"><img src="images/Discord_Logo.png" width="300" alt="Discord Logo" style="margin: 0 1em 0 1em" /></a>

[Join the Computational Archaeology Discord](https://discord.gg/Z9UXwjASM5)&nbsp; &nbsp;&nbsp; &nbsp;  ![](https://discordapp.com/api/guilds/975267909012189184/widget.png?style=shield)

## New to R and R Studio?{- #NewToR}

The network tutorials in this document are built for users with a basic familiarity with R and R-studio but if you're just getting started, don't worry. We have created a detailed guide to [Getting started with R](#GettingStarted). This document covers the installation of the required software and provides a basic introduction to the R programming environment that we hope will be enough to get you started. 

If you already have a basic familiarity with R and want to go further, there are numerous additional resources (most are completely free) to help you learn. Some resources we would recommend include *R for Data Science* [(Wickham and Grolemund 2017)](https://r4ds.had.co.nz/), *Advanced R* [(Wickham 2019)](https://adv-r.hadley.nz/), *the R Cookbook, 2nd edition* [(Long and Teetor 2019)](https://rc2e.com/somebasics), and *R in Action* and the associated *Quick-R* website [(Kabacoff 2015)](https://www.statmethods.net/). In addition to this [Ben Marwick](https://anthropology.washington.edu/people/ben-marwick) has created an excellent repository of [resources for using R in archaeology](https://github.com/benmarwick/ctv-archaeology) as well as an ever-growing list of archaeological publications that include R code. The website associated with this book [(archnetworks.net)](https://www.archnetworks.net) also includes a list of archaeological articles focused on network research that include data and code. Reproducing published results is, in our experience, one of the best ways to learn advanced techniques and data management in R so we suggest you give it a try.

## Contribute To the Project{- #Contributing}

We welcome contributions to this project from the community and the GitHub platform helps us facilitate that. You will first need to [sign up for a GitHub account](https://github.com/) and log in. If you find something that needs updating or changing (typos or errors) you can simply click the "View source" link at the right sidebar on the relevant page and then click the edit icon found near the top of the code block and make your proposed changes. These changes will be saved in a new "fork" of the document and we will review these and implement them where relevant and happily add your name to our list of contributors. 

If you detect a larger error such as code not running or if you would like to request a new feature or  update, you can create an issue using the [issue tracker](https://github.com/mpeeples2008/ArchNetSci/issues) page associated with the project repository.

All contributors must agree to adhere to our [code of conduct](https://github.com/mpeeples2008/ArchNetSci/blob/main/CODE_OF_CONDUCT.md).

## Help Build the Community{- #Community}

We are devoted to seeing the community of archaeological network practitioners grow and we hope our book and these online resources will help to make this happen. You can support the growth of our community too!

* Spread the word to your friends and colleagues
* Share links to these online resources on social media using the [#archnetworks](https://twitter.com/search?q=%23archnetworks&src=typed_query) hashtag
* Please cite the book *and* the Online Companion if you use methods or code from this document
* Star the [GitHub project repository](https://github.com/mpeeples2008/ArchNetSci) and contribute to the project
* [Join the Computational Archaeology Discord](https://discord.gg/Z9UXwjASM5) and invite other interested people
* Share articles, teaching resources, data, or other archaeological network materials for posting on our associated website [(archnetworks.net)](https://archnetworks.net)

## Project License{-}

[![](https://i.creativecommons.org/l/by-nc-nd/4.0/88x31.png)](http://creativecommons.org/licenses/by-nc-nd/4.0/)

This Online Companion to Archaeological Network Science is licensed under a [Creative Commons Attribution-NonCommercial-NoDerivitives 4.0 International License](http://creativecommons.org/licenses/by-nc-nd/4.0/).

## Acknowledgements{- #Acknowledgements}

This online bookdown project and the associated book were made possible thanks to the support of several generous funding sources including: The Carlsberg Foundation, in the context of the Past Social Networks Project (CF21-0382); the National Science Foundation through both the Archaeology and the Measurement, Methodology, and Statistics programs (grant #1758690 and #1758606); and the School of Human Evolution and Social Change at Arizona State University. Thank you to Jens Emil Bødstrup Christoffersen for providing detailed comments on and for testing the initial version of this online bookdown document. Any errors that remain are our own.

![](images/NSF_Logo.png){width=150px} ![](images/ASU_SHESC_Logo.png){width=300px}

# Gettings Started with R{- #GettingStarted}

In order to follow along with the code and examples in this document, you will need recent installations of both R and R-Studio on your computer. R and R-studio are available for Windows, MacOS, and Linux. This section provides a very brief overview of how to get up and running. Following this, we introduce the basics of R and R-Studio to get you ready for the tutorials in the remainder of this document. If you follow this tutorial here we are confident you will be able to engage with the examples and code in this Online Companion.

## Download and Install R{- #InstallR}

The first step is to install a recent version of R (we recommend 4.2 or later as this document was originally created in version 4.2). Follow the instructions below for the appropriate operating system.

* The first step is to go to the R project website [www.r-project.org](https://www.r-project.org) and click on the [CRAN](https://cran.r-project.org/mirrors.html) link under "Downloads" on the left hand side.
* Choose a mirror for your download selecting one in your country or the "Cloud" option.
* Next, click on Windows, MacOS, or your Linux distribution and follow the instructions below.

### Windows{- #Windows}

* Click the "base" sub-directory on the left hand side of the screen and then click "Download R-4.2.0 for Windows" (the version number should be 4.2 or later) to download the most recent version as an executable.
* Once your download is complete, run the *.exe file and answer questions as prompted to complete the installation. 

### MacOS{- #MacOS}

To install R on MacOS, you first need to know which chip manufacturer your Mac has. In order to determine which chip you have go to the Apple menu and select "About This Mac" and look for information under "Processor" or "Chip" in the window that pops up. It will either be Intel or M1.

* Next, click the on the link under "Latest release" for the *.pkg file for the appropriate Mac processor in your computer. There is a separate notarized and signed .pkg file Macs with Intel processors and Macs with Apple M1 processors (mostly produced 2020 and later). Note, these .pkg files are not interchangeable so confirm which one you need before attempting to install.
* Once you have downloaded the appropriate .pkg, run it and answer the questions during the install as required.

### Linux{- #Linux}

Linux installations of R are primarily done through the console but the instructions are slightly different depending on which distribution you are using.

* Click on the link for the appropriate Linux distribution and then follow the detailed instructions provided. 
* The "R-core" or "R-base" builds are the ones you want to choose. 
* Follow the instructions for your build and install any recommended dependencies. 

## Download and Install R-Studio{- #InstallRStudio}

R-studio is an integrated development environment (IDE) for R, Python, and related programming tools that provides additional features for running and debugging code and data management. We see this IDE as essential to working with large and complex R projects. 

In order to install R-Studio:

* Go to the R-Studio website [www.rstudio.com](https://www.rstudio.com/) and click "Download" at the top of the screen.
* Select the "RStudio Desktop" option.
* Download and run the latest "installer" file for the appropriate operating system.
* Run the downloaded file and answer questions at prompts as appropriate. R-Studio should automatically detect your installation of R.

## Run R-Studio{- #RunRStudio}

Once you've installed both R and R-Studio, open R-studio and look for the Console window (it will typically be the left hand side of the screen). That will tell you the version of R that is associated with the installation of R-Studio. If all goes well, it should be the recent version of R you just installed. 

![R Version Installed](images/r-version.jpg){width=100%}

## R and R-Studio Basics{- #RBasics}

R is a powerful statistical analysis platform that can be used to conduct some quite complex analyses. The learning curve is a bit steep when first getting started but the payoff is HUGE because the ecosystem of existing R scripts and packages is so large and diverse. We cannot hope to cover everything R and R-Studio can do in this very short intro here. Our tutorial here is a version of the "Introduction to R programming" that Peeples has used in the first week of his Quantitative and Formal Methods in Archaeology class for a number of years. Hopefully this will get you started. 

Although R seems complicated at first, many quite complex statistical analyses are run with just a few lines of code. Once you learn the basics, more complex features of R are really just combinations of these basic procedures. You won't become an R expert overnight, but we've seen many students pick up the basics quite quickly and begin to take on their first independent analyses in R in a matter of hours. 

### Organization of R-Studio{- #Org}

First off, let's take a look at the R-Studio setup. When you first open R-Studio for the first time, you will see a screen divided into 3 panes. Before getting started click on "File" at the top of the screen and go to "New File > R Script" to open a 4th pane. You should see something like the screen below. 

<div class="rmdtip">
<p>Note that the color of your screen may be different as I am using a
particular “dark mode” color setting that I find easier on my eyes. To
change your color scheme, go to the top of the R-Studio window and click
on “Tools &gt; Global Options &gt; Appearance” and then select a color
mode that works for you.</p>
</div>

![R-Studio](images/r-studio.jpg){width=100%}

Organization of R-Studio Windows:

* **Workspace** - The pane in the top left contains the Workspace tabs which is where you can write code and other documents prior to executing the code. 
* **Console** - The pane at the bottom left is the console where you can type and run commands directly. When you execute code from the workspace, it will also appear here.
* **Environment/History** - The pane at the upper right includes tabs for Environment (a list of objects and functions currently initialized) and History (a list of previous commands run at the console). 
* **Files/Plots/Packages** - The lower right pane has tabs for Files (which shows files in the current directory), Plots (where plots created in the console will be displayed), Packages (a list of additional packages installed and initialized in R), and Help (where you can get information about particular functions and packages). 

Note that the locations and visibility of these panes can be changed by going to "View > Panes" and selecting different options. In the set of tutorials that follow we are going to focus on the Console first and will introduce the other panels and what they provide along the way.

### Mathematical Operations{- #Math}

Getting started with R is as simple as typing directly into the Console. You can use the R console like a calculator to conduct mathematical operations. Simply type the numbers and operators at the console and hit enter to calculate. The answer will output directly on the console by default. Try typing the following at the console:


```r
3 + 3
#> [1] 6
4 * 10
#> [1] 40
50 / 5
#> [1] 10
```

R uses `( )` for bracketing groups of operations. These can be nested to do more complex mathematical operations or to determine the order of operations. For example compare the two equations below:


```r
((4 * 5 + 3) / 2) * 12
#> [1] 138

(((4 * 5)) + 3 / 2) * 12
#> [1] 258
```

R uses typical mathematical operators including `+ - * /` for addition, subtraction, multiplication, and division and `^` to raise a number to an exponent. 


```r
5^2
#> [1] 25
5^(2 + 1)
#> [1] 125
```

Anything placed after a `#` in a block of code will be treated as a comment and not evaluated:


```r
4 * 20 # comment here
#> [1] 80
3 * 4 # 4 + 4 will not be evaluated as it is after the #
#> [1] 12
```

### Creating Variables/Objects{- #Variables}

R can also assign numbers, characters, or more complex operations to variables (also known as objects in this context) which can then be used in mathematical operations. Typically, we assign values to a object using the `<-` assign command but `=` also works. For example:


```r
test_var <- 50
test_var
#> [1] 50

test2 = 10 + test_var
test2
#> [1] 60

char1 <- "hello world"
char1
#> [1] "hello world"
```

Object names in R are case sensitive and cannot include spaces. Object names can include numbers and letters but must start with a letter. It is a good idea to use descriptive object names where the object will be used repeatably.

When formatting object names there are a few common styles such as:

* **`snake_case_style`** - see the little snakes (underscores) in the place of spaces
* **`CamalCaseStyle`** - see the capitalized humps denoting each word
* **`kebab-case-style`** - skewered right down the middle

In general any of these styles is fine, but we suggest you try to remain consistent. Also, avoid using `.` to separate words as that is used by particular R functions and calls in other ways and can cause confusion. 

![Illustration by Allison Horst `@allison_horst`](images/case.jpg){width=100%}

Many mathematical constants are built right into R so be sure not to overwrite any of these (or any other function) by giving an object the same name.


```r
pi
#> [1] 3.141593
LETTERS
#>  [1] "A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M" "N"
#> [15] "O" "P" "Q" "R" "S" "T" "U" "V" "W" "X" "Y" "Z"
letters
#>  [1] "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n"
#> [15] "o" "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z"
month.name
#>  [1] "January"   "February"  "March"     "April"    
#>  [5] "May"       "June"      "July"      "August"   
#>  [9] "September" "October"   "November"  "December"
```


### Logical Operators{- #Logical}

R can also use logical operators (see list below). These operators can be used in conjunction with other operations and return a value indicating `TRUE` or `FALSE`. These can be used in more complex functions and conditional statements as we will see below.


|Operator |Meaning                  |
|:--------|:------------------------|
|<        |less than                |
|>        |greater than             |
|<=       |less than or equal to    |
|>=       |greater than or equal to |
|==       |exactly equal to         |
|!=       |not equal to             |
|&        |and                      |
|&#124;   |or                       |



```r
v <- 50
v > 20
#> [1] TRUE
v < 20
#> [1] FALSE
v * 2 == 100
#> [1] TRUE
```

Logical operators can also include *and* statements with the `&` symbol and *or* statements with `|`. For example:


```r
v <- 40
v > 20 & v < 30 # and statement
#> [1] FALSE
v > 20 | v < 30 # or statement
#> [1] TRUE
```

### Vectors{- #Vectors}

R can also assign a vector of numbers or characters to a variable and preform operations using that vector. For example in the following we use the `c()` (c for combine) command to create a vector and subject it to a mathematical or other operation.


```r
z <- c(2, 4, 6, 8, 10, 12)
z / 2
#> [1] 1 2 3 4 5 6
```

If you want to call a particular value or selection of values in a vector you can use the `[]` square brackets and indicate which item(s) you are interested in.


```r
z[3] # item 3 in object z
#> [1] 6
z[4:6] # items 4 through 6 in object z
#> [1]  8 10 12
z[c(3, 2, 1)] # items 3, 2, 1, in that order from object z
#> [1] 6 4 2
```

We can also search vectors or other objects for specific values:


```r
vec_obj <-
  c("Ohtani",
    "Wheeler",
    "Correa",
    "Semien",
    "Soto",
    "Guerrero Jr.",
    "Correa")

vec_obj[vec_obj == "Correa"]
#> [1] "Correa" "Correa"
```

To see if a particular value is in a given object we can use the `%in%` operator and get a logical value in return.


```r
"Ohtani" %in% vec_obj
#> [1] TRUE

"Judge" %in% vec_obj
#> [1] FALSE
```


### Using Basic R Functions{- #Functions}

R has a number of built-in functions that perform many common operations and statistical analyses. We have already used one of these above `c()` and it was so fast and easy you might have missed it. Functions are typically used by typing the name of the function followed by a set of parenthesis that contain all of the arguments that the function expects. For example:


```r
v <- c(5, 10, 15, 20, 25, 30, 2000)
max(v)
#> [1] 2000
min(v)
#> [1] 5
mean(v)
#> [1] 300.7143
median(v)
#> [1] 20
log(v, base = exp(1)) # argument setting the base
#> [1] 1.609438 2.302585 2.708050 2.995732 3.218876 3.401197
#> [7] 7.600902
log10(v)
#> [1] 0.698970 1.000000 1.176091 1.301030 1.397940 1.477121
#> [7] 3.301030
round(pi, digits = 2) # argument setting the number of digits to retain
#> [1] 3.14
```

For a list of some of the most frequently used built-in functions see [this Quick-R](https://www.statmethods.net/management/functions.html) page. 

### Tabular Data{- #Tabular}

R can be used to work with tabular data as well. Typically it is most convenient to read such data for a file for very large tabular data [(see working with files below)](#WorkingWithFiles), but we can also generate simple numeric tabular data directly in R using the `matrix()` function. In the following example we create a two-row, two-column matrix by converting a vector of numbers into a matrix by specifying the number of rows `nrow` and number of columns `ncol`. The assignments we make inside the `matrix()` function are called arguments. 


```r
dat <- c(3, 4, 2, 20)
mat1 <- matrix(data = dat, nrow = 2, ncol = 2)
mat1
#>      [,1] [,2]
#> [1,]    3    2
#> [2,]    4   20
```

Note that the `matrix()` function reads the numbers in first by column and then by row. If we want want to change that we can first investigate the options for this function using the `help()` function. In order to see the documentation for a given function simply type `help("NameOfFunction")` at the console or `?NameOfFunction`. 


```r
?matrix
```

![help for matrix function](images/help.jpg){width=100%}

And let's zoom in to one piece in particular:

![byrow argument](images/byrow.jpg){width=100%}

As we can see in the help materials for matrix, there is an additional argument we did not use called `byrow` which is set to `FALSE` by default. Let's change that to `TRUE` and check the results. Note that you can use capital `F` and `T` in the place of `FALSE` and `TRUE` in functions but it is generally good form to write it out in context where you are sharing your code publicaly. Note also that our function call can span multiple rows and will automatically end when we close the parentheses. This multi-line formatting will be essential for making longer function calls readable.

<div class="rmdtip">
<p>R-Studio has a nice built-in command for formatting code, especially
very long lines of code, into multiple lines that are easier to read.
This command also creates proper spacing between operators and objects.
To give it a try, simply highlight a chunk of code in the workspace area
and hit “Ctrl+Shift+A” (or “Cmd+Shift+A” for a Mac) to reformat the code
within the selection. There are many other nifty R-Studio keyboard
shortcuts. <a
href="https://support.rstudio.com/hc/en-us/articles/200711853-Keyboard-Shortcuts-in-the-RStudio-IDE">Check
here</a> for more info.</p>
</div>



```r
mat2 <- matrix(
  data = dat,
  nrow = 2,
  ncol = 2,
  byrow = TRUE
)
mat2
#>      [,1] [,2]
#> [1,]    3    4
#> [2,]    2   20
```

Just like we did with vectors, we can also use matrices for many mathematical and statistical functions that are built directly into R. For example, let's run a Fisher's Exact Test using the `fisher.test` function to assess the independence of rows and columns in this table.


```r
fisher.test(mat2)
#> 
#> 	Fisher's Exact Test for Count Data
#> 
#> data:  mat2
#> p-value = 0.07474
#> alternative hypothesis: true odds ratio is not equal to 1
#> 95 percent confidence interval:
#>    0.5875228 107.8450263
#> sample estimates:
#> odds ratio 
#>   6.815654
```

The output includes information about the data we used to run the test, a p-value, the alternative hypothesis, confidence intervals, and the odds ratio. The output we get from any given function will vary depending on the application. See the `help()` documents for your function of interest to get more info about output.

### Data Types in R{- #DataTypes}

There are many different types of data that R understands but we focus here on the most common categories. This includes numeric data, integer data, character data, logical data, and factors.

* **numeric data** - This is the designation used for real numbers which can include a decimal point.
* **integer data** - This is the designation for whole numbers without a decimal. To designate a number as an integer type, you can add `L` after the number (see example below). Note that R automatically converts between numeric and integer data as necessary in mathematical operations.
* **character data** - This is the designation for any string of characters that does not exclusively consist of numbers. Character data can be a single character such as `"a"` or a long string `"this string is character data"`. In general R displays character data inside `" "`.
* **logical data** - This is the designation for evaluations of logical statements and takes the form of `TRUE` or `FALSE`.
* **factors** - Factors are nominal variables stored as vectors as R objects which have distinct "levels" which each value must be. Factors are useful in both many statistical procedures and visualizations in that unique values can be treated as "groups" rather than simply unique character data. To designate data as a factor, use the `as.factor()` function. Note that factors can be numbers but they will be treated as nominal characters when evaluated.

It is possible to determine what type of data an R object contains using the `str()` function. Let's look at examples for each type below:


```r
num <- c(12.3, 32.4, 53, 4.2, 4, 22.3)
str(num)
#>  num [1:6] 12.3 32.4 53 4.2 4 22.3

int <- c(1L, 2L, 5L, 6L)
str(int)
#>  int [1:4] 1 2 5 6

char <- c("string1", "string2", "This too is a string")
str(char)
#>  chr [1:3] "string1" "string2" "This too is a string"

tf <- c(TRUE, FALSE, FALSE, TRUE) # note the lack of " "
str(tf)
#>  logi [1:4] TRUE FALSE FALSE TRUE

fac <- as.factor(c("type1", "type2", "type2", "type3"))
str(fac)
#>  Factor w/ 3 levels "type1","type2",..: 1 2 2 3
```

### Object Types in R{- #ObjectTypes}

The four most common object types in R are vectors, matrices, lists, and data frames. We have already explored vectors and matrices but we can define these and the other classes in more detail here.

* **vector** - a combined set of values all of the same type (character, numeric, etc.). Note that if you mix numbers and character data, R will assume every entry represents character data.
* **matrix** - a set of values in a rectangular two-way table all of the same type (character, numeric, etc.)
* **data frame** - a set of values in a rectangular two-way table where different columns can be different data types
* **list** - a list is a collection of other R objects that can be vectors, matrices, data frames or others in any format that are combined into a single object.

#### Vectors{- #Vec}

We have already introduced vectors above but we can point out one more feature that is often useful in assessing vectors. The `length()` function tells you how many elements are in a vector.


```r
v <- c(1, 6, 4, 8, 7, 5, 3, 8, 10, 44)
length(v)
#> [1] 10
```

#### Matrices{- #Mat}

Once again, we have already introduced matrices above but there are a few more details that are worth addressing here. Again, if you want to call a specific value in a matrix you can use the `[ , ]` square brackets with the row number listed followed by a comma and the column number. For example:


```r
mat1
#>      [,1] [,2]
#> [1,]    3    2
#> [2,]    4   20
mat1[2, 1] # row 2 column 1
#> [1] 4
```

If you want to know the size of a matrix, you can use the `dim()` dimensions function:


```r
dim(mat1)
#> [1] 2 2
```


#### Data Frame{- #DF} 

As the brief definitions above suggest, data frames are very similar to matrices but can include mixed data types in the same rectangular table. Each row and column must, however, have the same number of entries. A data frame can be created by combining a set of vectors. For example:


```r
col1 <- c("mammoth", "mastadon", "bison")
col2 <- c(50L, 52L, 14L)
col3 <- c(11.14, 22.23, 656.34)
col4 <- as.factor(c("type1", "type1", "type2"))
col5 <- c(TRUE, FALSE, TRUE)

dat <- data.frame(col1, col2, col3, col4, col5)
dat
#>       col1 col2   col3  col4  col5
#> 1  mammoth   50  11.14 type1  TRUE
#> 2 mastadon   52  22.23 type1 FALSE
#> 3    bison   14 656.34 type2  TRUE
```

If we want to look at what kind of data R understands each column to be, we can use the `str()` or structure function.


```r
str(dat)
#> 'data.frame':	3 obs. of  5 variables:
#>  $ col1: chr  "mammoth" "mastadon" "bison"
#>  $ col2: int  50 52 14
#>  $ col3: num  11.1 22.2 656.3
#>  $ col4: Factor w/ 2 levels "type1","type2": 1 1 2
#>  $ col5: logi  TRUE FALSE TRUE
```

Note that the `dim()` function also works on data frames as does the `[,]` call for specific items:


```r
dat
#>       col1 col2   col3  col4  col5
#> 1  mammoth   50  11.14 type1  TRUE
#> 2 mastadon   52  22.23 type1 FALSE
#> 3    bison   14 656.34 type2  TRUE
dim(dat)
#> [1] 3 5
dat[2, 1]
#> [1] "mastadon"
```

#### Lists{- #List}

A list is simply a convenient way of combining multiple objects into a single object. It doesn't matter what type of objects they are. Lists can be defined using the `list()` function. For example:



```r
out1 <- list(mat1, dat, c(1, 2, 4)) # create a list containing 3 objects
out1
#> [[1]]
#>      [,1] [,2]
#> [1,]    3    2
#> [2,]    4   20
#> 
#> [[2]]
#>       col1 col2   col3  col4  col5
#> 1  mammoth   50  11.14 type1  TRUE
#> 2 mastadon   52  22.23 type1 FALSE
#> 3    bison   14 656.34 type2  TRUE
#> 
#> [[3]]
#> [1] 1 2 4
```

If you want to call a specific element of the list you use double square brackets `[[]]` along with the numeric index in the middle:


```r
out1[[3]]
#> [1] 1 2 4
```

You can even stack sets of double and single brackets to call specific items within list elements:


```r
out1[[3]][2] # item 2 in list object 3
#> [1] 2
out1[[2]][2, 1] # row 2 column 1 in list object 2
#> [1] "mastadon"
```


## The Workspace Tab{- #WorkspaceTab}

Now that we are starting to get into more complex calls and functions, it will be useful to write and edit the code before executing it rather than typing it directly into the Console. To do this, we can work in the Workspace tab R script document we created at the beginning of this tutorial (Go to File > New File > R Script to open a new document). These .R documents can be edited and saved on your computer so that you can return to them later. Let's take a look at how this works.

Think of the R script document as a draft of what you plan to type to the Console. 

### Setting the Working Directory{- #Directory}

Before we get started, let's save the blank R file we just created. First, we want to define the "Working Directory" where files associated with this project will go. To do that go to the menu at the top of the screen and click "Session > Set Working Directory > Choose Working Directory" and then navigate to the location where you would like to save the file. Next, click on "File > Save As" and define a name for your R script. This should end in .R as this is the extension R and R studio recognize for R Scripts.

### Working with your first R script{- #FirstScript}

Now that you have saved this script, you can type mathematical operations, functions, and other code just as we did directly in the Console above. The main advantage is that if you make a mistake you can go back and fix it more easily. Go ahead and copy the code in the next code chunk below and paste it in your R script int he Workspace window and then save the document.


```r
mat3 <- matrix(
  data = c(4, 5, 1, 5, 1, 5),
  nrow = 2,
  ncol = 3,
  byrow = T
)

mat3^2
```

Once you have this saved, highlight all of the code in the Workspace window and then click the "Run" button on the top right side of this pane (see yellow arrow below).

![Workspace](images/workspace.jpg){width=100%}

This will execute the code in your Console and print the results. Let's say when we ran this code, we realized that we actually wanted to raise `mat3` to the 3rd power or we typed one number in our data incorrectly. We can make those changes and then select the code and click run again to do this. This is the true power of scripts in that they allow us to make changes and modify our code easily as we go without retyping commands. Anything you can do in the console you can first set up in the Workspace pane.

## Installing and Using Packages{- #InstallPackages}

So far, everything we have done has involved packages included in "base" R and only internal built-in functions. One of the best things about R is the ecosystem of packages created and peer reviewed by others for all manner of statistical analyses you can imagine. There is a package out there for just about everything so it is always a good idea to check before you start to write any complex script on your own.

In order to install external packages, you need to know the name of the package you want and you simply type `install.packages("NameOfPackage")` at the console. Let's try installing the `vegan` package first which includes lots of useful functions for community ecology research.


```r
install.packages("vegan")
```

Once our package installs, we can "call" it or initialize it using the `libaray()` function. Notice that when we load this package it also loads `permute` and `lattice` which are two additional packages used within `vegan`. These dependencies were automatically installed when you installed the `vegan` package.


```r
library(vegan)
#> Loading required package: permute
#> Loading required package: lattice
#> This is vegan 2.6-2
```

Now we can use not just the base R functions, but also the functions within the `vegan` package. Within this package one particularly useful function is called `diversity()` which allows us to calculate all manner of common diversity measures. Remember to check `?diversity` if you want to learn more about the package and its arguments. Let's give it a try by creating a vector and then calculating two different diversity indices on that vector:


```r
vec1 <- c(1, 6, 2, 7, 45, 3, 6, 2, 4, 6, 7, 2)

diversity(vec1, index = "shannon")
#> [1] 1.831803

diversity(vec1, index = "simpson")
#> [1] 0.7259993
```

<div class="rmdtip">
<p>As this example shows, once a package is loaded using the
<code>library()</code> function, there is nothing special about using
external functions. They are called at the Console just like built-in
functions. There is, however, one additional consideration. Since there
are so many packages and they are created by so many people, sometimes
two packages will use the same function name. For example, the
<code>igraph</code> and <code>sna</code> packages both use the function
name <code>degree()</code> for degree centrality. If both packages are
initialized in R, how will R know which one to use? The solution for
this is to use the package name directly in the function call like the
code below:</p>
</div>



```r
igraph::degree(data) # igraph degree function
sna::degree(data) # sna degree function
```

When writing code that others will use, it may be a good idea to include package names in function calls to avoid ambiguity.

<div class="rmdtip">
<p>There are tons of useful packages out there and it can sometimes be a
bit overwhelming trying to find them. Searching in a search engine for
the simple letter “R” can also yield unexpected results. One helpful tip
when searching for packages is to include “CRAN” or “package” in the
search terms. CRAN stands for the Comprehensive R Archive Network and
this is the archive that contains most of the peer reviewed and
established packages for R.</p>
</div>

## Working with External Files{- #WorkingWithFiles}

In many cases we may wish to either write or read an external files with R. Frequently these files take the shape of spreadsheets such as Excel documents or csv (comma separated value) documents. R has many functions for reading in such data and most are built-in to base R. Let's try this out by first writing a .csv (comma separated value) file from a matrix we generate and then reading it back in. Note that any files you write from the console will go directly to the R working directory unless you otherwise specify.

To write a csv file we use the `write.csv()` function. First we will create a simple matrix, add row names and column names, and then export it. Note that we write this file to a sub-folder of the working directory called "data" here. You could change that to something else but R will return an error if you attempt to read or write from folders that you haven't first created outside of R-Studio.


```r
vec2 <- c(4, 2, 65, 4, 2, 4, 6, 4, 2)

# Notice in the matrix call below we don't enter 'nrow'
# and other argument names as R automatically expects
# them to occur in the order mentioned in the documentation
mat4 <- matrix(vec2, 3, 3) # 3 row 3 column matrix
row.names(mat4) <- c("row 1", "row 2", "row 3") # assign row names
colnames(mat4) <- c("A", "B", "C") #assign col names
mat4 # view matrix
#>        A B C
#> row 1  4 4 6
#> row 2  2 2 4
#> row 3 65 4 2

# Export the matrix as a csv file
write.csv(mat4, file = "data/output_mat.csv")
```

Once you export this file, you should see it appear in the File pane in the bottom right of R-Studio within the working directory.

![File pane](images/files.jpg)

If you want to read this file back in, we can simply use the `read.csv()` function. Let's give it a try and create a new object called, `read_mat` from the results of the function. We use the argument `header = T` to indicate that the first row represents column names and `row.names = 1` to indicate that the first column includes the row names.


```r
read_mat <-
  read.csv(file = "data/output_mat.csv",
           header = T,
           row.names = 1)
read_mat
#>        A B C
#> row 1  4 4 6
#> row 2  2 2 4
#> row 3 65 4 2
```

<div class="rmdwarning">
<p>It is important to note here, however, that the
<code>read.csv()</code>function doesn’t know the difference between a
data frame and a matrix unless you specify. Indeed, if we check, R sees
<code>read_mat</code> as a data frame. For some purposes this doesn’t
matter but where it does, we can convert it to a matrix using the
<code>as.matrix()</code> function.</p>
</div>


```r
str(read_mat)
#> 'data.frame':	3 obs. of  3 variables:
#>  $ A: int  4 2 65
#>  $ B: int  4 2 4
#>  $ C: int  6 4 2
read_mat2 <- as.matrix(read_mat)
is.matrix(read_mat2)
#> [1] TRUE
```

There are lots of different functions for reading in files in different formats and we will introduce some of these later in the subsequent sections of this tutorial where relevant. For an overview of some of the most common file types [see this Quick-R tutorial](https://www.statmethods.net/input/importingdata.html).

## Plotting Data{- #Plotting}

One of the great features of R is the ability to make all kinds of amazing data visualizations. Making simple graphics is very easy but as we will see, defining very specific details often requires a number of different packages and considerable care. Indeed, the vast majority of functions used in this Online Companion are used for visualizations.

Let's start with something simple by creating two vectors and then creating a bi-plot comparing them. When you use the `plot()` function the plot will automatically appear in the bottom right pane of your R-Studio window. We use the `rnorm()` function here to generate random numbers from a normal distribution. 

<div class="rmdtip">
<p>In the chunk of code below, and many other places in this document,
we use the <code>set.seed()</code> function. This function expects an
integer and uses that number to initialize the random number generator
built into R. If you use the same seed on your own computer, you will
get the same results we do here. If we entered a different number in
<code>set.seed()</code> we would get different results. This helps us
ensure our code is reproducible.</p>
</div>



```r
set.seed(465)
# Create a random normal variable with 5000 entries and
# a mean of 40 and standard deviation of 3
x <- rnorm(5000, mean = 40, sd = 3)
# Create a random normal variable with 5000 entries and
# a mean of 5 and standard deviation of 0.5
y <- rnorm(5000, mean = 5, sd = 0.5)
# plot the results
plot(x, y)
```

<img src="index_files/figure-html/unnamed-chunk-47-1.png" width="672" />

We can also easily create a histogram of a single variable with the additional argument `breaks` which determines how many bars the histogram will have:


```r
hist(x, breaks = 20) # breaks defines the number of bars
```

<img src="index_files/figure-html/unnamed-chunk-48-1.png" width="672" />

And boxplots:


```r
boxplot(x, y)
```

<img src="index_files/figure-html/unnamed-chunk-49-1.png" width="672" />

There are lots of data visualizations built into base R and we suggest exploring the [R Gallery Book](https://bookdown.org/content/b298e479-b1ab-49fa-b83d-a57c2b034d49/) which outlines many options.

<div class="rmdnote">
<p>In the remainder of the Online Companion we will go into detail in
how to modify and configure visualizations but it worth mentioning one
more common visualization tool that has almost eclipsed base R graphics
in popularity. That is the package <code>ggplot2</code>. This package
can be used for all sorts of visualizations and it uses a format that is
somewhat different from that of base R.</p>
</div>

Let's take a look at an example of a plot using the `ggplot2` format:


```r
install.packages("ggplot2")
```


```r
library(ggplot2)

df <- data.frame(x, y)

ggplot(data = df) +
  geom_point(aes(x = x, y = y))
```

<img src="index_files/figure-html/unnamed-chunk-52-1.png" width="672" />

In the code chunk above, we created a data frame (which `ggplot2` requires) combining our random x and y variables. Next, we made a generic call to ggplot2 using the `ggplot(data = df)` line. This creates a ggplot object set up with `df` as the data considered. Notice this line is followed by a `+`. This package will continue to read lines until a line does not end with this symbol and `ggplot` calls can often be quite long. 

The next line was the `geom_point()` function. This package designates different kinds of visualizations as `geom_` and there are many options (`geom_histogram`, `geom_bar`, `geom_polygon`, etc.). The `geom_point` function refers to a simple point plot. The argument inside the function is defined as `aes(x = x, y = y)`. In this package `aes` stands for aesthetics. In this case, we are using this aesthetics call to designate which variable will be on the x and which on the y axis, which is easy here as we named our variable appropriately.

From here `ggplot2` includes seemingly endless customization options. There are way too many options out there for us to cover here but a good place to start is the [R Graph Gallery](https://r-graph-gallery.com/) website. 

We will cover many examples and the [visualization section](#Visualization) of this tutorial in particular leans heavily on the `ggplot2` format but for now, let's just see a couple of additional examples. 


```r
# Histogram example
ggplot(data = df) +
  geom_histogram(aes(x = y), col = "blue", fill = "darkorchid4") +
  xlab("Numbers!!") +
  ylab("REALLY BIG NUMBERS") +
  theme_minimal()
```

<img src="index_files/figure-html/unnamed-chunk-53-1.png" width="672" />

```r

# Bined biplot example
ggplot(data = df) +
  geom_bin2d(aes(x = x, y = y)) +
  scale_fill_continuous(type = "viridis") +
  theme_bw()
```

<img src="index_files/figure-html/unnamed-chunk-53-2.png" width="672" />

```r

# Create data frame for bar plot example
df2 <- data.frame(d1 = rpois(50, lambda = 4),
                  gp = sample(size = 50, letters[1:4], replace = T))
ggplot(data = df2) +
  geom_bar(aes(x = d1, fill = factor(gp))) +
  theme_dark()
```

<img src="index_files/figure-html/unnamed-chunk-53-3.png" width="672" />

## Warnings and Messages in R{- #Warnings}

As you have run the code above or in other sections of this documents, you may have seen additional "warnings" or "messages" appear on the screen. For example something like: 

``stat_bin()` using `bins = 30`. Pick better value with `binwidth``

This output in the console is simply letting us know that we might want to select a different `binwidth` that better suits our data. Warnings and messages like this are often relatively benign like this, but may also indicate a bigger problem. For example, you may get a warning that a particular method is not appropriate for the data you have (perhaps because of missing data) even though results are provided. Keep a careful eye on these warnings and heed them when necessary. Often looking in the `help()` documentation for a given function will help you interpret these messages.

For the purposes of this online companion, however, we have "muted" these warnings and messages in the output except in a couple of places where we are pointing out something specific. Any messages you generate should be innocuous but feel free to ask if you have questions or concerns.

## More Advanced R Features{- #AdvancedR}

The examples so far have covered most of the basic features of R and R-Studio. There are just a few more things that are implemented in this online document that need a bit of additional explanation. If you can follow along with the examples above, you will be able to replicate most of the work in this document. The features in this section will help you expand your skills and better understand the more complicated code in this document. 

### Conditional Statements{- #Conditionals}

Another common need for programming in R is to conduct an action conditioned on another action or variable state. For example, if A is `TRUE` then do B. If statements like this are formally in R using the following format:


```r
# Example 1
if (test) {
  event1
}

# Example 2
if (test) {
  event1
} else {
  event2
}
```

In Example 1 above if the statement called `test` is evaluated as `TRUE` then `event1` is executed. If `test` is evaluated as `FALSE` then nothing happens.

Example 2 is an if...else statement. In this example if the statement called `test` is evaluated as `TRUE` then `event1` is executed. If `test` is evaluated as `FALSE` then `event2` is executed.

Let's take a look at a worked example that will print output on the screen depending on the outcome of the `test` expression.


```r
x <- 40

if (x > 50) {
  cat("Greater Than 50")
} else {
  cat("Less Than 50")
}
#> Less Than 50

if (x * 2 > 50) {
  cat("Greater Than 50")
} else {
  cat("Less Than 50")
}
#> Greater Than 50

if (x > 50) {
  cat("Greater Than 50")
}
```

In the first example above, the evaluation of `x > 50` was `FALSE` so the statement in brackets after `else` was evaluated. In the second example, the evaluation of `x*2 > 50` was `TRUE` so the first statement was evaluated. Finally, in the third example, `x > 50` was `FALSE` and since there is no `else` statement nothing happened. 

If you want to apply an `if...else` statement to a vector of values rather than one at a time, you can use a useful function `ifelse()`. The `ifelse()` function expects the first item in the parenthesis to be the test expression, followed by the event to execute if the statement is true and then the event to execute if the expression is false.


```r
x <- seq(5, 100, by = 5)
x
#>  [1]   5  10  15  20  25  30  35  40  45  50  55  60  65  70
#> [15]  75  80  85  90  95 100

ifelse(x > 50, "Greater Than 50", "Less Than 50")
#>  [1] "Less Than 50"    "Less Than 50"    "Less Than 50"   
#>  [4] "Less Than 50"    "Less Than 50"    "Less Than 50"   
#>  [7] "Less Than 50"    "Less Than 50"    "Less Than 50"   
#> [10] "Less Than 50"    "Greater Than 50" "Greater Than 50"
#> [13] "Greater Than 50" "Greater Than 50" "Greater Than 50"
#> [16] "Greater Than 50" "Greater Than 50" "Greater Than 50"
#> [19] "Greater Than 50" "Greater Than 50"
```

Another useful and frequently used conditional function is the `which()` function. This function allows you to evaluate which items in an object meet a given condition. Let's take a look at an example to see how this works:


```r
x <- seq(1, 10) # sequence of numbers 1 to 10
x
#>  [1]  1  2  3  4  5  6  7  8  9 10
which(x > 5)
#> [1]  6  7  8  9 10

y <- seq(2, 20, by = 2) # sequence of numbers 2 to 20 by 2s
y
#>  [1]  2  4  6  8 10 12 14 16 18 20
which(y > 10)
#> [1]  6  7  8  9 10
```

In the first example above, we created a sequence of numbers from 1 to 10 and then evaluated which were greater than 5. The results indicated that items 6, 7, 8, 9, and 10 in the vector were greater than 5. Note that these results are not referring to the values but instead are the numeric indexes of the values. The second example illustrates this. This is much like the first example by we create a sequence of numbers from 2 to 20 counting by 2s. When we evaluate which numbers in the vector are greater than 10, our results tell us the 6th, 7th, 8th, 9th, and 10th numbers are greater than 10.

### Loops{- #Loops}

A loop provides a set of instructions for R to repeat a code block some number of times based on rules we supply. The typical syntax is:


```r
for (value in sequence) {
  event
}
```

What this means is that for every value in a sequence of values, evaluate the expression in the `event` chunk. Let's take a look at a worked example to help clarify this.


```r
for (i in 1:5) { # for every value in the sequence from 1:5
  print(i * 2)
}
#> [1] 2
#> [1] 4
#> [1] 6
#> [1] 8
#> [1] 10
```

As this example helps illustrate, the `for (i in 1:5)` statement defines `i = 1` and then evaluates the statement `print(i * 2)`, and then defines `i = 2` and evaluates `print(i * 2)`, and so on until it completes the chunk for `i = 5`. The key feature of for loops is that we can use the value assigned to the iterator `i` in the statement inside the curly brackets `{}` to evaluate the statement for a range of values. The sequence of values assigned to the iterator are arbitrary and can occur in any order:


```r
val_seq <- c(5, 1, 8, 4, 1, 5, 7)

for (m in val_seq) {
  print(m)
}
#> [1] 5
#> [1] 1
#> [1] 8
#> [1] 4
#> [1] 1
#> [1] 5
#> [1] 7
```

We can also assign the results of any expressions in the curly brackets to a new object. If you want to retain all results and not have the results rewritten, you will need to first define an output object before you start.


```r
# Compare these two chunks of code
for (z in 1:10) {
  out <- z
}
out
#> [1] 10

out <- NULL
for (z in 1:10) {
  out[z] <- z
}
out
#>  [1]  1  2  3  4  5  6  7  8  9 10
```

In the second example, the statement within the brackets tells R to assign the value of `z` to `out` at position `[z]` and therefore all results are retained rather than rewritten each sequence of the loop.

There is a lot more than can be done with loops but this basic description should be all you need to know to understand the code in this document.

### Custom Functions{- #CustomFunctions}

Finally, we are going to end with a discussion of how R can be used to create custom functions. If there is some operation you do again and again, it doesn't make sense to keep copying and pasting the code every time. It makes more sense to define a function and then just call that. Once defined, a custom function works just like all the built-in and package functions we've seen above. Here is the basic syntax of a function in R:


```r
function_name <- function(arguments) {
  result <- expression_to_evaluate
  return(result)
}

# Once defined function can be run as
function_name(arguments)
```

This format is a fairly simple example. Some functions can be quite complex, but that complexity is usually a product of combining loops and conditional statements and other processes discussed above within the function rather than anything new or beyond what we've shown you so far. Let's take a look at a simple worked example to see how custom functions work:


```r
do_something <- function(x, y) {
  result <- (x * y) + (x - y)^2
  return(result)
}

do_something(4, 5)
#> [1] 21

do_something(10, 5)
#> [1] 75
```

As this shows, any named argument in the function call can be used in the expression evaluated within the brackets. Functions can contain many lines of code and many arguments but the features and format are the same as the simple examples here. Let's look at a somewhat more complex function to see how this works:


```r
myfunct <- function(x) {
  z <- NULL
  for (i in seq_len(length(x))) {
    z[i] <- (x[i] * i) / 5
  }
  return(z)
}

val_seq <- seq(1:10)
myfunct(val_seq)
#>  [1]  0.2  0.8  1.8  3.2  5.0  7.2  9.8 12.8 16.2 20.0
```

Let's break down what is happening in the chunk of code above. First, we defined a function with one argument `x`. Inside the function expression we then initialized a new variable for the output called `z` by simply setting it to `NULL` or empty. We then enter a for loop that iterates values of `i` for a sequence of numbers from 1 to the length of vector `x` using the `seq_len()` call.. The value of `z` at position `i` is defined as the value of `x` at position `i` times `i` divided by `5`. Once this loop finishes, the function returns the vector `z` with the results. As this example shows, arguments need not be limited to single values and can include vectors, data.frames, matrices, lists, or any type of R object.

To clarify how the iterator works, the function `seq_len()` creates a sequence of numbers from 1 to the number indicated. When you are setting the number of runs of a loop based on the length of some other object it is good practice to use this function.


```r
seq_len(10)
#>  [1]  1  2  3  4  5  6  7  8  9 10
```


## Test Your Skills{- #TestYourSkills}

If you've followed along with this tutorial so far, you should be able to do many basic operations in R and R-Studio. Let's now put your skills to the test. Use what you have learned above to create a function that converts Fahrenheit temperatures to Celsius. The formula for this conversion is `(F_temp - 32) * 5 / 9`. Create a function that reads in the F temperature and outputs C and the run it for the  sequence of values below. 

Hints: Remember that you can't use `F` as an object name because that is the designation R uses for `FALSE`. Also, think about what you are trying to accomplish here. You want to create a function that iterates across a vector. That's very much what the previous example did so you can use that code as inspiration.


```r
f_temp <- c(44, 59, 59, 39, 50, 59, 35)
```

Once you have a working function, the use the `round()` function to convert your results to integers (check `?round()` if you need hints on how to do that) and output these results into an object called `res`. Finally run the chunk of code below for a surprise:


```r
paste(c(LETTERS[res]), collapse = "")
```

We have provided the answer below but give this a try on your own first before peeking at the answer.

No peeking until you try!!

![Artwork by Allison Horst `@allison_horst`](images/monster_support.jpg){width=100%}

Here is our solution below:


```r
convert_temp <- function(f) {
  results <- NULL
  for (i in seq_len(length(f))) {
    results[i] <- ((f[i] - 32) * 5 / 9)
  }
  return(results)
}

f_temp <- c(44, 59, 59, 39, 50, 59, 35)
out <- convert_temp(f_temp)
out
#> [1]  6.666667 15.000000 15.000000  3.888889 10.000000
#> [6] 15.000000  1.666667

res <- round(out, digits = 0)
res
#> [1]  7 15 15  4 10 15  2

paste(c(LETTERS[res]), collapse = "")
#> [1] "GOODJOB"
```

