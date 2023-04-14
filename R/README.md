# R Code Directory

All code for the humdrumR R package is kept in this directory.


# Code Style

## Pipes

Pipes are great for on-the-fly coding, but for consistency, we don't use pipes in the codebase.
This means that ALL function calls are of the standard form: `f(...)`, never `... |> f()`.

## Assignment

The `<-` assignment operator should be used for all assignments.

```
#BAD

x = 5

mean(x) -> z

#GOOD

x <- 5

z <- mean(x)

```

---------

`=` is used for naming function arguments, including naming elements of lists or vectors.

```
#BAD

mean = 5


#GOOD

dnorm(x, mean = 5)

list(mean = 5, sd = 2)


```

----


Functions should always be named across multiple lines, even if they are very short:

```
#BAD

myFunc <- function(x, y) x + y

#GOOD

myFunc <- function(x, y) {
	x + y
}

```

This is because the Rstudio editors doesn't recognize the one-line functions in some contexts!





## Space

All operators should be surrounded by spaces:

```
#BAD

x<-2

2+2

4<x

#GOOD

x <- 2

2 + 2

4 < x

```

Commas should be followed by spaces, but NOT preceded by a space.

```
#BAD

c(x,y,z)

c(x , y , z)

c(x,y = 3)

#GOOD

c(x, y, z)

c(x, y = 3)


```

Missing arguments should be marked with a space, with commas otherwise folling the same logic:

```
#BAD

matrix[,2]

matrix[, 2]

#GOOD

matrix[ , 2]

matrix[ , x = 2]


```




## Braces

An opening curly brace should NEVER go on its own line and should always be followed by a new line.
A closing curly brace should always be the first non-white space on its own line.

```

#BAD

if (TRUE)
{
	# do this
}

if (TRUE)
{
	# do this
} 
else {
	# else do this}
}

local({
	# do some stuff })

if (TRUE)
{
	# do this
} 
else {
	# else do this}
}

local({
	# do some stuff
	})

#GOOD

if (TRUE) {
	# do this
} 

if (TRUE) {
	# do this
} else {
	# else do this
}

for (i in 1:10) {
	# do this
}

local({
	# do this stuff
	})


```

## If 


Generally avoid nested if/else branches.
A single layer of nesting is the best sollution sometimes, and is acceptable, but beyond that, try to avoid it.

---

Generally, you should attempt to make different if/else branches roughly equivalent in scope/complexity.
If you have cases where one branch is much more complex than the other, can you break it down into smaller pieces in some way?

---

If the true or false blocks consist of single short expressions, write the whole thing on one line:

```
if (is.null(x)) x <- 3

if (is.null(x)) x <- else y <- x


```

Otherwise write out the whole thing 

```
if (TRUE) {
	# do this
} 

if (TRUE) {
	# do this
} else {
	# else do this
}

```

---

Remember, if `if(FALSE)` and there is no `else`, the expression evaluates as `NULL`.
Use this!

```
#BAD
if (predicate) {
	x <- # blah
} else {
	x <- NULL
}


#GOOD

x <- if (predicate) {
	#blah
}
	

#BETTER if blah is short

x <- if (predicate) # blah



```

---



## Return

`return()` should only be explicitely called for an early return.
At the end of a function, the output should just be the last evaluated line.

```
#BAD

func <- function(x) {
	y <- # dostuff(x)
	
	return(y)

}


#GOOD

func <- function(x) {
	y <- # dostuff(x)
	
	y

}

#BETTER (don't assign to y at all, if there's no reason to)

func <- function(x) {
	dostuff(x)
}

```


Early returns should generally be used near the beginning of the function---especially based on an argument check.

```
#GOOD

f <- function(x) {
	if (!predicate(x)) return(# blah)

	# do complex stuff!
}


```


However, there are many cases where early returns are useful, can they can be used anywhere.
For example, anytime you are using an if-else to choose between returning two things, but one branch is much shorter/simpler than the other, it is better to use an early return.
Sometimes, this can mean flipping the predicate from TRUE to FALSE.

```
#BAD
f <- function(...) {

	x <- # doComplexStuff(...)

	if (x > 0) {
		# a lot more complex code
		# a lot more complex code
		# a lot more complex code
		# a lot more complex code
	} else {
		-x
	}



}

#GOOD

f <- function(...) {

	x <- # doComplexStuff(...)

	if (x <= 0) return(-x)

	# a lot more complex code
	# a lot more complex code
	# a lot more complex code
	# a lot more complex code




}


```


## Blocks as Expression

In R, everything is an expression, which returns a result.
A block of code---`{ # stuff in here}`---is itself an expression!
The "result" of the expression is the last result:

```
{
	2 + 2
	3 + 2
	1 - 0
} # result is 0

```

In general, code should be written so that blocks simply evalulate to a single value---in other words, each block should serve *one* purpose.
Side effects, should be avoided, but are sometimes necessary.


--------------------

In general, *any* aspect of a block that can be factored out, should be.
In many case an if block is used to assign alternate values to the same variable.
The result of the if block should be assigned, rather than assigning the variable separately in each block:

```
# BAD:
if (TRUE) {
	x <- # complex expression
} else {
	x <- # another expression
}	

# GOOD:

x <- if(TRUE) {
	# complex expression
} else {
	# another expression
}


```

Similarly, if the output of if branches has the same function applied to it (with identical arguments), just call the function on the whole if block!

```

#BAD:

if (TRUE) {
	mean(x)
} else {
	mean(y)
}



#GOOD:


mean(if (TRUE) x else y)

```

Better yet, if only a single argument within a block is really conditional, apply the condition there:

```

#BAD

if (TRUE) {
	mean(x - 10)
} else {
	mean(x - y)

}

#GOOD

mean(x - (if (TRUE) 10 else y))


```


## Anonymous functions

### Map()

When using an anonymous function as the `f` argument to `Map()`, the only way to make the indentation look readable is to put the
mapped (`...`) arguments first, then put the anonymous function after them on a new line.
This *requires* that the function argument is named (`f`).
This when the mapped arguments are concise, this isn't a big improvement,
but it's much more readable when the mapped arguments are even remotely complicated.

Example:

```

#BAD 

Map(\(x, y) {
	# code here
	# code here

	},
    code(here, for(the[x], argument)),
    code(here, for(the[y], argument)))

#GOOD


Map(# code(here, for(the[x], argument)),
    # code(here, for(the[y], argument)),
    \(x, y) {
	# code here
	# code here

	})
    )

```

# Naming

Generally, few variables should be used.
If there is no reason to assign a value to a new variable, don't.
In particular, the last value in a function doesn't generally need to be named.

```
#BAD

f <- function(data) {

	nextStep <- doStuff(data)
	anotherStep <- doMoreStuff(nextStep)
	
	output <- doFinalStep(anotherStep)
}

#GOOD

f <- function(data) {

	nextStep <- doStuff(data)
	anotherStep <- doMoreStuff(nextStep)
	
	doFinalStep(anotherStep)

}

#IF "doStuff" and "doMoreStuff" are really this simple and concise, this would be even BETTER:

f <- function(data) {
	doFinalStep(doMoreStuff(doStuff(data)))
}



```





## Upper and Lower Case

Single-word names should be all lowercase.
Use lower camelcase for multi-word names: 

```
#BAD:

inputdata

time_series_data



#GOOD:

inputData

timeSeriesData

```


However, (col)names of items within lists/data.frames should be captilized (upper camel case).
An object created first as a local variable will have a lower-camel-case name, but change to the same name but upper camel case if put into a list or data.frame.
(This means that calls to `$` should always have a capital letter after the `$`.)

```
#GOOD

inputData <- # 

outputData1 <- # f(inputData)
outputAnotherDim <- # h(inputData)

list(OutputData = outputData1, OutputAnotherDim = outputAnotherDim)


```

----

+ `x` is an appropriate name for the first (main) argument of a method, because different methods of that function will all have different types of input, but need to share the same argument name.

---

Argument/variable names representing vectorized objects should not be pluralized, even though they may represent a collection of data.
The whole point of vectorization is to allow us to think of a collection (vector) as a single piece of data---and in some cases, a vector might be of length one, so it *is* a single datapoint.
We reflect that in a singular name.

```
#BAD

f <- function(notes) {
	# do stuff to notes
}

#GOOD

f <- function(note) {
	# do stuff to note
}


```

Plurals can and should be used for collections of more than one vectors of similar data.
For example, a data.frame where each column is a different collection of rhythmic values might be called `rhythms`.


---

## Functions

Functions should be named to reflect what they do.
Since functions should do only *one* job, this should be easy---if a function's functionality can't be expressed in single, simple name, the function should probably be factored.






----


Variables should be given informative names, describing what they are.

Variables representing equivalent types of information should be given consistent (same) names across different functions.
This means that functions which work together in pipelines, with the output of function1 becoming the input to function2, the output variable of function1 should have the same name as the input variable (argument) of function2.

```
step1 <- function(originaldata) {
	nextdata <- # result of some processes
	
	nextdata
}

```

If a serious of commands modify values, while still representing conceptually the same information, and the original values are never used again, reuse the same variable name.
Thus, names should reflect abstractly and generally what the data is, not what it is at a particular point.
For example, `input` is a bad name because a value is only `input` at the beginning of the function---as you modify it, it is not longer just input.
Instead, name it after what it *is*.

```
#BAD

analyze <- function(data) {
	centeredData <- data - mean(data)
        normalizedData <- data / sd(data)

        roundedData <- round(normalizedData, 2)

        ### etc.
	
}

#GOOD

analyze <- function(data) {
	data <- data - mean(data)
	data <- data / sd(data)
	data <- round(data, 2)

	### etc.


}

#BETTER (group conceptually similar processes)

analyze <- function(data) {

	data <- (data - mean(data)) / sd(data)
	data <- round(data, 2)

	### etc.

```


## Scope

Never assign global variables

----

When you need to calculate and assign a set of variables as *only* as stepping stones to a final resulting variable, wrap the whole process in a `local` environment.
This makes sure these stepping-stone variables don't clutter the local namespace.
Why do we care?
It means that as a coder, if you reading a function and see the `local({###})` you know you can ignore everything inside the `local`---none of those variables are being used somewhere else in the function.

```
#BAD

f <- function(x, y) {

	a <- doSomething(x)
	b <- doSomethingElse(x, a, ...)
	c <- doSomethingAgain(a, b)
	
	mainPointOfFunction(c, y)


}


#GOOD

f <- function(x, y) {

	c <- local({
		a <- doSomething(x)
		b <- doSomethingElse(x, a, ...)
		doSomethingAgain(a, b)
	})
	
	mainPointOfFunction(c, y)


}
```

In this `GOOD` example, once `c` is calculated, we can forget about `a` and `b`---we know they aren't being used anywhere else.



# Vectorization

Vectorization should be used wherever possible.
This means that functions should accept a vector ($length \geq 0$) as their input arguments and return a vector of the same length as output.
If there are multiple input arguments, the first argument is the "main" vectorized arguments---other arguments are either vectorized (must be the same length as the first argument) or $length = 1$.


Generally, functions and expressions in `humdrumR` should be purely vectorized.
I.e., create expressions between vectors as objects.
When repeated evaluation is needed, use `lapply`, `sapply` (only if *certain* that the output will always be at least one vector, and all the same length), and `tapply`.


## Loops


`for` and `while` loops should only be used in very restrictive circumstances.

`for` should only be used in cases where the `n` is very small ($n < 10$), a few hundred at most.


### Index Arguments 

When using `for`, or doing `lapply`/`sapply` across indices, the index argument should be named in an informative way.

`i` and `j` should only be used when the index number is an integer index (`i` for rows and `j` for columns, if those dimensions make sense).
Otherwise, the argument should named to reflect that the thing is.


# Documentation Style


## param

All exported functions should have all arguments described in roxygen2 `@param` fields.
Each `@param` field should include two to four paragraphs, in the following order: 

1. Description (mandatory)
2. Default value (optional)
3. Constraints (mandatory)
4. Context/details (optional)

This will look something like this:


```
#' @param argname ***Concise digestible description of what the argument does.***
#' 
#' Defaults to `value`.
#'
#' Must be a `class` vector; must be length `x` or less.
#'
#' This argument is parsed blah blah blah.
```

Note that the formatting `***` that bracket the first part of the description can't have spaces immediately inside them.

#### Description

The description should be concise and readable.
The description should convey to a user the *purpose* of the argument---why they would use it.
Details of *how* to use the argument will appear in the main body of the function man.

#### Defaults


If an argument has not default value, simply omit this paragraph.

The defaults paragraph should more or less just say "Defaults to `x`", where `x` is some value.
However, each function manual's "signature" will already list the *literal* value.
The @param field is a place to put the default in words---like saying "zero" instead of `0`.
For example, in the `count()` man, I say the `beat` argument "Defaults to a whole-note", rather than `rational(1)`, which is the literal default.

#### Constraints

We now list the *requirements* for the argument.
If these requirements aren't meant, errors will occur.
The most important constraints involve the type/class restraints on the argument.
The second most important is constraints on the length (or size/shape) of the argument.
Constraints should be stated in sentences beginning "Must be...".

We will refer to a `length(x) == 1` constraint as "a singleton."

If there are more than one unrelated constraints, they can be presented in a semicolon-delimited list---the semicolon should be interpreted as an AND.
However, when possible, constraints should be expressed more concisely in a single sentence.
For example, instead of saying "Must be `character`; must be a singleton; the string must not be empty", we can write "Must be a singleton, non-empty `character` value."

+ Singleton logical arguments should always be written as "Must be a singleton `logical` value: an on/off switch."
+ Arguments that must be integers or natural numbers (i.e., whole numbers) but where the class is allowed to be `numeric` have special descriptions, either:
  + "whole number(s)"
  + "natural number(s)"
  + "positive natural number(s)"


#### Details

Some arguments may require no extra details, in which case this paragraph is omitted.

The most common "detail" to include involve explanations of how `NA` or `NULL` values are handled by the function.
There might also be useful descriptions of how an argument is parsed or interpreted.
Finally, sometimes a link to a related man should be included.


### Param inheritance


Whenever possible, arguments that are passed directly to internal functions should use `@inheritParams` to inherit the `@param` description from the internal function.
This will require writing the parent description in a more general language, so it applies equally to all functions inheriting it.

Put the `@inheritParams` tag(s) *above* the `@param` tag(s)!
