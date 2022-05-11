# R Code Directory

All code for the humdrumR R package is kept in this directory.


# Code Style

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


