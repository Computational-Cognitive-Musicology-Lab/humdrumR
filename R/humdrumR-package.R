# Package documenation ----

#' humdrumR
#'
#' `humdrumR` is a toolkit for the analysis of data encoded in the [humdrum syntax](http://www.humdrum.org/guide/ch05/).
#' The humdrum syntax is an incredibly flexible, and powerful, scheme for encoding musical data.
#' Tens of thousands of musical scores (and other musical data) have been encoded in the humdrum syntax, many available online through repositories such as 
#' [KernScores](http://kern.ccarh.org/).
#' The `humdrumR` package is intended as a modernized replacement for the original [humdrum toolkit](http://www.humdrum.org/), leveraging
#' the power of `R` to give us unprecedented power to manipulate and analyze humdrum data using concise, expressive syntax.
#'
#' @section Package design:
#' 
#' The package `humdrumR` has seven main components:
#' 
#' + To represent humdrum data in R, we have the [humdrumR][humdrumR::humdrumRclass] [S4 class](http://adv-r.had.co.nz/S4.html), and it's core component 
#'   the [humdrum table][humdrumR::humTable]. 
#' + To create `humdrumR` data, a sophisticated humdrum data parser: [readHumdrum].
#'   `humdrumR` data can also be written back to humdrum-syntax text files using [writeHumdrum].
#' + To filter `humdrumR` data, we have the [subset()/filter()][subset.humdrumR()] functions, as well as methhods for
#'   `R`'s standard [indexing operators][base::Extract] (`[]` and `[[]]`).
#' + To manipulate and modify `humdrumR` data, we have the [with and within][withinHumdrum] methods for `humdrumR` objects, and tidyverse
#'   aliases [mutate()], [summarise()], and [reframe()].
#' + To facilitate the development of functions to work with humdrum tokens---which are simple character strings packed with information---, 
#'   a useful API we call our [regular-expression dispatch system][humdrumR::humdrumDispatch].
#' + Several [modules][humdrumPitch] for representing and manipulating musical pitch information, 
#'   including our core [tonalInterval] class to represent tonal pitch.
#' + A [module][humdrumR::humRhythm] for representing and manipulating musical rhythm information, 
#'   with a core [rhythmInterval] class to represent rhythms.
#'
#'
#' @section Package options:
#' 
#' The `humdrumR()` function sets general options for the package,
#' mostly related to how [humdrumR data objects][humdrumRclass] are viewed.
#' Each argument to the function manipulates a print/package option: for any argument that is 
#' not used, the option remains in its current setting (i.e., unchanged).
#' These package options are all enumerated and explained in the **Arguments** section above.
#' 
#' @keywords internal
#' @docType package
#' @name humdrumR
#' @importFrom MASS fractions
#' @importFrom combinat permn
#' @importFrom utils combn
#' @importFrom glue glue glue_collapse
#' @importFrom abind abind
#' @importFrom stringr str_count str_detect str_dup str_extract str_match str_pad str_replace str_split str_sub
#' @importFrom stringi stri_enc_detect2 stri_read_raw stri_trans_totitle
#' @importFrom rlang %|% %||% 
#' @importFrom bit64 as.integer64 is.integer64
#' @importFrom numbers primeFactors
#' @importFrom data.table data.table rbindlist setorder setindex set setorderv setcolorder copy as.data.table is.data.table frank CJ setnames setkey
#' @importFrom scales ContinuousRange
#' @importFrom dplyr summarise select filter mutate pull reframe group_by ungroup summarize count
#' @importFrom tidyselect eval_select
#' @importFrom tidyr pivot_wider pivot_longer
#' @importFrom ggplot2 ggplot update_geom_defaults scale_color_gradientn scale_type aes geom_bar geom_point
#' @export summarise select filter mutate pull reframe group_by ungroup summarize count
NULL


#' `humdrumR`'s root directory on your machine.
#'
#' `humdrumRroot` is the path to where the `humdrumR` package is install on your machine.
#' When you installed `humdrumR` a few basic humdrum files were stored here as well, 
#' in subdirectories `examples` and `HumdrumData`.
#'
#' @rdname humdrumR
#' @export
humdrumRroot <- system.file(package = 'humdrumR')



# Package global data ----


### groupby ----

autoArgTable  <- rbind(data.table(Argument = 'groupby',  Type = 'melodic',  
                                  Function = c('mint', 'delta', 'sigma', 'lag', 'ditto', 'ioi', 'sumTies', 'hop'), 
                                  Expression = list(quote(list(Piece = Piece, Spine = Spine, Path = Path)))),
                       data.table(Argument = 'groupby', Type = 'harmonic',
                                  Function = c('hint', 'sonority'),
                                  Expression = list(quote(list(Piece = Piece, Record = Record)))),
                       data.table(Argument = 'groupby', Type = 'structure',       
                                  Function = c('timeline', 'timestamp', 'timecount', 'onbeat', 'subpos', 'metlev', 'metcount'),
                                  Expression = list(quote(list(Piece = Piece, Spine = Spine, Path = Path, ParentPath = ParentPath, Record = Record, Stop = Stop)))),
                       data.table(Argument = 'orderby', Type = 'harmonic',
                                  Function = 'hint',
                                  Expression = list(quote(list(Piece = Piece, Record = Record, Spine = Spine, Path = Path, Stop = Stop)))),
                       data.table(Argument = 'meter', Type = 'meter',
                                  Function = c('metlev', 'metcount'),
                                  Expression = list(quote(TimeSignature))),
                       data.table(Argument = 'pickup', Type = 'pickup',
                                  Function = c('metlev', 'timeline', 'timestamp', 'metcount', 'metsubpos'),
                                  Expression = list(quote(Bar == 0L))),
                       data.table(Argument = 'Key', Type = 'Keyed',
                                  Function = c('mint', 'hint', 'int', 'sonority'),
                                  Expression = list(quote(Key))),
                       data.table(Argument = 'Exclusive', Type = 'Exclusive',
                                  Function = c('mint', 'hint', 'int'),
                                  Expression = list(quote(Exclusive))),
                       data.table(Argument = 'BPM', Type = 'tempo',
                                  Function = 'timestamp',
                                  Expression = list(quote(BPM))),
                       data.table(Argument = 'Tandem', Type = 'Tandem',
                                  Function = 'tandem',
                                  Expression = list(quote(Tandem)))
)

setOldClass('quosure')
setOldClass('quosures')


####### Pitch  ----


#' humdrumR and pitch
#' 
#' [humdrumR][humdrumR::humdrumR] includes a number of intertwined data structures, and associated functions, 
#' for representing and manipulating musical pitch information.
#' 
#' @section Tonality:
#' 
#' There are four data types extensively used in `humdrumR` to encode/process [tonal](https://en.wikipedia.org/wiki/Tonality) musical information:
#' 
#' + [integers][base::integer] --- used to encode "[line-of-fifths](https://en.wikipedia.org/wiki/Circle_of_fifths)" tonal information
#' + [tonalInterval][tonalIntervalS4] --- embeds line-of-fifth tonal integers alongside [octave](https://en.wikipedia.org/wiki/Octave)
#'    and [cent]("https://en.wikipedia.org/wiki/Cent_(music)") information to encode most tonal pitch representations (solfege, intervals, letternames, etc.)
#' + [diatonicSet] --- combines line-of-fifth tonal integer representations to represent diatonic tonality, including alterations of basic diatonic scale(s).
#' + [tertianSet] --- an extension of `diatonicSet` used to encode  [tertian](https://en.wikipedia.org/wiki/Tertian) diatonic harmonies.
#' 
#' Users will rarely need to engage with these data types.
#' Rather, users will work with humdrum data where pitch information is encoded in strings, and wish to manipulate and analyze such data.
#' The most widely used `humdrumR` tools are your [pitch conversion/manipulation functions][pitchFunctions], including [kern()],
#' and functions like [invert()] and [transpose()].
#' These functions make use of sophisticated, and flexible pitch [parsing][pitchParsing] and [deparsing][pitchDeparsing] functions, 
#' which are the bridge between the "core" pitch representations listed above and real-world humdrum data.
#' 
#' 
#' @section Atonality:
#' 
#' **THIS SECTION IS INCOMPLETE**
#' 
#' In addition, there are xxx data types used to encode non-tonal (or [atonal](https://en.wikipedia.org/wiki/Atonality)) pitch information.
#' 
#' + [integers][base::integer] --- used to encode [semitones](https://en.wikipedia.org/wiki/Semitone) (as well as [MIDI](https://en.wikipedia.org/wiki/MIDI) numbers).
#' + [xxx][xxx] --- sets?
#' + [xxx][xxx] --- 12-tone rows?
#' 
#' @name humdrumPitch
NULL

# Lessons ----


#' What is "partial matching"?
#' 
#' @section Partial matching explained:
#'
#' `R` has a very useful functionality called "*partial matching*," where we can
#' match a **incomplete** character string or variable name with a list of options.
#' This is achieved using the base-`R` function [pmatch()], but many `R` functions make use of it,
#' as do *many* `humdrumR` functions.
#' 
#' For example, let's say we have a `data.frame` (let's call it `df`) with three columns: `"Number"`, `"Letter"`, and `"Date"`:
#' 
#' ```{r}
#' df <- data.frame(Number = 1:2, Letter = c('A', 'B'), Date = c("January", "February"))
#' ````
#' 
#' If I want to access the `Number` column, most programming languages would require I write at very least `df$Number`.
#' However, `R` will give me the correct field even if I write `df$Numb`, `df$Num`, or even `df$N`.
#' This is partial matching!
#' The matching happens left-to-right, so as long as I get the beginning of variable right, it will work.
#' 
#' Of course, partial matching only works up to the point that the string matches unambiguously.
#' For example, if added a `Dare` column to `df`, then `df$D` or `df$Da` would return `NULL` because they are ambiguous.
#' You'd need to write at least `Dar` or `Dat` to get the `Dare` and `Date` columns respectively.
#' 
#' @family {R lessons.}
#' @name partialMatching
NULL

#' "Evaluating" "Expressions" in "Environments"?
#' 
#' @section Expressions:
#' 
#' The term "expression" is just a fancy way of describing any bit of (valid) code than can be parsed
#' and evaluated (executed) by R.
#' For example, the following bits of code are all valid R "expressions":
#' 
#' + `2 + 2`
#' + `sqrt(2)`
#' + `x <- (1:10)^2`
#' + `log(x, base = 10) |> mean(na.rm = TRUE)`
#' + `sum((x - mean(x))^2)`
#' + ```
#'   { 
#'     x <- 1:10
#'     b <- mean(x)
#'     z <- x * z
#'   }
#'   ```
#' 
#' Expressions are frequently built of other expressions: so `2 + 2` is an expression, and `sqrt(2 + 2)` is an expression.
#' The `{ }` operators are used to group any valid expressions into one bigger expression.
#' You can also use `;` to write two expressions on the same line, like `x <- 2; log(x^2)`
#' 
#' @section Evaluation:
#' 
#' An expression like `sum((x - mean(x))^2)` is just a sequence of characters until we do something with it.
#' We call this "*evaluating*" the expression.
#' This exactly what the R "interpreter" does when you "run" some R code.
#' 
#' In R, an evaluated expression always "returns" a "*result*"---a value, like a number, `character` string, or some other data.
#' Some expressions might "return" `NULL` as their result, but it's still a result!
#' In a multi-line expression, like `{sqrt(2); 2 + 2}` or
#' 
#' ```
#' {
#'   x <- 2
#'   x^2
#' }
#' ```
#' 
#' the result of the overall expression is simply the result of the last expression---so the last two examples both return the result `4`.
#' 
#
#' 
#' @section Environment:
#' 
#' To evaluate an expression, R must look up any variable names in the expression in the current "*environment*";
#' For example, the expression `sum((x - mean(x))^2)` includes the variables `sum`, `mean`, and `x`.
#' The variables `sum` and `mean` are [base] R functions, so R will find them no problem (unless you [remove][rm()] them).
#' However, `x` is not generally going to be "defined" unless *you've* defined it.
#' If you try to evaluate `sum((x - mean(x))^2)`, you'll get an error if `x` is not defined.
#' 
#' There are many different "[environments][environment]" in R, where R will search for variables:
#' When you run R, you can save variables in the *global environment*; R-packages have their own environments;
#' *every* time you call a function, 
#' the function has its *own* environment inside it---this is why a function can "see" it's own arguments,
#' but variable you save inside a function isn't "visible" outside the function.
#' 
#' One of the greatest features of R is that we can often tell R to evaluate an expression using
#' a specific [data.frame] as the environment, so we can use the column names of our data as variables.
#' The humdrumR [with(in)][withinHumdrum] (and their tidyverse equivalents) use this functionality a lot!
#' 
#' ### Incomplete expressions
#' 
#' One of the most annoying things that can happen in R is if you try running something and it kind just hangs,
#' getting stuck with nothing happening no matter how many times you press enter.
#' This is usually because you have (accidentally) provided R and *incomplete* expression.
#' For example, `2 + ` is an incomplete expression---that `+` needs a number after it!
#' Failing to have properly paired parentheses will often result in incomplete expressions:
#' For example, `mean(sqrt(log(x))` is an incomplete expression!
#'
#' @family {R lessons.}
#' @name evaluatingExpressions
NULL


#' What are "recycling" or "padding"?
#' 
#' @section Recycling and Padding results:
#' 
#' Many R functions will "recycle" their results; other functions will "pad" their results.
#' What does this mean?
#' The two options refer to different strategies R code often provides to 
#' maintain [vectorization][vectorization].
#' The key idea of vectorization is that we want the length/size of function inputs and outputs to be the same.
#' If I give a 100-long vector to a function as input, I want a 100-long vector as output.
#' 
#' #### Result is too short (pad or recycle)
#' 
#' What happens if I have a that outputs a result that is *shorter* than the input, but I **need**
#' it to be the same length?
#' Well, one option is to "pad" the output with `NA` values.
#' For example, if I call `mean(1:9)`, my 9-long input results in a scalar (1-long) output (`5`).
#' To force the output to be 9-long (to match the input), I could pad it with `NA` like `c(5, NA, NA, NA, NA, NA, NA, NA, NA)`.
#' 
#' In many cases, padding a result like `mean(1:9)` is not very useful---the useful information (the mean, in this case)
#' is stuck at only one index!
#' Instead, it is more useful to "recycle" the result.
#' What we do is take the result and duplicate it ("recycle") over and over again until it matches the length of the
#' input.
#' So we'd recycle the result of `mean(1:9)` to be `c(5, 5, 5, 5, 5, 5, 5, 5, 5)`.
#' This is often (but not always) very useful!
#' 
#' The most common (and best) case for recycling is, like our example above, when the result we recycle is a "scalar" (single)
#' value.
#' That one value is simply copied `length(input)` times.
#' We can also recycle results that are non-scalar.
#' For example, imagine we have a function `summ()` which calculates the minimum, median, and maximum of a vector:
#' the call `summ(1:9)` would return `c(1, 5, 9)`.
#' This result could be recycled to get `c(1, 5, 9, 1, 5, 9, 1, 5, 9)`.
#' This sort of recycling is less likely to be useful, and can be confusing sometimes---for example, there is
#' probably no meaningful reason why the median should line up with the original input values `c(2, 5, 8)`.
#' However, R will (generally) happily do it for you!
#' It is good practice to *only* rely on scalar recycling, and avoid non-scalar recycling unless you are really
#' sure it is what you want.
#' 
#' One final note: if the result you are recycling isn't a length which evenly divides the input, you will see an *warning* message
#' saying `longer object length is not a multiple of shorter object length`.
#' To give an example, image if we used the `range()` function, which returns the minimum and the maximum but not the median, on our input:
#' the result of `range(1:9)` is `c(1, 9)`, and this would recycle as `c(1, 9, 1, 9, 1, 9, 1, 9, 1)`.
#' The last repetition of the result is cut short, because two does not evenly divide nine.
#' Since non-scalar recycling of results is often not useful or meaningful in *general*, R takes it
#' as a particularly bad sign that your result does not evenly divide your input, which is why you get a warning.
#' 
#' 
#' #### Result is too long (index)
#' 
#' What happens if I have a function that outputs a result that is *longer* than the input, but I **need**
#' it to be the same length?
#' Well, the most obvious thing to do is cut off the excess---so something like `head(output, n = length(input)`.
#' Of course, that may or may not make sense depending
#' on what the function is doing!
#' 
#' @family {R lessons.}
#' @name recycling
#' @aliases padding
NULL

#' What is "vectorization"?
#'
#' @section Vectorization explained:
#' 
#' Many R operations/functions are "vectorized," meaning that they take in vectors and output vectors that are the same length.
#' This means that we, as programmers, don't need to worry about each element of the vector;
#' We can treat a vector like a single object, and R will oblige us.
#' For example, we can do math like:
#'
#' ```
#'
#' 2^(0:10) - 1
#'
#' (1:10) - (10:1)
#'
#' sqrt(c(5, 10, 16))
#' ```
#'
#' Or work with strings like:
#'
#' ```
#' paste(1:26, letters, sep = ': ')
#'
#' paste('Chord', 1:10)
#'
#' # Regular expressions:
#' grepl('[aeiou]', letters)
#' 
#' ```
#'
#' Or get logical values:
#'
#' ```
#' 2^(0:100) > 50
#'
#' 1:10 %% 2 == 0
#'
#' 1:20 %in% 2^(0:4)
#' 
#' ```
#' 
#' Of course, other R functions take in vectors and return totally new vectors (or just scalars).
#' Examples:
#'
#' ```
#'
#' length(seq(50, 90, by = .2))
#'
#' length(letters) # letters is a built-in vector which is always there!
#'
#' sum(c(1, 5, 9))
#' mean(c(1, 5, 9))
#' max(c(1, 5, 9))
#'
#' range(c(1, 100, 2, -4))
#' which(c(TRUE, FALSE, TRUE, TRUE))
#' 
#' ```
#'
#' Vectorization works very well when you are working with vectors that are either 1) all the same length or 2) length 1 (scalar).
#' If vectors are different lengths, the shorter one will be "recycled" (repeated) to match the longer one.
#'
#' ```
#' c(0, 5) * 1:10
#' ```
#' 
#' @family {R lessons.}
#' @name vectorization
NULL

#' What are "grouping factors"?
#' 
#' @section Grouping and "split-apply-combine" explained:
#' 
#' The concept of "grouping factors" is widely used in R, allowing
#' us to quickly *split* datasets ([vector]s or [data.frame]s) into subgroups, 
#' work with the subgroups independent (*apply* functions to them), 
#' and then re*combine* them as needed.
#' Various R functions specify "grouping factors" in a confusing
#' variety of subtly different ways, usually as function arguments named
#' things like `INDEX`, `INDICES`, `f`, `by`, or `groupby`.
#' In `humdrumR`, we adopt the tidyverse [dplyr] approach, using the
#' `group_by()` function (and/or the `.by` argument).
#' 
#' Any [atomic vector][vector] with at least two 
#' unique values, or "levels", can be used as a grouping factor---generally,
#' grouping vectors are coerced into [factor]s.
#' Each unique level in a grouping vector/factor represents a single group.
#' Any vector, or [data.frame] that is *the same length/height* as the grouping factor
#' can then be broken into these groups, taking all the indices where the grouping factor
#' equals each group in turn.
#' Since we generally try to work with data.frames, which by definition contain a bunch
#' of vectors that are the same length, we can use any vector/column in a data.frame
#' to group any of the other vectors, or the rows of the whole data.frame.
#' 
#' Most functions allow you to specifiy multiple grouping factors/vectors (so long as they are all
#' the same length).
#' The groups are then defined by every *unique combination* of elements in the vectors.
#' So, for example, if we use the vectors `c('A', 'A', 'A', 'B', 'B', 'B')` and 
#' `c(1, 1, 2, 2, 3, 3)` as grouping factors, we'll get four groups with levels `1A`, `2A`, `2B`, and `3B`.
#' 
#' Note that groups created by grouping factors are *not* neccessarily contiguous.
#' If we use a vector like `c(1, 1, 2, 2, 1, 1)` as grouping factor, we get two groups: `1` and `2`.
#' The `1` group would include the 1st, 2nd, 5th, and 6th indices, even though they are 
#' separated in the grouping factor.
#' If you *want* contiguous groups you must make them.
#' The `humdrumR` function [segments()] can be used to generate strictly contiguous grouping factors.
#' For example, `segments(c(1, 1, 2, 2, 1, 1))` will return `c(1, 1, 2, 2, 3, 3)`.
#' 
#' @family {R lessons.}
#' @name groupingFactors
NULL

# Options -----

humdrumR_defaults <- list(
  view = 'humdrum',
  dataTypes = 'GLIMDd',
  maxRecordsPerFile = 40L,
  maxTokenLength = 20L,
  nullPrint = 'NA2dot',
  syntaxHighlight = TRUE,
  censorEmptyRecords = 30L
)

humdrumRoptions <- function() options('humdrumR_options')[[1]]
humdrumRoption <- function(name) {
  opts <- humdrumRoptions()
  opts[[pmatch(name[1], names(opts))]]
}

#' Change humdrumR global parameters
#' 
#' The `humdrumR()` *function* is used to set 
#' global package options within an R session, mostly regarding
#' the viewing of [humdrumR datasets][humdrumRclass].
#' 
#' @param view ***How should humdrumR data be printed?***
#' 
#' There are three options: `"humdrum"`, `"score"`, and `"table"` (aliases `"data.frame"` and `"tibble"`).
#' These options are [partially matched][partialMatching].
#' 
#' Use [select()] to determine which fields to show.
#' 
#' @param dataTypes ***Which types of humdrum record(s) to view.***
#' 
#' Defaults to `"GLIMDd"` for `as.lines()` and `as.matrix()`; `"Dd"` for `as.data.frame()`;
#' `"LIMDd"` for `as.matrices()` and `as.data.frames()`.
#' 
#' Must be a single `character` string. Legal values are `'G', 'L', 'I', 'M', 'D', 'd'` 
#' or any combination of these (e.g., `"LIM"`).
#' (See the [humdrum table][humTable] documentation for explanation.)
#' 
#' @param maxRecordsPerFile ***How many records should be shown in each file, when more than one file is present?***
#' 
#' Defaults to `40`.
#' 
#' Can be any positive whole number.
#' 
#' @param maxTokenLength ***Length at which longer tokens are censored with ...***
#' 
#' Defaults to `16`.
#' 
#' Can be any positive whole number.
#' 
#' @param nullPrint ***How should null data points print?***
#' 
#' Default is `"NA2dot"`.
#' 
#' Must be a single character string, [partially matching][partialMatchng] `"NA2dot"`, `"dot2NA"`, `'charNA2dot"`, or `"asis"`.
#' `"NA2dot"` means all `NA` values are converted to `"."`; `"dot2NA` means all `"."` are converted to `NA`; `charNA2dot` means `NA` values
#' in `character` vectors are converted to `NA`, but not in other atomic types; `"asis"` means either `NA` or `"."` values may print, depending
#' on what is in the field.
#' 
#' @param syntaxHighlight ***Should syntax highlighting (coloring) be used in printout?***
#' 
#' Defaults to `TRUE`.
#' 
#' Must be a singleton logical value; an on/off switch.
#' 
#' @param censorEmptyRecords ***Should consecutive records be "censored" (compressed) in printout?***
#' 
#' Defaults to `30`.
#' 
#' Can be any positive whole number, up to `Inf`.
#' If `Inf`, no censoring will occur.
#' 
#' @examples
#' 
#' # change default view to table
#' humdrumR("table")
#' 
#' humdrumR(view = 'humdrum', maxRecordsPerFile = 50)
#' 
#' # see the humdrumR package directory contents
#' dir(humdrumRroot) 
#' 
#' @rdname humdrumR
#' @export
humdrumR <- function(view, dataTypes, maxRecordsPerFile, maxTokenLength, nullPrint, syntaxHighlight, censorEmptyRecords) {
  curOptions <- oldOptions <- humdrumRoptions()
  

  if (!missing(view)) {
    view <- checks(view, xplegal(c('score', 'table', 'tibble', 'humdrum', 'data.frame')))
    curOptions$view <- match.arg(view, c('score', 'table', 'tibble', 'humdrum', 'data.frame'))
  }
  if (!missing(dataTypes)) {
    dataTypes <- checkTypes(dataTypes, 'humdrumR')
    curOptions$dataTypes <- dataTypes
  }
  if (!missing(maxRecordsPerFile)) {
    checks(maxRecordsPerFile, xwholenum & xpositive)
    curOptions$maxRecordsPerFile <- maxRecordsPerFile
  }
  if (!missing(maxTokenLength)) {
    checks(maxTokenLength, xwholenum & xpositive)
    curOptions$maxTokenLength <- maxTokenLength
  }
  if (!missing(nullPrint)) {
    checks(nullPrint, xcharacter & xlen1 & xvalues('NA2dot', 'charNA2dot', 'asis', 'dot2NA'))
    curOptions$nullPrint <- nullPrint
  }
  if (!missing(syntaxHighlight)) {
    checks(syntaxHighlight, xTF)
    curOptions$syntaxHighlight <- syntaxHighlight
  }
  if (!missing(censorEmptyRecords)) {
    checks(censorEmptyRecords, xTF | (xwholenum & xpositive))
    
    if (is.logical(censorEmptyRecords)) censorEmptyRecords <- if (censorEmptyRecords) 30L else Inf
    curOptions$censorEmptyRecords <- censorEmptyRecords
  }
  
  options(humdrumR_options = curOptions)
  
  invisible(oldOptions)
}



# Default settings ----

oldoptions <- options()
# options(conflicts.policy = 'depends.ok')


.onLoad <- function(libname, pkgname) {
  oldoptions <<- options()
  options(#prompt = 'humdrumℝ> ', continue = 'humdrumℝ... ', 
          scipen = 4, digits = 7, humdrumR_options = humdrumR_defaults)
}

.onAttach <- function(libname, pkgname) {
  packageStartupMessage('Welcome to humdrumℝ!\n', '\tYou are using humdrumℝ version ', 
      as.character(packageVersion('humdrumR')))
}

.onUnload <- function(libpath) {
  cat('Thanks for using humdrumℝ!\n')
  do.call('options', oldoptions)
}

#' @export
humdrumR_version <- as.character(packageVersion('humdrumR'))

## Bootswatch flatly in plots ----

flatly <- c('#18BC9C', '#F39C12', '#3498DB', '#E74C3C', '#2C3E50')
flatly_continuous <- function(n, k = 2, alpha = 1) {
  structure(setalpha(colorRampPalette(flatly[k + 0:1])(n), alpha = alpha),
            name = 'flatly_continuous', class = 'palette')
}
flatly_discrete <- function(n, alpha = 1) {
  structure(setalpha(colorRampPalette(flatly)(n), alpha = alpha), name = 'flatly_discrete', class = 'palette')
}
# setHook('plot.new', function() {
#   par(family = 'Lato', col.main = flatly[5], col.axis = flatly[5], col.sub = flatly[5],
#       col.lab = flatly[5])
#   })




