# humdrumR 7.0.3

### count() and table()


We wanted to use the `tally()` as humdrumR's main "count stuff" function.
Unfortunately, we found that `tally()` already exists in `dplyr` and it (the generic `tally`) can't really be extended as we'd like.
We've reimplemented everything focusing on an extension of the `dplyr::count()` function.
This also necessitated renaming a *our* (metric) `count()` function to `timecount()`.
Our `count()` method now generates a cool "distribution" table, which is easily to manipulate.
This is all mostly working pretty well, but is still a work in progress and not documented.

We've also extended `base::table()` to work with humdrumR data, but we'll be emphasizing using `count()` in our documentation.

### harte()

We've implemented parsing and deparsing of the [Harte](https://github.com/Computational-Cognitive-Musicology-Lab/Star-Wars-Thematic-Corpus) syntax for representing chords.
All our chord functions should be able to read harte notation, and there is now a `harte()` function for outputting (deparsing) **harte.


# humdrumR 7.0.0


HumdrumR 0.7.0.0 includes some pretty major changes from the previous (0.6.x.x) versions.
Notably, we have updated the whole package to make it more consistent with the [Tidy-verse](https://www.tidyverse.org/).
We've also incorporated an option to use humdrumR functions in a super-concise manner which is closely modeled off the original humdrum toolkit.
Finally, we have changed humdrumR data objects to behave a little more like "normal" R data.frames (or tibbles), and allowed you to more easily "see" and manipulate your data as a data.frame.
Our reference manuals and articles have all been updated to reflect these changes.



## Tidy-verse style


HumdrumR has been changed in a few ways align more with the Tidy-verse, and particular, the [dplyr](https://dplyr.tidyverse.org/) package.
The following `dplyr` packages now have humdrumR methods:

+ `select()`
+ `filter()`
+ `mutate()`
+ `reframe()`
+ `summarize()`
+ `group_by()`
+ `pull()`


### select()

The biggest change to the package is the use of the `select()` function.
In humdrumR, we have dropped the use of term "Active Field" in favor of the term "Selected Field."
Use the `select()` function to select fields---you can no longer use `$` for this purpose.


### filter()

You can now call `filter()` as an alternative to `subset()`---they work exactly the same!


### mutate(), summarize(), and reframe()

We now use the `dplyr` "verbs" `mutate()`, `summarize()`, and `reframe()` as our main tools for manipulating humdrumR data.
These commands work *very* similarly to the `within()` and `with()` functions from previous humdrumR versions, but are slightly different in how they recycle results.
The base-R `with()` and `within()` functions can still be used as before---in fact, `with()` is still particularly useful sometimes, because it is basically the same as going `reframe() |> pull()` in one step.

### group_by()

Instead of providing a `by = ` argument to humdrumR functions, you should now call `group_by()` as a separate command.
Your data will stay grouped until you call `ungroup()`.

### pull()

The `dplyr` "verb" `pull()` can also be used to extract a field from a humdrumR dataset.
There are also functions `pull_data.frame()`, `pull_data.table()` (requires [data.table](https://rdatatable.gitlab.io/data.table/), and `pull_tibble()` (requires [tibble](https://tibble.tidyverse.org/)), to pull multiple fields into a "normal" R data.frame/data.table/tibble.


Note that the `$` command has been changed to be consistent with base-R and the Tidyverse.
This command will now simply extract a field from humdrumR data.



## Humdrum style

A new feature is that many of humdrumR's functions can be applied directly to humdrumR data, without using `mutate()`, `within()`, etc.
For example, you can take some humdrum data and pipe it directly to the `kern()` function, like:  `humData |> kern()`.


## Data.frame view

A new option in humdrumR 0.7.0.0 is to view humdrumR data a normal R data.frame, instead of showing the data as a humdrum-syntax score.
Call the command `humdrumR("data.frame")` once to switch to the new view.
When in "data.frame view," humdrumR will still only show you the selected fields.
If you want to see all the fields, use `select(everythign())`.

If you want to switch back to the "humdrum view" (the default), call `humdrumR("humdrum")`.


