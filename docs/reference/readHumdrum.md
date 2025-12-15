# Find and read humdrum files into R

These functions find valid humdrum files on your local machine and read
them into `humdrumR`.

## Usage

``` r
knownInterpretations

findHumdrum(
  ...,
  contains = NULL,
  recursive = FALSE,
  allowDuplicates = FALSE,
  verbose = FALSE
)

readHumdrum(
  ...,
  recursive = FALSE,
  contains = NULL,
  allowDuplicates = FALSE,
  verbose = FALSE,
  tandems = "known",
  reference = "all"
)
```

## Format

An object of class `data.table` (inherits from `data.frame`) with 24
rows and 5 columns.

## Arguments

- ...:

  ***One or more patterns used to identify files to read.***

  Must be `character` strings.

  For details: see the "REpath-patterns" section below.

- contains:

  ***REGEX for filtering files.***

  Defaults to `NULL`.

  Must be `character`.

  If `!is.null(contains)`, the `contains` argument is is treated as
  regular expressions: only files which contain matches to *all* of
  these regular expressions are read. Thus,
  `readHumdrum('.*krn$', contains = "EEE")` will only read kern files
  which contain matches to `"EE"`—which is kern for the E two octaves
  below middle C (or lower).

- recursive:

  ***Should files be found recursively through sub directories?***

  Defaults to `FALSE`.

  Must be a singleton `logical` value: an on/off switch.

  If `TRUE`, the final part of the search pattern (i.e., the file
  search) is searched for recursively through all sub directories.

- allowDuplicates:

  ***Indicating what should happen if multiple search patterns match the
  same files.***

  Defaults to `FALSE`.

  Must be a singleton `logical` value: an on/off switch.

  If `allowDuplicates = TRUE`, any such files are read multiple times,
  grouped into their respective corpora by the `Label` field. If
  `allowDuplicates = FALSE`, any redundant files are only read into the
  corpus of the first pattern they match.

- verbose:

  ***Whether to print filename while reading or not.***

  Defaults to `FALSE`.

  Must be a singleton `logical` value: an on/off switch.

  If `TRUE`, the names of matching files are printed before parsing
  begins. This is very useful as a check to make sure you aren't reading
  the wrong files!

- tandems:

  ***Controls which, if any, tandem interpretations are parsed into
  their own fields.***

  Defaults to `"known"`.

  Must be `character`.

- reference:

  ***Which reference records should be parsed into fields.***

  Defaults to `"all"`.

  Must be `character`.

## Details

`findHumdrum` does the work of finding and reading the text files into
`R`. `readHumdrum` utilizes `findHumdrum` to read files, then parses
them to create a [humdrum
table](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humTable.md) and
build a [humdrumR data
object](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humdrumRclass.md)
around the table.

## REpath-patterns

"REpath-patterns" are specified using `...` arguments. In combination,
all the `...` arguments are used to search for file paths. Each part of
the search path you specify (`"dirpart/dirpart/filepart"`, etc) are
matched as regular expressions against directories/files on your disc.
Thus, we can say things like `findHumdrum("../^A.*/.*krn$")`, which
would match any kern files in any directory beginning with a capital
`"A"` in the directory above the current working directory. For
conveniance, you can break the path across multiple arguments instead of
using delimited strings: For example, the code
`findHumdrum("..", "^A.*", ".*krn$")` will give an identical result as
the previous example (`findHumdrum("../^A.*/,*krn$")`). This is useful
when searching for more than one pattern (see next paragraph) in the
same directory.

If you want to search for *more than one* pattern, you can input them as
a character vector: For instance,
`readHumdrum(c("mozart", "beethoven")`—this command will search for
filenames containing "mozart" OR "beethoven." This works for directories
too: `readHumdrum(c("Mozart", "Beethoven"), ".*krn$")` will look for any
kern files in directories containing "Mozart" OR "Beethoven." If
patterns are named, these names will show up as identifying patterns in
the
[humdrumR](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humdrumRclass.md)
object's `Label` field. Unnamed patterns are simply labeled with
numbers. We refer to files matched from regex patterns to be
"subcorpora" of the total corpus.

Normal (system appropriate) conventions (i.e., directories separated by
`"/"`, `'~'` at beginning to indicate home, `".."` to indicate directory
above working directory, etc.) are followed. If a pattern contains a
solo dot followed by a file sep—e.g., `"./"`, `"x/./y"`—this is treated
as the current directory, not a regular expression. If a pattern
contains two dots—e.g., `"../"`—this is treated as the directory above,
not a regular expression. If you want to create a regular expression to
match any directory, use `".*/"`.

The regex pattern `""` matches any file (it is changed to `".*"`). If
you don't specifiy any `...` argument, `findHumdrum` (or `readHumdrum`)
will default to `".*"` as well. Thus, `readHumdrum()` will read any
humdrum files in the working directory.

(If two or more files in different directories share the same name, a
unique name is created for each file by appending the names of the
directories they occupy, recursively until the names are unique.)

If a single humdrum file has multiple pieces in it—meaning that all
spine paths close with `*-`, then open again with `**`—then they are
parsed separately. They are distinguished in the `Piece` field. If there
are no multi-piece files, `Piece` and `File` will be identical.

## Validity

`findHumdrum` and `readHumdrum` automatically ignore non-text files.
What's more, any files which contain humdrum syntax errors (checked by
[`validateHumdrum()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/validateHumdrum.md))
are automatically skipped. If you want to see specifically what errors
occurred, call
[`validateHumdrum()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/validateHumdrum.md)
directly and use its `errorReport.path` argument.

## Tandem Interpretations

All tandem interpretations in a humdrum dataset are summarized in the
[humdrum
table's](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humTable.md)
`Tandem` field, which is described in detail
[here](https://humdrumR.ccml.gtcmt.gatech.edu/reference/tandem.md). In
addition, certain "known" tandem interpretations are parsed into their
*own* fields automatically. For example, `*clefG4` and "`*clefF2` are
parsed as `Clef` data, while `*k[b-]` is parsed as a `KeySignature`. The
"known" tandem interpretations that `humdrumR` recognizes are encoded in
a built-in table called `knownInterpretations`. Each interpretation has
a humdrumR name (`"Clef"`, `"TimeSignature"`, etc.) as well as a regular
expression associated with it.

The `tandems` argument to `readHumdrum` controls which tandem
interpretations are parsed into their own fields. This can be helpful to
either save processing time and memory by *not* parsing interpretations
you won't need, or to parse interpretations that humdrumR doesn't
recognize. The default value for the `tandems` argument is `"known"`. If
the `tandems` argument contains `"known"` *all* tandem interpretations
in the built-in `knownInterpretations` table are parsed. Users may
specify different interpretations to parse in two ways:

1.  character strings matching one of the name values from the `Name`
    column of `knownInterpretations`. For instance, if you specify
    `tandems = c('Clef', 'TimeSignature')`, only clef (e.g.,
    `"*clefG2"`), and time signature (e.g., `"*M3/4"`) intepretations
    will be parsed.

2.  if the character string(s) in `tandem` do not exactly match one of
    the names in `knownInterpretations$Name`, they are treated as
    regular expressions and used to match tandem interpretations in the
    data. This allows users to parse non-standard tandem interpretations
    that humdrumR doesn't already know about.

If any values in `tandems` are named, these names will be used for
resulting fields. If no matches to an given interpretation are found, no
field is created for that interpretation. If `tandems = NULL`, then no
tandem interpretations are parsed.

## Reference Records

By default (`reference = "all"`), humdrumR reads all reference records
in the data. The reference code for each record (e.g, the "OTL", in
"!!!OTL: xxx") is used as the name of an associated field. (If a
reference record has no reference code (i.e., it lacks a colon), the
field is called "Unkeyed.") In large datasets with many reference
records, the reference data can actually make up a large portion of the
humdrum table, and eat up a lot of memory. In these cases, we might not
want to read all (or any) reference records—we can instead read only the
reference records that we are planning to use in our analyses (if any).
If `reference = NULL`, no reference records are parsed. Otherwise, the
character values of `reference` are treated as reference codes and only
matching reference records are parsed. For instance,
`readHumdrum(_, reference = "OTL")` will *only* parse OTL reference
records. If the values of `reference` are named, these names are used to
name associated fields. Thus, by specifing
`reference = c(Title = 'OTL')`, you can use "OTL" reference records to
populate a field called "Title".

If there are more than one reference records with the same reference
code, either explicitely numbered (e.g., "!!!COM1:", "!!!COM2:") all are
read and rather than making two or more fields, a single field is
created ("COM" in this) with the multiple values separated by ";".

If your humdrum data includes files containing multiple pieces, special
consideration is needed to determine (or guess) which reference records
(or other global comments) "go with" which piece. Obviously, reference
records at the beginning and end of each file are grouped with the first
and last pieces respectively. However, reference records that are
between pieces in any multi-piece file require some guess work.
`readHumdrum()` will look at reference codes and attempt to group
in-between reference records into pieces in a logical way by avoiding
duplicated reference codes.

## Spines and Paths

In the [humdrum syntax](http://www.humdrum.org/guide/ch05/), data is
placed in "spines," which are not the same as "columns" in a
spreadsheet. A "column" refers to a tab-delineated group of values.
"Spines" can be a single column, or they may (at any time) split into
multiple columns, which can in turn split again, using the `"*^"`
interpretation token. The reverse can happen as well, with two or more
columns merging into a single column, using the `"v"` token. This means
that, while humdrum data at first glance looks like a simple
two-dimensional table, it is actually a flexible tree structure. As
spines split and merge, the total number of columns can change during a
piece, creating a "ragged" edge. Another similar issue is that a corpus
of humdrum files may have varying numbers of spines/columns, between
pieces. ("Global" comment/reference records are also a special case, as
that are always a single value, even if interspersed with multi-column
local records.) `readHumdrum` assumes a slightly more strict version of
the humdrum syntax: that all the spines which appear at the beginning of
a file (headed with exclusive interpretations like `"**kern"`) can never
merge into each other. Thus, a humdrum file read into `humdrumR` must
not end with fewer columns than it starts. Spine merges (`"*v"`) can
only happen within spine paths that originally split off the same spine.
This extra-strict specification of spine paths in the humdrum syntax is,
fortunately, something that has been informally followed in most humdrum
datasets.

Our strict spine-path definition makes everything work fairly simply:
Within a piece, the spines which appear at the beginning of the piece
are the "true" spines throughout the piece, numbered from left to right,
starting from `1L`. For each local token, the value in the `Spine` field
is an integer indicating which of these "true" spines it belongs
to—global tokens have a `NA` value in their `Spine` field, because they
do not belong to any spine. Any spine path splits (`"*^"`) from the main
spines form **spine paths**. Every spine's paths are numbered in the
`Path` field, from right to left, starting from `0L`. A spine with no
splits will have all `0L`s in its `Path` field.

## Result

`findHumdrum` returns a "fileFrame" (`data.table`), listing all file
names, the patterns they match, the directories they were found in,
*and* the raw text content of these files.

`readHumdrum` returns a fully parsed [humdrumR
object](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humdrumRclass.md).

## Examples
