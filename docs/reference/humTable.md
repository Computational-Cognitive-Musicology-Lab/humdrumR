# Humdrum tables (and their "fields")

In the
[humdrumR](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humdrumR.md)
package, the fundamental data structure is called a **humdrum table**. A
humdrum table encodes all the information in a collection of one or more
humdrum-syntax files as a single
[data.table](https://rdrr.io/pkg/data.table/man/data.table.html) (A
`data.table` is an "enhanced" version of R's standard
[data.frame](https://rdrr.io/r/base/data.frame.html)). Humdrum tables
are stored "inside" every
[humdrumRclass](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humdrumRclass.md)
object that you will work with, and various `humdrumR` functions allow
you to study or manipulate them. If you want to directly access the
humdrum table within a
[humdrumRclass](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humdrumRclass.md)
object, use the `getHumtab()` function.

The `getHumtab()` function extracts the humdrum table from a [humdrumR
object](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humdrumRclass.md).

Use the `fields()` function to list the current fields in a
[humdrumRclass](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humdrumRclass.md)
object.

## Usage

``` r
getHumtab(humdrumR, dataTypes = "GLIMDd")

fields(
  humdrumR,
  fieldTypes = c("Data", "Structure", "Interpretation", "Formal", "Reference",
    "Grouping", "selected")
)

# S3 method for class 'humdrumR'
names(humdrumR)
```

## Arguments

- humdrumR:

  ***HumdrumR data.***

  Must be a [humdrumR data
  object](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humdrumRclass.md).

- dataTypes:

  ***Which types of humdrum record(s) to include in the output.***

  Defaults to `"GLIMDd"`.

  Must be a `character` string, which specifies which types of data
  tokens/records to extract. Legal values are: `"G"` (global comments),
  `"L"` (local comments), `"I"` (interpretations), `"M"` (barlines),
  `"D"` (non-null data), or `"d"` (null data). Multiple types can be
  specified in a single string: e.g., `"GLIMD"`. Note that `"I"` also
  grabs `"E"` (exclusive) and `"S"` (spine-control) tokens.

- fieldTypes:

  ***Which types of fields to list.***

  Shows all fields by default.

  Must be a `character` vector. Legal options are `"Data"`,
  `"Structure"`, `"Interpretation"`, `"Formal"`, `"Reference"`, and
  `"Grouping"`. You can also pass `"selected"` to extract only the
  [selected
  fields](https://humdrumR.ccml.gtcmt.gatech.edu/reference/selectedFields.md).
  Types can be [partially
  matched](https://humdrumR.ccml.gtcmt.gatech.edu/reference/partialMatching.md)—for
  example, `"S"` for `"Structure"`.

## Details

In a humdrum table, by default, humdrum data is organized in a maximally
"long" (or "tall") format, with each and every single "token" in the
original data represented by a single row in the table. Even
multiple-stops—tokens separated by spaces—are broken onto their own
rows. Meanwhile, each column in the humdrum table represents a single
piece of information associated with each token, which we call a
**field**. Throughout this documentation, you should keep in mind that a
"token" refers to a *row* in the humdrum table while a "field" refers to
a *column*:

- Token = row

- Field = column

## Fields:

There are six types of fields in a humdrum table:

1.  Data fields

2.  Structure fields

3.  Interpretation fields

4.  Formal fields

5.  Reference fields

6.  Grouping fields

When first created by a call to
[`readHumdrum()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/readHumdrum.md),
every humdrum table has at least nineteen fields: one data field
(`Token`), two interpretation fields (`Tandem` and `Exclusive`), three
formal fields, and thirteen structure fields. Additional formal,
interpretation, or reference fields *may* be present depending on the
content of the humdrum file(s), and you can create additional data
fields by using
[within.humdrumR()](https://humdrumR.ccml.gtcmt.gatech.edu/reference/withinHumdrum.md),
[`mutate.humdrumR()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/withinHumdrum.md),
or other functions.

### Data fields:

*Data* fields are used to describe individual data points in humdrum
data (as opposed to groups of points). Every humdrum table starts with a
data field called **Token**, which contains `character` strings
representing the original strings read from the humdrum files. Users can
create as many additional data fields as they like. Every call to
[`withinHumdrum()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/withinHumdrum.md)
generates new data fields.

### Structure fields:

Every humdrum table has thirteen *Structure* fields, which describe
where each data token was "located" in the original humdrum data: which
file, which spine, which record, etc. See the vignette on humdrum syntax
to fully understand the terms here.

- *File info*:

  - `Filename` :: `character`

    - The unique name of the humdrum file. This may include an appended
      path if more than one file with the same name were read from
      different directories (see the
      [`readHumdrum()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/readHumdrum.md)
      docs).

  - `Filepath` :: `character`

    - The full file name (always includes its full path).

  - `Label` :: `character`

    - A label specified during the call to
      [`readHumdrum()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/readHumdrum.md),
      associated with a particular `readHumdrum` "REpath-pattern." If no
      label was specified, patterns are just labeled `"_n"`, where "`n`"
      is the number of the pattern.

  - `File` :: `integer`

    - A unique number associated with each file (ordered alphabetically,
      starting from `1`).

  - `Piece` :: `integer`

    - A number specifying the number of the *piece* in the corpus. This
      is identical to the `File` field except when more than one piece
      were read from the same file.

- *Location info*:

  - `Spine` :: `integer`

    - The spine, numbered (from left-to-right) starting from `1`.

    - This field is `NA` wherever `Global == TRUE`.

  - `Path` :: `integer`

    - The "spine path." Any time a `*^` spine path split occurs in the
      humdrum data, the right side of the split becomes a new "path."
      The original path is numbered `0` with additional paths numbered
      with integers to the right. (If there are no spine path splits,
      the `Path` field is all `0`s.)

    - This field is always `NA` when `Global == TRUE`.

  - `ParentPath` :: `integer`

    - For spine paths (i.e., where `Path > 0`), which path was the
      parent from which this path split? Where `Path == 0`, parent path
      is also `0`.

  - `Record` :: `integer`

    - The record (i.e., line) number in the original file.

  - `DataRecord` :: `integer`

    - The *data* record enumeration in the file, starting from `1`.

  - `Stop` :: `integer`

    - Which token in a multi-stop token, numbered starting from `1`.

    - In files with no multi-stops, the `Stop` field is all `1`s.

    - This field is always `NA` when `Global == TRUE`.

  - `Global` :: `logical`

    - Did the token come from a global record (as opposed to a local
      record)?

    - When `Global == TRUE`, the `Spine`, `Path`, and `Stop` fields are
      always `NA`.

- *Token info*:

  - `Type` :: `character`

    - What type of record is it?

      - `"G"` = global comment.

      - `"L"` = local comment

      - `"I"` = interpretation

      - `"M"` = measure/barline

      - `"D"` = non-null data

      - `"d"` = null data

      - `"E"` = exclusive interpretation

      - `"S"` = spine-control tokens (`*^`, `*v`, `*-`)

### Interpretation fields:

*Interpretation* fields describe interpretation metadata in the humdrum
file(s). Humdrum interpretations are tokens that "carry forward" to data
points after them, unless cancelled out by a subsequent interpretation.
(See the humdrum syntax vignette for a detailed explanation.) *All*
humdrum data must have an *exclusive* interpretation so humdrum tables
always have an `Exclusive` (:: `character`) field indicating the
exclusive interpretation associated with each token/row of the `Token`
field.

Humdrum data may, or may not, include additional *tandem*
interpretations. A universal rule for parsing tandem interpretations is
impossible, because A) tandem interpretations can "overwrite" each other
and B) users can create their own tandem interpretations. The best we
can do in all cases is to identify *all* tandem interpretations that
have appeared previously in the spine (counting most recent first). All
these previous interpretations are encoded in a single character string
in the `Tandem` field (see the
[`tandem()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/tandem.md)
docs for details). If working with non-standard interpretations, users
can parse the `Tandem` field using the
[`tandem()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/tandem.md)
function. If no tandem interpretations occur in a file, the `Tandem`
field is full of empty strings (`""`).

Fortunately, many tandem interpretations are widely used and
standardized, and these interpretations are known by `humdrumR`.
Recognized interpretations (such as `*clefG4` and `*k[b-]`) are
automatically parsed into their own fields by a call to
[`readHumdrum()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/readHumdrum.md).
See the
[`readHumdrum()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/readHumdrum.md)
documentation for more details.

### Formal fields:

*Formal* fields indicate musical sections, or time windows within a
piece, including formal designations ("verse", "chorus", etc.) and
measures/bars. Humdrum data may or may not include formal metadata
fields, indicated by the token `"*>"`. Classified formal marks are put
into fields matching their name. Unclassified formal marks are placed in
a field called `Formal` as a default. Nested formal categories are
appended with an underscore and a number for each level of descent:
`Formal_1, Formal_2, ..., Formal_N`. If part of a section is not given a
name in a lower hierarchical level, the field is simply empty (`""`) at
that point.

Humdrum data may, or may not, also include barlines (tokens beginning
`"="`). However, humdrum tables *always* include three formal fields
related to barlines:

- `Bar` :: `integer`

  - How many barline records (single or double) have passed before this
    token?

  - If no `"="` tokens occur in a file, `Bar` is all zeros.

  - Note that this field is independent of whether the barlines are
    labeled with numbers in the humdrum file!

- `DoubleBar` :: `integer`

  - How many *double*-barline records have passed before this token?

  - If no `"=="` tokens occur in a file, `DoubleBar` is all zeros.

- `BarLabel` :: `character`

  - Any characters that occur in a barline-token *after* an initial
    `"="` or `"=="`. These include the `"-"` in the common "implied
    barline" token `"=-"`, repeat tokens (like `"=:||"`), and also any
    *explicit* bar numbers.

  - Note that the `Bar` field always enumerate *every* bar record, while
    measure-number labels in humdrum data (which appear in the
    `BarLabel` field) may do weird things like skipping numbers,
    repeating numbers, or having suffixes (e.g., `"19a"`). If no barline
    tokens appear in the file, `BarLabel` is all empty strings (`""`).

If no barline tokens are present in a file, `Bar` and `DoubleBar` will
be nothing but `0`s, and `BarLabel` will be all `NA`.

### Reference fields:

*Reference* fields describe any **Reference Records** in the humdrum
data. Every reference record (records beginning `"!!!"`) in any humdrum
file in a corpus read by
[readHumdrum](https://humdrumR.ccml.gtcmt.gatech.edu/reference/readHumdrum.md)
is parsed into a field named by the reference code: `"XXX"` in
`"!!!XXX"`. Reference tokens are all identical throughout any humdrum
piece. If a reference code appears in one file but not another, the
field is `NA` in the file which does not have the code. If no reference
records appear in any files read by
[`readHumdrum()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/readHumdrum.md),
no reference fields are created.

Examples of common reference records are `"!!!COM:"` (composer) and
`"!!!OTL:"` (original title). Any humdrum data with these records will
end up having `COM` and `OTL` fields in its humdrum table.

### Grouping fields:

Grouping fields are special fields which may be created by calls to
[group_by()](https://humdrumR.ccml.gtcmt.gatech.edu/reference/groupHumdrum.md).
These fields are deleted by calls to
[ungroup()](https://humdrumR.ccml.gtcmt.gatech.edu/reference/groupHumdrum.md).
These fields are generally hidden/inaccessible to users.

## Null data

In humdrum syntax, there is no requirement that every spine-path
contains data in every record. Rather, spines are often padded with
*null tokens*. In some cases, entire records may be padded with null
tokens. Each type of humdrum record uses a different null token:

- *Intepretation*: `*`

- *Comment*: `!`

- *Barline*: `=`

- *Data*: `.`

Many `humdrumR` functions automatically ignore null data, unless you
specifically tell them not to (usually, using `dataTypes` argument).
Whenever different `fields()` are created or
[selected](https://humdrumR.ccml.gtcmt.gatech.edu/reference/selectedFields.md),
`humdrumR` reevaluates what data locations it considers null. Note that
`humdrumR` considers data locations to be "null" when

- the selected fields are all `character` data *and* the token is a one
  of `c(".", "!", "!!", "=", "*", "**")`; **or**

- the selected fields are all `NA` (including `NA_character_`).

When `humdrumR` reevaluates null data, the `Type` field is updated,
setting data records to `Type == "d"` for null data and `Type == "D"`
for non-null data. This is the main mechanism `humdrumR` functions use
to ignore null data: most functions only look at data where
`Type == "D"`.

Whenever you print or
[export](https://humdrumR.ccml.gtcmt.gatech.edu/reference/writeHumdrum.md)
a \[humdrumR
object[humdrumRclass](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humdrumRclass.md),
null data in the selected fields prints as `"."`—thus `NA` values print
as `.`. Thus, if you are working with numeric data with `NA` values,
these `NA` values will print as `"."`.

## Reshaping

Breaking the complex syntax of humdrum data into the "flat" structure of
a humdrum table, with every single token on one line of a `data.table`,
makes humdrum data easier to analyze. Of course, thanks to the structure
fields, we can easily regroup and reform the original humdrum data or
use the structure of the data (like spines) in our analyses. However, in
some cases, you might want to work with humdrum data in a different
structure or "shape." `humdrumR` has several options for
["collapsing"](https://humdrumR.ccml.gtcmt.gatech.edu/reference/collapseHumdrum.md)
tokens within humdrum tables,
["cleaving"](https://humdrumR.ccml.gtcmt.gatech.edu/reference/cleave.md)
different parts of the data into new fields, or otherwise [reshaping
humdrum
data](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humCoercion.md)
into basic R data structures you might prefer.

## Querying Fields

The `fields()` function takes a [humdrumR
object](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humdrumRclass.md)
and returns a
[`data.table::data.table()`](https://rdrr.io/pkg/data.table/man/data.table.html),
with each row describing an available field in the humdrum table. The
output table has five columns:

- `Name`

  - The field name.

- `Class`

  - The [`class()`](https://rdrr.io/r/base/class.html) of the data in
    the field.

- `Type`

  - The type of field (described above). Can be `"Data"`, `"Structure"`,
    `"Interpretation"`, `"Formal"`, `"Reference"`, or `"Grouping"`.

- `Selected`,

  - A `logical` indicating which fields are
    [selected](https://humdrumR.ccml.gtcmt.gatech.edu/reference/selectedFields.md).

- `GroupedBy`

  - A `logical` indicating which, if any, fields are currently
    [grouping](https://humdrumR.ccml.gtcmt.gatech.edu/reference/groupingFactors.md)
    the data.

Using the [`names()`](https://rdrr.io/r/base/names.html) function on a
[humdrumR
object](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humdrumRclass.md)
will get just the field names, the same as `fields(humData)$Name`.

## See also

To actually extract fields from [humdrumR
data](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humdrumRclass.md),
see the
[`pull()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/pullHumdrum.md)
family of functions.

## Examples

``` r
humData <- readHumdrum(humdrumRroot, "HumdrumData/BachChorales/chor00[1-4].krn")
#> Finding and reading files...
#>  REpath-pattern '/home/nat/.tmp/RtmpjOz08Z/temp_libpath13452e707787ad/humdrumR/HumdrumData/BachChorales/chor00[1-4].krn' matches 4 text files in 1 directory.
#> Four files read from disk.
#> Validating four files...
#> all valid.
#> Parsing four files...
#> Assembling corpus...
#> Done!

fields(humData)
#>                Name     Class           Type Selected GroupedBy Complement
#>              <char>    <char>         <char>    <int>    <lgcl>     <lgcl>
#>  1:           Token character           Data        1     FALSE      FALSE
#>  2:             Bar   integer         Formal        0     FALSE      FALSE
#>  3:        BarLabel character         Formal        0     FALSE      FALSE
#>  4:       DoubleBar   integer         Formal        0     FALSE      FALSE
#>  5:          Formal character         Formal        0     FALSE      FALSE
#>  6:             BPM character Interpretation        0     FALSE      FALSE
#>  7:            Clef character Interpretation        0     FALSE      FALSE
#>  8:       Exclusive character Interpretation        0     FALSE      FALSE
#>  9:      Instrument character Interpretation        0     FALSE      FALSE
#> 10: InstrumentClass character Interpretation        0     FALSE      FALSE
#> 11:             Key character Interpretation        0     FALSE      FALSE
#> 12:    KeySignature character Interpretation        0     FALSE      FALSE
#> 13:     Mensuration character Interpretation        0     FALSE      FALSE
#> 14:          Tandem character Interpretation        0     FALSE      FALSE
#> 15:   TimeSignature character Interpretation        0     FALSE      FALSE
#> 16:             AGN character      Reference        0     FALSE      FALSE
#> 17:             CDT character      Reference        0     FALSE      FALSE
#> 18:             COM character      Reference        0     FALSE      FALSE
#> 19:             EED character      Reference        0     FALSE      FALSE
#> 20:             EEV character      Reference        0     FALSE      FALSE
#> 21:             OPR character      Reference        0     FALSE      FALSE
#> 22:         OTL@@DE character      Reference        0     FALSE      FALSE
#> 23:          OTL@EN character      Reference        0     FALSE      FALSE
#> 24:             PC# character      Reference        0     FALSE      FALSE
#> 25:             SCT character      Reference        0     FALSE      FALSE
#> 26:             SMS character      Reference        0     FALSE      FALSE
#> 27:             YOR character      Reference        0     FALSE      FALSE
#> 28:         hum2abc character      Reference        0     FALSE      FALSE
#> 29:           title character      Reference        0     FALSE      FALSE
#> 30:      DataRecord   integer      Structure        0     FALSE      FALSE
#> 31:            File   integer      Structure        0     FALSE      FALSE
#> 32:        Filename character      Structure        0     FALSE      FALSE
#> 33:        Filepath character      Structure        0     FALSE      FALSE
#> 34:          Global   logical      Structure        0     FALSE      FALSE
#> 35:           Label character      Structure        0     FALSE      FALSE
#> 36:      ParentPath   integer      Structure        0     FALSE      FALSE
#> 37:            Path   integer      Structure        0     FALSE      FALSE
#> 38:           Piece   integer      Structure        0     FALSE      FALSE
#> 39:          Record   integer      Structure        0     FALSE      FALSE
#> 40:           Spine   integer      Structure        0     FALSE      FALSE
#> 41:            Stop   integer      Structure        0     FALSE      FALSE
#> 42:            Type character      Structure        0     FALSE      FALSE
#>                Name     Class           Type Selected GroupedBy Complement
#>              <char>    <char>         <char>    <int>    <lgcl>     <lgcl>

getHumtab(humData)
#>                                             Token   Bar BarLabel DoubleBar
#>                                            <char> <int>   <char>     <int>
#>    1:             !!!COM:\tBach, Johann Sebastian    NA     <NA>        NA
#>    2:            !!!CDT:\t1685/02/21/-1750/07/28/    NA     <NA>        NA
#>    3:      !!!OTL@@DE:\tAus meines Herzens Grunde    NA     <NA>        NA
#>    4: !!!OTL@EN:      From the Depths of My Heart    NA     <NA>        NA
#>    5:                            !!!SCT:\tBWV 269    NA     <NA>        NA
#>   ---                                                                     
#> 1684:                                         4c#    11       10         0
#> 1685:                                         4d#    11       10         0
#> 1686:                                         4e;    11       10         0
#> 1687:                                          ==    12        =         1
#> 1688:                                          *-    12        =         1
#>       Formal    BPM   Clef Exclusive Instrument InstrumentClass    Key
#>       <char> <char> <char>    <char>     <char>          <char> <char>
#>    1:   <NA>   <NA>   <NA>      <NA>       <NA>            <NA>   <NA>
#>    2:   <NA>   <NA>   <NA>      <NA>       <NA>            <NA>   <NA>
#>    3:   <NA>   <NA>   <NA>      <NA>       <NA>            <NA>   <NA>
#>    4:   <NA>   <NA>   <NA>      <NA>       <NA>            <NA>   <NA>
#>    5:   <NA>   <NA>   <NA>      <NA>       <NA>            <NA>   <NA>
#>   ---                                                                 
#> 1684:      B  MM100 clefG2      kern  I"Soprano           ICvox     E:
#> 1685:      B  MM100 clefG2      kern  I"Soprano           ICvox     E:
#> 1686:      B  MM100 clefG2      kern  I"Soprano           ICvox     E:
#> 1687:      B  MM100 clefG2      kern  I"Soprano           ICvox     E:
#> 1688:      B  MM100 clefG2      kern  I"Soprano           ICvox     E:
#>       KeySignature Mensuration
#>             <char>      <char>
#>    1:         <NA>        <NA>
#>    2:         <NA>        <NA>
#>    3:         <NA>        <NA>
#>    4:         <NA>        <NA>
#>    5:         <NA>        <NA>
#>   ---                         
#> 1684:  k[f#c#g#d#]      met(c)
#> 1685:  k[f#c#g#d#]      met(c)
#> 1686:  k[f#c#g#d#]      met(c)
#> 1687:  k[f#c#g#d#]      met(c)
#> 1688:  k[f#c#g#d#]      met(c)
#>                                                               Tandem
#>                                                               <char>
#>    1:                                                           <NA>
#>    2:                                                           <NA>
#>    3:                                                           <NA>
#>    4:                                                           <NA>
#>    5:                                                           <NA>
#>   ---                                                               
#> 1684: MM100,met(c),M4/4,E:,k[f#c#g#d#],clefG2,I"Soprano,Isoprn,ICvox
#> 1685: MM100,met(c),M4/4,E:,k[f#c#g#d#],clefG2,I"Soprano,Isoprn,ICvox
#> 1686: MM100,met(c),M4/4,E:,k[f#c#g#d#],clefG2,I"Soprano,Isoprn,ICvox
#> 1687: MM100,met(c),M4/4,E:,k[f#c#g#d#],clefG2,I"Soprano,Isoprn,ICvox
#> 1688: MM100,met(c),M4/4,E:,k[f#c#g#d#],clefG2,I"Soprano,Isoprn,ICvox
#>       TimeSignature       AGN                       CDT
#>              <char>    <char>                    <char>
#>    1:          <NA> \tchorale \t1685/02/21/-1750/07/28/
#>    2:          <NA> \tchorale \t1685/02/21/-1750/07/28/
#>    3:          <NA> \tchorale \t1685/02/21/-1750/07/28/
#>    4:          <NA> \tchorale \t1685/02/21/-1750/07/28/
#>    5:          <NA> \tchorale \t1685/02/21/-1750/07/28/
#>   ---                                                  
#> 1684:          M4/4 \tchorale \t1685/02/21/-1750/07/28/
#> 1685:          M4/4 \tchorale \t1685/02/21/-1750/07/28/
#> 1686:          M4/4 \tchorale \t1685/02/21/-1750/07/28/
#> 1687:          M4/4 \tchorale \t1685/02/21/-1750/07/28/
#> 1688:          M4/4 \tchorale \t1685/02/21/-1750/07/28/
#>                            COM               EED        EEV    OPR
#>                         <char>            <char>     <char> <char>
#>    1: \tBach, Johann Sebastian Craig Stuart Sapp 2009/05/22   <NA>
#>    2: \tBach, Johann Sebastian Craig Stuart Sapp 2009/05/22   <NA>
#>    3: \tBach, Johann Sebastian Craig Stuart Sapp 2009/05/22   <NA>
#>    4: \tBach, Johann Sebastian Craig Stuart Sapp 2009/05/22   <NA>
#>    5: \tBach, Johann Sebastian Craig Stuart Sapp 2009/05/22   <NA>
#>   ---                                                             
#> 1684: \tBach, Johann Sebastian Craig Stuart Sapp 2009/05/22   <NA>
#> 1685: \tBach, Johann Sebastian Craig Stuart Sapp 2009/05/22   <NA>
#> 1686: \tBach, Johann Sebastian Craig Stuart Sapp 2009/05/22   <NA>
#> 1687: \tBach, Johann Sebastian Craig Stuart Sapp 2009/05/22   <NA>
#> 1688: \tBach, Johann Sebastian Craig Stuart Sapp 2009/05/22   <NA>
#>                                OTL@@DE                      OTL@EN    PC#
#>                                 <char>                      <char> <char>
#>    1:      \tAus meines Herzens Grunde From the Depths of My Heart    \t1
#>    2:      \tAus meines Herzens Grunde From the Depths of My Heart    \t1
#>    3:      \tAus meines Herzens Grunde From the Depths of My Heart    \t1
#>    4:      \tAus meines Herzens Grunde From the Depths of My Heart    \t1
#>    5:      \tAus meines Herzens Grunde From the Depths of My Heart    \t1
#>   ---                                                                    
#> 1684: \tEs ist das Heil uns kommen her                        <NA>    \t4
#> 1685: \tEs ist das Heil uns kommen her                        <NA>    \t4
#> 1686: \tEs ist das Heil uns kommen her                        <NA>    \t4
#> 1687: \tEs ist das Heil uns kommen her                        <NA>    \t4
#> 1688: \tEs ist das Heil uns kommen her                        <NA>    \t4
#>              SCT                                                    SMS
#>           <char>                                                 <char>
#>    1:  \tBWV 269 B&H, 4th ed, Alfred D&ouml;rffel, c.1875, plate V.A.10
#>    2:  \tBWV 269 B&H, 4th ed, Alfred D&ouml;rffel, c.1875, plate V.A.10
#>    3:  \tBWV 269 B&H, 4th ed, Alfred D&ouml;rffel, c.1875, plate V.A.10
#>    4:  \tBWV 269 B&H, 4th ed, Alfred D&ouml;rffel, c.1875, plate V.A.10
#>    5:  \tBWV 269 B&H, 4th ed, Alfred D&ouml;rffel, c.1875, plate V.A.10
#>   ---                                                                  
#> 1684: \tBWV 86/6 B&H, 4th ed, Alfred D&ouml;rffel, c.1875, plate V.A.10
#> 1685: \tBWV 86/6 B&H, 4th ed, Alfred D&ouml;rffel, c.1875, plate V.A.10
#> 1686: \tBWV 86/6 B&H, 4th ed, Alfred D&ouml;rffel, c.1875, plate V.A.10
#> 1687: \tBWV 86/6 B&H, 4th ed, Alfred D&ouml;rffel, c.1875, plate V.A.10
#> 1688: \tBWV 86/6 B&H, 4th ed, Alfred D&ouml;rffel, c.1875, plate V.A.10
#>                                                                                                                                                                                                                                                                                YOR
#>                                                                                                                                                                                                                                                                             <char>
#>    1: 371 vierstimmige Choralges&auml;nge von Johann Sebastian Bach, ; 4th ed. by Alfred D&ouml;rffel (Leipzig: Breitkopf und H&auml;rtel, ; c.1875). 178 pp. Plate "V.A.10".  reprint: J.S. Bach, 371 Four-Part ; Chorales (New York: Associated Music Publishers, Inc., c.1940).
#>    2: 371 vierstimmige Choralges&auml;nge von Johann Sebastian Bach, ; 4th ed. by Alfred D&ouml;rffel (Leipzig: Breitkopf und H&auml;rtel, ; c.1875). 178 pp. Plate "V.A.10".  reprint: J.S. Bach, 371 Four-Part ; Chorales (New York: Associated Music Publishers, Inc., c.1940).
#>    3: 371 vierstimmige Choralges&auml;nge von Johann Sebastian Bach, ; 4th ed. by Alfred D&ouml;rffel (Leipzig: Breitkopf und H&auml;rtel, ; c.1875). 178 pp. Plate "V.A.10".  reprint: J.S. Bach, 371 Four-Part ; Chorales (New York: Associated Music Publishers, Inc., c.1940).
#>    4: 371 vierstimmige Choralges&auml;nge von Johann Sebastian Bach, ; 4th ed. by Alfred D&ouml;rffel (Leipzig: Breitkopf und H&auml;rtel, ; c.1875). 178 pp. Plate "V.A.10".  reprint: J.S. Bach, 371 Four-Part ; Chorales (New York: Associated Music Publishers, Inc., c.1940).
#>    5: 371 vierstimmige Choralges&auml;nge von Johann Sebastian Bach, ; 4th ed. by Alfred D&ouml;rffel (Leipzig: Breitkopf und H&auml;rtel, ; c.1875). 178 pp. Plate "V.A.10".  reprint: J.S. Bach, 371 Four-Part ; Chorales (New York: Associated Music Publishers, Inc., c.1940).
#>   ---                                                                                                                                                                                                                                                                             
#> 1684: 371 vierstimmige Choralges&auml;nge von Johann Sebastian Bach, ; 4th ed. by Alfred D&ouml;rffel (Leipzig: Breitkopf und H&auml;rtel, ; c.1875). 178 pp. Plate "V.A.10".  reprint: J.S. Bach, 371 Four-Part ; Chorales (New York: Associated Music Publishers, Inc., c.1940).
#> 1685: 371 vierstimmige Choralges&auml;nge von Johann Sebastian Bach, ; 4th ed. by Alfred D&ouml;rffel (Leipzig: Breitkopf und H&auml;rtel, ; c.1875). 178 pp. Plate "V.A.10".  reprint: J.S. Bach, 371 Four-Part ; Chorales (New York: Associated Music Publishers, Inc., c.1940).
#> 1686: 371 vierstimmige Choralges&auml;nge von Johann Sebastian Bach, ; 4th ed. by Alfred D&ouml;rffel (Leipzig: Breitkopf und H&auml;rtel, ; c.1875). 178 pp. Plate "V.A.10".  reprint: J.S. Bach, 371 Four-Part ; Chorales (New York: Associated Music Publishers, Inc., c.1940).
#> 1687: 371 vierstimmige Choralges&auml;nge von Johann Sebastian Bach, ; 4th ed. by Alfred D&ouml;rffel (Leipzig: Breitkopf und H&auml;rtel, ; c.1875). 178 pp. Plate "V.A.10".  reprint: J.S. Bach, 371 Four-Part ; Chorales (New York: Associated Music Publishers, Inc., c.1940).
#> 1688: 371 vierstimmige Choralges&auml;nge von Johann Sebastian Bach, ; 4th ed. by Alfred D&ouml;rffel (Leipzig: Breitkopf und H&auml;rtel, ; c.1875). 178 pp. Plate "V.A.10".  reprint: J.S. Bach, 371 Four-Part ; Chorales (New York: Associated Music Publishers, Inc., c.1940).
#>       hum2abc              title DataRecord  File    Filename
#>        <char>             <char>      <int> <int>      <char>
#>    1:   -Q '' @{PC#}. @{OTL@@DE}         NA     1 chor001.krn
#>    2:   -Q '' @{PC#}. @{OTL@@DE}         NA     1 chor001.krn
#>    3:   -Q '' @{PC#}. @{OTL@@DE}         NA     1 chor001.krn
#>    4:   -Q '' @{PC#}. @{OTL@@DE}         NA     1 chor001.krn
#>    5:   -Q '' @{PC#}. @{OTL@@DE}         NA     1 chor001.krn
#>   ---                                                        
#> 1684:   -Q '' @{PC#}. @{OTL@@DE}         59     4 chor004.krn
#> 1685:   -Q '' @{PC#}. @{OTL@@DE}         60     4 chor004.krn
#> 1686:   -Q '' @{PC#}. @{OTL@@DE}         61     4 chor004.krn
#> 1687:   -Q '' @{PC#}. @{OTL@@DE}         NA     4 chor004.krn
#> 1688:   -Q '' @{PC#}. @{OTL@@DE}         NA     4 chor004.krn
#>                                                                                                 Filepath
#>                                                                                                   <char>
#>    1: /home/nat/.tmp/RtmpjOz08Z/temp_libpath13452e707787ad/humdrumR/HumdrumData/BachChorales/chor001.krn
#>    2: /home/nat/.tmp/RtmpjOz08Z/temp_libpath13452e707787ad/humdrumR/HumdrumData/BachChorales/chor001.krn
#>    3: /home/nat/.tmp/RtmpjOz08Z/temp_libpath13452e707787ad/humdrumR/HumdrumData/BachChorales/chor001.krn
#>    4: /home/nat/.tmp/RtmpjOz08Z/temp_libpath13452e707787ad/humdrumR/HumdrumData/BachChorales/chor001.krn
#>    5: /home/nat/.tmp/RtmpjOz08Z/temp_libpath13452e707787ad/humdrumR/HumdrumData/BachChorales/chor001.krn
#>   ---                                                                                                   
#> 1684: /home/nat/.tmp/RtmpjOz08Z/temp_libpath13452e707787ad/humdrumR/HumdrumData/BachChorales/chor004.krn
#> 1685: /home/nat/.tmp/RtmpjOz08Z/temp_libpath13452e707787ad/humdrumR/HumdrumData/BachChorales/chor004.krn
#> 1686: /home/nat/.tmp/RtmpjOz08Z/temp_libpath13452e707787ad/humdrumR/HumdrumData/BachChorales/chor004.krn
#> 1687: /home/nat/.tmp/RtmpjOz08Z/temp_libpath13452e707787ad/humdrumR/HumdrumData/BachChorales/chor004.krn
#> 1688: /home/nat/.tmp/RtmpjOz08Z/temp_libpath13452e707787ad/humdrumR/HumdrumData/BachChorales/chor004.krn
#>       Global  Label ParentPath  Path Piece Record Spine  Stop   Type
#>       <lgcl> <char>      <int> <int> <int>  <int> <int> <int> <char>
#>    1:   TRUE     _1         NA    NA     1      1    NA    NA      G
#>    2:   TRUE     _1         NA    NA     1      2    NA    NA      G
#>    3:   TRUE     _1         NA    NA     1      3    NA    NA      G
#>    4:   TRUE     _1         NA    NA     1      4    NA    NA      G
#>    5:   TRUE     _1         NA    NA     1      5    NA    NA      G
#>   ---                                                               
#> 1684:  FALSE     _1          0     0     4     90     4     1      D
#> 1685:  FALSE     _1          0     0     4     91     4     1      D
#> 1686:  FALSE     _1          0     0     4     92     4     1      D
#> 1687:  FALSE     _1          0     0     4     93     4     1      M
#> 1688:  FALSE     _1          0     0     4     94     4     1      S
#>       Exclusive.Token _rowKey_
#>                <char>    <int>
#>    1:            <NA>        1
#>    2:            <NA>        2
#>    3:            <NA>        3
#>    4:            <NA>        4
#>    5:            <NA>        5
#>   ---                         
#> 1684:            kern     1684
#> 1685:            kern     1685
#> 1686:            kern     1686
#> 1687:            kern     1687
#> 1688:            kern     1688
getHumtab(humData, dataTypes = 'D')
#>       Token   Bar BarLabel DoubleBar Formal    BPM   Clef Exclusive Instrument
#>      <char> <int>   <char>     <int> <char> <char> <char>    <char>     <char>
#>   1:    4GG     0     <NA>         0      A  MM100 clefF4      kern     I"Bass
#>   2:     4G     1        1         0      A  MM100 clefF4      kern     I"Bass
#>   3:     4E     1        1         0      A  MM100 clefF4      kern     I"Bass
#>   4:    4F#     1        1         0      A  MM100 clefF4      kern     I"Bass
#>   5:     4G     2        2         0      A  MM100 clefF4      kern     I"Bass
#>  ---                                                                          
#> 838:    4g#    10        9         0      B  MM100 clefG2      kern  I"Soprano
#> 839:    4f#    10        9         0      B  MM100 clefG2      kern  I"Soprano
#> 840:    4c#    11       10         0      B  MM100 clefG2      kern  I"Soprano
#> 841:    4d#    11       10         0      B  MM100 clefG2      kern  I"Soprano
#> 842:    4e;    11       10         0      B  MM100 clefG2      kern  I"Soprano
#>      InstrumentClass    Key KeySignature Mensuration
#>               <char> <char>       <char>      <char>
#>   1:           ICvox     G:        k[f#]        <NA>
#>   2:           ICvox     G:        k[f#]        <NA>
#>   3:           ICvox     G:        k[f#]        <NA>
#>   4:           ICvox     G:        k[f#]        <NA>
#>   5:           ICvox     G:        k[f#]        <NA>
#>  ---                                                
#> 838:           ICvox     E:  k[f#c#g#d#]      met(c)
#> 839:           ICvox     E:  k[f#c#g#d#]      met(c)
#> 840:           ICvox     E:  k[f#c#g#d#]      met(c)
#> 841:           ICvox     E:  k[f#c#g#d#]      met(c)
#> 842:           ICvox     E:  k[f#c#g#d#]      met(c)
#>                                                              Tandem
#>                                                              <char>
#>   1:                  MM100,M3/4,G:,k[f#],clefF4,I"Bass,Ibass,ICvox
#>   2:                  MM100,M3/4,G:,k[f#],clefF4,I"Bass,Ibass,ICvox
#>   3:                  MM100,M3/4,G:,k[f#],clefF4,I"Bass,Ibass,ICvox
#>   4:                  MM100,M3/4,G:,k[f#],clefF4,I"Bass,Ibass,ICvox
#>   5:                  MM100,M3/4,G:,k[f#],clefF4,I"Bass,Ibass,ICvox
#>  ---                                                               
#> 838: MM100,met(c),M4/4,E:,k[f#c#g#d#],clefG2,I"Soprano,Isoprn,ICvox
#> 839: MM100,met(c),M4/4,E:,k[f#c#g#d#],clefG2,I"Soprano,Isoprn,ICvox
#> 840: MM100,met(c),M4/4,E:,k[f#c#g#d#],clefG2,I"Soprano,Isoprn,ICvox
#> 841: MM100,met(c),M4/4,E:,k[f#c#g#d#],clefG2,I"Soprano,Isoprn,ICvox
#> 842: MM100,met(c),M4/4,E:,k[f#c#g#d#],clefG2,I"Soprano,Isoprn,ICvox
#>      TimeSignature       AGN                       CDT                      COM
#>             <char>    <char>                    <char>                   <char>
#>   1:          M3/4 \tchorale \t1685/02/21/-1750/07/28/ \tBach, Johann Sebastian
#>   2:          M3/4 \tchorale \t1685/02/21/-1750/07/28/ \tBach, Johann Sebastian
#>   3:          M3/4 \tchorale \t1685/02/21/-1750/07/28/ \tBach, Johann Sebastian
#>   4:          M3/4 \tchorale \t1685/02/21/-1750/07/28/ \tBach, Johann Sebastian
#>   5:          M3/4 \tchorale \t1685/02/21/-1750/07/28/ \tBach, Johann Sebastian
#>  ---                                                                           
#> 838:          M4/4 \tchorale \t1685/02/21/-1750/07/28/ \tBach, Johann Sebastian
#> 839:          M4/4 \tchorale \t1685/02/21/-1750/07/28/ \tBach, Johann Sebastian
#> 840:          M4/4 \tchorale \t1685/02/21/-1750/07/28/ \tBach, Johann Sebastian
#> 841:          M4/4 \tchorale \t1685/02/21/-1750/07/28/ \tBach, Johann Sebastian
#> 842:          M4/4 \tchorale \t1685/02/21/-1750/07/28/ \tBach, Johann Sebastian
#>                    EED        EEV    OPR                          OTL@@DE
#>                 <char>     <char> <char>                           <char>
#>   1: Craig Stuart Sapp 2009/05/22   <NA>      \tAus meines Herzens Grunde
#>   2: Craig Stuart Sapp 2009/05/22   <NA>      \tAus meines Herzens Grunde
#>   3: Craig Stuart Sapp 2009/05/22   <NA>      \tAus meines Herzens Grunde
#>   4: Craig Stuart Sapp 2009/05/22   <NA>      \tAus meines Herzens Grunde
#>   5: Craig Stuart Sapp 2009/05/22   <NA>      \tAus meines Herzens Grunde
#>  ---                                                                     
#> 838: Craig Stuart Sapp 2009/05/22   <NA> \tEs ist das Heil uns kommen her
#> 839: Craig Stuart Sapp 2009/05/22   <NA> \tEs ist das Heil uns kommen her
#> 840: Craig Stuart Sapp 2009/05/22   <NA> \tEs ist das Heil uns kommen her
#> 841: Craig Stuart Sapp 2009/05/22   <NA> \tEs ist das Heil uns kommen her
#> 842: Craig Stuart Sapp 2009/05/22   <NA> \tEs ist das Heil uns kommen her
#>                           OTL@EN    PC#        SCT
#>                           <char> <char>     <char>
#>   1: From the Depths of My Heart    \t1  \tBWV 269
#>   2: From the Depths of My Heart    \t1  \tBWV 269
#>   3: From the Depths of My Heart    \t1  \tBWV 269
#>   4: From the Depths of My Heart    \t1  \tBWV 269
#>   5: From the Depths of My Heart    \t1  \tBWV 269
#>  ---                                              
#> 838:                        <NA>    \t4 \tBWV 86/6
#> 839:                        <NA>    \t4 \tBWV 86/6
#> 840:                        <NA>    \t4 \tBWV 86/6
#> 841:                        <NA>    \t4 \tBWV 86/6
#> 842:                        <NA>    \t4 \tBWV 86/6
#>                                                         SMS
#>                                                      <char>
#>   1: B&H, 4th ed, Alfred D&ouml;rffel, c.1875, plate V.A.10
#>   2: B&H, 4th ed, Alfred D&ouml;rffel, c.1875, plate V.A.10
#>   3: B&H, 4th ed, Alfred D&ouml;rffel, c.1875, plate V.A.10
#>   4: B&H, 4th ed, Alfred D&ouml;rffel, c.1875, plate V.A.10
#>   5: B&H, 4th ed, Alfred D&ouml;rffel, c.1875, plate V.A.10
#>  ---                                                       
#> 838: B&H, 4th ed, Alfred D&ouml;rffel, c.1875, plate V.A.10
#> 839: B&H, 4th ed, Alfred D&ouml;rffel, c.1875, plate V.A.10
#> 840: B&H, 4th ed, Alfred D&ouml;rffel, c.1875, plate V.A.10
#> 841: B&H, 4th ed, Alfred D&ouml;rffel, c.1875, plate V.A.10
#> 842: B&H, 4th ed, Alfred D&ouml;rffel, c.1875, plate V.A.10
#>                                                                                                                                                                                                                                                                               YOR
#>                                                                                                                                                                                                                                                                            <char>
#>   1: 371 vierstimmige Choralges&auml;nge von Johann Sebastian Bach, ; 4th ed. by Alfred D&ouml;rffel (Leipzig: Breitkopf und H&auml;rtel, ; c.1875). 178 pp. Plate "V.A.10".  reprint: J.S. Bach, 371 Four-Part ; Chorales (New York: Associated Music Publishers, Inc., c.1940).
#>   2: 371 vierstimmige Choralges&auml;nge von Johann Sebastian Bach, ; 4th ed. by Alfred D&ouml;rffel (Leipzig: Breitkopf und H&auml;rtel, ; c.1875). 178 pp. Plate "V.A.10".  reprint: J.S. Bach, 371 Four-Part ; Chorales (New York: Associated Music Publishers, Inc., c.1940).
#>   3: 371 vierstimmige Choralges&auml;nge von Johann Sebastian Bach, ; 4th ed. by Alfred D&ouml;rffel (Leipzig: Breitkopf und H&auml;rtel, ; c.1875). 178 pp. Plate "V.A.10".  reprint: J.S. Bach, 371 Four-Part ; Chorales (New York: Associated Music Publishers, Inc., c.1940).
#>   4: 371 vierstimmige Choralges&auml;nge von Johann Sebastian Bach, ; 4th ed. by Alfred D&ouml;rffel (Leipzig: Breitkopf und H&auml;rtel, ; c.1875). 178 pp. Plate "V.A.10".  reprint: J.S. Bach, 371 Four-Part ; Chorales (New York: Associated Music Publishers, Inc., c.1940).
#>   5: 371 vierstimmige Choralges&auml;nge von Johann Sebastian Bach, ; 4th ed. by Alfred D&ouml;rffel (Leipzig: Breitkopf und H&auml;rtel, ; c.1875). 178 pp. Plate "V.A.10".  reprint: J.S. Bach, 371 Four-Part ; Chorales (New York: Associated Music Publishers, Inc., c.1940).
#>  ---                                                                                                                                                                                                                                                                             
#> 838: 371 vierstimmige Choralges&auml;nge von Johann Sebastian Bach, ; 4th ed. by Alfred D&ouml;rffel (Leipzig: Breitkopf und H&auml;rtel, ; c.1875). 178 pp. Plate "V.A.10".  reprint: J.S. Bach, 371 Four-Part ; Chorales (New York: Associated Music Publishers, Inc., c.1940).
#> 839: 371 vierstimmige Choralges&auml;nge von Johann Sebastian Bach, ; 4th ed. by Alfred D&ouml;rffel (Leipzig: Breitkopf und H&auml;rtel, ; c.1875). 178 pp. Plate "V.A.10".  reprint: J.S. Bach, 371 Four-Part ; Chorales (New York: Associated Music Publishers, Inc., c.1940).
#> 840: 371 vierstimmige Choralges&auml;nge von Johann Sebastian Bach, ; 4th ed. by Alfred D&ouml;rffel (Leipzig: Breitkopf und H&auml;rtel, ; c.1875). 178 pp. Plate "V.A.10".  reprint: J.S. Bach, 371 Four-Part ; Chorales (New York: Associated Music Publishers, Inc., c.1940).
#> 841: 371 vierstimmige Choralges&auml;nge von Johann Sebastian Bach, ; 4th ed. by Alfred D&ouml;rffel (Leipzig: Breitkopf und H&auml;rtel, ; c.1875). 178 pp. Plate "V.A.10".  reprint: J.S. Bach, 371 Four-Part ; Chorales (New York: Associated Music Publishers, Inc., c.1940).
#> 842: 371 vierstimmige Choralges&auml;nge von Johann Sebastian Bach, ; 4th ed. by Alfred D&ouml;rffel (Leipzig: Breitkopf und H&auml;rtel, ; c.1875). 178 pp. Plate "V.A.10".  reprint: J.S. Bach, 371 Four-Part ; Chorales (New York: Associated Music Publishers, Inc., c.1940).
#>      hum2abc              title DataRecord  File    Filename
#>       <char>             <char>      <int> <int>      <char>
#>   1:   -Q '' @{PC#}. @{OTL@@DE}          1     1 chor001.krn
#>   2:   -Q '' @{PC#}. @{OTL@@DE}          2     1 chor001.krn
#>   3:   -Q '' @{PC#}. @{OTL@@DE}          3     1 chor001.krn
#>   4:   -Q '' @{PC#}. @{OTL@@DE}          5     1 chor001.krn
#>   5:   -Q '' @{PC#}. @{OTL@@DE}          6     1 chor001.krn
#>  ---                                                        
#> 838:   -Q '' @{PC#}. @{OTL@@DE}         56     4 chor004.krn
#> 839:   -Q '' @{PC#}. @{OTL@@DE}         57     4 chor004.krn
#> 840:   -Q '' @{PC#}. @{OTL@@DE}         59     4 chor004.krn
#> 841:   -Q '' @{PC#}. @{OTL@@DE}         60     4 chor004.krn
#> 842:   -Q '' @{PC#}. @{OTL@@DE}         61     4 chor004.krn
#>                                                                                                Filepath
#>                                                                                                  <char>
#>   1: /home/nat/.tmp/RtmpjOz08Z/temp_libpath13452e707787ad/humdrumR/HumdrumData/BachChorales/chor001.krn
#>   2: /home/nat/.tmp/RtmpjOz08Z/temp_libpath13452e707787ad/humdrumR/HumdrumData/BachChorales/chor001.krn
#>   3: /home/nat/.tmp/RtmpjOz08Z/temp_libpath13452e707787ad/humdrumR/HumdrumData/BachChorales/chor001.krn
#>   4: /home/nat/.tmp/RtmpjOz08Z/temp_libpath13452e707787ad/humdrumR/HumdrumData/BachChorales/chor001.krn
#>   5: /home/nat/.tmp/RtmpjOz08Z/temp_libpath13452e707787ad/humdrumR/HumdrumData/BachChorales/chor001.krn
#>  ---                                                                                                   
#> 838: /home/nat/.tmp/RtmpjOz08Z/temp_libpath13452e707787ad/humdrumR/HumdrumData/BachChorales/chor004.krn
#> 839: /home/nat/.tmp/RtmpjOz08Z/temp_libpath13452e707787ad/humdrumR/HumdrumData/BachChorales/chor004.krn
#> 840: /home/nat/.tmp/RtmpjOz08Z/temp_libpath13452e707787ad/humdrumR/HumdrumData/BachChorales/chor004.krn
#> 841: /home/nat/.tmp/RtmpjOz08Z/temp_libpath13452e707787ad/humdrumR/HumdrumData/BachChorales/chor004.krn
#> 842: /home/nat/.tmp/RtmpjOz08Z/temp_libpath13452e707787ad/humdrumR/HumdrumData/BachChorales/chor004.krn
#>      Global  Label ParentPath  Path Piece Record Spine  Stop   Type
#>      <lgcl> <char>      <int> <int> <int>  <int> <int> <int> <char>
#>   1:  FALSE     _1          0     0     1     20     1     1      D
#>   2:  FALSE     _1          0     0     1     22     1     1      D
#>   3:  FALSE     _1          0     0     1     23     1     1      D
#>   4:  FALSE     _1          0     0     1     25     1     1      D
#>   5:  FALSE     _1          0     0     1     27     1     1      D
#>  ---                                                               
#> 838:  FALSE     _1          0     0     4     86     4     1      D
#> 839:  FALSE     _1          0     0     4     87     4     1      D
#> 840:  FALSE     _1          0     0     4     90     4     1      D
#> 841:  FALSE     _1          0     0     4     91     4     1      D
#> 842:  FALSE     _1          0     0     4     92     4     1      D
#>      Exclusive.Token _rowKey_
#>               <char>    <int>
#>   1:            kern       29
#>   2:            kern       31
#>   3:            kern       32
#>   4:            kern       34
#>   5:            kern       36
#>  ---                         
#> 838:            kern     1680
#> 839:            kern     1681
#> 840:            kern     1684
#> 841:            kern     1685
#> 842:            kern     1686
```
