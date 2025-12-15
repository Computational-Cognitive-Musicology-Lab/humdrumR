# Get tandem interpretation information from humdrum data

`tandem` extracts tandem interpretations from the raw `Tandem` spine in
[humdrumR
object](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humdrumRclass.md).

## Usage

``` r
tandem(regex, Tandem)
```

## Arguments

- regex:

  ***A regular expression to match tandem interpretations.***

  Must be a single `character` string.

  You should not include a `*` at the beginning—the `*` marker for
  tandem interpretations are already removed from the `Tandem` field.

- Tandem:

  ***Parsed tandem interpretation data.***

  Must be `atomic`.

  Should always be the `Tandem` field from a [humdrumR
  object](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humdrumRclass.md).

## Details

Every
[humdrumR](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humdrumRclass.md)
object has a field called `Tandem`, which is a vector of strings which
accumulates tandem interpretations in each Spine. At each record, all
the previous tandems that occured in each spine are listed (comma
separated), with the most recent appearing first.

For example, consider this file:

    **kern **kern
    *C:    *C:
    *Ibass *Isoprn
    2G     4g
    .      4f
    2C     2e
    *G:    *G:
    2D     2f#
    2G     2g
    *-     *-

The `Tandem` field for these two spines would look like this:

    ""
    "C:"                 "C:"
    "Ibass,C:"           "Isoprn,C:"
    "Ibass,C:"           "Isoprn,C:"
    "Ibass,C:"           "Isoprn,C:"
    "Ibass,C:"           "Isoprn,C:"
    "G:,Ibass,C:"        "G:,Isoprn,C:"
    "G:,Ibass,C:"        "G:,Isoprn,C:"
    "G:,Ibass,C:"        "G:,Isoprn,C:"
    "G:,Ibass,C:"        "G:,Isoprn,C:"

Notice that the `"C:"` isn't erased by the appearance of `"G:"`—this is
a naive parser that doesn't "know" that `"C:"` and `"G:"` are related.
The earlier tandem (`"C:"`) is just pushed further back onto the stack.

Don't worry, the [humdrumR data
parser](https://humdrumR.ccml.gtcmt.gatech.edu/reference/readHumdrum.md)
*does* recognize many common tandem interpretations (like `*C:` and
`*G:`) and will automatically parse them if they are present—in this
case, they are put into the `Key` field automatically. However, the
`Tandem` field is retained in case your data contains any novel tandem
intepretations that `humdrumR` does not recognize.

## tandem

If your data *does* contain novel/unknown tandem interpretations, you
can use the `tandem` function to pull them out of the `Tandem` field.
The first argument to `tandem` must be the `Tandem` field from a
[humdrumR
object](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humdrumRclass.md).
The second argument (`regex`) is a regular expression which is matched
against the the tandem interpretations. For each token in `Tandem`, the
most recent match (if any) is retained.

For example, if we wanted to manually extract the key information from
the `Tandem` field (which `humdrumR` automatically does for you), we
could call `tandem(Tandem, "[A-Ga-g][#-]*:")`.

## See also

Read more about tandem interpretations in `humdrumR` in the [humdrum
table
docs](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humTable.md).
[`readHumdrum()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/readHumdrum.md)
also preprocesses some "known" tandem interpretations.
