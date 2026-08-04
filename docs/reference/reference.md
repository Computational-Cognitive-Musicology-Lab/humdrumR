# Summarize reference records in a humdrumR corpus

`reference` is used to tabulate the reference records present in a
[humdrumR](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humdrumRclass.md)
corpus. `reference` is one of
[humdrumR](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humdrumR.md)'s
basic [corpus summary
functions](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humSummary.md).

## Usage

``` r
reference(x, ...)

reference('OTL')

reference(humdata)

# S3 method for class 'humReference'
refTable[i, j, drop = FALSE]
```

## Arguments

- x:

  ***Input for extracting reference information.***

  Must be a `character` string (to look up a reference code) or a
  [humdrumR](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humdrumRclass.md).

- drop:

  ***Whether to return normal
  [data.table](https://rdrr.io/pkg/data.table/man/data.table.html) or a
  `humReference` table.***

  Defaults to `FALSE`.

  Must be a singleton `logical` value: an on/off switch.

  If `drop = TRUE`, a normal
  [data.table](https://rdrr.io/pkg/data.table/man/data.table.html) is
  returned instead of a `humReference` table.

- i:

  ***Index for rows.***

  If `numeric`, selects rows by index. If `character`, the string is
  matched as a regular expression against filenames in the corpus.

- j:

  ***Index for columns.***

  If `numeric`, selects columns by index. If `character`, [partially
  matched](https://humdrumR.ccml.gtcmt.gatech.edu/reference/partialMatching.md)
  against column names (reference codes).

## Details

`reference` can be used to look up information about common reference
codes: supply a reference code as a `character` string to `reference`
and
[humdrumR](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humdrumRclass.md)
will check it against known reference codes and print information about
matching codes (if there is one). For instance, `reference('OTL')`
returns a description of the standard humdrum `!!!OTL` reference record
(original title metadata).

When applied to a [humdrumR
corpus](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humdrumRclass.md)
`reference` returns a special `data.frame` called a `humReference`
table. A `humReference` table has one column for each reference code
that appears in the corpus. Since reference records can be too long to
print on one screen, and humdrum files can have multiple of the same
type of reference code, a `humReference` table normally prints only the
number of each type of reference record to appear in each piece.
However, if only one type of reference code is present in a
`humReference` table, the complete reference records for that code
*will* be printed for each piece. Likewise, if only one piece is present
in the table, all of that piece's complete reference records are
printed. Thus, if you want to see actual reference records, try indexing
the `humReference` table down to one column or row (see below).

A `humReference` table has one row for each piece in the corpus. Rows
are labeled with each file name and piece number index. In addition,
when a `humReference` object is printed, three different summary totals
are printed for each reference code:

- **Any** indicates how many pieces in the corpus contain at least one
  example of each code.

- **Sum** indicates the total number of each reference code to appear in
  the corpus, including multiple appearances in one piece (like multiple
  `"!!!COM"` records).

- **Unique** tabulates the number of unique token in the corpus, for
  each code. If your corpus only has two unique composers (encoded in
  "!!!COM"), the **Unique** total will be `2`. This assumes that tokens
  are *exactly* identical, including white space; so
  `"!!!COM: J.S. Bach"` and `"!!!COM: JS Bach"` will be counted as two
  unique reference records.

## See also

Other corpus summary functions:
[`census()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/census.md),
[`humSummary`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/humSummary.md),
[`interpretations()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/interpretations.md),
[`spines()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/spines.md)

## Examples

``` r
reference('COM')
#> 
#> (Authorship Information)
#> 
#>  !!!COM  =  Composer's name
#> 
#>  Examples:
#>          x!!COM: Chopin, Fryderyk; Chopin, Frederick
#>          x!!COM1: Composer, A. 
#>          x!!COM2: Composer, B.
#> 
reference('OTL')
#> 
#> (Work Identification Information)
#> 
#>  !!!OTL  =  Title

rs <- readHumdrum(humdrumRroot, "HumdrumData/RollingStone/.*hum")
#> Finding and reading files...
#>  REpath-pattern '/home/nat/.tmp/RtmpBDgnJw/temp_libpath28db92437ebb13/humdrumR/HumdrumData/RollingStone/.*hum' matches 13 text files in 1 directory.
#> Thirteen files read from disk.
#> Validating thirteen files...
#> all valid.
#> Parsing thirteen files...
#> Assembling corpus...
#> Done!
reference(rs)
#> 
#> ###### Reference records in humdrumR corpus "rs" (thirteen pieces):
#> ###### By piece:
#>                                         COC  EED  ENC    ***
#>              ACDC_BackInBlack.hum [ 1]    1    1    1    ***
#>      AlGreen_LetsStayTogether.hum [ 2]    1    1    1    ***
#>    CarlPerkins_BlueSuedeShoes.hum [ 3]    1    1    1    ***
#>      DerekAndTheDominos_Layla.hum [ 4]    1    1    1    ***
#>            EltonJohn_YourSong.hum [ 5]    1    1    1    ***
#>   JanisJoplin_MeAndBobbyMcGee.hum [ 6]    1    1    1    ***
#>       JohnnyCash_IWalkTheLine.hum [ 7]    1    1    1    ***
#>           LedZeppelin_Kashmir.hum [ 8]    1    1    1    ***
#>          Nirvana_AllApologies.hum [ 9]    1    1    1    ***
#>      Steppenwolf_BornToBeWild.hum [10]    1    1    1    ***
#> StevieWonder_LivingForTheCity.hum [11]    1    1    1    ***
#>     TheBeachBoys_GodOnlyKnows.hum [12]    1    1    1    ***
#>            TheBeatles_HeyJude.hum [13]    1    1    1    ***
#>                                         COC  EED  ENC    ***
#> 
#> ###### Totals:
#>                                   Any:   13   13   13    ***
#>                                   Sum:   13   13   13    ***
#>                                Unique:   13    1    1    ***
#> 
#> ###### Reference records in humdrumR corpus "rs" (thirteen pieces):
#> 
#>              (six columns not displayed due to screensize***)

reference('COC')
#> 
#> (Authorship Information)
#> 
#>  !!!COC  =  Composer(s) corporate name
reference(rs)[ , 'COC']
#> 
#> ###### Reference records in humdrumR corpus "rs[, j]" (thirteen pieces):
#> ###### By piece:
#>                                        COC
#>             ACDC_BackInBlack.hum [ 1]  AC/DC
#>     AlGreen_LetsStayTogether.hum [ 2]  Al Green
#>   CarlPerkins_BlueSuedeShoes.hum [ 3]  Carl Perkins
#>     DerekAndTheDominos_Layla.hum [ 4]  Derek and the Dominos
#>           EltonJohn_YourSong.hum [ 5]  Elton John
#>  JanisJoplin_MeAndBobbyMcGee.hum [ 6]  Janis Joplin
#>      JohnnyCash_IWalkTheLine.hum [ 7]  Johnny Cash
#>          LedZeppelin_Kashmir.hum [ 8]  Led Zeppelin
#>         Nirvana_AllApologies.hum [ 9]  Nirvana
#>     Steppenwolf_BornToBeWild.hum [10]  Steppenwolf
#> StevieWonder_LivingForTheCity.hum [11]  Stevie Wonder
#>    TheBeachBoys_GodOnlyKnows.hum [12]  The Beach Boys
#>           TheBeatles_HeyJude.hum [13]  The Beatles
#>                                        COC
#> 
#> ###### Totals:
#>                                  Any:  13
#>                                  Sum:  13
#>                               Unique:  13
#> 
#> ###### Reference records in humdrumR corpus "rs[, j]" (thirteen pieces):

reference(rs)[3, ]
#> 
#> ###### Reference records in humdrumR corpus "rs[i]" (one piece):
#> CarlPerkins_BlueSuedeShoes.hum [3]
#>                          COC: Carl Perkins
#>                          EED: Nathaniel Condit-Schultz
#>                          ENC: Nathaniel Condit-Schultz, automated
#>   In original RS 5x20 subset: True
#>                          ONB: Translated from original encodings in the Rolling Stone Corpus (rockcorpus.midside.com), May 2019; Original transcribers noted in comments in each spine: !D.T. = David Temperley; !T.d.C. = Trevor de Clercq
#>                          OTL: Blue Suede Shoes
#>                          RRD: 1956/
#>      Rolling Stone List Rank: 95
#>                          YOE: David Temperley, Trevor de Clercq
       
```
