# humdrumR

`humdrumR` is a R library for analyzing musicological data encoded in the [humdrum](www.humdrum.org) data format.



## Installation

### Installing R

To use `humdrumR`, you'll first need to install [R](https://www.r-project.org/), version 3.2 or later.
We *highly* recommend that you install the Integrated Development Environment [Rstudio](https://rstudio.com/) as well!
This link---[Installing R and RStudio](https://rstudio-education.github.io/hopr/starting.html)---is a good starting place.

### Installing humdrumR

`humdrumR` is not yet available on the standard R package repository, [CRAN](https://cran.r-project.org/), so (for now) it can only be installed from this github repository.
To install the latest version (master branch) of `humdrumR`, you'll first need to install the R package [devtools](https://www.rdocumentation.org/packages/devtools/versions/2.2.1)---`devtools` is used for creating and maintaining R packages.
Luckily, `devtools` is on CRAN, so all you have to do is open an R session on your machine and type:

```{r}

install.packages('devtools')

```

Once you have successfuly installed `devtools`, you are now ready to use it to install `humdrumR` straight from github.
In an R session, type:

```{r}

devtools::install_github("Computational-Cognitive-Musicology-Lab/humdrumR", build_vignettes = TRUE)

```

### humdrumR source

Another option---useful, if you want to inspect, or contribute to, the `humdrumR` source code---is to actually download the `humdrumR` package source from github and install it directly.
You'll first need to install [git](https://git-scm.com/book/en/v2/Getting-Started-Installing-Git).
Once git is installed on your computer, you can download the `humdrumR` repository by clicking on the "Clone or download" on the github page.

Alternatively, you can use git directly: navigate to the directory on machine where you'd like to save the package source and type:


```

git clone https://github.com/Computational-Cognitive-Musicology-Lab/humdrumR

```

Once you've done this, you can install the source on your computer using `devtools`:
Open an R session and use `setwd` to move your working directory inside the repository you just downloaded (i.e., cloned).
Once there, type 

```{r}

devtools::install()

```


> If you downloaded Rstudio, you can use the "Open Project" option and select the file `humdrumR.Rproj` from the repository.
> Once you've done this you can just click "Install and Restart" in the "Build" pane.

---

Once you've completed the installtion of `humdrumR` you can used it by simply calling:

```{r}

library(humdrumR)

```

## Documentation

At this stage, most humdrumR functions are at least partially documented, so you can easily read about them by using the `?` operator with the name of a function.
A good place to start is:

```{r}

library(humdrumR)

?humdrumR

```

You should also check out the "vignettes" for `humdrumR`, which offer a easier introduction to the package.
If you installed from github using the `build_vignettes = TRUE` option, you should be able to simply call:

```{r}

browseVignettes("humdrumR")

```

