# Humdrum$_{\mathbb{R}}$

Humdrum$_{\mathbb{R}}$ is an R library for analyzing musicological data encoded in the [humdrum](https://www.humdrum.org) data format.
The most up to date info on using humdrumR$_{\mathbb{R}}$ be found at our [github website](https://computational-cognitive-musicology-lab.github.io/humdrumR/).





## Installation

### Installing R

To use humdrum$_{\mathbb{R}}$, you'll first need to install [R](https://www.r-project.org/), version 4.1 or later.
We *highly* recommend that you install the Integrated Development Environment [Rstudio](https://rstudio.com/) as well!
This link---[Installing R and RStudio](https://rstudio-education.github.io/hopr/starting.html)---is a good starting place.

### Installing humdrum$_{\mathbb{R}}$

Humdrum$_{\mathbb{R}}$ is not yet available on the standard R package repository, [CRAN](https://cran.r-project.org/), so (for now) it can only be installed from this github repository.
To install the latest version (master branch) of humdrum$_{\mathbb{R}}$, you'll first need to install the R package [devtools](https://www.rdocumentation.org/packages/devtools/versions/2.2.1)---`devtools` is used for creating and maintaining R packages.
Luckily, `devtools` is on CRAN, so all you have to do is open an R session on your machine and type:

```{r}

install.packages('devtools')

```

Once you have successfully installed `devtools`, you are now ready to use it to install humdrum$_{\mathbb{R}}$ straight from github.
In an R session, type:

```{r}

devtools::install_github("Computational-Cognitive-Musicology-Lab/humdrumR", build_vignettes = TRUE)

```

#### Humdrum$_{\mathbb{R}}$ source

Another option---useful, if you want to inspect, or contribute to, the humdrum$_{\mathbb{R}}$ source code---is to actually download the humdrum$_{\mathbb{R}}$ package source from github and install it directly.
You'll first need to install [git](https://git-scm.com/book/en/v2/Getting-Started-Installing-Git).
Once git is installed on your computer, you can download the humdrum$_{\mathbb{R}}$ repository by clicking on the "Clone or download" on the github page.

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

Once you've completed the installation of humdrum$_{\mathbb{R}}$ you can used it by simply calling:

```{r}

library(humdrumR)

```

## Documentation

At this stage, most humdrum$_{\mathbb{R}}$ functions are documented, and all the documentation can be found 
on our [github website](https://computational-cognitive-musicology-lab.github.io/humdrumR/).
The "Articles" list at the top of the page is the best place to start,
in particular the [Overview of humdrumR](https://computational-cognitive-musicology-lab.github.io/humdrumR/articles/Overview.html) and
[Getting started with humdrumR](https://computational-cognitive-musicology-lab.github.io/humdrumR/articles/GettingStarted.html) articles.
Documentation for specific functions and other general topics can be found in the "[Reference](https://computational-cognitive-musicology-lab.github.io/humdrumR/reference/index.html)"
section.

Once humdrum$_{\mathbb{R}}$ is installed, you can also access documentation directly in R sessions.
On the R command line, use the `?` operator with the name of a function or topic to see the documentation:

```{r}

library(humdrumR)

?humdrumR

```
