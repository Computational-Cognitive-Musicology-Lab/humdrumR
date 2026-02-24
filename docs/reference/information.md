# Information theory

Many computational musicology analyses rely on probabilistic modeling
and [information
theory](https://en.wikipedia.org/wiki/Information_theory). HumdrumR
includes functions to make these sorts of analyses quick and easy. These
functions are closely connected to our
[distribution](https://humdrumR.ccml.gtcmt.gatech.edu/reference/distribution.md)
functions, which can be used to calculate/estimate the probability of
data observations.

## Details

The most fundamental tools of information theory are statistics that
characterize probability *distributions*. Thus, they are descriptive
statistics, which describe a distribution (usually, the distribution of
values in your data) using a single number. Such information-theoretic
descriptive statistics can be computed using the
[`entropy()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/entropy.md)
(joint or conditional entropy),
[`xentropy()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/entropy.md)
(cross entropy),
[`kld()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/entropy.md)
(Kullback–Leibler divergence), and
[`mutual()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/mutual.md)
(mutual information) functions. In contrast, other information theory
metrics are calculated "point-wise": one value for each data
observation. Our point-wise information theory functions are
[`data.table::like()`](https://rdrr.io/pkg/data.table/man/like.html)
(likelihood),
[`info()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/entropy.md)
(information content),
[`pentropy()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/entropy_by.md)
(pointwise conditional entropy), and
[`pmutual()`](https://humdrumR.ccml.gtcmt.gatech.edu/reference/mutual.md)
(pointwise mutual information).

Note that all of these functions calculate or utilize *empirical*
statistics—i.e., they describe *your data*. They are not (necessarily)
representative of the "true" information content in real music. They may
be used as *estimates* of the "true" entropy of music we study, but this
assumes that our sample is representative and that our probabilistic
models make sense (i.e., make valid assumptions).
