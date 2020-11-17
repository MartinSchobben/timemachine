
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Introduction to the timemachine App

This app is intended for educational purposes. The interactive plots
should instill a better appreciation of long geological timescales and
our current understanding of past, present and future climate. The
current rate of climate change is unprecedented when compared to what we
know of past climate and extreme climate events, such as the
Paleocene-Eocene Thermal Maximum (PETM; \~56 Ma).

I am still developing the app.

# Data sources and compilation

The surface temperature record spanning the Cretaceous up to the
Holocene is based on the corrected
![\\delta](http://chart.apis.google.com/chart?cht=tx&chl=%5Cdelta
"\\delta")<sup>18</sup>O dataset of Westerhold et al. (2020), and
following the same corrections for the presence of ice as described in
Hansen et al. (2013). The Holocene temperatures are based on the global
temperature anomaly (Standard<sub>5x5Grid</sub>) stack of Marcott et al.
(2013) spliced on top of a 1961–1990 mean temperature of
14![\\,](http://chart.apis.google.com/chart?cht=tx&chl=%5C%2C "\\,")°C
(Hansen et al. 2013). The instrumental HadCrut4 sea surface anomaly
dataset was downloaded from the Climatic Research Unit (University of
East Anglia) and Met Office
[website](https://crudata.uea.ac.uk/cru/data/temperature/HadSST3-gl.dat).
This record was spliced on top of the mean temperature of the Marcott et
al. (2013) record for the interval between 1961 and 1990. Future
extrapolations (scenarios or Representative Concentration Pathways) are
based on the General Circulation Model data generated by the BCC\_CM1
model, which was downloaded from the
[Climate4impact](https://climate4impact.eu/) website. The composite
dataset of the app can be recompiled by running the scripts contained in
the data-raw directory. The function `read_instrum_data()` will generate
a global average value for the instrumental HadCrut4 data, see also the
website above for the same code. The script `reduce_clim_model.R` can be
used to flatten the array of time-incremented model data spanning up
2100.

# Basic usage of the R package

## Installation

You can install the released version of timemachine and run the app from
your local console.

``` r
# Install timemachine from GitHub:
# install.packages("devtools")
devtools::install_github("MartinSchobben/timemachine")
```

## Usage

Load point with `library`.

``` r
library(timemachine)
```

## Run the app

Start the app by running.

``` r
timemachine_app()
```

# Credits

The *timemachine* App is created with *shiny*(Chang et al. 2020) in the
R (R Core Team 2020). The package and app rely on a set of external
packages from the tidyverse universe, including: *dplyr* (Wickham,
François, et al. 2020), *tidyr* (Wickham 2020b), *tibble* (Müller and
Wickham 2020), *ggplot2* (Wickham 2016), *rlang* (Henry and Wickham
2020). Package development is aided by; *devtools* (Wickham, Hester, and
Chang 2020), *roxygen2* (Wickham, Danenberg, et al. 2020), *testthat*
(Wickham 2011). This README file is generated with *knitr* (Xie 2020 ,
2015), *rmarkdown* (Allaire et al. 2020; Xie, Allaire, and Grolemund
2018). The graphics for the chronostratigraphic plots was aided by the
packages; *gridExtra* (Auguie 2017), *gtable* (Wickham and Pedersen
2019), and *Cairo* (Urbanek and Horner 2020).

The book: *Mastering Shiny*, by Wickham (2020a) greatly helped
development of the app.

# References

<div id="refs" class="references">

<div id="ref-rmarkdown1">

Allaire, JJ, Yihui Xie, Jonathan McPherson, Javier Luraschi, Kevin
Ushey, Aron Atkins, Hadley Wickham, Joe Cheng, Winston Chang, and
Richard Iannone. 2020. *Rmarkdown: Dynamic Documents for R*.
<https://github.com/rstudio/rmarkdown>.

</div>

<div id="ref-gridExtra">

Auguie, Baptiste. 2017. *GridExtra: Miscellaneous Functions for "Grid"
Graphics*. <https://CRAN.R-project.org/package=gridExtra>.

</div>

<div id="ref-shiny">

Chang, Winston, Joe Cheng, JJ Allaire, Yihui Xie, and Jonathan
McPherson. 2020. *Shiny: Web Application Framework for R*.
<https://CRAN.R-project.org/package=shiny>.

</div>

<div id="ref-Hansen2013">

Hansen, James, Makiko Sato, Gary Russell, and Pushker Kharecha. 2013.
“Climate sensitivity, sea level and atmospheric carbon dioxide.”
*Philosophical Transactions of the Royal Society A: Mathematical,
Physical and Engineering Sciences* 371 (2001).
<https://doi.org/10.1098/rsta.2012.0294>.

</div>

<div id="ref-rlang">

Henry, Lionel, and Hadley Wickham. 2020. *Rlang: Functions for Base
Types and Core R and ’Tidyverse’ Features*.
<https://CRAN.R-project.org/package=rlang>.

</div>

<div id="ref-Marcott2013">

Marcott, Shaun a., Jeremy D. Shakun, Peter U. Clark, and Alan C. Mix.
2013. “A Reconstruction of Regional.” *Science (New York, N.Y.)* 339
(6124): 1198–1201. <http://www.ncbi.nlm.nih.gov/pubmed/23471405>.

</div>

<div id="ref-tibble">

Müller, Kirill, and Hadley Wickham. 2020. *Tibble: Simple Data Frames*.
<https://CRAN.R-project.org/package=tibble>.

</div>

<div id="ref-rversion">

R Core Team. 2020. *R: A Language and Environment for Statistical
Computing*. Vienna, Austria: R Foundation for Statistical Computing.
<https://www.R-project.org/>.

</div>

<div id="ref-Cairo">

Urbanek, Simon, and Jeffrey Horner. 2020. *Cairo: R Graphics Device
Using Cairo Graphics Library for Creating High-Quality Bitmap (Png,
Jpeg, Tiff), Vector (Pdf, Svg, Postscript) and Display (X11 and Win32)
Output*. <https://CRAN.R-project.org/package=Cairo>.

</div>

<div id="ref-Westerhold2020">

Westerhold, Thomas, Norbert Marwan, Anna Joy Drury, Diederik Liebrand,
Claudia Agnini, Eleni Anagnostou, James S. K. Barnet, et al. 2020. “An
astronomically dated record of Earth’s climate and its predictability
over the last 66 million years.” *Science* 369 (6509): 1383–8.
<https://doi.org/10.1126/SCIENCE.ABA6853>.

</div>

<div id="ref-testthat">

Wickham, Hadley. 2011. “Testthat: Get Started with Testing.” *The R
Journal* 3: 5–10.
<https://journal.r-project.org/archive/2011-1/RJournal_2011-1_Wickham.pdf>.

</div>

<div id="ref-ggplot2">

———. 2016. *Ggplot2: Elegant Graphics for Data Analysis*.
Springer-Verlag New York. <https://ggplot2.tidyverse.org>.

</div>

<div id="ref-Wickham2020">

———. 2020a. *Mastering shiny; Build interactive apps, reports &
Dashboards*.

</div>

<div id="ref-tidyr">

———. 2020b. *Tidyr: Tidy Messy Data*.
<https://CRAN.R-project.org/package=tidyr>.

</div>

<div id="ref-roxygen2">

Wickham, Hadley, Peter Danenberg, Gábor Csárdi, and Manuel Eugster.
2020. *Roxygen2: In-Line Documentation for R*.
<https://CRAN.R-project.org/package=roxygen2>.

</div>

<div id="ref-dplyr">

Wickham, Hadley, Romain François, Lionel Henry, and Kirill Müller. 2020.
*Dplyr: A Grammar of Data Manipulation*.
<https://CRAN.R-project.org/package=dplyr>.

</div>

<div id="ref-devtools">

Wickham, Hadley, Jim Hester, and Winston Chang. 2020. *Devtools: Tools
to Make Developing R Packages Easier*.
<https://CRAN.R-project.org/package=devtools>.

</div>

<div id="ref-gtable">

Wickham, Hadley, and Thomas Lin Pedersen. 2019. *Gtable: Arrange ’Grobs’
in Tables*. <https://CRAN.R-project.org/package=gtable>.

</div>

<div id="ref-knitr2">

Xie, Yihui. 2015. *Dynamic Documents with R and Knitr*. 2nd ed. Boca
Raton, Florida: Chapman; Hall/CRC. <https://yihui.org/knitr/>.

</div>

<div id="ref-knitr1">

———. 2020. *Knitr: A General-Purpose Package for Dynamic Report
Generation in R*. <https://yihui.org/knitr/>.

</div>

<div id="ref-rmarkdown2">

Xie, Yihui, J. J. Allaire, and Garrett Grolemund. 2018. *R Markdown: The
Definitive Guide*. Boca Raton, Florida: Chapman; Hall/CRC.
<https://bookdown.org/yihui/rmarkdown>.

</div>

</div>
