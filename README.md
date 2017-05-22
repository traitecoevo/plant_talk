# Introduction to the `plant` package in R

This code repository contains code needed to reproduce the slides of a talk given on 1st March 2016 in a webinar organised by the BES, to coincide with the release of our paper, appearing as part of the BES special issue [*"Demography beyond the population"*](http://onlinelibrary.wiley.com/subject/code/000046/homepage/cross_journal_special_feature.htm):

Falster D, FitzJohn R, Brännström Å, Dieckmann U, Westoby M (2016) **plant: A package for modelling forest trait ecology and evolution**. *Methods in Ecology and Evolution* 7: 136–146. [10.1111/2041-210X.12525](http://doi.org/10.1111/2041-210X.12525)

**Slides**: A copy of the compiled presentation is available at on figshare at DOI: [10.6084/m9.figshare.3422983.v3](http://doi.org/10.6084/m9.figshare.3422983.v3). And you can [watch the video here](https://www.brighttalk.com/webcast/11201/190899)

# Rebuilding

## Requirements

Fonts (all are freely downloadable)

* [Font Awesome](http://fortawesome.github.io/Font-Awesome/)
* [Yanone Kaffeesatz](https://www.google.com/fonts#UsePlace:use/Collection:Yanone+Kaffeesatz)
* [Bitstream Vera Sans Mono](http://ftp.gnome.org/pub/GNOME/sources/ttf-bitstream-vera/1.10/)


You will also need to have [ghostscript installed](https://www.ghostscript.com/download/gsdnld.html), for image manipulation.

Making the pdf requires a reasonably complete XeLaTeX (and fontspec) installation (e.g. [MacTeX](https://tug.org/mactex/) for OSX or [MikTex](http://miktex.org/) for windows).

To make figures and compile the pdf we we use the [remake](https://github.com/richfitz/remake) package for R. See the info there for installation instructions.

## Running

Download this repo, then open a new R session with this project set as working directory. We use a number of packages, these can be easily installed by remake:

```r
remake::install_missing_packages()
```

Then, to generate the figures in the paper, run

```r
remake::make("figures")
```


And then the slides:

```r
remake::make()
```

# Acknowledgements

Sincere thanks to Rich FitzJohn who generated earlier versions of most figures and is a coauthor on the related paper.
