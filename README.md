# Readme

<img src=https://user-images.githubusercontent.com/54356140/171158278-b506f752-6b17-4084-bdfe-c426261c4cfa.png width="144" height="121" >

This is the repository of the **`ClimBayes`** package (simple CLIMate models from a BAYESian perspective) in [R](https://www.r-project.org/). It provides data and code to perform **Bayesian inference of climate parameters using multi-box energy balance models (EBMs)**. Please see the manuscript M. Schillinger et al.  "Separating internal and externally-forced contributions to global temperature variability using a Bayesian stochastic energy balance framework" (2022), submitted to Chaos and available as preprint http://arxiv.org/abs/2206.14573, for example application and further documentation of the ClimBayes package.

The primary goal of **`ClimBayes`** is to provide a **versatile and powerful tool for climate parameter estimation** from global mean temperature data. To this end, it combines a Bayesian approach with an N-box simple climate model. To estimate the best fit to the observations, the climate drivers (i.e. radiative forcing) and the temperature response are required as inputs. By combining these inputs with prior information on the model's parameters via Bayes theorem, reliable posterior distribution of the model's parameter can be inferred and used to study the forced temperature response across temporal scales.  

Please find the **`tutorial`-vignette** which explains how to perform the model fit using the HadCRUT4 oberservations and PMIP3 forcing data sets as an example. The vignette also describes how the package can be used to generate synthetic temperature timeseries from the N-box model given arbitrary forcing. The **`config_file`-vignette** explains how to include and document own data sets using `*config.yml`. 

**Authors:** Maybritt Schillinger, Beatrice Ellerhoff, Robert Scheichl, Kira Rehfeld

**Responsibility for this repository:** Maybritt Schillinger ([@m-schillinger](https://github.com/m-schillinger)) and Beatrice Ellerhoff ([@bellerhoff](https://github.com/bellerhoff))

Please see the `./license.md` for terms of use. 

## Installation in RStudio

The package can be easily installed using `devtools` in [R](https://www.r-project.org/):

`require(devtools)` 

`devtools::install_git("https://github.com/paleovar/ClimBayes")`

`library(ClimBayes)` 

Please use your personal github token as password. 

If you'd like to contribute to the package, you might want to clone it (type `git clone https://github.com/paleovar/ClimBayes.git` in the terminal) and open the `.Rproj` in `RStudio`. Please don't hesitate to report issues, bugs, etc. to the authors (maybritt.schillinger(at)iup.uni-tuebingen.de, beatrice.ellerhoff(at)student.uni-tuebingen.de).

## Repository content

Our repository contains all relevant code and data of the `ClimBayes`-package. The structure is very similar to that of many R packages, with a DESCRIPTION, NAMESPACE, README and license file. The directories `./R`, `/man`, `./src` and `./inst` contain internal and external functions, their documentation, imported C++ code as well as the internal data of the package. Tutorial can be found in `./vignettes`. We provide `./tests` with relevant test scripts to check the functionality and speed of the package. `./data-raw` contains example script to load your own data sets. Processed data is stored in `./data`. 

## Data references

As exemplary data sets, the `ClimBayes` package loads data from:
- **Morice, C. P., J. J. Kennedy, N. A. Rayner, and P. D. Jones**, *Quantifying uncertainties in global and regional temperature change using an ensemble of observational estimates: The HadCRUT4 dataset*, Geophysical Research (2012)
- **G. A. Schmidt et al.**, *Climate forcing reconstructions for use in PMIP simulations of the Last Millennium (v1.1)*, Geoscientific Model Development (2012)

## Acknowledgements

We thank the [MET Office](https://www.metoffice.gov.uk/hadobs/hadcrut4/) and the [Paleoclimate Modelling Intercomparison Project Phase III](http://pmip3.lsce.ipsl.fr/) for making available their data from surface air temperature observations, and for compiling and providing reconstructions of radiative forcing.

We also thank the [R Core team](https://www.R-project.org/) and the developers of all packages that `ClimBayes` buids on. Please see `citation()` for details on the R Core Team and `citation("packagename")` for details on the developers of individual packages.

The development of this package has been supported by funds of the Deutsche Forschungsgemeinschaft (DFG, German Research Foundation), [Project No. 395588486](https://gepris.dfg.de/gepris/projekt/395588486?context=projekt&task=showDetail&id=395588486&), by the [PalMod](https://www.palmod.de/) project (subproject no. 01LP1926C), the [Heinrich-Böll-Stiftung](https://boell.de/), and the [Studienstiftung des deutschen Volkes](https://www.studienstiftung.de/). The study benefited from discussions within the [CVAS](https://pastglobalchanges.org/science/wg/cvas/intro) working group, a working group of the [Past Global Changes (PAGES)](https://pastglobalchanges.org/pal) project. We thank the members of the [Earth's climate and environmental dynamics](https://www.iup.uni-heidelberg.de/en/research/paleoclimate-dynamics) and the [SPACY](https://uni-tuebingen.de/climatology/) groups for discussion at different stages of the package development. 

*Maybritt Schillinger & Beatrice Ellerhoff*
