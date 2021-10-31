
# MAPPA

This is a standalone version of MAPPA (MAchine learning of
Parameter-Phenotype mapping for Analyzing dynamical biological systems
models) implemented using the R Shiny package. Please check the website
demonstrating a part of MAPPA for interactive figures
(<https://phasespace-explorer.niaid.nih.gov>).

## Table of Contents

1.  [How to install and run](#how_to_install_and_run)
2.  [Key concepts for using MAPPA](#key_concepts_for_using_MAPPA)
3.  [Overall structure of the app](#Overall_structure)
4.  [Quick tutorial](#Quick_tutorial)

## How to install and run <a name="how_to_install_and_run"></a>

Install this package using `devtools`. Then, run the function
`launch_MAPPA`.

``` r
devtools::install_github("pkm304/MAPPA")
library(MAPPA)
launch_MAPPA()
```

## Key concepts for using MAPPA <a name="key_concepts_for_using_MAPPA"></a>

#### PPM (Parameter-Phenotype Map)

A Parameter-Phenotype Map (PPM) consists of parameter space, phenotypic
space, and Machine Learning (ML) models connecting parameter space and
phenotypic space. ML models can be considered as phenomenological
solutions of dynamical models, trained by a data-driven way.

<img src="man/figures/PPM.png" width="500" />

#### Parameter space

A parameter space consists of kinetic parameters of dynamical models and
their feasible ranges. As dynamical models become realistic, the
dimensionality of the parameter space also increases. Practically we
randomly sample parameter combinations to cover the entire parameter
space. To intuitively visualize high-dimensional space, we apply
t-distributed Stochastic Neighbor Embedding (t-SNE) for the sampled
parameter combinations to plot in 2D.

#### Phenotypic space

A phenotypic space consists of a single or multiple phenotype(s). Each
phenotype is quantitative or categorical values defining a certain
aspect of simulation outcomes throughout the parameter space of a
dynamical model, such as single cell gene-gene expression correlations
or quality factor for oscillation as in the main paper.

#### Machine Learning (ML) model

A ML model is a random forest regression/classification model. Parameter
combinations and phenotypes are input data and output data, respectively
for training ML models. In realistic dynamical models with
high-dimensional parameter spaces, nonlinearity, and stochasticity,
analytical solutions to describe quantitative relationships between
parameter and phenotypic spaces are often intractable. Here, ML models
can be considered as phenomenological solutions of dynamical models,
trained by a data-driven way. Not only ML model can predict phenotypes
for given parameter combinations, ML models can also output variable
importance in global and local levels, which leads us to better
understand behaviors of dynamical models.

#### Variable Importance

The random forest algorithm implemented as the randomForest package in R
generates variable importance both in the global level averaged for all
data points in training sets and in the local level for each individual
data points in training sets with the ‘importance’ and ‘localImp’
options turned on, respectively. Variable importance is the most crucial
information to understand the systems’ behaviors in terms of the
controllability of each of kinetic parameters. Although the global
variable importance (GVI) gives a general overview, the behaviors in the
neighborhood of each region of the parameter space can differ each
other, and the local variable importance (LVI) does capture this aspect.
We can cluster the patterns of LVIs with the hierarchical clustering
method and visualize them along with t-SNE plots.

#### Parameter key

Each parameter combination is assigned its own parameter key to make it
easily distinguishable and accessible. The basic syntax of parameter
keys in this paper is to assign the date when parameter combinations are
generated and capital letter combinations (e.g., 042015\_AAACEZGP).

## Overall structure of the app <a name = "Overall_structure"></a>

### Graphical user interface

Once MAPPA is launched, A Shiny App will begin showing following default
pages.

  - Overview
    
    <img src="man/figures/Initial.png" width="500" />

  - Workspace
    
    <img src="man/figures/Workspace.png" width="700" />

  - Parameter combination sampling
    
      - Initial sampling
    
    <img src="man/figures/Init_sampling.png" width="700" />
    
      - Additional sampling
    
    <img src="man/figures/Add_sampling.png" width="700" />

  - ML model training
    
    <img src="man/figures/ML_train.png" width="700" />

  - Explor PPM
    
    <img src="man/figures/Exp_PPM.png" width="700" />
    
    omitted
    
    <img src="man/figures/Exp_PPM_2.png" width="700" />

### PPM object

1.  Creating a PPM object

This is the central object in the MAPPA package implemented as S4 class
type. A PPM object houses parameter space, phentipic space, and
ml.models with associated functions to add to and obtain from those
elements from a PPM object. A PPM object can be created in the Shiny app
or through commands:

``` r
library(MAPPA)
PPM.obj <- func_create_PPM( PPM.name = "Hello World!")
get.PPM.name(PPM.obj)
```

    ## [1] "Hello World!"

2.  Inspect/Retrieving the elements of the PPM object.

Users can inspect the inside of the created PPM object using access
functions or directly using `@` after the object name in the R console.

  - using `get.***` functions

<!-- end list -->

``` r
ppm.name <-  get.PPM.name(object = PPM.obj)
ppm.prms.space <- get.prm.ranges(object = PPM.obj)
## There are other functions to access the objects.
```

  - or use `@`

<!-- end list -->

``` r
PPM.obj@
```

There are several more `get.***` functions to be listed below. Users do
not have to know all of these functions and can almost entirely rely on
the Shiny app. However, some functions are necessary since MAPPA does
not provide all extra packages, yet. This will be explained in the Quick
tutorial section when required.

3.  Overall data structure of a PPM object

Since the created object `PPM.obj` is almost empty, all the elements
preset at the creation can be shown as below. (Please do not try this
with an objects with filled elements).

``` r
PPM.obj
```

    ## An object of class "PPM"
    ## Slot "PPM.name":
    ## [1] "Hello World!"
    ## 
    ## Slot "model":
    ## list()
    ## 
    ## Slot "prm.space":
    ## $prm.ranges
    ## list()
    ## 
    ## $initial.prm.combs
    ## list()
    ## 
    ## $additional.prm.combs
    ## list()
    ## 
    ## $counter
    ## $counter$unif_grid
    ## [1] 0
    ## 
    ## $counter$pseudoranddom
    ## [1] 0
    ## 
    ## $counter$`sobol'`
    ## [1] 0
    ## 
    ## $counter$latin_hyp
    ## [1] 0
    ## 
    ## 
    ## 
    ## Slot "phenotypes":
    ## list()
    ## 
    ## Slot "ml.models":
    ## list()
    ## 
    ## Slot "analysis":
    ## $tsne
    ## list()
    ## 
    ## 
    ## Slot "simulation":
    ## list()

## Quick tutorial <a name="Quick_tutorial"></a>

### Overall workflow

1.  Define a dynamical model and determine parameters to be varied with
    their associated plausible ranges.

2.  Create a PPM object.

3.  Sample initial parameter combinations uniformly across parameter
    space defined by the plausible prameter space. Addition sampling can
    be performed adpatively focusing on certain region of interest later
    on.

4.  (Not part of MAPPA) Run simulations for sampled parameter
    combinations and obtain phenotypes of interest out of the simulation
    results.

5.  (Back to MAPPA) Register the obtained phenotypes out of simulations
    to the PPM object (to be explained in the Example section below).

6.  Train machine learning models mapping parameter space to phenotypes
    obtained from simulations, thereby obtaining all emements of PPMs
    (parameter space, phenotypic space, machine learning models mapping
    parameter space and phenotypic space).

7.  Explore PPMs using various visualization methods.

### Example

In this section, we work through MAPPA using an example model from
<a href ='https://ascpt.onlinelibrary.wiley.com/doi/10.1002/psp4.29'>
(Tavassoly et al., 2015)</a>, which is a detailed model describing
interplay between autophagy and apoptosis. Users should use both Shiny
App and R concole
interactively.

#### Define a dynamical model and determine parameters to be varied with their associated plausible ranges

First, we take optimal model parameter values provided by the model
provided by Tavasolly et
al.

``` r
prms.values.optimum <- c(C=20, BCL2T=3, IP3RT=1, CaT=2, CASP =0, wjnk_s = 1, wdapk_s = 1, 
                                                 wbcl_j = 1, wmtor_ca = -1, watg_m = -1, wbh_a = 1, wbec_d = 1, Wcp_Ca = 1, 
                                                 W_ATG5C = -1, ka = 1.77, kda = 0.0948, ksb = 2.08, kc = 0.510, krb = 1.41, 
                                                 kra = 3.83, Gj = 1.73, Gd = 4.05, Gb = 5.21, Gt = 1.61, Gg = 1.04, Gh = 0.0101, 
                                                 Gl = 3.43, Ga = 0.5240, Gcp = 1.95, sigmaj = 2.42, sigmad = 1.01, sigmab = 4.57,
                                                 sigmah = 2.89, sigmag = 20.8, sigmat = 3.51, sigmal = 7.99, sigmacp = 32.3, 
                                                 sigmaA = 4.83, kcasp = 2.01, kin = 9.64, kout = 6.31, wjnk0 = -1.22, wdapk0 = -2.98,
                                                 wbcl0 = -0.614, wmtor0 = 0.202, watg0 = 0.144, wbh0 = -1.26, wbec0 = -0.647, 
                                                 W_ATG50 = 0.215, Wcp_0 = -0.514, wbh_s = 0.0801, wbh_c = 0.0003)
```

Then, based on biological consideration, choose what parameters to vary
and determine associated ranges to sample from. An example is as below.

``` r
# Chosen parameters to be varied
prms.varied <- c("C", "BCL2T", "ka","kda", "ksb", "kc", "krb",
                                 "kra", "Gj", "Gd", "Gb", "Gt", "Gg", "Gh", "Gl", 
                                 "Ga", "Gcp", "sigmaj", "sigmad", "sigmab", "sigmah", 
                                 "sigmag", "sigmat", "sigmal", "sigmacp", "sigmaA", "kcasp", 
                                 "kin", "kout", "wjnk0", "wdapk0", "wbcl0", "wmtor0", "watg0", 
                                 "wbh0", "wbec0", "W_ATG50", "Wcp_0", "wbh_s", "wbh_c")


# Determine ranges for chosen prms. In this example we set the range 0.2 fold ~ 1.8 fold of the optimal values. 
# Users flexiably specifiy ranges based on their own purpose and qustions.
# For simplicity, MAPPA deals with only positive values. Respective signs can be restored at the actual simulation codes.
prms.values.optimum.varied  <- abs(prms.values.optimum[prms.varied]) 
prms.optimum.varied.min <- prms.values.optimum.varied - 0.8*prms.values.optimum.varied # the lower limits of the ranges
prms.optimum.varied.max <- prms.values.optimum.varied + 0.8*prms.values.optimum.varied # the upper limits of the ranges

prm.ranges <- data.frame(names = names(prms.values.optimum.varied),
                                                 min = prms.optimum.varied.min,
                                                 max = prms.optimum.varied.max)

#Save as csv files with header names, names, min, max
write.table(prm.ranges, file = "examples/prm_ranges.csv", quote = F, row.names = F, col.names = T, sep = ",")
```

Next, launch MAPPA using `launch_MAPPA()`, preferablly from a new R
session.
