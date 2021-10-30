
# MAPPA

This is a standalone version of MAPPA (MAchine learning of
Parameter-Phenotype mapping for Analyzing dynamical biological systems
models) implemented using the R Shiny package. Please check the website
demonstrating a part of MAPPA for interactive figures
(<https://phasespace-explorer.niaid.nih.gov>).

## Table of Contents

1.  [How to install and run](#how_to_install_and_run)
2.  [Key concepts for using MAPPA](#key_concepts_for_using_MAPPA)
3.  [How to use](#How_to_use)

## How to install and run <a name="how_to_install_and_run"></a>

Install this package using `devtools`. Then, run the function
`launch_MAPPA`.

``` r
devtools::install_github("pkm304/MAPPA")
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

## How to use<a name = "How_to_use"></a>

Once MAPPA is launched, A Shiny App will begin and show overall
conceptual schematics of MAPPA.

<img src="man/figures/Initial.png" width="500" />

### Overall structure of the app

There are 4 main pages that can be selected at the sidebar. Empty
default pages are shown below.

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

### Quick tutorial
