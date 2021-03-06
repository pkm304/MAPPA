
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
data points in training sets with the ???importance??? and ???localImp???
options turned on, respectively. Variable importance is the most crucial
information to understand the systems??? behaviors in terms of the
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
    space defined by the plausible prameter space. Additional sampling
    can be performed adpatively focusing on certain region of interest
    later on.

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

In this section, we walk through MAPPA using an example model of the
germinal center (GC) reaction, which is an ODE model describing the
interactions of germinal center B cells, T follicular helper cells, and
T follicular regulatory cells at the cell population level to dissect
the duration of GC
reactions.

<img src="man/figures/GC_model.png" width="400" />

#### 1\. Define a dynamical model and determine parameters to be varied with their associated plausible ranges.

First, based on typical parameter values and biological consideration,
we define a set of parameter ranges by choosing parameters to vary and
defining their associated rangee as a data frame with columns of `name`,
`min`, `max`. Save the data frame object in the csv format. Please
adhere to this format specified for MAPPA.

``` r
# Typical parameter values
prms.typical <- c(K_tfh_blz = 0.1,
                                    n_tfh_blz = 1,
                                    r_tfh_0 = 0.7,
                                    d_tfh_0 = 0.7,
                                    K_blz_tfh = 1.2,
                                    n_blz_tfh = 1,
                                    r_tfr_0 = 0.7,
                                    d_tfr_0 = 0.7,
                                    K_blz_tfr = 10,
                                    n_blz_tfr = 1,
                                    K_tfr_tfh = 1/5)

# Parameter ranges
# Users flexiably specifiy ranges based on their own purpose and qustions.
# For simplicity, MAPPA deals with only positive values. Respective signs can be restored at the actual simulation codes.
prms.ranges <- data.frame(name = c("K_tfh_blz", "n_tfh_blz", "K_blz_tfh", "n_blz_tfh",  "K_blz_tfr", "n_blz_tfr", "K_tfr_tfh"),
                                                 min =  c(0.05,         0.2,          1,           0.2,         5,          0.2,           0.2      ),
                                                 max =  c(0.15,         5,            1.5,         5,           10,           5,            0.5 ))

#Save as csv files with header names, names, min, max
write.csv(prms.ranges, file = "examples/prm_ranges.csv", quote = F, row.names = F)
```

#### 2\. Launch MAPPA using `launch_MAPPA()`, preferablly from a new R session.

``` r
library(MAPPA)
launch_MAPPA()
```

#### 3\. Create and save a PPM object.

<img src="man/figures/create_PPM.png" width="600" />

Since currently MAPPA is not stable, please make sure save the PPM
object frequently not to lose the analysis
results.

<img src="man/figures/save_PPM1.png" width="600" />

<img src="man/figures/save_PPM2.png" width="600" />

#### 4\. Register parameter ranges to PPM and sample parameter combinations

Go to the `Parameter combination sampling` ???\> `Initial sampling` tab.

<img src="man/figures/prm_smpl_tab.png" width="600"/>

Load the parameter ranges specified above. Or, the parameter ranges can
be specified in MAPPA.

<img src="man/figures/load_prm_ranges.png" width="600"/>

Specify parameter sampling options.

<img src="man/figures/gen_prm_combs.png" width="600"/>

Register the sampled parameter combinations to PPM and save as a text
file.

<img src="man/figures/save_prmset1.png" width="600"/>

<img src="man/figures/save_prmset2.png" width="600"/>

#### 5\. Generate tSNE or UMAP emebedding of the sampled parameter combinations.

We have not included tSNE and UMAP in MAPPA yet. Users are encouraged to
generate the embeddings on their own.

``` r
library(Rtsne)

#Load the PPM object.
PPM.obj <- readRDS("examples/PPM_GC_reaction.Rds")

#Obtain the generated parameter combinations.
prmset.list <- get.init.prm.combs(object = PPM.obj,
                                                                    prm.ranges.name = "prm.ranges", #Specify the exact name of the parameter ranges defined in MAPPA
                                                                    name = "prmset" ##Specify the exact name of the parameter combinations defined in MAPPA
                                                                    )

names(prmset.list)
```

    ## [1] "method"      "log.scale"   "raw.smpl"    "prm.combs"   "prm.combs.z"
    ## [6] "prmset"

There are 6 elements in the retieved list.

  - method: sampling method.
  - log.scale: whether parameters are sampled in the log scale or not.
  - raw.smpl: raw values of sampled parameter combinations between 0 and
    1 for Sobol???, pseudorandom, and latin hypercube.
  - prm.combs: sampled parameter combinations.
  - prm.combs.z: standardized sampled parameter combinations.
  - rd\_seed: seed numbers assigned to each parameter combination for
    stochastic simulations.

<!-- end list -->

``` r
#Use the standardized parameter combinations for embedding
if(0){ #Do not run for the tutorial since it may takes long.
    prmset.tsne <- Rtsne( prmset.list$prm.combs.z[,2:8])
  prmset.tsne <- data.frame(pkey = prmset$pkey, prmset.tsne$Y)
  names(prmset.tsne) <- c("pkey", "tSNE1", "tSNE2")
}
#Loading precomputed tSNE for this tutirial, 
prmset.tsne <- readRDS("examples/tSNE_tutorial.Rds")


# Register the tSNE embedding to the PPM object
PPM.obj <- readRDS("examples/PPM_GC_reaction.Rds")
add.tsne.coord(PPM.obj) <- list( prm.combs.name = "prmset", #Specify the exact name of the parameter combinations defined in MAPPA
                                       tsne.coord = prmset.tsne)
saveRDS(PPM.obj,"examples/PPM_GC_reaction.Rds")
```

#### 6\. Conduct simulations across sampled parameter combinations, aggregate simulation results, define (a) phenotype(s) of interest, and register to PPM. (This should be done outside of MAPPA.)

Users can conduct simulations for the model across sampled parameter
combinations with any tools of their preferences. For the GC reaction
model, we conducted the simulations usmg the Matlab. Here, we showcase
MAPPA using the simulations results in scheme 2 (delayed TFR):

<img src="man/figures/simulation_scheme.png" width="400"/>

We aggregated the simulation results for the demonstration purpose here.

``` r
#Load simulation results
df.sim.results <- readRDS("examples/sim_results_tutorial.Rds")

#Plot all simulation results
if(0){
library(tidyverse)
df.sim.results %>%   ggplot() +geom_line(aes(x=Time, y = B_gc, group = pkey), alpha = 0.05) 
df.sim.results %>%   ggplot() +geom_line(aes(x=Time, y = TFH, group = pkey), alpha = 0.05)
df.sim.results %>%   ggplot() +geom_line(aes(x=Time, y = TFR, group = pkey), alpha = 0.05)
}
```

<img src="man/figures/sim_results.png" width="600"/>

Then, we focused on how long the GC rection persist. Therefore, we
defined the phenotype of durtation of the GC reaction as area under
curve of the time trajectory of the total GC B cells divided by the max
level of the total GC B cells across the time, denoted as `Dur.GC`.

``` r
# Defining various phenotypes
library(tidyverse)
phens_delay_TFR <- df.sim.results %>% group_by(pkey) %>% summarise(max.B_gc = max(B_gc), time.max.B_gc = Time[which.max(B_gc)], min.B_gc = min(B_gc), min.TFR = min(TFR), max.TFR = max(TFR), B_gc.at.1000 = B_gc[length(B_gc)],  min.TFH = min(TFH), max.TFH = max(TFH) )  %>% data.frame()
phens_delay_TFR <- phens_delay_TFR %>% mutate(B_gc.ratio.1000.max = B_gc.at.1000/max.B_gc)

#Function for trapezoidal integration to obtain area under curve (AUC).
AUC = function(mat){
  mat = mat[order(mat[,1]),]
  N = nrow(mat) - 1
  mat[is.na(mat)] = 1
  sum = 0
  for(i in 1:N){
    sum = sum + (mat[i,2]+mat[i+1,2])*(mat[i+1,1]-mat[i,1])/2
  }
  return(sum)
}

if(0){# Do not run
    phens_delay_TFR$AUC.B_gc <- sapply(X = phens_delay_TFR$pkey, function(x){ # takes long
        mat <- df.sim.results %>% filter(pkey == x) %>% select(Time,B_gc)
        return(AUC(mat))
    })
}

#Load the precomputed result for this tutorial
phens_delay_TFR$AUC.B_gc <- readRDS("examples/AUC.B_GC_tutorial.Rds")
phens_delay_TFR$dur.B_gc <- phens_delay_TFR$AUC.B_gc/phens_delay_TFR$max.B_gc
hist(phens_delay_TFR$dur.B_gc)
```

![](README_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

``` r
# Register the phenotype, Dur.GC to the PPM object
PPM.obj <- readRDS("examples/PPM_GC_reaction.Rds")
add.phenotypes(PPM.obj) <-  list(name = "dur.GC_delay_TFR", #Specify the name of the phenotype
                             prm.combs.name = "prmset",
                             phenotype = data.frame(pkey = phens_delay_TFR$pkey, #Specify the exact name of the parameter combinations defined in MAPPA
                                                                         dur.GC_delay_TFR = phens_delay_TFR$dur.B_gc) #Specify the same name of the phenotype
                            )
saveRDS(PPM.obj,"examples/PPM_GC_reaction.Rds")
```

#### 7\. (Back to MAPPA) Train machine learning (ML) models in a flexible manner.

Be sure to reload the modified PPM object with the new tSNE/UMAP
embedding and the phenotype, Dur.GC.

<img src="man/figures/ML_model_1.png" width="600"/>

Go to the `ML model training` tab and specify the phenotype, set(s) of
parameter combinations, input parameters, and model types (regression or
classification). For imbalanced datasets, setting `Balanced training`
option as Yes perform balanced sampling of data from each class building
each tree.

<img src="man/figures/ML_model_2.png" width="600"/>

Click the `Further manipuation` button for additional specification of
ML model training. Especially, for classification models, the output
classes should be defined here. Then, click the `Train ML model!` for
model training.

<img src="man/figures/ML_model_3.png" width="600"/>

Once the ML model training is done, the results are shown. Users can
register the trained ML models to the PPM or remove them. Prediction
performaces can be examined.

<img src="man/figures/ML_model_4.png" width="600"/>

<img src="man/figures/ML_model_5.png" width="600"/>

The procedures of training regression ML models are similar to those
explained for classification models.

<img src="man/figures/ML_model_6.png" width="600"/>

The results of regression ML models are shown as below.

<img src="man/figures/ML_model_7.png" width="600"/>

<img src="man/figures/ML_model_8.png" width="600"/>

#### 8\. Explore PPM

1.  Launching PPM visualization

Select a phenotype of interest, parameter sets in t-SNE, and a ML model,
and then click the ???Launch Visualization\!??? button. Then a t-SNE plot
for selected parameter sets will come up below.

<img src="man/figures/PPM_exp_1.png" width="600"/>

2.  Manipulation of tSNE/UMAP plot, GVI of ML model, and selecting
    points

Adjust various aspects of the t-SNE/UMAP plot, such as the color scale,
the size of points, and transparency of points. Users can select a point
by a single click, through which original and scaled parameter values of
the selected point along with the corresponding parameter key is shown.
Users can select multiple points by dragging the mouse pointer on the
plot. By double clicking the dragged region, Users can zoom in further.

<img src="man/figures/PPM_exp_2.png" width="600"/>

3.  Further information

Click the `Further Info for selected ML model` button for further
information, such as the training results and prediction accuracies of
the ML model.

<img src="man/figures/PPM_exp_3.png" width="600"/>

<img src="man/figures/PPM_exp_4.png" width="600"/>

4.  Heatmaps of parameter combinations and LVIs for dragged points

In the `Heatmaps for selected points` tab, visualize individual
parameter combinations and local variable importances (LVIs) for dragged
points by hierarchical clustering heatmaps. Define clusters for the LVI
heatmap and inspect them through the tSNE/UMAP plot.

<img src="man/figures/PPM_exp_5.png" width="600"/>

<img src="man/figures/PPM_exp_6.png" width="600"/>

<img src="man/figures/PPM_exp_7.png" width="600"/>

<img src="man/figures/PPM_exp_8.png" width="600"/>

5.  LVI and in-silico perturbation

In the `LVI and parameter perturbation` tab, the LVI of the selected
parameter combination is shown. Under the guidance of the LVI, which
suggests the phenotype???s local sensitivity to each parameter, users can
perturb two parameters and see the prediction of the phenotype in a 2D
heatmap or 3D surface plot. Moreover, users can generate and save the
perturbed parameter combinations for further validation simulation.

<img src="man/figures/PPM_exp_9.png" width="600"/>

<img src="man/figures/PPM_exp_10.png" width="600"/>

5.  Validation of in-silico perturbation

In the `Validation` tab, further visualize the results of validation
simulations in comparison with predictions of in-silico parameter
perturbations. With the scatter plot in the rightmost, the validity of
the prediction of the in-silico parameter perturbation can be assessed.

#### Advanced usages

##### Addtional parameter sampling

``` r
# prm.combs <- prmset.list$prm.combs
# phens_delay_TFR$pkey <- as.character(phens_delay_TFR$pkey)
# 
# prm.combs.sel <- prm.combs %>% filter(pkey %in% (phens_delay_TFR %>% filter(dur.B_gc >= 100 & dur.B_gc < 800) %>% select(pkey) %>% c() %>% unlist()))
# 
# write.csv(prm.combs.sel, "examples/prmset.sel.csv", quote = F, row.names = F)
```

##### Applying filtering
