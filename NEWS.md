# climwin 1.0.0

## Major changes

`climwin` version 1.0.0 includes a number of major changes to coincide with the release of our corresponding paper [[1](http://onlinelibrary.wiley.com/doi/10.1111/2041-210X.12590/full)]. We have tried to make most of our changes backwards compatible, but any major issues should be reported to the package maintainer (liam.bailey@anu.edu.au).

### New function *slidingwin*:
`climwin` aims to distinguish between two separate methods for testing climate windows. The commonly used sliding window approach [[1](http://onlinelibrary.wiley.com/doi/10.1111/2041-210X.12590/full)] and less common weighted window approach [[2](http://www.journals.uchicago.edu/doi/abs/10.1086/659101)]. To ensure the distinction between these two methods is clear, the function *climatewin* has been made redundant and been replaced with the function *slidingwin*. Parameters used in *slidingwin* and *climatewin* are identical. Users can now conduct a sliding window analysis using *slidingwin* and a weighted window analysis using *weightwin*. 

### *randwin* for weighted window analysis:
The function *randwin* can now also be used to conduct randomisations with a weighted window approach (i.e. using the function *weightwin*). Users must now define whether randomisations are to be conducted using a sliding window ("sliding") or weighted window ("weighted") approach with the parameter 'window'. Note that all parameters from *weightwin* will be required to run *randwin* using a weighted window approach (e.g. 'par', 'weightfunc').

### Cohort parameter:
When a group of biological measurements covers two years (e.g. Southern hemisphere species which breed between November - February [[2](http://www.journals.uchicago.edu/doi/abs/10.1086/659101)]) use of 'absolute' climate windows will cause these measurements to be split across different calendar years. To overcome this issue, we include a 'cohort' parameter to our functions. 

The cohort variable will determine which biological measurements should be grouped together (e.g. measurements from the same breeding season), and ensure that these measurements share the same reference day. The cohort variable should come from the same dataset as the 'bdate' parameter (i.e. variables should have equal lengths).

See our advanced vignette for more details:

`vignette("advanced_climwin", package = "climwin")`

### Spatial parameter:
Climate window analysis often requires large amounts of data to effectively determine periods of climate sensitivity. Often this is achieved through temporal replication (collecting many years of data on the same population) but can also be achieved through spatial replication (collecting data on multiple populations), or, ideally, a combination of the two. The new parameter 'spatial' allows users to carry out a climate window analysis with data from multiple populations by linking each set of biological measurements to a corresponding set of climate data.

Users can include data from multiple study sites/populations in their climate dataset (i.e. the dataset used for parameters 'cdate' and 'xvar'), with a new site ID variable included to distinguish between different sites. Similarly, the user can add a new site ID variable to the biological dataset that can be used to link biological measurements to the corresponding climate data. When carrying out analyses the user can then include the parameter 'spatial', a list item, containing the biological site ID variable and climate site ID variable respectively. During model fitting, the climate window analysis will extract different climate data for each biological record based on the provided site ID.

**N.B.** Spatial replication in climate window analysis works on the assumption that all populations share the same period of climate sensitivity. If this is NOT the case, populations should be analysed separately.

See our advanced vignette for more details:

`vignette("advanced_climwin", package = "climwin")`

### Cox proportional hazard models:
Proportional hazard models may often be useful for climate window analyses on phenological data. We have included the ability for users to fit proportional hazard models for the parameter 'baseline' using the function *coxph*.  For more detail on understanding the use of proportional hazard models for phenology analysis see [[2](http://www.journals.uchicago.edu/doi/abs/10.1086/659101)].

### Function *pvalue*:
In previous versions of `climwin` climate windows have been compared visually using a number of methods (e.g. deltaAICc distribution, model weights). In this newest version, we have included two metrics that allow for a standard method of distinguishing real periods of climate sensitivity in biological data.

These two metrics, $P_{C}$ and $P_{\Delta AICc}$, determine the likelihood that a given climate window would occur by chance, given the results of a *randwin* analysis on the same data. They can be calculated using the new function *pvalue*. For more information on the effectiveness of the new metrics, please see [[1](http://onlinelibrary.wiley.com/doi/10.1111/2041-210X.12590/full)].

### 'exclude' parameter:
Although `climwin` helps us move away from the selection of arbitrary climate windows the method is inherently exploratory, raising concerns about overfitting. Climate data from short duration time windows are particularly likely to show spurious relationships in climate window analysis [[1](http://onlinelibrary.wiley.com/doi/10.1111/2041-210X.12590/full)]. The inclusion of the function *pvalue* (above) reduces the chance that these short duration windows will be mistaken as 'real' climate signals; however, these spurious windows can still cause problems when conducting multi-model inferencing as the short duration windows may be distinctly different from other top models. As a solution, we include the parameter 'exclude' in the functions *slidingwin* and *randwin*. This allows users to exclude windows of a specific duration and lag to prevent these small windows from interfering with analyses.

## Minor changes

Parameter changes:

- 'furthest' and 'closest' are now combined into a single parameter 'range'.

- 'cutoff.day' and 'cutoff.month' are now combined into a single parameter 'refday'.

- 'cvk' parameter is now redundant, replaced with parameter 'k'.

- 'thresh' parameter is now redundant, replaced with parameter 'binary'.

- 'type' now accepts possible arguments 'absolute' and 'relative' (rather than 'fixed' and 'variable').

-----------------

# climwin 0.1.2

Fixed bug which caused convergence issues using cross-validation.

-----------------

# climwin 0.1.1

Fixed serious bug causing an error in 'plotwin' and 'plotall'. Naming mismatch in the 'closest' column in climatewin$Dataset. Column name changes from Closest to closest.

-----------------

# climwin 0.1.0

Our newest release aims to speed up the functions and provide greater versatility to users. In addition, we have produced a vignette providing a detailed introduction on how to use the climwin package. See `vignette("climwin")` for more.

We have made some changes to the names and levels or parameters that should be checked in the help documentation.

## Major changes

* Changes to parameter names and levels (e.g. Cinterval levels are now in the format "day", "week", "month" rather than "D", "W", "M"). Please check the help documentation of each function to familiarise yourself with the new function wording.

* Function `climatewin` (and corresponding functions) now contains parameter CVK. This provides the ability to run k-fold cross validation during climate window analysis. This provides a further check against finding strong climate signals by chance.

* Function `climatewin` (and corresponding functions) now contains parameters upper, lower and thresh. These allow users to adapt their climate data to consider climatic thresholds. This will be useful for those interested in investigating topics like growing degree or chill days.

* Function `climatewin` (and corresponding functions) now contains parameter centre. This allows users to carry out within-group centreing with their climate data. Note that within-group centreing will only test linear relationships.

* Function `climatewin` now includes functionality to test multiple parameter combinations (e.g. func = c("lin", "quad")) in succession. Please see the vignette `vignette("climwin")` for more detail.

* Function `crosswin` now includes parameter stat2. This allows users to test the correlation between two climate variables using different aggregate statistics (e.g. Mean temperature and minimum rainfall).

## Minor changes

* The parameter Xvar must now be a list item (e.g. Xvar = list(MassClimate$Temp)) 

------------------------

# climwin 0.0.1

* Initial package release. Forthcoming changes will be noted.