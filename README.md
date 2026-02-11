# Adapting monitoring to a changing seascape: increasing the efficiency, flexibility, and continuity of bottom trawl surveys in the Bering Sea and beyond

## Description 

This repository provides the code used for two interrelated projects:

1.  Vilas, D, Barnett, LAK, Punt, AE, Oyafuso, ZS, DeFilippo, LB, Siple, MC, Zacher, L & S Kotwicki. 2025. [Optimized stratified random surveys best estimate multispecies abundance in a rapidly changing ecosystem](https://doi.org/10.1093/icesjms/fsae158). *ICES Journal of Marine Science* 82(6): fsae158.
2.  Vilas, D, Barnett, LAK, Punt, AE, Oyafuso, ZS, DeFilippo, LB, Siple, MC, Hennessey, SM & S Kotwicki . In prep. "Can expanding spatial coverage of survey designs improve abundance estimation in a warming Arctic gateway?"

## Material and Methods

We investigated whether defining survey boundaries based on historical and future environmental conditions improves the precision and accuracy of abundance estimates in a multispecies survey. This repository includes code for two projects building toward this goal. In the first project, we fitted univariate spatiotemporal species distribution models to 16 stocks (14 species) using historical observations of fishery-independent bottom trawl survey catch-per-unit-effort and sea bottom temperature in the eastern and northern Bering Sea from 1982 to 2022. We used spatiotemporal models to simulate historical and future survey data from these models and optimize stratum boundaries and sample allocation for abundance estimation under a variety of environmental conditions (SBT scenarios folder; only applicable to project 1). We then compared simulated abundance estimates to the simulated true abundance among sampling designs and future temperature scenarios.

In the second project we optimized designs that included the upper continental slope, for years when both the shelf and slope surveys were conducted. We also evaluated distribution shifts by depth and latitude to determine how best to provide survey coverage for shifting stocks. This included an expanded species set and an analysis of whether the survey could be extended with only the original number of stations used to survey the EBS shelf. In addition, this included a correction for differences in selectivity between shelf and slope survey gears.

## Species/Stocks Included

The species and stocks set included in the first manuscript are 10 groundfish and 4 crab species (6 stocks) of the eastern Bering Sea:

| Stock | Scientific Name | Common Name |
|------------------------|------------------------|------------------------|
| arrowtooth flounder | *Atheresthes stomias* | arrowtooth flounder |
| Arctic cod | *Boreogadus saida* | Arctic cod |
| Tanner crab | *Chionoecetes bairdi* | Tanner crab |
| snow crab | *Chionoecetes opilio* | snow crab |
| saffron cod | *Eleginus gracilis* | saffron cod |
| Alaska pollock | *Gadus chalcogrammus* | Alaska pollock |
| Pacific cod | *Gadus macrocephalus* | Pacific cod |
| flathead sole | *Hippoglossoides elassodon* | flathead sole |
| Bering flounder | *Hippoglossoides robustus* | Bering flounder |
| northern rock sole | *Lepidopsetta polyxystra* | northern rock sole |
| yellowfin sole | *Limanda aspera* | yellowfin sole |
| Pribilof Islands blue king crab | *Paralithodes platypus* | blue king crab |
| St. Matthew Island blue king crab | *Paralithodes platypus* | blue king crab |
| Pribilof Islands red king crab | *Paralithodes camtschaticus* | red king crab |
| Bristol Bay red king crab | *Paralithodes camtschaticus* | red king crab |
| Alaska plaice | *Pleuronectes quadrituberculatus* | Alaska plaice |

The second analysis expanded to 19 groundfish and 4 crab species of the eastern Bering Sea:

| Scientific name                     | Common name                        |
|-------------------------------------|------------------------------------|
| *Anoplopoma fimbria*                | Sablefish                          |
| *Atheresthes evermanni*             | Kamchatka flounder                 |
| *Atheresthes stomias*               | Arrowtooth flounder                |
| *Bathyraja aleutica*                | Aleutian skate                     |
| *Boreogadus saida*                  | Arctic cod                         |
| *Chionoecetes bairdi*               | Tanner crab                        |
| *Chionoecetes opilio*               | Snow crab                          |
| *Eleginus gracilis*                 | Saffron cod                        |
| *Gadus chalcogrammus*               | Alaska pollock                     |
| *Gadus macrocephalus*               | Pacific cod                        |
| *Glyptocephalus zachirus*           | Rex sole                           |
| *Hippoglossoides elassodon*         | Flathead sole                      |
| *Hippoglossoides robustus*          | Bering flounder                    |
| *Lepidopsetta polyxystra*           | Northern rock sole                 |
| *Limanda aspera*                    | Yellowfin sole                     |
| *Paralithodes camtschaticus*        | Red king crab                      |
| *Paralithodes platypus*             | Blue king crab                     |
| *Pleuronectes quadrituberculatus*   | Alaska plaice                      |
| *Reinhardtius hippoglossoides*      | Greenland turbot                   |
| *Sebastes alutus*                   | Pacific ocean perch                |
| *Sebastes borealis*                 | Shortraker rockfish                |
| *Sebastes melanostictus/aleutianus* | Rougheye and blackspotted rockfish |
| *Sebastolobus alascanus*            | Shortspine thornyhead              |

## Sampling designs

Stratification scheme and station allocation information for each sampling design. The “optimized” stratification schemes represent the multispecies optimal design. All sampling designs consist of 15 strata in the first analysis, 10 in the second analysis, and 520 samples in the first analysis, 376 in the second analysis. The second analysis focused on optimized designs with depth and variance of sea bottom temperature stratification factors alone.

| Stratification scheme | Stratification factors | Sampling allocation |
|------------------------|------------------------|------------------------|
| existing | depth and geographical subregion | fixed |
| existing | depth and geographical subregion | balanced random |
| existing | depth and geographical subregion | random |
| optimized | depth | fixed |
| optimized | depth | balanced random |
| optimized | depth | random |
| optimized | variance of sea bottom temperature | fixed |
| optimized | variance of sea bottom temperature | balanced random |
| optimized | variance of sea bottom temperature | random |
| optimized | depth and variance of sea bottom temperature | fixed |
| optimized | depth and variance of sea bottom temperature | balanced random |
| optimized | depth and variance of sea bottom temperature | random |

## Scripts

The analysis is coded into 12 scripts and can be found in Bering_redesign/Scripts/ms sampling designs/ for manuscript 1, with similar structure for manuscript 2 in Bering_redesign/Scripts/ms slope/ that incorporates the continental slope operating models (and including additional scripts appended by \_wslope for slope models, as opposed to shelf models):

-   r0. Creates figures of the study area, sampling stations, and existing sampling design.
-   r1. Converts raw bottom-trawl data into the input data frame for the species distribution models.
-   r2. Exports SBT data from Bering 10K ROMS into the dataset.
-   r3. Prepares data for projecting models into the future (project 1) or performs calibration between gears (project 2).
-   r4. Fits operating models (OMs). Project 2 includes separate files for shelf (EBS_NBS) OMs, and for slope OMs.
-   r5. Simulates data from OM.
-   r6. Retrieves true indices from the OM, and prepares data for optimization.
-   r7A. Runs sampling optimization based on predicted densities from VAST OM and calculates stratification boundaries and sample allocations for each sampling design.
-   r7B. Plots stratification maps and comparisons. (combined into a single r7 script in project 2).
-   r8A. Simulates station allocations for each sampling design.
-   r8B. Simulates data and surveys for historical and projected years and prepares estimates to compute design-based indices for groundfish and crab species.
-   r9. Compares and plots design estimates versus true estimates.

Project 2 also includes the following visualization scripts

-   figures_simulated_data_distributions.R which plots metrics of distribution change
-   sensitivity_analysisSR.R which performs a sensitivity analysis to selectivity ratio estimates

## Data

-   data via axiom (including .csv versions)

## Archived output

Additional outputs containing true and estimated historical and projected indices, CV, and RRMSE of CV for project 1 can be found at:

<https://figshare.com/s/8e13d006d9822ef4334d>

## NOAA README

This repository is a scientific product and is not official communication of the National Oceanic and Atmospheric Administration, or the United States Department of Commerce. All NOAA GitHub project code is provided on an ‘as is’ basis and the user assumes responsibility for its use. Any claims against the Department of Commerce or Department of Commerce bureaus stemming from the use of this GitHub project will be governed by all applicable Federal law. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by the Department of Commerce. The Department of Commerce seal and logo, or the seal and logo of a DOC bureau, shall not be used in any manner to imply endorsement of any commercial product or activity by DOC or the United States Government.

## NOAA License

Software code created by U.S. Government employees is not subject to copyright in the United States (17 U.S.C. §105). The United States/Department of Commerce reserve all rights to seek and obtain copyright protection in countries other than the United States for Software authored in its entirety by the Department of Commerce. To this end, the Department of Commerce hereby grants to Recipient a royalty-free, nonexclusive license to use, copy, and create derivative works of the Software outside of the United States.

<img src="https://raw.githubusercontent.com/nmfs-general-modeling-tools/nmfspalette/main/man/figures/noaa-fisheries-rgb-2line-horizontal-small.png" alt="NOAA Fisheries" height="75"/>

[U.S. Department of Commerce](https://www.commerce.gov/) \| [National Oceanographic and Atmospheric Administration](https://www.noaa.gov) \| [NOAA Fisheries](https://www.fisheries.noaa.gov/)
