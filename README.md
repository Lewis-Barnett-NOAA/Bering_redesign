# Adapting monitoring to a changing seascape: increasing the efficiency, flexibility, and continuity of bottom trawl surveys in the Bering Sea and beyond

## Description
This repository provides the code used for an In Prep manuscript by Daniel Vilas, Lewis A.K. Barnett, André E. Punt, Zack S. Oyafuso, Lukas DeFilippo, Margaret C. Siple, Leah S. Zacher, and Stan Kotwicki entitled "**Which traditional sampling design best estimates abundance across species in a rapidly changing ecosystem?**".

## Material and Methods

We investigated whether defining survey boundaries based on historical and future environmental conditions improves the precision and accuracy of abundance estimates in a multispecies survey. We fitted univariate spatiotemporal species distribution models to 16 stocks (14 species) using historical observations of fishery-independent bottom trawl survey catch-per-unit-effort and sea bottom temperature in the eastern and northern Bering Sea from 1982 to 2022. We used spatiotemporal models to simulate historical and future survey data from these models and optimize stratuma boundaries and sample allocation for abundance estimation under a variety of environmental conditions. We then compared simulated abundance estimates to the simulated true abundance among sampling designs and future temperature scenarios.

![Flowchart - Frame 2](https://github.com/danielvilasgonzalez/Bering_redesign/assets/11243119/6fe456d5-9bf6-466a-a290-bdf9a1bbc17a)

## Species/Stocks Included

The species and stocks set included in the manuscript are 10 groundfish and 4 crab species (6 stocks) of the Bering Sea:

| Stock                               | Scientific Name                     | Common Name                           |
|-------------------------------------|-------------------------------------|---------------------------------------|
| arrowtooth flounder                 | *Atheresthes stomias*               | arrowtooth flounder                   |
| Arctic cod                          | *Boreogadus saida*                  | Arctic cod                            |
| Tanner crab                         | *Chionoecetes bairdi*               | Tanner crab                           |
| snow crab                           | *Chionoecetes opilio*               | snow crab                             |
| saffron cod                         | *Eleginus gracilis*                 | saffron cod                           |
| Alaska pollock                      | *Gadus chalcogrammus*               | Alaska pollock                        |
| Pacific cod                         | *Gadus macrocephalus*               | Pacific cod                           |
| flathead sole                       | *Hippoglossoides elassodon*         | flathead sole                         |
| Bering flounder                     | *Hippoglossoides robustus*          | Bering flounder                       |
| northern rock sole                  | *Lepidopsetta polyxystra*           | northern rock sole                    |
| yellowfin sole                      | *Limanda aspera*                    | yellowfin sole                        |
| Pribilof Islands blue king crab     | *Paralithodes platypus*             | blue king crab                        |
| St. Matthew Island blue king crab   | *Paralithodes platypus*             | blue king crab                        |
| Pribilof Islands red king crab      | *Paralithodes camtschaticus*        | red king crab                         |
| Bristol Bay red king crab           | *Paralithodes camtschaticus*        | red king crab                         |
| Alaska plaice                       | *Pleuronectes quadrituberculatus*   | Alaska plaice                         |


## Sampling designs

Stratification scheme and station allocation information for each sampling design. The “optimized” stratification schemes represent the multispecies optimal design. All sampling designs consist of 15 strata and 520 samples.


| Stratification scheme | Stratification factors                       | Sampling allocation       |
|-----------------------|----------------------------------------------|---------------------------|
| existing              | depth and geographical subregion             | fixed                     |
| existing              | depth and geographical subregion             | balanced random           |
| existing              | depth and geographical subregion             | random                    |
| optimized             | depth                                        | fixed                     |
| optimized             | depth                                        | balanced random           |
| optimized             | depth                                        | random                    |
| optimized             | variance of sea bottom temperature           | fixed                     |
| optimized             | variance of sea bottom temperature           | balanced random           |
| optimized             | variance of sea bottom temperature           | random                    |
| optimized             | depth and variance of sea bottom temperature | fixed                     |
| optimized             | depth and variance of sea bottom temperature | balanced random           |
| optimized             | depth and variance of sea bottom temperature | random                    |


## Scripts

The analysis is coded into 12 scripts and can be found in Bering_redesign/Scripts/ms sampling designs/:

- r0. Creates figures of the study area, sampling stations, and existing sampling design.
- r1. Converts raw bottom-trawl data into the input data frame for the species distribution models.
- r2. Exports SBT data from Bering 10K ROMS into the dataset.
- r3. Prepares data for projecting models into the future.
- r4. Fits operating models.
- r5. Simulates data from OM for historical and projected years.
- r6. Retrieves true indices from the OM, and prepares EBS+NBS data for optimization.
- r7A. Runs sampling optimization based on predicted densities from VAST OM EBS+NBS and calculates stratification boundaries and sample allocations for each sampling design.
- r7B. Plots stratification maps and comparisons.
- r8A. Simulates station allocations for each sampling design.
- r8B. Simulates data and surveys for historical and projected years and prepares estimates to compute design-based indices for groundfish and crab species.
- r9. Compares and plots design estimates versus true estimates.

## Data

  -  baseline_strata.RData contains the existing sampling design data.
  -  multisp_optimization_static_data.RData is the input data to run the multispecies optimization process.

## Additional data

Additional output data on true and estimated historical and projected indices, CV, and RRMSE of CV can be found at:

https://figshare.com/s/8e13d006d9822ef4334d

## NOAA README

This repository is a scientific product and is not official
communication of the National Oceanic and Atmospheric Administration, or
the United States Department of Commerce. All NOAA GitHub project code
is provided on an ‘as is’ basis and the user assumes responsibility for
its use. Any claims against the Department of Commerce or Department of
Commerce bureaus stemming from the use of this GitHub project will be
governed by all applicable Federal law. Any reference to specific
commercial products, processes, or services by service mark, trademark,
manufacturer, or otherwise, does not constitute or imply their
endorsement, recommendation or favoring by the Department of Commerce.
The Department of Commerce seal and logo, or the seal and logo of a DOC
bureau, shall not be used in any manner to imply endorsement of any
commercial product or activity by DOC or the United States Government.

## NOAA License

Software code created by U.S. Government employees is not subject to
copyright in the United States (17 U.S.C. §105). The United
States/Department of Commerce reserve all rights to seek and obtain
copyright protection in countries other than the United States for
Software authored in its entirety by the Department of Commerce. To this
end, the Department of Commerce hereby grants to Recipient a
royalty-free, nonexclusive license to use, copy, and create derivative
works of the Software outside of the United States.

<img src="https://raw.githubusercontent.com/nmfs-general-modeling-tools/nmfspalette/main/man/figures/noaa-fisheries-rgb-2line-horizontal-small.png" alt="NOAA Fisheries" height="75"/>

[U.S. Department of Commerce](https://www.commerce.gov/) \| [National
Oceanographic and Atmospheric Administration](https://www.noaa.gov) \|
[NOAA Fisheries](https://www.fisheries.noaa.gov/)

