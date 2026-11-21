# Code for: Skeletal muscle aging drives sex- and compartment-specific protein insolubility that is modifiable by proteostasis-directed treatments (Nature Aging, NATAGING-A16543-T)

Analysis and figure-generation code for the insoluble proteome of aging skeletal muscle (proteomics, aggregation propensity, D2O turnover/enrichment, Coomassie, AT680 aggregate imaging).
Code: https://github.com/OMRF-MillerB-Lab/Mesquita_ProteinAggregates_NatureAging - release v1.0 - MIT licence (see `LICENSE`).
Proteomics data: will be deposited in MassIVE (ProteomeXchange); the accession will be added here on deposition. Processed tables: Source Data.

---------------------------------------------------------------------------------------------

# Part A. Repository README (matches the Nature Code and Software Submission Checklist)

Contents: Overview - Repository contents - 1 System requirements - 2 Installation - 3 Demo - 4 Instructions for use - 5 What each script produces - Large input files - License - Part B (per-analysis READMEs)

## Overview
This repository contains the R code used to analyse and plot the manuscript data: limma-based differential analysis of the soluble and insoluble skeletal-muscle proteome (Adult vs Old and old-mouse treatments), GO cellular-component composition, aggregation-propensity analysis, D2O fraction-new and turnover modelling, Coomassie bulk-fraction quantification, and AT680 aggregate imaging quantification. Every script writes its statistical tables and the manuscript figure files.

## Repository contents
```
README.md              this file
LICENSE                licence text
AdOld/                 Adult vs Old proteomics, scripts 1-4        (B1)
OldTrt/                Old-treatment proteomics, scripts 1-4       (B2)
Prop/                  aggregation propensity + AGGRESCAN.Rmd      (B3)
Enrichment/            D2O fraction new                            (B4)
Turnover/              turnover fits and contrasts                 (B5)
Coomassie/             bulk soluble / insoluble                    (B6)
AT680/                 aggregate imaging                           (B7)
demo/                  small demo dataset and expected output
```
Each analysis folder holds one or more R Markdown scripts, an `R/` folder of figure-export functions, and the input files.

## 1. System requirements
- Software: R 4.5.0 with Bioconductor 3.21 (analyses were run and tested on Windows 11 x64, R 4.5.0 ucrt). Not tested on other operating systems or R versions.
- Key package versions: limma 3.64.3, clusterProfiler 4.16.0, org.Mm.eg.db 3.21.0, GO.db 3.21.0, AnnotationDbi 1.70.0, Biostrings 2.76.0, ComplexHeatmap 2.24.1, circlize 0.4.17, nipals 1.0, car 3.1-5, emmeans 2.0.2, lmerTest, ARTool, minpack.lm 1.2-4, rstatix, broom 1.0.12, readxl 1.4.5, tidyverse 2.0.0 (dplyr 1.2.0, ggplot2 4.0.3, tidyr 1.3.2, purrr 1.2.1, readr 2.2.0, stringr 1.6.0), patchwork 1.3.2, cowplot 1.2.0, ggrepel 0.9.6, ggtext 0.1.2, ggpattern 1.3.1, scales 1.4.0, viridisLite 0.4.3, ragg 1.5.2, svglite 2.2.2. Each rendered report ends with its session information.
- Hardware: a standard desktop or laptop. The analyses and the demo were run on a laptop with an Intel Core Ultra 5 135U (12 cores, 14 logical processors) and 32 GB RAM (Windows 11 x64). Not tested on smaller systems; 16 GB RAM or more is recommended. No non-standard hardware.
- Operating systems: tested on Windows 11 x64 only. Not tested on macOS or Linux (expected to work, but unverified).
- Aggregation-propensity scores: TANGO and WALTZ scores were precomputed with the publicly available tools; AGGRESCAN is implemented in R in `Prop/AGGRESCAN.Rmd`.
- Raw mass-spectrometry data are not needed to run the code; the scripts start from the DIA-NN / Skyline output tables listed in each section below.

## 2. Installation guide
1. Install R 4.5.0 and (optionally) RStudio.
2. Install the Bioconductor and CRAN packages listed above (`BiocManager::install(...)` / `install.packages(...)`). Each script checks for missing packages at the start and stops with a list; it does not install anything itself.
3. Download or clone this repository into a normal folder (do not run from inside a zip).
4. Typical install time on a normal desktop computer: approximately 10-20 minutes on a broadband connection (estimate; mostly downloading Bioconductor annotation packages such as `org.Mm.eg.db` and `GO.db`, and large packages such as `arrow`). All packages are available as precompiled Windows binaries, so no compiler or Rtools is needed.

## 3. Demo
- Demo: the Adult vs Old **soluble** analysis, `AdOld/1.Soluble_AdOld_v2.Rmd`. Inputs (already in `AdOld/`): `Aggregates_Soluble.csv` (649 proteins) and `Aggregates_SampleList_Mesquita.csv` (42 samples). These are the real data used in the paper.
- Run: open `AdOld/1.Soluble_AdOld_v2.Rmd` in RStudio, set the working directory to `AdOld/`, and knit (the `R/` folder must stay next to it).
- Expected output: the folder `Sol_AO/` (CSV tables, 600-dpi PNG and editable SVG figures) and the HTML report. Reference copies of the report and the CSV tables are in `demo/expected_output/`; see `demo/README_demo.md`.
- Expected run time on a normal desktop: about 2 minutes (1 min 50 s on the hardware listed under System requirements).
- The remaining Adult vs Old scripts (2-4) run the same way and are not part of the demo.

## 4. Instructions for use
Each analysis is one R Markdown file plus an `R/` folder of figure-export functions. Put the input files named in its section next to the Rmd, keep the `R/` folder alongside, and knit. Outputs (CSV tables, PNG at 600 dpi and editable SVG) are written to the output folder named in the section, and previews of every figure are embedded in the knitted HTML report. File and folder names can be changed in each script's *Settings* chunk. Figure sizes are set only in the `nm_dims` / `nm_dims_by_class` block of each script.

## 5. What each script produces
| Script | Analysis and outputs |
|---|---|
| `AdOld/1.Soluble_AdOld_v2.Rmd` | Adult vs Old soluble proteome: limma differential abundance (age, sex, age x sex), volcano plots, heatmaps, PCA |
| `AdOld/2.Insoluble_AdOld_v2.Rmd` | Adult vs Old insoluble proteome: differential abundance and figures |
| `AdOld/3.Solubility_AdOld_v2.Rmd` | Volume-corrected insoluble/soluble ratio (solubility) for Adult vs Old |
| `AdOld/4.Comp_AdOld_v2.Rmd` | GO cellular-component composition, abundance and detection (Adult vs Old) |
| `OldTrt/1.Soluble_OldTrt_v2.Rmd` | Old-treatment soluble proteome: Treatment x Sex limma |
| `OldTrt/2.Insoluble_OldTrt_v2.Rmd` | Old-treatment insoluble proteome and reversal of age effects |
| `OldTrt/3.Solubility_OldTrt_v2.Rmd` | Old-treatment solubility (insoluble/soluble ratio) and reversal |
| `OldTrt/4.Comp_OldTrt_v2.Rmd` | GO cellular-component composition, abundance and detection (old treatments) |
| `Prop/AdOld_Prop_v2.Rmd` | Adult vs Old aggregation propensity (soluble), fate and GO components |
| `Prop/7.OldTrt_Prop_v2.Rmd` | Old-mouse aggregation propensity and treatment fate |
| `Prop/AGGRESCAN.Rmd` | AGGRESCAN aggregation-propensity scores |
| `Enrichment/Enrichment_FN_v2.Rmd` | D2O fraction new: calculation, mixed-effects statistics, figures |
| `Turnover/Turnover_All.Rmd` | One-phase turnover fits, Adult vs Old and old-treatment contrasts, figures |
| `Coomassie/Coomassie_AdOld_v2.Rmd`, `Coomassie/Coomassie_OldTrt_v2.Rmd` | Bulk soluble and insoluble protein content (Coomassie): statistics and figures |
| `AT680/AT680_AggregateQuant_v2.Rmd` | AT680 aggregate imaging quantification: statistics and figures |

Notes: ggrepel label positions and point jitter use random starts, so figure layouts can differ by a fraction of a millimetre between runs; the statistical tables are reproduced identically.

## Large input files
The raw mass-spectrometry data will be deposited in MassIVE (accession to be added); the processed tables are provided in the Source Data files. After downloading, place each file next to the script that reads it (Part B lists the names) and check that the folder contains the files named there before knitting.

## License
Released under the MIT licence (see `LICENSE`).

---------------------------------------------------------------------------------------------

# Part B. Per-analysis READMEs

## B1. Adult vs Old proteomics (scripts 1-4, `AdOld/`)
Keep everything in ONE normal folder (do not run from inside a zip):
```
AdOld/
|-- 1.Soluble_AdOld_v2.Rmd
|-- 2.Insoluble_AdOld_v2.Rmd
|-- 3.Solubility_AdOld_v2.Rmd
|-- 4.Comp_AdOld_v2.Rmd
|-- Aggregates_Soluble.csv             
|-- Aggregates_Insoluble.csv          
|-- Aggregates_SampleList_Mesquita.csv
`-- R/
    |-- nm_export_engine_soluble_AO.R
    |-- nm_export_engine_insoluble_AO.R
    |-- nm_export_engine_ratio_AO.R
    `-- nm_export_engine_comp_AO.R
```
Knit each Rmd. Outputs: `Sol_AO/`, `Ins_AO/`, `Solubility_AO_v2/` (script 3) and `Comp_AO/` (script 4). Script 4 needs only `Aggregates_Insoluble.csv` and the sample list.

## B2. Old-treatment proteomics (scripts 1-4, `OldTrt/`)
```
1.Soluble_OldTrt_v2.Rmd      soluble fraction, Treatment x Sex limma
2.Insoluble_OldTrt_v2.Rmd    insoluble fraction + reversal of age effects
3.Solubility_OldTrt_v2.Rmd   volume-corrected log2(Insoluble/Soluble) + reversal
4.Comp_OldTrt_v2.Rmd         GO-CC composition, abundance and detection
R/nm_export_engine_OT.R         figure export for scripts 1-3
R/nm_export_engine_comp_OT.R    figure export for script 4
```
Put next to the Rmd: `Soluble_Old_One-way.csv` (scripts 1, 3), `Insoluble_Old_One-way.csv` (scripts 2, 3, 4), `Aggregates_SampleList_Mesquita.csv`, the `R/` folder; for scripts 2 and 3 also an `AgeResults/` folder with the Adult-vs-Old tables (see each Overview). Input names can be changed in the *Settings* chunk.
Packages: tidyverse, limma, ComplexHeatmap, circlize, ggrepel, patchwork, cowplot, clusterProfiler, org.Mm.eg.db, GO.db, AnnotationDbi, nipals, viridisLite, ragg, svglite (script 4 also scales, ggtext, car).

## B3. Aggregation propensity (`Prop/`)
```
7.OldTrt_Prop_v2.Rmd     Old-Control insoluble propensity and treatment fate
AdOld_Prop_v2.Rmd        Adult-vs-Old soluble propensity, fate and GO components
R/nm_export_engine_OT_Prop.R   figure export for 7.OldTrt_Prop
R/nm_export_engine_AO_Prop.R   figure export for AdOld_Prop
```
Put next to the Rmd files: `Aggregates_SampleList_Mesquita.csv`, `Aggregates_Soluble_raw.csv`, `Aggregates_Insoluble_raw.csv`, `Protein_Aggregation_Master.tsv` (precomputed TANGO / WALTZ / AGGRESCAN scores), the `R/` folder; for `7.OldTrt_Prop`: `Detect_SharedLoss_bySex.csv`, `Detect_ExSpecific_bySex.csv`, `Detect_RapaSpecific_bySex.csv` (from `Comp_OldTrt/SexSplit/Detection/`); for `AdOld_Prop`: `OldPredominant_bySex.csv` (from `Comp_AO/SexSplit/Detection/`).
`AGGRESCAN.Rmd`: R implementation of AGGRESCAN (a3v scale, sequence-length-dependent window of 5/7/9/11 residues, hot-spot threshold -0.02) that scores the proteins in a master FASTA file and maps the scores to the detected proteins. TANGO and WALTZ scores were precomputed with the public tools.
Packages: tidyverse, ragg, viridisLite, patchwork, ggtext, ggrepel, svglite, cowplot, car, ARTool, AnnotationDbi, org.Mm.eg.db, GO.db, scales, knitr (`AdOld_Prop` also rstatix, ggpubr).

## B4. D2O enrichment / fraction new (`Enrichment/`)
```
Enrichment_FN_v2.Rmd        soluble and insoluble fraction-new calculation, statistics, figures
R/nm_export_engine_FN.R     figure export functions
```
Put next to the Rmd: `NEH.csv`, `d2o.csv`, `soluble_skyline_export.csv`, `skyline_Export_insol.csv`, `Proteostasis_Soluble_NoNorm_report.parquet` (only the DIA-NN preparation section reads it), and the `R/` folder.
Packages: dplyr, tidyr, ggplot2, ggridges, patchwork, purrr, lmerTest, emmeans, minpack.lm, arrow, readr, ragg, svglite, cowplot, htmltools, DT, knitr.
Output: `fn_out/`.

## B5. Protein turnover (`Turnover/`)
```
Turnover_All.Rmd   one-phase association fits, planned contrasts, figures
R/nm_export_engine_Turnover.R              SVG/PNG export functions
```
Put `Turn_raw.csv`, `Turn_prism_fit.csv`, `Turn_prism_stats.csv` and the `R/` folder next to the Rmd, install the packages (ggplot2, dplyr, tidyr, readr, purrr, tibble, minpack.lm, ragg, patchwork, ggpattern, svglite), knit. Output: `TurnAll/`. Resize figures only in chunk `nm-figure-config` (`nm_dims`); preview settings are in `nm-inline-figures`. Note: the Prism files are used only to validate the R fits; GraphPad Prism was used for the original fits.

## B6. Coomassie bulk soluble/insoluble (`Coomassie/`)
```
Coomassie_OldTrt_v2.Rmd   Old Ctl / Ex / Rapa x Sex            -> Bulk_OT/
Coomassie_AdOld_v2.Rmd    Adult sed vs Old sed x Sex            -> Bulk_AO/   (contains optional paper-workflow blocks, see the comments in the script)
```
Put `Aggregates_Sol and Insoluble Coomassie_Mesquita.xlsx` next to the Rmd files. Packages: tidyverse, readxl, broom, emmeans, car, ggtext, patchwork, scales, grid (ragg is used if installed).

## B7. AT680 aggregate quantification (`AT680/`)
```
AT680_AggregateQuant_v2.Rmd      analysis + figures (6 endpoints, Main5, Main6, legend)
R/nm_export_engine_AT680.R       figure export / re-export functions
```
Put `AT680fulldetailedanalysis_clean.xlsx` next to the Rmd (or set `params$input_xlsx`), keep `R/`, knit. Packages: readxl, dplyr, tidyr, tibble, ggplot2, purrr, readr, patchwork, cowplot, ragg, svglite. Output: `AT680_Figures/` (PNG 600 dpi, SVG, and `QC/` CSV files).
