# Demo: Adult vs Old soluble proteome

This demo runs the Adult vs Old analysis of the soluble fraction on the real data from the paper.

## Inputs (in `../AdOld/`)
- `Aggregates_Soluble.csv` - DIA-NN protein intensities, 649 proteins x 42 samples
- `Aggregates_SampleList_Mesquita.csv` - sample metadata (age, treatment, sex, label, age in days)
- `R/` - figure-export functions (must stay next to the Rmd)

## Run
1. Install R 4.5.0 and the packages listed in the main README (the script stops and lists any that are missing).
2. Open `AdOld/1.Soluble_AdOld_v2.Rmd` in RStudio and set the working directory to the `AdOld/` folder (Session > Set Working Directory > To Source File Location).
3. Knit the document (or run chunk by chunk).

Expected run time on a normal desktop computer: about 2 minutes (1 min 50 s measured on a laptop with 12 cores and 32 GB RAM).

## Expected output
Running the script creates `Sol_AO/` inside `AdOld/` and the report `1.Soluble_AdOld_v2.html`:
- CSV tables (differential-abundance results and summaries)
- 600-dpi PNG and editable SVG figures
- an HTML report with the same figures, the statistical output and the session information

Reference copies of the HTML report and the CSV tables from a full run are in `expected_output/`. Your CSV values should match them; figure layouts can differ by a fraction of a millimetre (ggrepel label placement uses random starts).

## Next
Scripts 2-4 in `AdOld/` and the other analysis folders run the same way (inputs and outputs are listed in the main README).
