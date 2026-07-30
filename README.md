# NTD burden across the urbanisation gradient (GBD 2023)

Analysis code for: **Neural tube defect burden across the urbanisation gradient in 204 countries, 1990-2023: a cross-national ecological analysis of the Global Burden of Disease Study 2023**

## Repository structure

| File | Description |
|------|-------------|
| `global_ntd_analysis.R` | Main R analysis script: data extraction, EAPC calculation, segmented regression, ARIMA forecasting, figures 1-7, 9-10, tables 1-2, supplementary figures |
| `make_fig8_v2.py` | Python script for Figure 8 (income-stratified analysis by urbanisation quintile) |
| `stack_maps.py` | Python script to stack Figure 4 world map panels vertically |
| `submission/md2docx.py` | Markdown-to-DOCX conversion for manuscript submission |

## Data sources

- **GBD 2023 NTD DALY estimates**: https://vizhub.healthdata.org/gbd-results/ (cause ID 642)
- **World Bank urbanisation data**: https://data.worldbank.org/ (SP.URB.TOTL.IN.ZS)
- **World Bank income classifications**: https://data.worldbank.org/income-level
- **GBD 2023 SDI estimates**: https://ghdx.healthdata.org/record/ihme-data/gbd-2023-sdi-1950-2023

## Requirements

### R packages
- tidyverse, sf, rnaturalearth, forecast, segmented, scales, ggrepel

### Python packages
- pandas, matplotlib, seaborn, numpy, worldbank

## Citation

[DOI to be added upon publication]

## License

MIT
