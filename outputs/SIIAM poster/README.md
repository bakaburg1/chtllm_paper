## SIIAM Poster

Steps to generate the poster assets and render the poster for print.

### 1) Export figures from targets store

Use Rscript for a clean, non-interactive run:

```bash
Rscript "/Users/angelodambrosio/Dropbox/Lavoro/Consulenze Scientifiche/Dip Salute Pubblica Pisa/chtllm_package/outputs/SIIAM poster/make_poster_figs.R"
```

This creates PNGs under `outputs/SIIAM poster/figs/`.

### 2) Render the poster (HTML)

```bash
quarto render "/Users/angelodambrosio/Dropbox/Lavoro/Consulenze Scientifiche/Dip Salute Pubblica Pisa/chtllm_package/outputs/SIIAM poster/poster.qmd"
```

Open the resulting HTML in a browser and use Print to PDF.

- Paper size: A0 portrait.
- Margins: 15 mm.
- Background graphics: enabled.

### 3) Export to a high-resolution PNG (optional)

You can export the printed PDF to a raster image using `magick`:

```bash
magick -density 300 poster.html.pdf -quality 95 poster-300dpi.png
```

### Replacing logos

Replace `logo-ecdc.svg` and `logo-unipi.svg` with the official logos. Keep similar aspect ratios for best results.


