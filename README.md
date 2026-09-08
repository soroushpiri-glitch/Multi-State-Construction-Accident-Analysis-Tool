# Multi-State Construction Accident Analysis Tool

A research-oriented R Shiny application for automated extraction and analysis of information from construction accident investigation reports.

The application processes Fatality Assessment and Control Evaluation (FACE) reports from multiple U.S. states and combines rule-based natural language processing (NLP), PDF text extraction, optical character recognition (OCR), and a large language model (LLM) to extract and organize accident-related information.

## Features

The application supports:

- Multi-state FACE report processing
- PDF text extraction and OCR
- State-specific report parsing
- Incident summary extraction
- Incident date extraction
- Worker age extraction
- Contributing-factor extraction
- Safety recommendation extraction
- Gemini-assisted analysis of contributing factors
- Interactive visualization and review through an R Shiny interface
- Export of structured results for further analysis

## Technologies

The application is developed in R and uses several packages, including:

- `shiny`
- `dplyr`
- `stringr`
- `lubridate`
- `pdftools`
- `tesseract`
- `DT`
- `plotly`
- `tidyr`
- `gemini.R`
- `stringdist`

## Installation

Clone or download this repository and install the required R packages:

```r
install.packages(c(
  "shiny",
  "dplyr",
  "stringr",
  "lubridate",
  "pdftools",
  "tibble",
  "purrr",
  "shinyjs",
  "tesseract",
  "DT",
  "plotly",
  "tidyr",
  "stringdist",
  "later"
))
```

Install `gemini.R` separately if it is not already installed.

## Gemini API Configuration

The application uses the Gemini API for LLM-assisted contributing-factor extraction.

For security, API keys should **not** be stored directly in the source code.

Create or edit your local `.Renviron` file:

```r
file.edit("~/.Renviron")
```

Add your Gemini API key:

```text
GEMINI_API_KEY=your_api_key_here
```

Restart R and verify that the environment variable is available:

```r
Sys.getenv("GEMINI_API_KEY")
```

The application accesses the key using:

```r
setAPI(Sys.getenv("GEMINI_API_KEY"))
```

**Never commit your `.Renviron` file or API key to this repository.**

## Running the Application

Open `app.R` in RStudio and click **Run App**, or run:

```r
shiny::runApp()
```

Users can then upload supported FACE accident reports through the application interface for extraction and analysis.

## Method Overview

The general processing workflow is:

```text
FACE Report (PDF)
        ↓
PDF Text Extraction / OCR
        ↓
State-Specific Parsing
        ↓
Information Extraction
        ↓
Rule-Based NLP + LLM-Assisted Analysis
        ↓
Human Review
        ↓
Structured Results and Visualization
```

State-specific extraction routines are used to account for differences in report structure and formatting across FACE programs.

## Data

This repository contains the analysis software and does not redistribute the complete FACE report dataset.

Users should obtain publicly available FACE reports from their original sources and comply with the applicable terms and citation requirements.

## Research Use

This software was developed as part of research on automated analysis of construction accident reports using natural language processing and large language models.

The tool is intended for research and analytical purposes. Automated outputs should be reviewed before being used for safety-related conclusions or decision-making.

## Citation

If you use this software in academic research, please cite the associated publication.

Publication information will be added following publication of the manuscript.

## License

License information will be added to this repository.

## Author

**Soroush Piri**  
PhD Candidate  
Morgan State University
