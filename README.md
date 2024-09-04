---
title: "WhatsApp Chat Analysis"

---

# Introduction
WhatsApp Chat Analysis is a Shiny app that provides insights into WhatsApp chat history for multiple users. The app allows users to upload their WhatsApp chat text file and generates various visualizations and analyses, including message count, sentiment analysis, emoji usage, and word cloud generation.

# Features
- Upload WhatsApp chat text file
- Message count analysis
- Sentiment analysis
- Emoji usage analysis
- Word cloud generation
- User selection for word cloud generation

# Getting Started
1. Clone this repository to your local machine.
2. Install the required R packages by running:

```r
install.packages(c("shiny", "shinydashboard", "rwhatsapp", "ggplot2", "lubridate", "tidytext", "stopwords", "tidyverse", "tidymodels", "syuzhet", "RColorBrewer", "dplyr", "stringr", "tm", "quanteda", "wordcloud", "emojifont", "emoji"))
```

3. Run the Shiny app by executing `shinyApp(ui, server)` in your R console.
4. Upload your WhatsApp chat text file to the app.
5. Explore the various analyses and visualizations generated by the app.

**Note:** This app is for personal use only and should not be used for commercial purposes.

# License
This repository is licensed under the MIT License. See `LICENSE` for details.

# Contributing
Contributions are welcome! If you'd like to contribute to this repository, please fork the repository, make your changes, and submit a pull request.

# Issues
If you encounter any issues or have suggestions for improvement, please open an issue in this repository.

# Acknowledgments
This app was built using various R packages, including `shiny`, `shinydashboard`, `rwhatsapp`, `ggplot2`, `lubridate`, `tidytext`, `stopwords`, `tidyverse`, `tidymodels`, `syuzhet`, `RColorBrewer`, `dplyr`, `stringr`, `tm`, `quanteda`, `wordcloud`, `emojifont`, and `emoji`.
```
