# `squaRify`

`squaRify` is an R package for creating [treemaps](https://en.wikipedia.org/wiki/Treemapping) using base R. Treemaps are an effective way of visualizing hierarchical data by representing nested rectangles within larger rectangles. This package allows users to generate customizable treemaps to visually represent data structures efficiently.

## Features

- Base R Implementation: Utilizes native R functions to create treemaps without relying on any external libraries.
- Customizable: Users can modify colors, sizes, labels, and other visual aspects of the treemap.
- Hierarchical Visualization: Effectively displays hierarchical data through nested rectangles.

## Installation

You can install the package from GitHub using the `remotes` package:

```R
remotes::install_github(repo = "poisonalien/squaRify")
```

## Usage

```R
library(squrify)

#example data (GDP of G7 countries - in trillions)
gdp = c(26.85, 4.41, 4.30, 3.15, 2.92, 2.17, 2.09)
gdp_countries = c("U.S.", "Japan", "Germany", "UK", "France", "Italy", "Canada")

squarify::squarify(X = gdp, labels = gdp_countries)
```

![](https://github.com/PoisonAlien/trackplot/assets/8164062/e21282fe-2820-481d-8a9f-b3024806b040)

### Customization

```R
#example data (GDP of G7 and BRICS countries - in trillions)
g7 = c(26.85, 4.41, 4.30, 3.15, 2.92, 2.17, 2.09)
g7_countries = c("U.S.", "Japan", "Germany", "UK", "France", "Italy", "Canada")

brics = c(19.37, 3.73, 2.08, 2.06, 0.39)
gbrics_countries = c("China", "India", "Brazil", "Russia", "SA")

par(mfrow = c(1, 2), mar = c(0, 0, 3, 0))
squarify::squarify(
  X = g7,
  labels = g7_countries,
  col = "#273c75",
  alpha = 0.9,
  text_col = "white",
  borderCol = "white",
  sub_labels = paste0(g7, "T"),
  sub_text_col = "white",
  text_font = 2
)
title(main = "G7 $45.89T total")

squarify::squarify(
  X = brics,
  labels = brics_countries,
  col = "#c23616",
  alpha = 0.9,
  text_col = "white",
  borderCol = "white",
  sub_labels = paste0(brics, "T"),
  sub_text_col = "white",
  text_font = 2
)
title(main = "BRICS $27.63T total")
```


![](https://github.com/PoisonAlien/trackplot/assets/8164062/c84bcc99-17f4-4a2e-829d-93fcb5be31c2)

## Acknowledgements

The main algorithm is a direct translation of the [squarify](https://github.com/laserson/squarify) python package and, the main credit goes to [Uri Laserson](https://github.com/laserson). 
