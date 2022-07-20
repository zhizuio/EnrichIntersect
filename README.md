# EnrichIntersect

The EnrichIntersect R package provides a flexible tool for enrichment analysis based on user-defined sets. It allows users to perform over-representation analysis of the custom sets among any specified ranked feature list, hence making enrichment analysis applicable to various types of data from different scientific fields. EnrichIntersect also enables an interactive means to visualize identified associations based on, for example, the mix-lasso model ([Zhao et al. (2022)](https://doi.org/10.1016/j.isci.2022.104767)) or similar methods.

## Installation

Install the current development version with
``` r
library("devtools")
devtools::install_github("zhizuio/EnrichIntersect")
```

## Example


### Plot enrichment map

Data set `cancers_drug_groups` is a list including a score dataframe with 147 drugs as rows
and 19 cancer types as columns, and a dataframe with nice pre-defined drug groups (1st column)
of the 147 drugs (2nd column). 

``` r
library(EnrichIntersect)
x <- cancers_drug_groups$score
custom.set <- cancers_drug_groups$custom.set
enrich <- enrichment(x, custom.set)
 |==================================================| 100%
```

![](https://github.com/zhizuio/EnrichIntersect/blob/main/README_plot_enrich.pdf)

### Plot Sankey diagram for intersecting set through an array

Data set `cancers_genes_drugs` is an array with association scores between 56 genes (1st dimension), three cancer types (2nd dimension) and two drugs (3rd dimension). 

``` r
data(cancers_genes_drugs, package = "EnrichIntersect")
intersectSankey(cancers_genes_drugs, step.names=c("Cancers","Genes","Drugs"))
```

![](https://github.com/zhizuio/EnrichIntersect/blob/main/README_plot_sankey.pdf)


## Citation

Zhi Zhao, Shixiong Wang, Manuela Zucknick, Tero Aittokallio (2022). Tissue-specific identification of multi-omics features for pan-cancer drug response prediction. iScience, Available online, DOI: [https:// doi.org/10.1016/j.isci.2022.104767](https://doi.org/10.1016/j.isci.2022.104767).

Zhi Zhao, Manuela Zucknick, Tero Aittokallio (2022+). EnrichIntersect: an R package for enrichment analysis of custom sets and interactive visualization of intersecting sets. Manuscript.
