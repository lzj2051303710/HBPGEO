#HBPGEO: An Application System for Analysis and Visualization of gene expression data of hypertension.
HBPGEO is a web application for analyzing and visualizing gene expression data by R Shiny.

HBPGEO(https://github.com/lzj2051303710/HBPGEO) is a web-based tool that allows the users to select data in the database of HBPGEO and also allows users to upload analysis of gene expression data for other diseases. 
It can conduct differential gene expression analysis, and it can also realize screen key gene modules and hub genes based on WGCNA. Meanwhile, it can also conduct gene enrichment and pathway analysis. 

To run this app locally on your machine,download R or Rstudio and run the following command once to set up the environment:
BiocManager.packages(c("shiny","shinyTree","shinythemes","grid","lattice","latticeExtra","tidyr","dplyr","dendextend","reshape2",
"ggplot2","gplots","limma","RColorBrewer","DT","pheatmap","ggrepel","ggfortify","ggstatsplot","cluster","shinyvalidate","GO.db",
"org.Hs.eg.db","topGO","GSEABase","clusterProfiler","Rgraphviz","pathview","igraph","ggnewscale","WGCNA"),ask = F,update = F)

If you were ready for this package, You could now run the shiny app with just one command in R:
 (1) Download the source code(https://github.com/lzj2051303710/HBPGEO);
 (2) Run it locally in R or R Studio.

This program is free software, and the users can redistribute it and modify it under the terms of the GNU General Public License as published by the Free Software Foundation.
This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.