geneCardsLink <- function(val,name) {
  sprintf('<a href="https://www.genecards.org/cgi-bin/carddisp.pl?gene=%s" target="_blank" class="btn btn-primary">%s</a>',val,name)
}

# createLink for NCBI -----------------------------------------------------
ncbiLink <- function(val,ncbi) {
  sprintf('<a href="https://www.ncbi.nlm.nih.gov/gene/?term=%s" target="_blank" class="btn btn-primary">%s</a>',val,ncbi)
}

# createLink for Esemble --------------------------------------------------
ensemblLink <- function(val,ensembl) {
  sprintf('<a href="https://www.ensembl.org/Homo_sapiens/Gene/Summary?db=core;g=%s" target="_blank" class="btn btn-primary">%s</a>',val,ensembl)
}

helpbox <- function(width = 12) {
  column(width = width, 
    wellPanel(
      fluidRow(
        column(width = 12),
        img(src = 'HBPGEOswdt.jpg',width = '1400px',.noWS='after'))
    )
  )
}



fundbox <- function(width = 12) {
  column(width = width, 
    wellPanel(
      h3('HBPGEO'),
      p('HBPGEO is specially designed for gene expression data processing and visualization.
      The system mainly contains six parts:
      sample analysis、DEG、Gene module analysis、Hub genes analysis、Gene function analysis and About this system.
         ')
    )
  )
}



