server = function(input, output, session){
  options(shiny.maxRequestSize=60*1024^2)
  
#####Database Get Data Set
  ex <- reactive({ 
    inexprSet <- input$exprSet 
    if (is.null(inexprSet)) ex1=get(input$UserDataChoice1)[[2]] else 
      ex1 <- read.csv(inexprSet$datapath, header = TRUE) 
    
    ex=ex1
    rownames(ex)=ex[,1]
    ex=ex[,-1]
  }) 
  
####################################   
grouplist <- reactive({ 
    ingrouplist <- input$group_list 
     if (is.null(ingrouplist)) grouplist=get(input$UserDataChoice1)[[1]] 
     else 
       { 
         grouplist <- read.csv(ingrouplist$datapath, header = TRUE) 
         grouplist=grouplist[,2]
       }
   })    
   

###############################
   datTraits <- reactive({ 
     indatTraits <- input$Traits 
     if (is.null(indatTraits)) datTraits=get(input$UserDataChoice1)[[3]] else 
       datTraits <- read.csv(indatTraits$datapath, header = TRUE) 
     
     rownames(datTraits)= datTraits[,1]
     datTraits1= datTraits[,-1]
     allTraits = datTraits1
     fpkmSamples = colnames(ex())
     traitSamples =rownames(allTraits)
     traitRows = match(fpkmSamples, traitSamples)
     datTraits = allTraits[traitRows,] 
   })
   
 
   
###############################

  layout <- reactive({
    if (input$UserPanelLayout == "manual") {
      c(input$UserPanelLayoutCols, input$UserPanelLayoutRows)}
    else NULL
  })
 #########################################################
 output$PCA.ui <- renderUI({
 plotOutput("PCA", height = input$UserPrintHeight, width = input$UserPrintWidth)
 })
 ##########################
 output$PCA <- renderPlot({
 data_PCA = as.data.frame( t( ex() ) )
 data_PCA$group = grouplist()
 PCAplot=autoplot(prcomp( data_PCA[ ,1:(ncol(data_PCA)-1)]), data = data_PCA, colour = 'group', label =T,
               frame = F) + theme_bw()
print(PCAplot)
  
})
   
  output$DownloadPCA <- downloadHandler(
     filename = function(){
       paste0('PCA.',input$extPlot)
     },
     content = function(file,height = input$UserPrintHeight, width = input$UserPrintWidth){
       if(input$extPlot == 'pdf'){
         pdf(file,
             width = {if (input$UserPrintWidth == "auto") 7
           else as.numeric(input$UserPrintWidth)/100}, 
           height = as.numeric(input$UserPrintHeight)/100)
       }else if(input$extPlot == 'png'){
         png(file)
       }else{
         jpeg(file)
       }
       data_PCA = as.data.frame( t( ex() ) )
       data_PCA$group = grouplist()
       PCAplot=autoplot(prcomp( data_PCA[ ,1:(ncol(data_PCA)-1)]), data = data_PCA, colour = 'group', label =T,
                     frame = F) + theme_bw()
       print(PCAplot)
       dev.off()
     }
   )
  #########################################################
   output$barchart.ui <- renderUI({
    plotOutput("barchart", height = input$UserPrintHeight, width = input$UserPrintWidth)
  })
  ###########################
  output$barchart <- renderPlot({
    barchartplot=boxplot(ex(), col = rainbow(ncol(ex())*1.2),las=2)
    print(barchartplot)
    })
##########################   
   output$Downloadbarchart <- downloadHandler(
     filename = function(){
       paste0('barchart.',input$extPlot)
     },
       content = function(file,height = input$UserPrintHeight, width = input$UserPrintWidth){
         if(input$extPlot == 'pdf'){
           pdf(file,
               width = {if (input$UserPrintWidth == "auto") 7
                 else as.numeric(input$UserPrintWidth)/100}, 
               height = as.numeric(input$UserPrintHeight)/100)
         }else if(input$extPlot == 'png'){
           png(file)
         }else{
           jpeg(file)
         }
         
       barchartplot=boxplot(ex(), col = rainbow(ncol(ex())*1.2),las=1)
       print(barchartplot)
       dev.off()
     }
   )
   
######################################################
   output$clustering.ui <- renderUI({
     plotOutput("clustering", height = input$UserPrintHeight, width = input$UserPrintWidth)
   })
   
#############################
  output$clustering <- renderPlot({
    
    cluster=plot(hclust(dist(t(ex()))))
    
    print(cluster)
      })
  
   
   output$Downloadclustering <- downloadHandler(
     filename = function(){
       paste0('cluster.',input$extPlot)
     },
     
     content = function(file,height = input$UserPrintHeight, width = input$UserPrintWidth){
       if(input$extPlot == 'pdf'){
         pdf(file,
             width = {if (input$UserPrintWidth == "auto") 7
               else as.numeric(input$UserPrintWidth)/100}, 
             height = as.numeric(input$UserPrintHeight)/100)
       }else if(input$extPlot == 'png'){
         png(file)
       }else{
         jpeg(file)
       }
       
      cluster=plot(hclust(dist(t(ex())))) 
       
       print(cluster)
       dev.off()
     }
   )
   
 ############################################
 dttable <- reactive({
    SYMBOL=rownames(ex())
    ex_1=data.frame(SYMBOL,ex())
    
  })
   dttable1 <- reactive({
     tmp <-  dttable()
     tmp$NCBI <- ncbiLink(tmp$SYMBOL,tmp$SYMBOL)
     tmp$GeneCards <- geneCardsLink(tmp$SYMBOL,tmp$SYMBOL)
     tmp
   })
  output$table <- DT::renderDT(
     DT::datatable( dttable1(), 
                   escape = FALSE,
                   rownames = FALSE,
                   extensions = "Responsive",
                   options=list(
                   pageLength = 10,
                   lengthMenu = list(c(15, 50, 100, -1),c(15, 50, 100, "ALL")),
                   scrollX = TRUE,
                   scrollY = TRUE,
                   fixedColumns = TRUE,
                   fixedHeader = TRUE
                   )
    )
  )

  
  output$UserDownloadTable1 <- downloadHandler(
    filename = function(){
      paste0('diffLab.',input$extTable)
    },
    content = function(file){
      if(input$extTable == 'csv'){
        write.csv(dttable(), file, sep = ',', col.names = T, row.names = F, quote = F)
      }else{
        write.table(dttable(), file, sep = '\t', col.names = T, row.names = F, quote = F)
      }
    }
  )

  
################################################DEG

  
  layout <- reactive({
    if (input$UserPanelLayout1 == "manual") {
      c(input$UserPanelLayoutCols1, input$UserPanelLayoutRows1)}
    else NULL
  })
  #########################################################3
  
  
  design=reactive({
    design <- model.matrix( ~factor( grouplist() ) ) 
    
  })
  
  
  fit =reactive({ 
    fit=lmFit(ex(),design())
    fit=eBayes(fit)
  })
  
  nrDEG=reactive({
   
    nrDEG=topTable(fit(),coef=2,n=Inf)
  })

  
  logFC_cutoff<- reactive({
    if (is.na(input$inlogFC)) logFC_cutoff <- with(nrDEG() , mean( abs( logFC ) ) + 2 * sd( abs( logFC ) ) )
    else logFC_cutoff=input$inlogFC
  })
  
  diffLab <- reactive({
    diffLab<-nrDEG() 
        diffLab<- diffLab[diffLab$P.Value<input$diffLab_p & abs(diffLab$logFC )>logFC_cutoff(),]
    
  })
  
  diffLab1 <- reactive({
    SYMBOL=rownames(diffLab())
    diffLab1=data.frame(SYMBOL,diffLab()[1:5])
    tmp1 <- diffLab1
    tmp1$NCBI <- ncbiLink(tmp1$SYMBOL,tmp1$SYMBOL)
    tmp1$GeneCards <- geneCardsLink(tmp1$SYMBOL,tmp1$SYMBOL)
    tmp1
  })
  

   output$DEGtable <- DT::renderDT( 
    DT::datatable(diffLab1(), 
                   escape = FALSE,
                   rownames = FALSE,
                   extensions = "Responsive",
                   options=list(
                   pageLength = 15,
                   lengthMenu = list(c(15, 50, 100, -1),c(15, 50, 100, "ALL")),
                   scrollX = TRUE,
                   scrollY = TRUE,
                   fixedColumns = TRUE,
                   fixedHeader = TRUE
                    )
  ))
  
   
    output$UserDownloadDEGTable2 <- downloadHandler(
     filename = function(){
       paste0('DEG.',input$extTable2)
     },
     content = function(file){
       if(input$extTable2 == 'csv'){
         write.csv(diffLab1(), file, sep = ',', col.names = T, row.names = F, quote = F)
       }else{
         write.table(diffLab1(), file, sep = '\t', col.names = T, row.names = F, quote = F)
       }
     }
   )
############################
    output$DEGheatmap.ui <- renderUI({
      plotOutput("DEGheatmap", height = input$UserPrintHeight1, width = input$UserPrintWidth1)
    })
#################
  output$DEGheatmap <- renderPlot({
    
    mydata = ex()[rownames(diffLab()),] 
    mydata =as.matrix(mydata )
    mydatascale <- t(scale(t(mydata)))
    hr<-hclust(as.dist (1-cor(t(mydatascale),method="pearson")),method="complete")
    hc<-hclust (as.dist (1-cor(mydatascale, method="spearman")),method="complete")
    heatmap.2(mydata,Rowv=as.dendrogram (hr),Colv=as.dendrogram (hc),
              col=redgreen(75),scale="row",trace="none", key=T)
    
    print(heatmap.2) 
     })
##############

    output$DownloadDEGheatmap <- downloadHandler(
      filename = function(){
        paste0('DEGheatmap.',input$extPlot2)
      },
        content = function(file,height = input$UserPrintHeight1, width = input$UserPrintWidth1){
          if(input$extPlot2 == 'pdf'){
            pdf(file,
                width = {if (input$UserPrintWidth1 == "auto") 7
                  else as.numeric(input$UserPrintWidth1)/100}, 
                height = as.numeric(input$UserPrintHeight1)/100)
          }else if(input$extPlot2 == 'png'){
            png(file)
          }else{
            jpeg(file)
          }
          
        mydata = ex()[rownames(diffLab()),] 
        mydata =as.matrix(mydata )
        mydatascale <- t(scale(t(mydata)))
        hr<-hclust(as.dist (1-cor(t(mydatascale),method="pearson")),method="complete")
        hc<-hclust (as.dist (1-cor(mydatascale, method="spearman")),method="complete")
        heatmap.2(mydata,Rowv=as.dendrogram (hr),Colv=as.dendrogram (hc),
                  col=redgreen(75),scale="row",trace="none", key=T)
        
        print(heatmap.2)
        dev.off()
      }
    )
 
############################ 

    output$vocano.ui <- renderUI({
      plotOutput("vocano", height = input$UserPrintHeight1, width = input$UserPrintWidth1)
    })
#########################
  output$vocano <- renderPlot({
    DEG=nrDEG()

    DEG$change = as.factor(ifelse(DEG$P.Value < input$diffLab_p & abs(DEG$logFC) > logFC_cutoff(),
                                  ifelse(DEG$logFC > logFC_cutoff() ,'UP','DOWN'),'NOT')
    )
    this_tile <- paste0('Cutoff for logFC is ',round(logFC_cutoff(),3),
                        '\nThe number of up gene is ',nrow(DEG[DEG$change =='UP',]) ,
                        '\nThe number of down gene is ',nrow(DEG[DEG$change =='DOWN',])
    )
    
    g = ggplot(data=DEG, aes(x=logFC, y=-log10(P.Value), color=change)) +
      geom_point(alpha=0.4, size=1.75) +
      theme_set(theme_set(theme_bw(base_size=20)))+
      xlab("log2 fold change") + ylab("-log10 p-value") +
      ggtitle( this_tile  ) + theme(plot.title = element_text(size=15,hjust = 0.5))+
      scale_colour_manual(values = c('blue','black','red'))
    print(g)
})
#################  
    output$Downloadvocano <- downloadHandler(
      filename = function(){
        paste0('vocano.',input$extPlot2)
      },
      content = function(file,height = input$UserPrintHeight1, width = input$UserPrintWidth1){
        if(input$extPlot2 == 'pdf'){
          pdf(file,
              width = {if (input$UserPrintWidth1 == "auto") 7
                else as.numeric(input$UserPrintWidth1)/100}, 
              height = as.numeric(input$UserPrintHeight1)/100)
        }else if(input$extPlot2 == 'png'){
          png(file)
        }else{
          jpeg(file)
        }
        DEG=nrDEG()
        logFC_cutoff <- with(DEG,mean(abs( logFC)) + 2*sd(abs( logFC)) )
        DEG$change = as.factor(ifelse(DEG$P.Value < 0.05 & abs(DEG$logFC) > logFC_cutoff,
                                      ifelse(DEG$logFC > logFC_cutoff ,'UP','DOWN'),'NOT')
        )
        this_tile <- paste0('Cutoff for logFC is ',round(logFC_cutoff,3),
                            '\nThe number of up gene is ',nrow(DEG[DEG$change =='UP',]) ,
                            '\nThe number of down gene is ',nrow(DEG[DEG$change =='DOWN',])
        )
        g = ggplot(data=DEG, aes(x=logFC, y=-log10(P.Value), color=change)) +
          geom_point(alpha=0.4, size=1.75) +
          theme_set(theme_set(theme_bw(base_size=20)))+
          xlab("log2 fold change") + ylab("-log10 p-value") +
          ggtitle( this_tile  ) + theme(plot.title = element_text(size=15,hjust = 0.5))+
          scale_colour_manual(values = c('blue','black','red'))
        print(g)
        dev.off()
      }
    )

##############################
    diffLabMatrix <- reactive({
      ex()[rownames(diffLab()),] 
      
    })
##########################    
  
    output$DEGMatrix <- DT::renderDT( 
      DT::datatable(diffLabMatrix(), 
                    escape = FALSE,
                    rownames = T,
                    extensions = "Responsive",
                    options=list(
                      pageLength = 15,
                      lengthMenu = list(c(15, 50, 100, -1),c(15, 50, 100, "ALL")),
                      scrollX = TRUE,
                      scrollY = TRUE,
                      fixedColumns = TRUE,
                      fixedHeader = TRUE
                    )
      ))
    
    
    output$UserDownloadDEGMatrix <- downloadHandler(
      filename = function(){
        paste0('DEGMatrix.',input$extTable2)
      },
      content = function(file){
        if(input$extTable2 == 'csv'){
          write.csv(diffLabMatrix(), file, sep = ',', col.names = T, row.names = T, quote = F)
        }else{
          write.table(diffLabMatrix(), file, sep = '\t', col.names = T, row.names = T, quote = F)
        }
      }
    )       
#############################################################Gene module analysis

    
layout <- reactive({
 if (input$UserPanelLayout2 == "manual") {
 c(input$UserPanelLayoutCols2, input$UserPanelLayoutRows2)}
 else NULL
    })    
    
    
##############################    
    
    datExpr <- reactive({
    
      datExpr = t(ex()[order(apply(ex(),1,mad), decreasing = T)[1:as.numeric(input$Genes_data)],])
     
     
    })    


############################# Cluster to find outliers
     output$sampleclust.ui <- renderUI({
      plotOutput("sampleclust",height = input$UserPrintHeight2, width = input$UserPrintWidth2)
    })
#################
    output$sampleclust <- renderPlot({

      sampleTree2 = hclust(dist(datExpr()), method = "average")
      traitColors = numbers2colors(datTraits(), signed = FALSE)
      plotDendroAndColors(sampleTree2, traitColors,
                          groupLabels = names(datTraits()),
                          main = "Sample dendrogram and trait heatmap")
      abline(h = 65, col = "red")
    })
    ##############
    output$Downloadsampleclust <- downloadHandler(
      filename = function(){
        paste0('sampleclust.',input$extPlot3)
      },
        content = function(file,height = input$UserPrintHeight2, width = input$UserPrintWidth2){
          if(input$extPlot3 == 'pdf'){
            pdf(file,
                width = {if (input$UserPrintWidth2 == "auto") 7
                  else as.numeric(input$UserPrintWidth2)/100}, 
                height = as.numeric(input$UserPrintHeight2)/100)
          }else if(input$extPlot3 == 'png'){
            png(file)
          }else{
            jpeg(file)
          }
        sampleTree2 = hclust(dist(datExpr()), method = "average")
        traitColors = numbers2colors(datTraits(), signed = FALSE)
        plotDendroAndColors(sampleTree2, traitColors,
                            groupLabels = names(datTraits()),
                            main = "Sample dendrogram and trait heatmap")
        abline(h = 65, col = "red")
        
        dev.off()
      }
    )

######################
     datExpr1 <- reactive({
      if(input$off_text=="") datExpr1<-datExpr() else
       datExpr1<-datExpr()[-which(rownames(datExpr())==input$off_text),]
     })
     
     datTraits1<- reactive({
       if(input$off_text=="") datTraits1<-datTraits() 
       else datTraits1<-datTraits()[-which(rownames(datTraits())==input$off_text),]
     })
     
     grouplist1<- reactive({
       grouplist2=grouplist()
       if(input$off_text=="") grouplist1<-grouplist2 
       else grouplist1<-grouplist2[-which(rownames(datExpr())==input$off_text)]
    })
     

   nGenes <- reactive({
       nGenes = ncol(datExpr1())
     })   
     
     nSamples <- reactive({
       nSamples = nrow(datExpr1())
     }) 
enableWGCNAThreads()  
###########################################     
ex3table <- reactive({
  ex3_1= t(datExpr1())
  SYMBOL=rownames(ex3_1)
  ex3_1=data.frame(SYMBOL,ex3_1)
  tmp <- ex3_1
  tmp$NCBI <- ncbiLink(tmp$SYMBOL,tmp$SYMBOL)
  tmp$GeneCards <- geneCardsLink(tmp$SYMBOL,tmp$SYMBOL)
  tmp
})

output$DatExpr <- DT::renderDT( 
  DT::datatable(ex3table(), 
                escape = FALSE,
                rownames = FALSE,
                extensions = "Responsive",
                options=list(
                  pageLength = 15,
                  lengthMenu = list(c(15, 50, 100, -1),c(15, 50, 100, "ALL")),
                  scrollX = TRUE,
                  scrollY = TRUE,
                  fixedColumns = TRUE,
                  fixedHeader = TRUE
                )
  ))

output$UserDownloadWGCNA <- downloadHandler(
  filename = function(){
    paste0('Gene expression matrix.',input$extTable3)
  },
  content = function(file){
    if(input$extTable3 == 'csv'){
      write.csv(datExpr1(), file, sep = ',', col.names = T, row.names = F, quote = F)
    }else{
      write.table(datExpr1(), file, sep = '\t', col.names = T, row.names = F, quote = F)
    }
  }
)

##########################################
# Choose a set of soft-thresholding powers
collectGarbage()
sft1 <- reactive({
  sft1=pickSoftThreshold(datExpr1(), powerVector = c(c(1:10), seq(from = 12, to=30, by=2)), verbose = 5)
}) 

##################################3

fitIndices1 <- reactive({
  fitIndices1=sft1()$fitIndices[,1]
}) 

fitIndices2 <- reactive({
  fitIndices2=sft1()$fitIndices[,2]
})

fitIndices3 <- reactive({
  fitIndices3=sft1()$fitIndices[,3]
}) 

fitIndices5 <- reactive({
  fitIndices5=sft1()$fitIndices[,5]
}) 

#################
output$Soft_Threshold.ui <- renderUI({
  plotOutput("Soft_Threshold",height = input$UserPrintHeight2, width = input$UserPrintWidth2)
})

#################
output$Soft_Threshold <- renderPlot({
  par(mfrow = c(1,2))
  powers1<-c(c(1:10), seq(from = 12, to=30, by=2))
  cex1 = 0.85
  Soft_Thresholding=plot(fitIndices1(), -sign(fitIndices3())*fitIndices2(),
                         xlab="Soft Threshold (power)",ylab="Scale Free Topology Model Fit,signed R^2",type="n",
                         main = paste("Scale independence"));
  text(fitIndices1(), -sign(fitIndices3())*fitIndices2(),
       labels=powers1,cex=cex1,col="red");
  abline(h=0.85,col="red")
  plot(fitIndices1(), fitIndices5(),
       xlab="Soft Threshold (power)",ylab="Mean Connectivity", type="n",
       main = paste("Mean connectivity"))
  text(fitIndices1(), fitIndices5(), labels=powers1, cex=cex1,col="red")
 
})
##############
output$DownloadSoft_Threshold <- downloadHandler(
  filename = function(){
    paste0('Soft_Threshold.',input$extPlot3)
  },
  content = function(file,height = input$UserPrintHeight2, width = input$UserPrintWidth2){
    if(input$extPlot3 == 'pdf'){
      pdf(file,
          width = {if (input$UserPrintWidth2 == "auto") 7
            else as.numeric(input$UserPrintWidth2)/100}, 
          height = as.numeric(input$UserPrintHeight2)/100)
    }else if(input$extPlot3 == 'png'){
      png(file)
    }else{
      jpeg(file)
    }
    par(mfrow = c(1,2))
    powers1<-c(c(1:10), seq(from = 12, to=30, by=2))
    cex1 = 0.85
    Soft_Thresholding=plot(fitIndices1(), -sign(fitIndices3())*fitIndices2(),
                           xlab="Soft Threshold (power)",ylab="Scale Free Topology Model Fit,signed R^2",type="n",
                           main = paste("Scale independence"));
    text(fitIndices1(), -sign(fitIndices3())*fitIndices2(),
         labels=powers1,cex=cex1,col="red");
    abline(h=0.85,col="red")
    plot(fitIndices1(), fitIndices5(),
         xlab="Soft Threshold (power)",ylab="Mean Connectivity", type="n",
         main = paste("Mean connectivity"))
    text(fitIndices1(), fitIndices5(), labels=powers1, cex=cex1,col="red")
    print(Soft_Thresholding)
    
    dev.off()
  }
)

#####################################
softPower <- reactive({
  softPower = as.numeric(input$cutoff_soft)
  print(softPower)
}) 


####################################
heat_wgcna<-reactive({
  WGCNA_matrix<-datExpr1()
  progress <- shiny::Progress$new()
  on.exit(progress$close())
  progress$set(message = "Processing Data", value = 0)
    n<-2
  METree<-NULL
  adjacency = adjacency(WGCNA_matrix, power=softPower());
  TOM = TOMsimilarity(adjacency);
  dissTOM = 1-TOM
  geneTree = hclust(as.dist(dissTOM), method = "average");
  minModuleSize = input$minModule_Size;
 dynamicMods = cutreeDynamic(dendro = geneTree, distM = dissTOM,
                              deepSplit = 4, pamRespectsDendro = FALSE,
                              minClusterSize = minModuleSize);
  print("dynamicmod")
  dynamicColors = labels2colors(dynamicMods) 
  MEList = moduleEigengenes(WGCNA_matrix, colors = dynamicColors)
  MEs = MEList$eigengenes
  MEColors= dynamicColors
  print("MES")
  MEDiss = 1-cor(MEs);
  METree = hclust(as.dist(MEDiss), method = "average");
  
  MEDissThres = 0.2
  merge = mergeCloseModules(WGCNA_matrix, dynamicColors, cutHeight = MEDissThres, verbose = 3)
  mergedColors = merge$colors
  print(table(mergedColors))
  mergedMEs = merge$newMEs
  mergedMEDiss = 1-cor(merge$newMEs);
  METree = hclust(as.dist(mergedMEDiss), method = "average");
  progress$inc(1/(n), detail = paste("Doing part", 1,"/",2))
  Sys.sleep(0.1)
  moduleColors = mergedColors
  colorOrder = c("grey", standardColors(50))
  moduleLabels = match(moduleColors, colorOrder)-1
  moduleColors = labels2colors(moduleLabels)
 
  
  nGenes = ncol(WGCNA_matrix)
  nSamples = nrow(WGCNA_matrix)
  
  traitNames=names(WGCNA_matrix)
  geneTraitSignificance = as.data.frame(cor(WGCNA_matrix, datTraits1(), use = "p"))
  GSPvalue = as.data.frame(corPvalueStudent(as.matrix(geneTraitSignificance), nSamples))
  names(geneTraitSignificance) = paste("GS.", traitNames, sep="")
  names(GSPvalue) = paste("p.GS.", traitNames, sep="")
  
  
  modNames = substring(names(mergedMEs), 3)
  geneModuleMembership = as.data.frame(cor(WGCNA_matrix, mergedMEs, use = "p"))
  MMPvalue = as.data.frame(corPvalueStudent(as.matrix(geneModuleMembership), nSamples))
  names(geneModuleMembership) = paste("MM", modNames, sep="")
  names(MMPvalue) = paste("p.MM", modNames, sep="")
  
  {
    geneInfo0 = data.frame(probes= colnames(WGCNA_matrix),
                           moduleColor = moduleColors)
    
    for (Tra in 1:ncol(geneTraitSignificance))
    {
      oldNames = names(geneInfo0)
      geneInfo0 = data.frame(geneInfo0, geneTraitSignificance[,Tra],
                             GSPvalue[, Tra])
      names(geneInfo0) = c(oldNames,names(geneTraitSignificance)[Tra],
                           names(GSPvalue)[Tra])
    }
    
    for (mod in 1:ncol(geneModuleMembership))
    {
      oldNames = names(geneInfo0)
      geneInfo0 = data.frame(geneInfo0, geneModuleMembership[,mod],
                             MMPvalue[, mod])
      names(geneInfo0) = c(oldNames,names(geneModuleMembership)[mod],
                           names(MMPvalue)[mod])
    }
    geneOrder =order(geneInfo0$moduleColor)
    geneInfo = geneInfo0[geneOrder, ]
    
   }
  write.csv(geneInfo, file="geneInfo.csv")
  list(dynamicColors,mergedColors,mergedMEs,moduleColors,TOM,dissTOM,METree,
       geneTree,adjacency,geneInfo)
})
###############################################Dynamic cut tree
output$Dynamic.ui <- renderUI({
  plotOutput("Dynamic",height = input$UserPrintHeight2, width = input$UserPrintWidth2)
})

#################
output$Dynamic <- renderPlot({
  
  plotDendroAndColors(heat_wgcna()[[8]], cbind(heat_wgcna()[[1]], heat_wgcna()[[2]]),
                      c("Dynamic Tree Cut", "Merged dynamic"),
                      dendroLabels = FALSE, hang = 0.03,
                      addGuide = TRUE, guideHang = 0.05)
  
  #print() 
})
##############
output$DownloadDynamic <- downloadHandler(
  filename = function(){
    paste0('Dynamic_Tree_cut.',input$extPlot3)
  },
  content = function(file,height = input$UserPrintHeight2, width = input$UserPrintWidth2){
    if(input$extPlot3 == 'pdf'){
      pdf(file,
          width = {if (input$UserPrintWidth2 == "auto") 7
            else as.numeric(input$UserPrintWidth2)/100}, 
          height = as.numeric(input$UserPrintHeight2)/100)
    }else if(input$extPlot3 == 'png'){
      png(file)
    }else{
      jpeg(file)
    }
        plotDendroAndColors(heat_wgcna()[[8]], cbind(heat_wgcna()[[1]], heat_wgcna()[[2]]),
                        c("Dynamic Tree Cut", "Merged dynamic"),
                        dendroLabels = FALSE, hang = 0.03,
                        addGuide = TRUE, guideHang = 0.05)
    dev.off()
  }
)

###################################################Module-related hot map

output$Modules.ui <- renderUI({
  plotOutput("Modules",height = input$UserPrintHeight2, width = input$UserPrintWidth2)
})

#################
output$Modules <- renderPlot({

  plotEigengeneNetworks(heat_wgcna()[[3]], "", marDendro = c(0,4,1,2), marHeatmap = c(3,4,1,2), cex.lab
                        = 0.8, xLabelsAngle= 90)

  #print() 
})
##############
output$DownloadModules <- downloadHandler(
  filename = function(){
    paste0('Modules.',input$extPlot3)
  },
  content = function(file,height = input$UserPrintHeight2, width = input$UserPrintWidth2){
    if(input$extPlot3 == 'pdf'){
      pdf(file,
          width = {if (input$UserPrintWidth2 == "auto") 7
            else as.numeric(input$UserPrintWidth2)/100}, 
          height = as.numeric(input$UserPrintHeight2)/100)
    }else if(input$extPlot3 == 'png'){
      png(file)
    }else{
      jpeg(file)
    }
   
    plotEigengeneNetworks(heat_wgcna()[[3]], "", marDendro = c(0,4,1,2), marHeatmap = c(3,4,1,2), cex.lab
                          = 0.8, xLabelsAngle= 90)
    dev.off()
  }
)
###########################################################gene_topology
output$GenesModules.ui <- renderUI({
  plotOutput("GenesModules",height = input$UserPrintHeight2, width = input$UserPrintWidth2)
})

#################
output$GenesModules <- renderPlot({
  nSelect = 500
  set.seed(10)
  select = sample(nGenes(), size = nSelect)
  selectTOM = heat_wgcna()[[6]][select, select]
  selectTree = hclust(as.dist(selectTOM), method = "average")
  selectColors = heat_wgcna()[[4]][select]
  plotDiss = selectTOM^softPower()
  diag(plotDiss) = NA
  tomplot=TOMplot(plotDiss, selectTree, selectColors, main = "Network heatmap plot, selected genes")
  
  print(tomplot) 
})
##############
output$DownloadGenesModules <- downloadHandler(
  filename = function(){
    paste0('genes_topology.',input$extPlot3)
  },
  content = function(file,height = input$UserPrintHeight2, width = input$UserPrintWidth2){
    if(input$extPlot3 == 'pdf'){
      pdf(file,
          width = {if (input$UserPrintWidth2 == "auto") 7
            else as.numeric(input$UserPrintWidth2)/100}, 
          height = as.numeric(input$UserPrintHeight2)/100)
    }else if(input$extPlot3 == 'png'){
      png(file)
    }else{
      jpeg(file)
    }
    nSelect = 300
    set.seed(10)
    select = sample(nGenes(), size = nSelect)
    selectTOM = heat_wgcna()[[6]][select, select]
    selectTree = hclust(as.dist(selectTOM), method = "average")
    selectColors = heat_wgcna()[[4]][select]
    plotDiss = selectTOM^softPower()
    diag(plotDiss) = NA
    tomplot=TOMplot(plotDiss, selectTree, selectColors, main = "Network heatmap plot, selected genes")
    print(tomplot) 
    dev.off()
  }
)
#####################################Module_trait
output$Module_trait.ui <- renderUI({
  plotOutput("Module_trait",height = input$UserPrintHeight2, width = input$UserPrintWidth2)
})

#################
output$Module_trait <- renderPlot({
  nGenes = ncol(datExpr1())
  nSamples = nrow(datExpr1())
  
  moduleTraitCor = cor(heat_wgcna()[[3]], datTraits1(), use = "p")
  moduleTraitPvalue = corPvalueStudent(moduleTraitCor, nSamples)
  
  textMatrix = paste(signif(moduleTraitCor, 2), "\n (",
                     signif(moduleTraitPvalue, 1), ")", sep = "")
  
  dim(textMatrix) = dim(moduleTraitCor)

  
  labeledHeatmap(Matrix = moduleTraitCor,
                 xLabels = names(datTraits1()),
                 yLabels = names(heat_wgcna()[[3]]),
                 ySymbols = names(heat_wgcna()[[3]]),
                 colorLabels = FALSE,
                 colors = greenWhiteRed(50),
                 textMatrix = textMatrix,
                 setStdMargins = FALSE,
                 cex.text = 0.65,
                 zlim = c(-1,1),
                 main = paste("Module-trait relationships"))

  
  
  #print() 
})
##############
output$DownloadModule_trait <- downloadHandler(
  filename = function(){
    paste0('significance_of_Module_and_character .',input$extPlot3)
  },
  content = function(file,height = input$UserPrintHeight2, width = input$UserPrintWidth2){
    if(input$extPlot3 == 'pdf'){
      pdf(file,
          width = {if (input$UserPrintWidth2 == "auto") 7
            else as.numeric(input$UserPrintWidth2)/100}, 
          height = as.numeric(input$UserPrintHeight2)/100)
    }else if(input$extPlot3 == 'png'){
      png(file)
    }else{
      jpeg(file)
    }
    nGenes = ncol(datExpr1())
    nSamples = nrow(datExpr1())
    
    moduleTraitCor = cor(heat_wgcna()[[3]], datTraits1(), use = "p")
    moduleTraitPvalue = corPvalueStudent(moduleTraitCor, nSamples)
    
    textMatrix = paste(signif(moduleTraitCor, 2), "\n (",
                       signif(moduleTraitPvalue, 1), ")", sep = "")
    
    dim(textMatrix) = dim(moduleTraitCor)
    par(mar = c(10, 8.5, 3, 3))
    
    labeledHeatmap(Matrix = moduleTraitCor,
                   xLabels = names(datTraits1()),
                   yLabels = names(heat_wgcna()[[3]]),
                   ySymbols = names(heat_wgcna()[[3]]),
                   colorLabels = FALSE,
                   colors = greenWhiteRed(50),
                   textMatrix = textMatrix,
                   setStdMargins = FALSE,
                   cex.text = 0.65,
                   zlim = c(-1,1),
                   main = paste("Module-trait relationships"))
   
    
    dev.off()
  }
)
################################################################Module Significance  
output$MSModules.ui <- renderUI({
  plotOutput("MSModules",height = input$UserPrintHeight2, width = input$UserPrintWidth2)
})

#################
output$MSModules <- renderPlot({
  
  GS2=as.numeric(cor(datTraits1()[,1],datExpr1(),use="p",method= "pearson"))
  GeneSignificance2=abs(GS2)
  ModuleSignificance <- tapply(GeneSignificance2,heat_wgcna()[[4]],mean,na.rm=T)

  par(mar = c(0,4,2,0))
  print(ModuleSignificance)
    plotModuleSignificance(GeneSignificance2,heat_wgcna()[[2]],ylim=c(0,1),
                         srt=45,
                         cex.names = 0.8,
                         cex.lab   = 0.8,
                         xlab=" ",las=2,
                         main="Module Significance")
  #print() 
})
##############
output$DownloadMS <- downloadHandler(
  filename = function(){
    paste0('MS_GS.',input$extPlot3)
  },
  content = function(file,height = input$UserPrintHeight2, width = input$UserPrintWidth2){
    if(input$extPlot3 == 'pdf'){
      pdf(file,
          width = {if (input$UserPrintWidth2 == "auto") 7
            else as.numeric(input$UserPrintWidth2)/100}, 
          height = as.numeric(input$UserPrintHeight2)/100)
    }else if(input$extPlot3 == 'png'){
      png(file)
    }else{
      jpeg(file)
    }
    
    GS2=as.numeric(cor(datTraits1()[,1],datExpr1(),use="p",method= "pearson"))
    GeneSignificance2=abs(GS2)
    ModuleSignificance <- tapply(GeneSignificance2,heat_wgcna()[[4]],mean,na.rm=T)
    plotModuleSignificance(GeneSignificance2,heat_wgcna()[[2]],ylim=c(0,1),
                           srt=45,
                           cex.names = 0.8,
                           cex.lab   = 0.8,
                           xlab=" ",las=2,
                           main="Module Significance")
    dev.off()
  }
)


###############################################################file download
network_cytoscape<- reactive({
  #Input preparation
  datExpr<-datExpr1()
  group_fc<-list()
  cyt<-list()
  edgefile<-NULL
  nodefile<-NULL
  file_name<-NULL
  for (mod in 1:nrow(table(heat_wgcna()[[4]])))
  {
    modules = names(table(heat_wgcna()[[4]]))[mod]
    probes = colnames(datExpr)
    inModule = (heat_wgcna()[[4]] == modules)
    modProbes = probes[inModule]
    modGenes = modProbes
    modTOM = heat_wgcna()[[5]][inModule, inModule]
    dimnames(modTOM) = list(modProbes, modProbes)
     cyt[length(cyt)+1] = exportNetworkToCytoscape(modTOM,
                                                  edgeFile = paste("./files/CytoscapeInput-edges-", modules, ".txt", sep=""),
                                                  nodeFile = paste("./files/CytoscapeInput-nodes-", modules, ".txt", sep=""),
                                                  weighted = TRUE,
                                                  threshold = 0.02,
                                                  nodeNames = modProbes,
                                                  altNodeNames = modGenes,
                                                  nodeAttr = heat_wgcna()[[4]][inModule])
                                                  
    edgefile[length(edgefile)+1]<-paste("./files/CytoscapeInput-edges-", modules, ".txt", sep="")
    nodefile[length(nodefile)+1]<- paste("./files/CytoscapeInput-nodes-", modules, ".txt", sep="")
    file_name[length(file_name)+1]<-paste("./files/CytoscapeInput_GFC_", modules, ".txt", sep="")
    print("ok")
    
  }
  
  list(cyt,edgefile,nodefile,group_fc,file_name,modules)
})

output$DownloadAllNetworkToCytoscape <- downloadHandler(
  
  filename =function()
  {
    paste("output", "zip", sep=".")
    
  },
  content = function(file) {
    
    fs<-c()
    df<-as.data.frame(table(heat_wgcna()[[4]]))
    colnames(df)<-c("Modules","Number of genes")
    
    modules_num<- nrow(df)
    network<-network_cytoscape()
    
    if(!is.null(network[[1]]))
    {
      for(i in 1:modules_num)
      {
        print(i)
        fs<-c(fs,network[[2]][[i]],
              network[[3]][[i]],
              network[[5]][[i]])
          print(i)
          write.table(network_cytoscape()[[1]][[i]], file=network_cytoscape()[[5]][[i]], row.names = T, col.names=NA, quote = F, sep = "\t")
       
      }
    }
    print(fs)
    zip(zipfile=file, files=fs)
  },
  contentType = "application/zip"
  
)


###################################################################hub-genes
hubGenes <- reactive({
  NS1=networkScreening(y=datTraits1()[,1], 
                       datME=heat_wgcna()[[3]], 
                       datExpr=datExpr1(), 
                       corFnc = "cor", corOptions = "use = 'p'",
                       oddPower=3, 
                       blockSize=1000, 
                       minimumSampleSize=4, 
                       addMEy=TRUE, 
                       removeDiag=FALSE, 
                       weightESy=0.5,getQValues = TRUE) 
  rownames(NS1) <- colnames(datExpr1())
  GS_MM1=heat_wgcna()[[10]][(heat_wgcna()[[10]]$moduleColor==input$Module_text),]
  NS2<- NS1[rownames(NS1)%in% GS_MM1[,1],]
  GS=abs(GS_MM1[,3])
  MM=abs(GS_MM1[paste("MM",input$Module_text,sep="")])
  FilterGenes_spe2 = (GS>input$GS_soft & MM >input$MM_soft & 
                        NS2$p.Weighted<input$weighted_soft &
                        NS2$q.Weighted<input$qweighted_soft)
  
 trait_hubGenes_spe2<- rownames(NS2)[FilterGenes_spe2] 
  hubGenes<- NS2[rownames(NS2)%in% trait_hubGenes_spe2,]
  
})


output$hubGenesData<- DT::renderDT( 
  DT::datatable(hubGenes(), 
                escape = FALSE,
                rownames = T,
                extensions = "Responsive",
                options=list(
                  pageLength = 15,
                  lengthMenu = list(c(15, 50, 100, -1),c(15, 50, 100, "ALL")),
                  scrollX = TRUE,
                  scrollY = TRUE,
                  fixedColumns = TRUE,
                  fixedHeader = TRUE
                )
  ))

output$UserDownloadhubGenes <- downloadHandler(
  filename = function(){
    paste0(input$Module_text,'hubGenes.',input$extTable4)
  },
  content = function(file){
    if(input$extTable4 == 'csv'){
      write.csv(hubGenes(), file, sep = ',', col.names = T, row.names = T, quote = F)
    }else{
      write.table(hubGenes(), file, sep = '\t', col.names = T, row.names = T, quote = F)
    }
  }
)
########################################################Gene expression in different groups
output$GeneExp.ui <- renderUI({
  plotOutput("GeneExp")
})


#################
output$GeneExp <- renderPlot({
  data_plot = data.frame(pairinfo=c(1:length(grep( input$group1_text, grouplist1())),1:length(grep( input$group2_text, grouplist1()))),
                         group=grouplist1(),
                         datExpr1(),stringsAsFactors = F)
  y=data_plot[,colnames(data_plot)==input$gene_text]
  
  ggplot(data_plot, aes(group,y,fill=group)) +
    geom_boxplot() +
    geom_point(size=2, alpha=0.5) +
    geom_line(aes(group=pairinfo), colour="black", linetype="11") +
    xlab("") +
    ylab(paste("Expression of ",input$gene_text))+
    theme_classic()+
    theme(legend.position = "none")
 
   
})
##############
output$DownloadGeneExp <- downloadHandler(
  filename = function(){
    paste0(input$gene_text,'Boxpot.',input$extPlot4)
  },
  content = function(file){
    if(input$extPlot4 == 'pdf'){
      pdf(file)
    }else if(input$extPlot4 == 'png'){
      png(file)
    }else{
      jpeg(file)
    }
    
   
    data_plot = data.frame(pairinfo=c(1:length(grep( input$group1_text, grouplist1())),1:length(grep( input$group2_text, grouplist1()))),
                           group=grouplist1(),
                           datExpr1(),stringsAsFactors = F)
    y=data_plot[,colnames(data_plot)==input$gene_text]
     ggplot(data_plot, aes(group,y,fill=group)) +
      geom_boxplot() +
      geom_point(size=2, alpha=0.5) +
      geom_line(aes(group=pairinfo), colour="black", linetype="11") +
      xlab("") +
      ylab(paste("Expression of ",input$gene_text))+
      theme_classic()+
      theme(legend.position = "none")
    dev.off()
  }
)

#########################################

output$GeneExp2.ui <- renderUI({
 plotOutput("GeneExp2")
})

#################
 output$GeneExp2 <- renderPlot({
   ex4=t(datExpr1())
  TMP = ex4[rownames( ex4 ) == input$gene_text, ]
  data = as.data.frame(TMP)
  data$group = grouplist1()
 p=ggbetweenstats(data = data,x = group,  y = TMP ,xlab = "",
                  ylab = paste("Expression of ",input$gene_text))
 print(p)
  
})
##############
output$DownloadGeneExp2 <- downloadHandler(
  filename = function(){
   paste0(input$gene_text,'Violinogram.',input$extPlot4)
  },
 content = function(file){
    if(input$extPlot4 == 'pdf'){
      pdf(file)
   }else if(input$extPlot4 == 'png'){
      png(file)
    }else{
      jpeg(file)
    }

ex4=t(datExpr1())
TMP = ex4[rownames( ex4 ) == input$gene_text, ]
data = as.data.frame(TMP)
data$group = grouplist1()
p=ggbetweenstats(data = data,x = group,  y = TMP )
print(p)
    
   
   dev.off()
  }
)

###############################

layout <- reactive({
  if (input$UserPanelLayout3 == "manual") {
    c(input$UserPanelLayoutCols3, input$UserPanelLayoutRows3)}
  else NULL
})    


############################################################Gene function analysis
GeneNamesData <- reactive({ 
  inFile <- input$genenamesfile 
  if (is.null(inFile)) return(NULL) 
  data <- read.csv(inFile$datapath, header = TRUE) 
  
}) 

GeneNamesData1 <- reactive({ 
tmp2 <- GeneNamesData()
tmp2=as.data.frame(tmp2)
tmp2$NCBI <- ncbiLink(tmp2$SYMBOL,tmp2$SYMBOL)
tmp2$GeneCards <- geneCardsLink(tmp2$SYMBOL,tmp2$SYMBOL)
tmp2
}) 
output$GeneNames <- DT::renderDT(
  
  DT::datatable( GeneNamesData1(), 
                 escape = FALSE,
                 rownames = F,
                 extensions = "Responsive",
                 options=list(
                   pageLength = 15,
                   lengthMenu = list(c(10, 20,50, 100, -1),c(10, 20,50, 100, "ALL")),
                   scrollX = TRUE,
                   scrollY = TRUE,
                   fixedColumns = TRUE,
                   fixedHeader = TRUE
                 )
  )
)
output$UserDownloadGeneNames <- downloadHandler(
  filename = function(){
    paste0("GeneNamesflie", '.',input$extTable5)
  },
  content = function(file){
    if(input$extTable5 == 'csv'){
      write.csv(GeneNamesData(), file, sep = ',', col.names = T, row.names = T, quote = F)
    }else{
      write.table(GeneNamesData(), file, sep = '\t', col.names = T, row.names = T, quote = F)
    }
  }
)

############################################Gene function analysis
Genes <- reactive({
  bitr(GeneNamesData()[,1], fromType="SYMBOL", toType="ENTREZID", OrgDb="org.Hs.eg.db")
})

#####################################GO
output$GenesGO.ui <- renderUI({
  plotOutput("GenesGO",height = input$UserPrintHeight3, width = input$UserPrintWidth3)
})
#################
output$GenesGO <- renderPlot({
  All<- enrichGO(gene = Genes()$ENTREZID,
                     OrgDb= org.Hs.eg.db,
                     ont = "all",
                     pAdjustMethod = "BH",
                     minGSSize = 100,
                     pvalueCutoff = input$GO_pCutoff,
                     #qvalueCutoff = 0.05,
                     readable = TRUE)
 
  y = All[All$p.adjust < 0.5, asis = T] 
   
  GOall=dotplot(All, split="ONTOLOGY",showCategory=20,color = 'pvalue')+ 
    facet_grid(ONTOLOGY~.,scale="free")+
    scale_size(range=c(2, 10)) +
    scale_color_continuous(low='gold', high='green')
  print(GOall)
})
##############
output$DownloadGenesGO <- downloadHandler(
  filename = function(){
    paste0('EnrichmentGO.',input$extPlot5)
  },
  content = function(file,height = input$UserPrintHeight3, width = input$UserPrintWidth3){
    if(input$extPlot5 == 'pdf'){
      pdf(file,
          width = {if (input$UserPrintWidth3 == "auto") 7
            else as.numeric(input$UserPrintWidth3)/100}, 
          height = as.numeric(input$UserPrintHeight3)/100)
    }else if(input$extPlot5 == 'png'){
      png(file)
    }else{
      jpeg(file)
    }
    All<- enrichGO(gene = Genes()$ENTREZID,
                   OrgDb= org.Hs.eg.db,
                   ont = "all",
                   pAdjustMethod = "BH",
                   minGSSize = 100,
                   pvalueCutoff = input$GO_pCutoff,
                   #qvalueCutoff = 0.05,
                   readable = TRUE)
    
 
    GOall=dotplot(All, split="ONTOLOGY",showCategory=20,color = 'pvalue')+ 
      facet_grid(ONTOLOGY~.,scale="free")+
      scale_size(range=c(2, 10)) +
      scale_color_continuous(low='gold', high='green')
  print(GOall)
    dev.off()
  }
)



#####################################KEGG
output$GenesKEGG.ui <- renderUI({
  plotOutput("Genes_KEGG_dot",height = input$UserPrintHeight3, width = input$UserPrintWidth3)
})
#################
output$Genes_KEGG_dot <- renderPlot({
  genekk <- enrichKEGG(gene = Genes()$ENTREZID,
                   organism =input$organism_text,
                   pvalueCutoff = input$KEGG_pCutoff,
                   #qvalueCutoff = 0.05,
                   #minGSSize = 1,
                   #readable = TRUE ,
                   use_internal_data =FALSE)
  KK=dotplot(genekk,title="Enrichment_KEGG")
  print(KK)
})
##############
output$DownloadGenesKEGG <- downloadHandler(
  filename = function(){
    paste0('Enrichment_KEGG_dot.',input$extPlot5)
  },
  content = function(file,height = input$UserPrintHeight3, width = input$UserPrintWidth3){
    if(input$extPlot5 == 'pdf'){
      pdf(file,
          width = {if (input$UserPrintWidth3 == "auto") 7
            else as.numeric(input$UserPrintWidth3)/100}, 
          height = as.numeric(input$UserPrintHeight3)/100)
    }else if(input$extPlot5 == 'png'){
      png(file)
    }else{
      jpeg(file)
    }
    
    genekk <- enrichKEGG(gene = Genes()$ENTREZID,
                         organism =input$organism_text,
                         pvalueCutoff = input$KEGG_pCutoff,
                         #qvalueCutoff = 0.05,
                         #minGSSize = 1,
                         #readable = TRUE ,
                         use_internal_data =FALSE)
    KK=dotplot(genekk,title="Enrichment_KEGG")
    print(KK)
    dev.off()
  }
)

#####################################the relationship network diagram of Pathway and gene 

output$Genespathway.ui <- renderUI({
  plotOutput("Enrichment_pathway_dot",height = input$UserPrintHeight3, width = input$UserPrintWidth3)
})
#################
output$Enrichment_pathway_dot <- renderPlot({
  genes_kk <- enrichKEGG(gene = Genes()$ENTREZID,
                   organism =input$organism_text,
                   pvalueCutoff = input$KEGG_pCutoff,
                   #qvalueCutoff = 0.05,
                   #minGSSize = 1,
                   #readable = TRUE ,
                   use_internal_data =FALSE)
  genepathway=cnetplot(genes_kk,showCategory = 5)
  print(genepathway)
})
##############
output$DownloadGenespathway <- downloadHandler(
  filename = function(){
    paste0('pathway_network.',input$extPlot5)
  },
  content = function(file,height = input$UserPrintHeight3, width = input$UserPrintWidth3){
    if(input$extPlot5 == 'pdf'){
      pdf(file,
          width = {if (input$UserPrintWidth3 == "auto") 7
            else as.numeric(input$UserPrintWidth3)/100}, 
          height = as.numeric(input$UserPrintHeight3)/100)
    }else if(input$extPlot5 == 'png'){
      png(file)
    }else{
      jpeg(file)
    }
    genes_kk <- enrichKEGG(gene = Genes()$ENTREZID,
                           organism =input$organism_text,
                           pvalueCutoff = input$KEGG_pCutoff,
                           #qvalueCutoff = 0.05,
                           #minGSSize = 1,
                           #readable = TRUE ,
                           use_internal_data =FALSE)
    genepathway=cnetplot(genes_kk,showCategory = 5)
    print(genepathway)
    dev.off()
  }
)


    
 }

