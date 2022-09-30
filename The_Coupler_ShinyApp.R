#PACOTES

install.packages('shiny')
install.packages('shinydashboard')
install.packages('flexdashboard')
install.packages('dplyr')
install.packages('RVenn')
install.packages('igraph')

library(dplyr)
library(RVenn)
library(igraph)
library(shiny)
library(shinydashboard)
library(flexdashboard)

# UI e Server R

ui <- fluidPage(dashboardPage(dashboardHeader(title="The Coupler"), dashboardSidebar(
  
  fileInput("file1", "Selecione o arquivo:", accept = ".txt"),
  
  selectInput("sep", "Separador:", c(" ", Vírgula = ",", Ponto_Vírgula = ";", Tabulado = "\t")),
  
  selectInput("normalization", "Normalizações:", c("sem normalização", "Cosseno de Salton", "Índice de Jaccard")),
  
  fluidPage(h5(" ")),
  
  fluidPage(tags$a(href="https://github.com/rafaelcastanha/The-Coupler-Shiny-App", "Instruções de uso e Código (GitHub)")),
  
  fluidPage(h5(" ")),
  
  fluidPage(tags$a(href="http://lattes.cnpq.br/4834832439175113", "Currículo Lattes")),
  
  fluidPage(h5(" ")),
  
  fluidPage(tags$a(href="https://www.researchgate.net/profile/Rafael-Gutierres-Castanha-2", "ResearchGate")),
  
  fluidPage(h5(" ")), 
  
  out = fluidPage(h5("Desenvolvido por:")), out = fluidPage(h5("Rafael Gutierres Castanha")), 
  
  fluidPage(h5(" ")),
  
  out = fluidPage(h5("rafael.castanha@unesp.br")),
  
  actionButton("runmodel", "Coupling!")),
  
  dashboardBody(
    
    tabsetPanel(type="tab",
                
                tabPanel(title="Rede de Acoplamento Bibliográfico", column(textOutput("erro"), width = 12, plotOutput(outputId = "PlotCoupling",  width = "100%", heigh=680))), 
                
                tabPanel(title="Frequências de Acoplamento", dataTableOutput(outputId = "DataFrameCoupling"),downloadButton("dlfreq", "Download Data")),
                
                tabPanel(title="Unidades de Acoplamento",style='overflow-x: scroll', dataTableOutput(outputId = "DataFrameUnits"),downloadButton("dlunits", "Download Data")),
                
                tabPanel(title="Matriz de Citação", style='overflow-x: scroll', dataTableOutput(outputId = "DataFrameCit"),downloadButton("dlcit", "Download Data")),
                
                tabPanel(title="Matriz de Acoplamento", style='overflow-x: scroll', dataTableOutput(outputId = "DataFrameMatrix"),downloadButton("dlaba", "Download Data")),
                
                tabPanel(title="Matriz de Cocitação", style='overflow-x: scroll', dataTableOutput(outputId = "DataFrameCocit"),downloadButton("dlcocit", "Download Data"))
                
    ))))

server <- function(input, output){
  
  observe({
    
    #Arquivo
    
    input$runmodel
    
    if (input$runmodel==0)
      return()
    else
      
      isolate({
        
        r<-reactive({input$file1})
        
        req(r())
        
      })
    
    corpus<-isolate(read.table(r()$datapath, header = TRUE, sep = input$sep, quote="\""))
    
    
    #Corpus para dataframe
    
    corpus<-as.data.frame(corpus)
    
    #remover espaços e vazios
    
    corpus<-corpus
    corpus[corpus==""|corpus==" "|corpus=="   "]<-NA
    empty_columns<-sapply(corpus, function(x) all(is.na(x)|x==""))
    corpus<-corpus[,!empty_columns]
    
    #Contagem de itens citados por lista
    
    citados<-function(x){return(length(which(!is.na(x))))}
    itens_citados<-apply(X=corpus,FUN=citados,MARGIN=c(1,2))
    df1<-as.data.frame(itens_citados, header=TRUE)
    df2<-colSums(df1)
    df2<-as.data.frame(df2, header=TRUE)
    df2<-tibble::rownames_to_column(df2, "VALUE")
    colnames(df2)[1]<-"units"
    colnames(df2)[2]<-"refs"
    references<-df2
    
    #Transformação em objeto Venn
    
    corpus_aba<-Venn(corpus)
    
    #Intersecção Pareada: identificação das unidades de acoplamento
    
    ABA<-overlap_pairs(corpus_aba)
    
    #Unidades por acoplamento
    
    
    unit_aba<-na.omit(stack(ABA))
    
    units_coupling<-unit_aba %>% group_by(ind) %>% summarise(valeus=(paste(values, collapse="; ")))
    
    units_final<-as.data.frame(units_coupling)
    
    colnames(units_final)[1]<-"Unidades de Análise"
    colnames(units_final)[2]<-"Unidades de Acoplamento"
    
    #Intensidades de ABA
    
    df<-as.data.frame(table(stack(ABA)))
    
    df<-as.data.frame(table(stack(ABA)))
    
    if (nrow(df)==0) {
      
      output$PlotCoupling<-renderPlot({plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
        text(x = 0.5, y = 0.5, paste("WARNING: No couplings between units (ALERTA: Não há acoplamento entre as unidades!)"), cex = 1.5, col = "black")
        
      })
      
      stop
      
    }
    
    else {
      
      int_aba<-aggregate(Freq ~ ind, data = df, FUN = sum)
      Freq_ABA<-data.frame(do.call("rbind",strsplit(as.character(int_aba$ind),"...",fixed=TRUE)))
      Freq_ABA["Coupling"]<-int_aba$Freq
      
      m2=merge(Freq_ABA,df2,by.x="X2",by.y="units",all.x=TRUE)
      m1=merge(m2,df2,by.x="X1",by.y="units",all.x=TRUE)
      colnames(m1)[4]<-"refs_X2"
      colnames(m1)[5]<-"refs_X1"
      Freq_ABA<-m1 %>% select(X1,X2,"refs_X1","refs_X2","Coupling")
      
      #Normalizações 
      
      novacoluna<-c("Saltons_Cosine")
      Freq_ABA[,novacoluna]<-Freq_ABA$Coupling/sqrt(Freq_ABA$refs_X1*Freq_ABA$refs_X2)
      novacoluna_2<-c("Jaccard_Index")
      Freq_ABA[,novacoluna_2]<-Freq_ABA$Coupling/(Freq_ABA$refs_X1+Freq_ABA$refs_X2-Freq_ABA$Coupling)
      
      #Rede de acoplamento bibliográfico
      
      net_list<-filter(Freq_ABA, Coupling>0)
      links<-data.frame(source=c(net_list$X1), target=c(net_list$X2))
      network_ABA<-graph_from_data_frame(d=links, directed=F)
      
      edge_ABA<-net_list$Coupling
      edge_CS<-net_list$Saltons_Cosine
      edge_IJ<-net_list$Jaccard_Index
      
      #Matrizes Adjacencia
      
      #Matriz Acoplamento
      
      mtx<-as_adjacency_matrix(network_ABA)
      E(network_ABA)$weight<-net_list$Coupling
      mtx_ad<-as_adjacency_matrix(network_ABA, attr="weight")
      mtx_adj<-as.data.frame(as.matrix(mtx_ad))
      mtx_adj<-tibble::rownames_to_column(mtx_adj, " ")
      
      dt<-stack(corpus)
      dt2<-table(dt$values[row(dt[-1])], unlist(dt[-1]))
      mtx_cit<-t(dt2)
      mtx_cocit<-(t(mtx_cit) %*% mtx_cit)
      mtx_cocit<-as.table(mtx_cocit)
      
      #Matriz Cocitação
      
      mtx_cocit_df<-as.data.frame(mtx_cocit)
      links_cocit<-data.frame(source=c(mtx_cocit_df$Var1), target=c(mtx_cocit_df$Var2))
      network_cocit<-graph_from_data_frame(d=links_cocit, directed=T)
      E(network_cocit)$weight<-mtx_cocit_df$Freq
      mtx_adj_cocit<-as_adjacency_matrix(network_cocit, attr="weight")
      mtx_adj_cocit_df<-as.data.frame(as.matrix(mtx_adj_cocit))
      mtx_adj_cocit_df<-tibble::rownames_to_column(mtx_adj_cocit_df, " ")
      
      
      #Matriz Citação
      
      mtx_cit_df<-as.data.frame(mtx_cit)
      links_cit<-data.frame(source=c(mtx_cit_df$Var1), target=c(mtx_cit_df$Var2))
      network_cit<-graph_from_data_frame(d=links_cit, directed=T)
      E(network_cit)$weight<-mtx_cit_df$Freq
      mtx_adj_cit<-as_adjacency_matrix(network_cit, attr="weight")
      mtx_adj_cit_df<-as.data.frame(as.matrix(mtx_adj_cit))
      l<-length(references$units)
      l1=l+1
      l2<-length(unique(mtx_cit_df$Var2))+l
      mtx_citation<-mtx_adj_cit_df[1:l,l1:l2]
      mtx_citation<-tibble::rownames_to_column(mtx_citation, " ") 
      
      output$erro<-renderText({
        
        input$runmodel
        
        if (nrow(df)!=0) {print(" ")}
        
      })
      
      output$PlotCoupling <- renderPlot({
        
        input$runmodel
        
        if ("sem normalização" %in% input$normalization)
          
          plot(network_ABA,  layout=layout_as_star, edge.width=c(net_list$Coupling), vertex.size=9, vertex.color=rgb(0.8,0.6,0.8,0.9), vertex.label.color='black', edge.color='grey', vertex.label.cex=1)
        
        if ("Cosseno de Salton" %in% input$normalization)
          
          plot(network_ABA, layout=layout_as_star, edge.width=c(net_list$Saltons_Cosine*10), vertex.size=9, vertex.color=rgb(0.8,0.6,0.8,0.9), vertex.label.color='black', edge.color='grey', vertex.label.cex=1)
        
        if  ("Índice de Jaccard" %in% input$normalization)
          
          plot(network_ABA,  layout=layout_as_star, edge.width=c(net_list$Jaccard_Index*10), vertex.size=9, vertex.color=rgb(0.8,0.6,0.8,0.9), vertex.label.color='black', edge.color='grey', vertex.label.cex=1)
        
      })
      
      output$DataFrameCoupling <- renderDataTable(Freq_ABA)
      
      output$DataFrameUnits <- renderDataTable(units_final)
      
      output$DataFrameCit <- renderDataTable(mtx_citation)
      
      output$DataFrameMatrix <- renderDataTable(mtx_adj)
      
      output$DataFrameCocit <- renderDataTable(mtx_adj_cocit_df)
      
      output$dlfreq <- downloadHandler(
        filename = function(){
          paste("Frequências de Acoplamento", "txt", sep=".")
        },
        content = function(file){
          write.table(Freq_ABA, file, sep="\t", row.names = F, col.names = TRUE)
        })
      
      output$dlunits <- downloadHandler(
        filename = function(){
          paste("Unidades de Acoplamento", "txt", sep=".")
        },
        content = function(file){
          write.table(units_final, file, sep="\t", row.names = F, col.names = TRUE)
        })
      
      output$dlcit <- downloadHandler(
        filename = function(){
          paste("Matriz de Citacao", "txt", sep=".")
        },
        content = function(file){
          write.table(mtx_citation, file, sep="\t", row.names = F, col.names = TRUE)
        })
      
      
      output$dlaba <- downloadHandler(
        filename = function(){
          paste("Matriz de Acoplamento", "txt", sep=".")
        },
        content = function(file){
          write.table(mtx_adj, file, sep="\t", row.names = F, col.names = TRUE)
        })
      
      output$dlcocit <- downloadHandler(
        filename = function(){
          paste("Matriz de Cocitacao", "txt", sep=".")
        },
        content = function(file){
          write.table(mtx_adj_cocit_df, file, sep="\t", row.names = F, col.names = TRUE)
        })
    }
    
  })
  
}

shinyApp(ui, server)
