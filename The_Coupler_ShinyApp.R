#PACOTES

install.packages('shiny')
install.packages('shinydashboard')
install.packages('flexdashboard')
library(shiny)
library(shinydashboard)
library(flexdashboard)

# UI e Server R

ui <- fluidPage(dashboardPage(dashboardHeader(title="The Coupler"), dashboardSidebar(

fileInput("file1", "Selecione o arquivo:", accept = ".txt"),

selectInput("sep", "Separador:", c(" ", Vírgula = ",", Ponto_Vírgula = ";", Tabulado = "\t")),

selectInput("normalization", "Normalizações:", c("ABA (sem normalização)", "Cosseno de Salton", "Índice de Jaccard")),

fluidPage(h5(" ")),

fluidPage(tags$a(href="https://github.com/rafaelcastanha/The-Bibliographic-Coupler", "Código (GitHub)")),

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

tabPanel(title="Rede de Acoplamento Bibliográfico", column(width = 12, plotOutput(outputId = "PlotCoupling",  width = "100%", heigh=680))), 

tabPanel(title="Frequências de Acoplamento", dataTableOutput(outputId = "DataFrameCoupling")),

tabPanel(title="Unidades de Acoplamento", dataTableOutput(outputId = "DataFrameUnits")),

tabPanel(title="Matriz Citação", dataTableOutput(outputId = "DataFrameCit")),

tabPanel(title="Matriz Acoplamento", dataTableOutput(outputId = "DataFrameMatrix")),

tabPanel(title="Matriz Cocitação", dataTableOutput(outputId = "DataFrameCocit"))

))))

server <- function(input, output){

observe({

#bibliotecas 

library(dplyr)
library(RVenn)
library("igraph")

#Arquivo

input$runmodel

if (input$runmodel==0)
return()
else

isolate({

r<-reactive({input$file1})

req(r())

})

corpus<-isolate(read.table(r()$datapath, header = TRUE, sep = input$sep))


#Corpus para dataframe

corpus<-as.data.frame(corpus)

#remover espaços e vazios

corpus<-corpus
corpus[corpus==""|corpus==" "|corpus=="   "]<-NA

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

stack(ABA)

unit_aba<-na.omit(stack(ABA))

units_coupling<-unit_aba %>% group_by(ind) %>% summarise(valeus=(paste(values, collapse="; ")))

units_final<-as.data.frame(units_coupling)

colnames(units_final)[1]<-"Unidades de Análise"
colnames(units_final)[2]<-"Unidades de Acoplamento"

#Intensidades de ABA

df<-as.data.frame(table(stack(ABA)))
int_aba<-aggregate(Freq ~ ind, data = df, FUN = sum)
Freq_ABA<-data.frame(do.call("rbind",strsplit(as.character(int_aba$ind),"...",fixed=TRUE)))
Freq_ABA["ABA"]<-int_aba$Freq

m2=merge(Freq_ABA,df2,by.x="X2",by.y="units",all.x=TRUE)
m1=merge(m2,df2,by.x="X1",by.y="units",all.x=TRUE)
colnames(m1)[4]<-"refs_X2"
colnames(m1)[5]<-"refs_X1"
Freq_ABA<-m1 %>% select(X1,X2,"refs_X1","refs_X2","ABA")

#Normalizações 

novacoluna<-c("Saltons_Cosine")
Freq_ABA[,novacoluna]<-Freq_ABA$ABA/sqrt(Freq_ABA$refs_X1*Freq_ABA$refs_X2)
novacoluna_2<-c("Jaccard_Index")
Freq_ABA[,novacoluna_2]<-Freq_ABA$ABA/(Freq_ABA$refs_X1+Freq_ABA$refs_X2-Freq_ABA$ABA)

#Rede de acoplamento bibliográfico

net_list<-filter(Freq_ABA, ABA>0)
links<-data.frame(source=c(net_list$X1), target=c(net_list$X2))
network_ABA<-graph_from_data_frame(d=links, directed=F)

edge_ABA<-net_list$ABA
edge_CS<-net_list$Saltons_Cosine
edge_IJ<-net_list$Jaccard_Index

#Matrizes Adjacencia

#Matriz Acoplamento

mtx<-as_adjacency_matrix(network_ABA)
E(network_ABA)$weight<-net_list$ABA
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
l2<-length(unique(mtx_cit_df$Var2))
mtx_citation<-mtx_adj_cit_df[1:l,l1:l2]
mtx_citation<-tibble::rownames_to_column(mtx_citation, " ") 

output$PlotCoupling <- renderPlot({

input$runmodel

if ("ABA (sem normalização)" %in% input$normalization)

plot(network_ABA, edge.width=c(net_list$ABA), vertex.size=9, vertex.color=rgb(0.8,0.6,0.8,0.9), vertex.label.color='black', edge.color='grey', vertex.label.cex=1)

if ("Cosseno de Salton" %in% input$normalization)

plot(network_ABA, edge.width=c(net_list$Saltons_Cosine*10), vertex.size=9, vertex.color=rgb(0.8,0.6,0.8,0.9), vertex.label.color='black', edge.color='grey', vertex.label.cex=1)

if  ("Índice de Jaccard" %in% input$normalization)

plot(network_ABA, edge.width=c(net_list$Jaccard_Index*10), vertex.size=9, vertex.color=rgb(0.8,0.6,0.8,0.9), vertex.label.color='black', edge.color='grey', vertex.label.cex=1)

})

output$DataFrameCoupling <- renderDataTable(Freq_ABA)

output$DataFrameUnits <- renderDataTable(units_final)

output$DataFrameCit <- renderDataTable(mtx_citation)

output$DataFrameMatrix <- renderDataTable(mtx_adj)

output$DataFrameCocit <- renderDataTable(mtx_adj_cocit_df)

})

}

shinyApp(ui, server)