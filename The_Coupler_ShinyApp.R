#Memória

memory.limit(size=56000)

#PACOTES

library(dplyr)
library(RVenn)
library(igraph)
library(shiny)
library(shinydashboard)
library(flexdashboard)
library(visNetwork)
library(shinycssloaders)

# UI e Server R

ui <- fluidPage(

dashboardPage(title='the_coupler',dashboardHeader
(title = img(src="couplerbrancovf.png")),

dashboardSidebar(title=h5('Atualizado em: 15 out 2023 INPI:BR512023001129-0',
style = "margin-left:110px;margin-top:0px;font-size:10px"),

div(tags$style(HTML("section.sidebar .shiny-input-container {
padding: 0px 15px 0px 15px;
white-space: normal;
}"))),


div(tags$style(HTML(".form-group {
margin-bottom: 0px;
}"))),
fileInput("file1", "Selecione o arquivo:", accept = ".txt"),
selectInput("sep", "Separador:", c(" ", Vírgula = ",", Ponto_Vírgula = ";", Tabulado = "\t")),

selectInput("normalization", "Normalizações:", c("sem normalização", "Cosseno de Salton", "Índice de Jaccard")),

fluidPage(h5(" ")),

fluidPage(tags$a(href="https://github.com/rafaelcastanha/The-Coupler-Shiny-App", "Instruções e Código (GitHub)", style = "margin-left:15px;")),

fluidPage(h5(" ")),

fluidPage(tags$a(href="https://doi.org/10.20396/rdbci.v20i00.8671208", "Artigo do The Coupler", style = "margin-left:15px;")),

fluidPage(h5(" ")),

fluidPage(tags$a(href="https://zenodo.org/record/7130614#.YzdGCnbMLIU", "Arquivos para testes", style = "margin-left:15px;")),

fluidPage(h5(" ")),

fluidPage(tags$a(href="http://lattes.cnpq.br/4834832439175113", "Currículo Lattes", style = "margin-left:15px;")),

fluidPage(h5(" ")),

fluidPage(tags$a(href="https://www.researchgate.net/profile/Rafael-Gutierres-Castanha-2", "ResearchGate", style = "margin-left:15px;")),

fluidPage(h5(" ")),

out = fluidPage(h5("Desenvolvido por:", style = "margin-left:15px;")),

out = fluidPage(h5("Rafael Gutierres Castanha", style = "margin-left:15px;margin-top:0px;")),

fluidPage(h5(" ")),

out = fluidPage(h5("rafael.castanha@unesp.br", style = "margin-left:15px;margin-top:-10px;")),

actionButton("runmodel", "Coupling!", style="width:200px;")),

dashboardBody(tags$head(tags$style(HTML('
.container-fluid {
padding-right: 0px;
padding-left: 0px;}'))),

fluidRow(width = 12, height = NULL,

tabsetPanel(type="tab",

tabPanel(title="Rede de Acoplamento Bibliográfico",
conditionalPanel("input.runmodel!=0",
column(textOutput("erro"), width = 12,
withSpinner(visNetworkOutput("PlotCoupling", width = "100%", heigh=450)),
downloadButton("dl_net", "Download Rede (.html)"),
downloadButton("dl_pajek_net", "Download pajek (.net)")))),

            
tabPanel(title="Frequências de Acoplamento",
dataTableOutput(outputId = "DataFrameCoupling"),
downloadButton("dlfreq", "Download Data")),

tabPanel(title="Unidades de Acoplamento",style='overflow-x: scroll',
dataTableOutput(outputId = "DataFrameUnits"),
downloadButton("dlunits", "Download Data")),

tabPanel(title="Matriz de Citação", style='overflow-x: scroll',
dataTableOutput(outputId = "DataFrameCit"),
downloadButton("dlcit", "Download Matriz"),
downloadButton("dlcitpaj", "Download Pajek (.net)")),

tabPanel(title="Matriz de Acoplamento", style='overflow-x: scroll',
dataTableOutput(outputId = "DataFrameMatrix"),
downloadButton("dlaba", "Download Matriz"),
downloadButton("dl_pajek", "Download Pajek (.net)")),

tabPanel(title="Matriz de Cocitação", style='overflow-x: scroll',
dataTableOutput(outputId = "DataFrameCocit"),
downloadButton("dlcocit", "Download Matriz"),
downloadButton("dlcocitpaj", "Download Pajek (.net)"))

))))

)

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

corpus<-isolate(read.table(r()$datapath, header = FALSE, sep = input$sep, quote="\""))

colnames(corpus)<-corpus[1,]
corpus<-corpus[(-1),]
hd<-gsub("\\.$","",names(corpus))
colnames(corpus)<-hd

#Corpus para dataframe

corpus<-as.data.frame(corpus)

#remover espaços e vazios

corpus<-corpus
corpus[corpus==""|corpus==" "|corpus==" "]<-NA
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

if (nrow(df)==0) {

nodes <- data.frame(id = 1, shape = "icon", icon.face = 'Ionicons',
icon.code = c("f100"))
edges <- data.frame(from = c(1))

output$PlotCoupling<-renderVisNetwork({

visNetwork(nodes=nodes, edges=edges,shape = "icon",
main="WARNING: No couplings between units (ALERTA: Não há acoplamento entre as unidades!)") %>% addIonicons()

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

node1<-as.data.frame(references$units)
node2<-as.data.frame(references$units)
colnames(node1)[1]<-"id"
colnames(node2)[1]<-"label"
node<-data.frame(node1, node2)

ED_coupling<-mutate(links, width = edge_ABA)
colnames(ED_coupling)[1]<-"from"
colnames(ED_coupling)[2]<-"to"

ED_salton<-mutate(links, width = edge_CS*10)
colnames(ED_salton)[1]<-"from"
colnames(ED_salton)[2]<-"to"

ED_jaccard<-mutate(links, width = edge_IJ*10)
colnames(ED_jaccard)[1]<-"from"
colnames(ED_jaccard)[2]<-"to"

#Matrizes Adjacencia

dt<-stack(corpus)
dt2<-table(dt$values[row(dt[-1])], unlist(dt[-1]))

#Operacoes de matriz

mtx_cit<-t(dt2) #citacao

mtx_adj<-mtx_cit %*% t(mtx_cit) #acoplamento
diag(mtx_adj)<-0

#Matriz de Cocitação

mtx_cocit<-(t(mtx_cit) %*% mtx_cit) #cocitacao
diag(mtx_cocit)<-0

#matriz de citação

mtx_cit_df<-tibble::rownames_to_column(as.data.frame.matrix(mtx_cit), " ")

#Matriz de Acoplamento

mtx_adj_df<-tibble::rownames_to_column(as.data.frame(mtx_adj), " ")

#Matriz de cocitacao

mtx_adj_cocit_df<-tibble::rownames_to_column(as.data.frame(mtx_cocit), " ")

#MENSAGEM DE ERRO

output$erro<-renderText({

input$runmodel

if (nrow(df)!=0) {print(" ")}

})

vis<-visNetwork(node, ED_coupling) %>%
visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
visIgraphLayout(layout = "layout_with_fr")

output$PlotCoupling <- renderVisNetwork({

input$runmodel

if ("sem normalização" %in% input$normalization){

vis

}

else{

if ("Cosseno de Salton" %in% input$normalization){

visNetwork(node, ED_salton) %>%
visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
visIgraphLayout(layout = "layout_with_fr")}

else{
if ("Índice de Jaccard" %in% input$normalization)

visNetwork(node, ED_jaccard) %>%
visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
visIgraphLayout(layout = "layout_with_fr")

}
}


})

output$DataFrameCoupling <- renderDataTable(Freq_ABA)

output$DataFrameUnits <- renderDataTable(units_final)

output$DataFrameCit <- renderDataTable(mtx_cit_df)

output$DataFrameMatrix <- renderDataTable(mtx_adj_df)

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
write.table(mtx_cit_df, file, sep="\t", row.names = F, col.names = TRUE)
})


output$dlaba <- downloadHandler(
filename = function(){
paste("Matriz de Acoplamento", "txt", sep=".")
},
content = function(file){
write.table(mtx_adj_df, file, sep="\t", row.names = F, col.names = TRUE)
})

output$dlcocit <- downloadHandler(
filename = function(){
paste("Matriz de Cocitacao", "txt", sep=".")
},
content = function(file){
write.table(mtx_adj_cocit_df, file, sep="\t", row.names = F, col.names = TRUE)
})


#DOWNLOAD IMAGEM: HTML

output$dl_net <- downloadHandler(

filename = function(){
paste("Rede de Acoplamento_img", "html", sep=".")
},

content = function(file){
visSave(graph=vis, file = file)
})


#DOWNLOAD REDE DE ACOPLAMENTO

#Construção da rede de acoplamento (pajek) para download

rede_paj<-graph_from_adjacency_matrix(mtx_adj, weighted = T, mode = "undirected")
E(rede_paj)$weight<-E(rede_paj)$weight
V(rede_paj)$id <- as.character(unique(row.names(mtx_adj)))

#Output download Pajek (aba de matriz)

output$dl_pajek <- downloadHandler(

filename = function(){

paste("Rede de Acoplamento", "net", sep=".")
},

content = function(file){
write.graph(rede_paj, file, format = "pajek")
})


#Output download Pajek (aba da rede)

output$dl_pajek_net <- downloadHandler(

filename = function(){


paste("Rede de Acoplamento", "net", sep=".")
},

content = function(file){
write.graph(rede_paj, file, format = "pajek")
})


#DOWNLOAD REDE DE COCITACAO

rede_cocit<-graph_from_adjacency_matrix(mtx_cocit, weighted = T, mode = "undirected")
E(rede_cocit)$weight<-E(rede_cocit)$weight
V(rede_cocit)$id <- as.character(unique(row.names(mtx_cocit)))

output$dlcocitpaj <- downloadHandler(

filename = function(){
paste("Rede de Cocitação", "net", sep=".")
},

content = function(file){
write.graph(rede_cocit, file, format = "pajek")
})

#DOWNLOAD REDE DE CITACAO

rede_cit<-graph_from_incidence_matrix(mtx_cit, weighted = T, directed = T, mode='out')
id_cit<-c(row.names(mtx_cit),colnames(mtx_cit))
E(rede_cit)$weight<-E(rede_cit)$weight
V(rede_cit)$id <- as.character(id_cit)

output$dlcitpaj <- downloadHandler(

filename = function(){
paste("Rede de Citação", "net", sep=".")
},

content = function(file){
write.graph(rede_cit, file, format = "pajek")
})


}

})

}

shinyApp(ui, server)
