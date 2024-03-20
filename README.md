![alt text](https://raw.githubusercontent.com/rafaelcastanha/The-Coupler-Shiny-App/main/coupler_git.bmp)

Ferramenta para análises relacionais de citação com foco no acoplamento bibliográfico (análise de similaridade).

1) A ferramenta foi inicialmente concebida para análise bibliométriccas porém pode ser usada para qualquer unidade de análise processando dados de diferentes natureza (documentos, autores, periódicos, palavras-chave, DOI, seguidores em redes sociais, gestão de pessoas, cooperação em projetos, etc).
2) Selecione o arquivo em extensão txt para executar o processamento: O arquivo deve ser organizado em colunas separadas por vírgula, ponto e vírgula ou tabulado, em que o cabeçalho representa o item citante e cada linha abaixo do cabeçalho, os itens citados.
3) É desejavel que cada coluna contenha valores únicos.
4) Selecione a separação do seu arquivo: vírgula, ponto e vírgula ou tabulado.
5) Clique em "Coupling!".
6) X1 e X2 representam os itens comparados (ex: Autor 1 e Autor 2 a serem acoplados).
7) refs_X1 e refs_x2 representam o tamanho (cardinalidade) da lista referências (itens citados; elementos que compõe cada unidade) pelos itens citantes X1 e X2.
8) "Coupling" representa o número de itens em comum entre X1 e X2 (em alusão a bibliographic coupling).
9) Jaccard_Index e Saltons_Cosine representam as normalizações via Índice de Jaccard e Cosseno de Salton.
10) "Unidades de Acoplamento" identifica quais foram a unidades responsáveis por promover a intersecção entre X1 e X2.
11) A "Matriz de Citação" não é ponderada e apresenta a matriz booleana de citação (matriz incidência) composta por 0 ou 1.
12) A "Matriz de Acoplamento" é ponderada pelos valores de acoplamento presentes em "Coupling".
13) Os valores de entrada da "Matriz de Cocitação" representam a quantidade de listas de referências em que cada par é cocitado.
14) É possível fazer o download das matrizes e das redes de citação, acoplamento e cocitação em formato .net (pajek)
15) Caso nenhuma das unidades de análise se acoplem entre si, será retornada uma mensagem de alerta.
16) Web-app em funcionamento em: https://rafaelcastanha.shinyapps.io/thecoupler/

17) Contato: rafael.castanha@unesp.br

18) Assista o vídeo explicativo do funcionamento a parte principal do código da ferramenta em: https://youtu.be/f95I_gc6vi8

19) Artigo publicado: https://doi.org/10.20396/rdbci.v20i00.8671208
