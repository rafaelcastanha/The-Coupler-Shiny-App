Ferramenta para análises relacionais de citação com foco no acoplamento bibliográfico.

1) A ferramenta pode ser usada para qualquer unidade de análise (documentos, autores, periódicos, palavras-chave, DOI, seguidores em redes sociais, etc).
2) Selecione o arquivo em extensão txt para executar.
3) Selecione a separação do seu arquivo: vírgula, ponto e vírgula ou tabulado.
4) Clique em "Coupling!".
5) X1 e X2 representam os itens comparados (ex: Autor 1 e Autor 2 a serem acoplados).
6) refs_X1 e refs_x2 representam o tamanho (cardinalidade) da lista referências (itens citados) pelos itens X1 e X2.
7) "ABA" representa o número de itens citados em comum por X1 e X2 (em alusão a Acoplamento Bibliográfico de Autores).
8) Jaccard_Index e Saltons_Cosine representam as normalizações via Índice de Jaccard e Cosseno de Salton.
9) "Unidades de Acoplamento" identifica quais forems a unidades responsáveis por promover a intersecção entre X1 e X2.
10) A "Matriz de Citação" não é ponderada e apresenta a matriz booleana de citação composta por 0 ou 1.
11) A "Matriz de Acoplamento" é ponderada via ABA.
12) Os valores de entrada da "Matriz de Cocitação" representam a quantidade de listas de referências em que cada par é cocitado.
13) Em caso de erro com apostrofo ('), considere excluir os apostrofos, reescrever o nome ou substituir, por exemplo, por underline (_).

14) Webapp em funcionamento em: https://rafaelcastanha.shinyapps.io/thecoupler/

15) Contato: rafael.castanha@unesp.br

16) Assista o vídeo explicativo do funcionamento do código da ferramenta em: https://youtu.be/f95I_gc6vi8
