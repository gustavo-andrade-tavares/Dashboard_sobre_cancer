# Dashboard_sobre_cancer
Dashboard sobre câncer: Aplicação de shinydashboard

Os dados estão disponíveis no site: https://canceratlas.cancer.org/data/map/

Inicialmente foi feito os ajustes dos dados Cancer-Atlas, resultando em dois arquivos .rds (formato próprio do R). 

Em Dados_cancer.rds foi adicionado as geometrias dos países, como também a tradução de algumas variáveis para a visualização no mapa. 

Uma segunda cópia dos dados foi necessária para a elaboração dos gráficos, uma vez que, as geometrias dos países mudavam a classe do data frame e atrapalhava na utilização de funções padrão do R. Além disso, foi incluído os continentes dos respectivos países para melhor visualização nos gráficos.

Foi utilizado pacotes como leaflet, shiny e shinydashboard para criação do aplicativo.

Foi feito visualizações em mapa, gráficos interativos e tabelas com estatísticas.

Link para a aplicação: 
https://guandrade.shinyapps.io/shinydashboard_app/