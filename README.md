Classificador_Confinamento

Pipeline estatístico robusto para classificação operacional de ovinos em confinamento utilizando ciência de dados, seleção estrutural de variáveis, regressão logística penalizada de Firth, bootstrap inferencial e predição marginal.

Objetivo do Projeto

O projeto tem como finalidade desenvolver um sistema robusto, interpretável e biologicamente coerente para classificação de animais aptos ao confinamento, utilizando:

indicadores produtivos;
características zootécnicas iniciais;
indicadores parasitológicos;
modelagem estatística penalizada;
inferência robusta;
estabilidade estrutural das variáveis.

O pipeline foi desenvolvido priorizando:

interpretabilidade biológica;
robustez estatística;
reprodutibilidade;
modularização;
rastreabilidade científica;
futura integração com aplicações Shiny e Precision Livestock Farming (PLF).
Estrutura do Projeto
Classificador_Confinamento/

├── app.R
├── global.R
├── renv.lock
├── README.md
├── .gitignore
│
├── data/
│   ├── dados_brutos/
│   ├── dados_processados/
│   └── modelos_salvos/
│
├── R/
│   ├── 01_preprocessamento.R
│   ├── 02_subgrupos.R
│   ├── 03_desfecho.R
│   ├── 04_elasticnet.R
│   ├── 05_stability_selection.R
│   ├── 06_firth.R
│   ├── 07_bootstrap_inferencial.R
│   ├── 08_predicao.R
│   ├── 09_roc.R
│   ├── 10_mapas.R
│   ├── 11_tabelas.R
│   ├── 12_plots.R
│   ├── utils.R
│   └── themes.R
│
├── tests/
│
├── reports/
│
└── www/
Fluxo Analítico

O pipeline segue a seguinte lógica metodológica:

Pré-processamento
        ↓
Separação por sexo
        ↓
Construção do desfecho operacional
        ↓
Elastic Net (seleção estrutural preliminar)
        ↓
Stability Selection
        ↓
Regressão Logística de Firth
        ↓
Bootstrap inferencial
        ↓
Predições marginais
        ↓
Validação ROC/AUC
        ↓
Dashboard Shiny
Módulos Implementados
01 — Pré-processamento

Responsável por:

limpeza dos dados;
padronização;
transformação de variáveis;
construção de indicadores;
tipagem adequada.
02 — Subgrupos

Separação da base em:

machos;
fêmeas.

Objetivo:
reduzir heterogeneidade biológica e aumentar robustez inferencial.

03 — Desfecho Operacional

Construção do desfecho binário:

APTO;
NÃO APTO.

Baseado em múltiplos critérios produtivos:

peso final;
peso de carcaça fria;
rendimento de carcaça;
ECC;
acabamento;
conformação.
04 — Elastic Net

Seleção estrutural preliminar de variáveis utilizando:

penalização Elastic Net;
regularização;
shrinkage.

Objetivo:
reduzir dimensionalidade e evitar overfitting.

05 — Stability Selection

Avaliação da estabilidade estrutural das variáveis via bootstrap.

Saídas:

frequência de seleção;
estabilidade;
robustez estrutural.
06 — Regressão Logística de Firth

Modelagem final utilizando regressão logística penalizada de Firth.

Adequado para:

pequenas amostras;
separação parcial;
fatores raros.

Saídas:

Odds Ratios;
ICs;
pseudo R²;
probabilidades preditas.
07 — Bootstrap Inferencial

Avaliação da robustez inferencial dos coeficientes.

Saídas:

distribuições bootstrap;
IC robustos;
estabilidade inferencial.
08 — Predição

Geração de:

probabilidades preditas;
curvas marginais;
efeitos preditos;
grades de predição.
Metodologias Estatísticas Utilizadas
Elastic Net;
Stability Selection;
Bootstrap;
Regressão Logística Penalizada de Firth;
Pseudo R² de McFadden;
Inferência robusta;
Predição marginal;
Curvas ROC/AUC.
Variáveis Utilizadas

O pipeline utiliza exclusivamente variáveis baseline (pré-confinamento), evitando data leakage.

Exemplos:

peso inicial;
ECC inicial;
idade;
grupo genético;
Famacha;
OPG;
OOPG;
indicadores parasitológicos.
Reprodutibilidade

O projeto utiliza:

renv

para gerenciamento reprodutível de dependências.

Restaurar ambiente
renv::restore()
Executando o Projeto
1. Clonar repositório
git clone https://github.com/FernandoMHaesbaert/Classificador_Confinamento.git
2. Restaurar dependências
renv::restore()
3. Executar scripts de teste

Exemplo:

source("tests/teste_06_firth.R")
Status Atual
Implementado
 Pré-processamento
 Separação por subgrupos
 Desfecho operacional
 Elastic Net
 Stability Selection
 Regressão de Firth
 Bootstrap inferencial
 Predições marginais
Em desenvolvimento
 ROC/AUC
 Dashboard Shiny
 Relatórios automáticos
 Persistência de modelos
 Interpretação visual avançada
Aplicações Futuras

O projeto poderá ser integrado a:

sistemas PLF;
dashboards Shiny;
aplicativos de classificação operacional;
sistemas embarcados;
monitoramento zootécnico inteligente.
Autor

Dr. Fernando Haesbaert
Pesquisador em Ciência de Dados na EMBRAPA Caprinos e Ovinos

Licença

Projeto desenvolvido para fins científicos, acadêmicos e de pesquisa.
