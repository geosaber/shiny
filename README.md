# Aplicativo de Geoestatística Aplicada

## Descrição
Aplicativo Shiny para análise geoestatística utilizando o dataset Meuse do pacote sp.

## Funcionalidades
1. **Dados e Mapa**: Visualização do dataset Meuse
2. **Análise Exploratória**: Estatísticas, histogramas, testes de normalidade
3. **Interpolação IDW**: Interpolação por distância inversa ponderada
4. **Autocorrelação**: Análise espacial (Moran, LISA)
5. **Variografia**: Variogramas omnidirecionais, direcionais e agrupados
6. **Krigagem**: Ordinária, universal e por indicadora

## Requisitos
- R 4.0+
- Pacotes listados no `manifest.json`

## Deploy no Posit Connect
1. Certifique-se de que todos os arquivos estão no diretório
2. Faça upload do diretório completo
3. O Posit Connect instalará automaticamente as dependências

## Estrutura de Arquivos
- `app.R`: Código principal do aplicativo
- `manifest.json`: Metadados e dependências
- `README.md`: Esta documentação
