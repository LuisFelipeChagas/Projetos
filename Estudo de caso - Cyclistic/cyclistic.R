install.packages('tidyverse')
install.packages('rio')
install.packages('hms')


library('tidyverse')
library('rio')
library('hms')

# Mesclando arquivos .csv em um único arquivo. 

divvy_tripdata_year = list.files('C:\\Users\\luisf\\Desktop\\database',
                        pattern='*.csv', full.names = TRUE) %>% 
  lapply(read_csv) %>% 
  bind_rows

export(divvy_tripdata_year, file = 'C:\\Users\\luisf\\Desktop\\database\\divvy_tripdata_year.csv')

# -------------------------------------------------------------------------------------
# Não é necessário recarregar o conjunto de dados, pois ele já está no Global Environment.
bikes_data = read_csv("C:\\Users\\luisf\\Desktop\\database\\Resultados\\cyclistic_tripdata_year.csv")


glimpse(bikes_data)
summary(bikes_data)

## Remover possíveis linhas duplicadas

glimpse(distinct(bikes_data))

## Comparando as duas funções glimpse(), percebe-se que não haviam linhas duplicadas no conjuntode dados.

summary(bikes_data)

## Adcionando coluna duração da viagem (ended_at - started_at). 

bikes_data["trip_time"] <- as_hms(bikes_data$ended_at - bikes_data$started_at)

glimpse(bikes_data)
summary(bikes_data)

## Criando coluna dia da semana

bikes_data["Week_day_start"] <- weekdays(bikes_data$started_at)

glimpse(bikes_data)
summary(bikes_data)

## Buscando valores NA no conjunto de dados

n_row <-nrow(bikes_data)
round(colSums(is.na(bikes_data)*100/n_row), 2)

# Após o processamento dos dados, salvarei o conjunto de dados tratado em um outro arquivo .csv

export(bikes_data, file = 'C:\\Users\\luisf\\Desktop\\database\\Resultados\\Cyclistic_tripdata_year_clear.csv')

## Análise descritiva ----------------------------------------------------------

# Não é necessário recarregar o conjunto de dados, pois ele já está no Global Environment.
bikes_data = read_csv("C:\\Users\\luisf\\Desktop\\database\\Resultados\\Cyclistic_tripdata_year_clear.csv")

# Média, máximo e mínimo da duração dos passeios:
descriptive_analysis = bikes_data %>% 
  summarize(average_trip_time = as_hms(mean(trip_time)),
            max_trip = as_hms(max(trip_time)),
            min_trip = as_hms(min(trip_time)))

## Aqui foi visto uma incoerência, com valores de tempo de passeio negativos

inconsistencies <- bikes_data %>% 
  filter(trip_time < 0)

## São 146 observações, que apresentam tempos negativos de passeio. Esses valores serão filtrados da análise.

# Excluindo as linhas com tempos negativos:
bikes_data <- bikes_data %>% 
  filter(trip_time > 0)

# Recalculando média, max e min após a filtragem

descriptive_analysis = bikes_data %>% 
  summarize(average_trip_time = as_hms(mean(trip_time)),
            max_trip = as_hms(max(trip_time)),
            min_trip = as_hms(min(trip_time)))

# Observando passeios com menos de 1 minuto:

one_min_trips <- bikes_data %>% 
  filter(trip_time < 1*60)

n_row_one <- nrow(one_min_trips)
one_per_total <- round(((n_row_one/n_row)*100), 2)


## Filtrando também os passeios com menos de um minuto (possível erro no cojunto de dados):

bikes_data <- bikes_data %>% 
  filter(trip_time > 1*60)

# Recalculando média, max e min após a filtragem

descriptive_analysis = bikes_data %>% 
  summarize(average_trip_time = as_hms(mean(trip_time)),
            max_trip = as_hms(max(trip_time)),
            min_trip = as_hms(min(trip_time)))

# Dúvida com relação ao tempo limite dos passeios. Pode existir tempos longos (acima de 5 horas)? 

long_trip_time <- bikes_data %>% 
  filter(trip_time > 5*60*60)

n_row <- nrow(bikes_data)
n_row_long <- nrow(long_trip_time)
per_total <- round(((n_row_long/n_row)*100), 2)

# Apenas 0.22% dos passeios possuem mais de 5 horas. Na análise, essas viagens não serão filtradas.

# Dias da semana com mais viagens:
week_day_count <- bikes_data %>% 
  group_by(Week_day_start) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count))
  
## Fazer as mesmas análises considerando os subgrupos membro e casual
 
## Membros -------------------------------------------------------------------

bikes_data_members <- bikes_data %>% 
  filter(member_casual == 'member')

## Média, máximo e mínimo da duração dos passeios - membros:

descriptive_analysis_members = bikes_data_members %>% 
  summarize(average_trip_time = as_hms(mean(trip_time)),
            max_trip = as_hms(max(trip_time)),
            min_trip = as_hms(min(trip_time)))

## Dias da semana com mais viagens - membros:

week_day_count_members <- bikes_data_members %>% 
  group_by(Week_day_start) %>% 
  summarize(count = n()) %>% 
  arrange(desc(count))

## Os membros utilizam mais as bicicletas durante a semana (provavelmente à trabalho)

## Casuais

bikes_data_casual <- bikes_data %>% 
  filter(member_casual == 'casual')

## Média, máximo e mínimo da duração dos passeios - casuais:

descriptive_analysis_casual = bikes_data_casual %>% 
  summarize(average_trip_time = as_hms(mean(trip_time)),
            max_trip = as_hms(max(trip_time)),
            min_trip = as_hms(min(trip_time)))

## Dias da semana com mais viagens - casuais:

week_day_count_casual <- bikes_data_casual %>% 
  group_by(Week_day_start) %>% 
  summarize(count = n()) %>% 
  arrange(desc(count))

# os passageiros casuais utilizam mais as bicicletas nos finais de semana.

# Insight: focar nos passageiros casuais que utilizam a bicicleta durante a semana.

## Visualização: -----------------------------------------------------

# O conjunto de dados será resumido (eliminar colunas que não influenciam nas análises feitas), para o processo de visualização

# Selecionando apenas as colunas importantes para a análise feita

bikes_data_viz <- bikes_data %>% 
  select(started_at, ended_at, member_casual, trip_time)

# Salvando o conjunto de dados, para ser utilizado no Power BI
export(bikes_data_viz, file = 'C:\\Users\\luisf\\Desktop\\database\\bikes_data_viz.csv')
