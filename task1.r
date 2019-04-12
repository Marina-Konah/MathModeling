
# для региона 68 рассчитайте урожайность пшеницы в 2006 году, взяв для рассчета средние суммы 
# активных температур за текущий год, с 10 ближайших метеостанций но убирая из рассчета активных 
# температур дни с температурой выше 25 градусов
# Регион 68 - Тамбов с координатами 52.721246,  41.452238

#Библиотеки:
library(tidyverse)
library(rnoaa)
library(lubridate)

# Создадим векторы с данными для расчета:
af = c(0.00,0.00,0.00,32.11, 26.31,25.64,23.20,18.73,16.30,13.83,0.00,0.00)
bf = c(0.00, 0.00, 0.00, 11.30, 9.26, 9.03,8.16, 6.59, 5.73, 4.87, 0.00, 0.00)
df = c(0.00,0.00, 0.00, 0.33, 1.00, 1.00, 1.00, 0.32, 0.00, 0.00, 0.00, 0.00)
Kf = 300 #  Коэффициент использования ФАР
Qj = 1600 # калорийность урожая культуры
Lj = 2.2 #  сумма частей основной и побочной продукции
Ej = 25 #   стандартная влажность культуры

# Загрузим данные о метеостанциях:
#station_data = ghcnd_stations()
# Сохраним в файл "station_data.сsv".
#write.csv(station_data, "station_data.csv")

# Сохраним данные в вектор для дальнейшей работы:
station_data = read.csv("station_data.csv")
# Зададим название вектора и координаты столицы региона:
tambov = data.frame(id = "tambov", latitude = 52.721246,  longitude = 41.452238)
# Выполним поиск 2 ближайших метеостанций с данными по среднемесячной температуре год по 10 стани
#   и сохраним в вектор.
tambov_around = meteo_nearby_stations(lat_lon_df = tambov, station_data = station_data,
                                        limit = 10, var = "TAVG",
                                        year_min = 2018, year_max = 2018)

# Создадим таблицу, в которую будем сохранять данные со всех станций:
all_data = tibble()

for (i in 1:10)
{
  tambov_id = tambov_around[["tambov"]][["id"]][i]
  # Загрузим данные для станции:
  data = meteo_tidy_ghcnd(stationid = tambov_id,
                          var="TAVG",
                          date_min="2018-01-01",
                          date_max="2018-12-31")
}
data = mutate(data, test = tavg>=50 & tavg<=250)
data = drop_na(data, tavg)

for (i in 1:10)
{  
    all_data = bind_rows(all_data, data %>%
                         mutate(year = year(date), month = month(date)) %>%
                         group_by(month, year) %>%
                         summarise (tavg = sum(tavg[test==TRUE])/10))
}


# Изменения в таблице сохранятся в векторе clean_data.
clean_data = all_data %>%
  # Добавим колонку month для группировки данных:
  group_by(month) %>%
  # Найдем месячный d и cумму активных тмператур для каждой станции:
  summarise(s = mean(tavg, na.rm = TRUE)) %>%
  # Добавим колонки для расчета:
  mutate (a = af, b = bf, d = df) %>%
  # Рассчитаем урожайность для каждого месяца:
  mutate (fert = ((a + b * 1.0 * s) * d * Kf) / (Qj * Lj * (100-Ej)) )
#Согласно расчету, урожайность пшеницы в Саратовской области в 2006 году составила (ц/га):
Yield = sum(clean_data$fert); Yield
