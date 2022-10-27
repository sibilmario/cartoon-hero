#--------------------------------------------------------------------------------------
# 2 savaitė įskaitinė užduotis
# --------------------------------------------------------------------------------------

# Užduotis:
# 1) pasikraukite reikiamus paketus,
# 2) susitvarkykite lokalę,
# 3) įsikelkite nurodytus duomenis į R,
# 4) atlikite jų techninę apžvalgą,
# 5) atlikite nurodytas duomenų manipuliacijas

# PASTABA: parašykite kodą kaip radote atsakymą, o ne tik atsakymą.
# t.y. atsakyme turi būti "length(vektorius)", o ne komandos rezultatas pvz., "45".
# Atsakymą galima ir pakomentuoti žodžiu, bet privalo būti ir komandos, kuriomis jūs remiatės rašydami savo atsakymą.

# Pildykite dokumentą nuo skilties -- "Analizės kodas"
# --------------------------------------------------------------------------------------

# Apie duomenis

# Dirbsime su duomenimis "zuvu_kaulines_ploksteles.csv"
# Duomenis sudaro Trispyglės Dyglės žuvies (https://en.wikipedia.org/wiki/Three-spined_stickleback) matavimai.
# Matuotas kaulinių plokštelių skaičius ir nustatytas žuvies genotipas pagal vieną geną, Ectodysplasin.
# Taip pat pateikiamas kiekvienos žuvies unikalus identifikacinis numeris.

# Trispyglė Dyglė turi tris kaulinius dyglius keteroje, du dyglius pilvo srityje ir šonines kaulines plokšteles.
# Šie kauliniai dariniai ("šarvai") apsaugo žuvis nuo plėšrių žuvų ir paukščių.
# Gėluose vandenyse šios žuvys turi mažiau kaulinių darinių, nei sūriuose vandenyje.
# Colosimo et.al. (2004) nustatė, kad kaulinių darinių skaičiui daro didelę įtaka vienas genas -- Ectodysplasin.
# Buvo sukryžmintos sūriuose vandenyse ir gėluose vandenyse gyvenusios Trispyglės Dyglės.
# F2 kartoje gaute individų, kurie turėjo MM, Mm ir mm genotipus.
# M variantas yra iš sūriuose vandenyse gyvenusių tėvų, o m geno variantas -- iš gėlavandenių.

# --------------------------------------------------------------------------------------
# Analizės kodas
# --------------------------------------------------------------------------------------
# 1) Reikiami paketai:
library(base)
library(dplyr)
# 2) Lokalė:
Sys.setlocale(locale = "Lithuanian")
# 3) Duomenų įsikėlimas:
zuvu_kaulines_ploksteles <- read.csv("D:/downloads/2_savaite_iskaitine_uzduotis/2_savaite_iskaitine_uzduotis/zuvu_kaulines_ploksteles.csv")
View(zuvu_kaulines_ploksteles)

# 4) Atlikite duomenų techninę apžiūrą:

# * Kokia tai duomenų truktūra?
class(zuvu_kaulines_ploksteles)
## [1] "data.frame"
# * Kokio dydžio ši struktūra?
dim(zuvu_kaulines_ploksteles)
## [1] 344   3
nrow(zuvu_kaulines_ploksteles)
## [1] 344
ncol(zuvu_kaulines_ploksteles)
## [1] 3
# * kaip ji atrodo?
head(zuvu_kaulines_ploksteles)
##     id kaulines_ploksteles zuvu_genotipas
## 1  4-1                  11             mm
## 2  4-2                  63             Mm
## 3  4-4                  22             Mm
## 4  4-5                  10             Mm
## 5 4-10                  14             mm
## 6 4-12                  11             mm
tail(zuvu_kaulines_ploksteles)
##        id kaulines_ploksteles zuvu_genotipas
## 339 4-367                  60             Mm
## 340 4-368                   8             mm
## 341 4-369                  62             MM
## 342 4-371                  14             mm
## 343 4-372                  60             Mm
## 344 4-373                  11             mm
# * ar ji tvarkinga?
# taip.
dplyr::glimpse(zuvu_kaulines_ploksteles)
## Rows: 344
## Columns: 3
## $ id                  <chr> "4-1", "4-2", "4-4", "4-5", "4-10…
## $ kaulines_ploksteles <int> 11, 63, 22, 10, 14, 11, 58, 36, 3…
## $ zuvu_genotipas      <chr> "mm", "Mm", "Mm", "Mm", "mm", "mm…

# 5) Atlikite nurodytas duomenų manipuliacijas:
# a) atspausdinkite pateiktos lentelės eilutes nuo 60 iki 75
zuvu_kaulines_ploksteles[60:75, ]
##       id kaulines_ploksteles zuvu_genotipas
## 60 4-155                  64             MM
## 61 4-158                  64             MM
## 62 4-160                  62             Mm
## 63 4-161                  63             MM
## 64 4-162                  30             Mm
## 65 4-164                  13             mm
## 66 4-168                  10             mm
## 67 4-170                  49             MM
## 68 4-171                  62             Mm
## 69 4-172                  43             Mm
## 70 4-173                  66             MM
## 71 4-175                  61             MM
## 72 4-177                  26             Mm
## 73 4-178                  63             MM
## 74 4-180                  59             Mm
## 75 4-183                  66             MM
# b) apskaičiuokite vidutinį kaulinių plokštelių skaičių
mean(zuvu_kaulines_ploksteles$kaulines_ploksteles)
## ats.:[1] 43.43314
# c) apskaičiuokite kiek žuvų turėjo specifinį genotipą
# (tai apskaičiuoti reiktų išsitraukti zuvu_genotipas stulpelį kaip vektorių ir panaudoti funkciją `table()`)
table(zuvu_kaulines_ploksteles$zuvu_genotipas)
## mm  Mm  MM
## 88 174  82
# Kokio genotipo žuvų yra daugiausia/mažiausia?
## Mm - daugiausia, mm - mažiausia
# d) iš pateiktos lentelės atsirinkite tik eilutes, kurių zuvu_genotipas stulpelio vertės yra mm -- išsaugokite šią mažesnę lentelė į naują kintamąjį pavadinimu "zuvys_mm"
zuvys_mm <- filter(zuvu_kaulines_ploksteles, zuvu_genotipas == "mm")
# e) apskaičiuokite naujos lentelės "zuvys_mm" kaulinių plokštelių vidurkį.
mean(zuvys_mm$kaulines_ploksteles)
## [1] 11.67045
# ar gautas kaulinių plokštelių skaičiaus vidurkis yra didesnis ar mažesnis už
# bendrą kaulinių plokštelių vidurkį (neskirstant pagal genotipą).
43.43314>11.67045
# f) zuvys_mm lentelėje sukurkite naują stulpelį (pavadinimu "skirtumas"), kuriame būtų užrašytas kiekvienos žuvies kaulinių plokštelių skaičiaus ir plokštelių skaičiaus vidurkio skirtumas
zuvys_mm <- mutate(zuvys_mm, skirtumas = kaulines_ploksteles - 43.43314)
