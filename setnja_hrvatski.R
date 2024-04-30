#Ovdje simuliramo 10000 šetnji uz pomoć bacanja novčića i računamo relativnu frekvenciju pozicije 0
# Funkcija za izračun relativne frekvencije da će osoba biti na određenoj poziciji
rel_frekvencija_pozicije <- function(koraci, ciljana_pozicija, broj_simulacija) {
  # Inicijalizacija brojača za odabranu poziciju
  brojac <- 0
  
  # Simulacija šetnji
  for (s in 1:broj_simulacija) {
    pozicija <- 0
    for (i in 1:koraci) {
      ishod <- sample(c(0, 1), 1)
      korak <- ifelse(ishod == 1, 1, -1)
      pozicija <- pozicija + korak
    }
    if (pozicija == ciljana_pozicija) {
      brojac <- brojac + 1
    }
  }
  
  # relativna frekvencija
  rel_frekvencija <- brojac / broj_simulacija
  
  return(rel_frekvencija)
}

# Broj koraka u simulaciji
broj_koraka <- 8

# Odabrana pozicija
ciljana_pozicija <- 0

# Broj simulacija
broj_simulacija <- 10000

# Izračun relativne frekvencije da će osoba biti na odabranoj poziciji
ciljana_pozicija_frekvencija <- rel_frekvencija_pozicije(broj_koraka, ciljana_pozicija, broj_simulacija)
