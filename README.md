# Funkcija za simulaciju 20 slučajnih šetnji, izračun frekvencija pozicija i prikaz dijagrama pozicija koje smo dobili simulacijom
simulacija_slucajne_setnje <- function(koraci, broj_setnji) {
  # Pohrana konačnih pozicija svih šetnji
    final_pozicija <- numeric(broj_setnji)	
  # Simulacija svih šetnji
  for (setnja in 1:broj_setnji) {
    pozicija <- 0
    pozicije <- c(pozicija)
    for (i in 1:koraci) {
      ishod <- sample(c(0, 1), 1)
      korak <- ifelse(ishod == 1, 1, -1)
      pozicija <- pozicija + korak
      pozicije <- c(pozicije, pozicija)
    }
    final_pozicija[setnja] <- pozicija
  }
  
  # Izračun frekvencija pozicija
  pozicije_brojac <- table(final_pozicija)
  
  return(list(final_pozicija = final_pozicija, pozicije_brojac = pozicije_brojac))
}

# Broj koraka u svakoj simulaciji
broj_koraka <- 8

# Broj šetnji koje želimo simulirati, recimo da u razredu imamo 20 učenika
broj_setnji <- 20

# Simuliraj sve slučajne šetnje i izračunaj frekvencije pozicija
simulacije <- simulacija_slucajne_setnje(broj_koraka, broj_setnji)

# Ispis pozicija za svaku šetnju
print(simulacije$final_pozicija)

# Ispis frekvencija pozicija
print(simulacije$pozicije_brojac)

# Dijagram pozicija
barplot(simulacije$pozicije_brojac, xlab = "Pozicija", ylab = "Frekvencija", main = "Dijagram pozicija")
