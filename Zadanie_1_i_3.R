# 1. Stwórz funkcję o nazwie kostka, która będzie symulować n rzutów kostką.
# Wskazówka: Użyj funkcji sample() do losowania liczby oczek od 1 do 6.
kostka <- function(n) {
  wyniki <- sample(1:6, n)
  return(wyniki)
}

# 3. Stwórz funkcję o nazwie pole_kola, która oblicza pole powierzchni koła dla danego promienia.
pole_kola <- function(r) {
  pole <- pi*r^2
  return(pole)
}