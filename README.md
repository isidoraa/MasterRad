Sistemi za preporucivanje:

data_prep.R - Unos podataka i priprema za obradu.

evaluation.R - Funkcije za evaluaciju rezultata.

content_based.R - Sistem za preporucivanje na osnovu sadrzaja. Koristi se kosinusna razdaljina kao mera slicnosti.

user-based-knn.R - Sistem za preporucivanje na osnovu saradnje (preciznije, na osnovu okoline korisnika). Koristi se metoda k najblizih suseda za izbor okoline i Pirsonov koeficijent korelacije kao mera slicnosti.

hybrid-logistic.R - Hibridni sistem za preporucivanje koji koristi model logisticke regresije formiran na osnovu podataka o korisnicima (pol, godine, zanimanje), zanrova filmova i ocena koje su korisnici davali.

Korisceni su podaci MovieLens 1M Dataset koji mogu da se skinu odavde: https://grouplens.org/datasets/movielens/
