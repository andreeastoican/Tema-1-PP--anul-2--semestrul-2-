Tema 1 PP

Stoican Elena-Andreea 
322CB

Enunt: http://elf.cs.pub.ro/pp/teme13/scheme-interogare

Pentru rezolvarea temei am cautat in baza de date elemete care se potrivesc cu cele din 
partea de Search folosind functia verificaPotrivire care imi intoarce o lista cu perechi 
de forma (variabila . valoare) daca se potrivesc sau #f daca nu se potrivesc.

Variabilele care sunt legate atunci cand se cauta potriviri se inlocuiesc peste tot in 
partea de Search cu elementele la care au fost legate folosind functia 
inlocuiesteVariabila.

Daca in partea de Search gasesc un predicat, aplic functia eval pentru a vedea daca
expresia este adevarata. Inainte de asta verific daca predicatul contine variabile care nu 
au fost legate, iar daca exista se arunca eroare.

La final se obtine o lista cu liste de perechi cu toate legarile posibile.
Pe aceasta lista se aplica functia map pentru a se obtine o lista doar cu legarile care se 
cer in partea de Select.
Daca in partea de Select exista cel putin o variabila care nu se gaseste in partea de Search
arunc din nou eroare.


