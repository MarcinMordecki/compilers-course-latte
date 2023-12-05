# Użyte frameworki/języki:
Kompilator dla latte (na razie tylko Frontend) powstał przy użyciu tego stacka technologicznego, co interpreter na JPP, czyli Haskell + BNFC + Alex + Happy.
# Struktura projektu:
W korzeniu projektu znajduje się ten plik README oraz Makefile.
W folderze src znajdują się: 
- plik latte.cf z gramatyką BNFC języka Latte, dostarczony wraz z treścią zadania;
- pliki Main.hs i Frontend.hs, zawierające implementacje frontendu kompilatora;
# Kompilacja projektu:
Wywołanie polecenia make powoduje pojawienie się plików:
- w korzeniu projektu - latc, który przyjmuje przy wywołaniu ścieżkę do pliku, który ma skompilować (na razie tylko parsowanie + frontendowe checki).
- w folderze src - AbsLatte.hs, LexLatte.hs oraz ParLatte.hs, czyli składni AST, Lexera oraz Parsera składni Latte, wygenerowanych dzięki BNFC, Alex i Happy.
- w folderze src - tymczasowego folderu build, zawierającego pośrednie pliki, z których korzysta tworzony program.

Wywołanie polecenia make clean powoduje usunięcie wszystkich plików powstałych w wyniku kompilacji projektu.
