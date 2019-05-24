#include <stdio.h>
#include <stdbool.h>

bool esPrimo(int n) {
    if (n <= 1) return false;
    if (n == 2) return true;
    for (int i = 2; i < n; i++)
        if (n % i == 0)
            return false;

    return true;
}

int main()
{
    int cantidadPrimos = 15;

    int contador = 0;
    int primo = 1;
    while (contador < cantidadPrimos) {
        if (esPrimo(primo)) {
            printf("%d\n", primo);
            contador++;
        }
        primo++;
    }

    return 0;
}
