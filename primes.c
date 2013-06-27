#include <stddef.h>
#include <math.h>
#include <stdio.h>

#define NUM_PRIMES 100000
int primes[NUM_PRIMES];

char isPrime(int n)
{
    int limit = sqrt((double) n);
    int i = 0;

    for (; primes[i] < limit; ++i)
    {
        if (n % primes[i] == 0)
        {
            return 0;
        }
    }

    return 1;
}

int main(int argc, char const *argv[])
{
    primes[0] = 2;
    size_t pIndex = 1;

    int n = 3; // number to consider being prime

    while (pIndex < NUM_PRIMES)
    {
        if (isPrime(n))
        {
            primes[pIndex] = n;
            pIndex++;
            printf("%d\n", n);
        }

        n += 2; // next odd number
    }
    return 0;
}
