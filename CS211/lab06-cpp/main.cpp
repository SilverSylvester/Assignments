#include <iostream>
#include <cmath>
#include <unordered_map>
#include <map>
#include <climits>
#include <time.h>

typedef unsigned long long u64;

u64 modpow(u64 b, u64 e, u64 m) {
    b %= m;
    u64 r = 1;
    while (e > 0) {
        if (e & 1)
            r = (r * b) % m;
        b = (b*b) % m;
        e >>= 1;
    }
    return r;
}

u64 baby_steps_giant_steps(u64 p, u64 g, u64 gxp) {
    u64 s = ceil(sqrtl(p));
    u64 counter = 0;

    std::unordered_map<u64, int> a(s);
    for (u64 i = 0; i < s; i++)
        a.insert({ gxp * modpow(g, i, p) % p, i });

    for (u64 i = 1; i < s + 1; i++) {
        u64 val = modpow(g, i*s, p);
        std::unordered_map<u64, int>::iterator it = a.find(val);
        if (it != a.end())
            return (i*s - a.at(val));
    }
    return 0;
}

u64 brute_force(u64 p, u64 g, u64 gxp) {
    u64 x = 1;
    // Iterate until x overflows
    while (x++) {
        if (modpow(g,x,p) == gxp)
            return x;
    }
}

u64 decrypt(u64 p, u64 pk, u64 c1, u64 c2) {
    return modpow(c1, p - pk - 1, p) * c2 % p;
}

int main(int argc, char **argv) {
    u64 p = 24852977;
    u64 g = 2744;
    u64 gxp = 8414508;
    u64 c1 = 15268076;
    u64 c2 = 743675;

    std::cout << "\nBaby-step-giant-step algorithm:\n";
    
    clock_t st, tt;

    st = clock();
    u64 pk = baby_steps_giant_steps(p, g, gxp);
    tt = clock() - st;

    double bsgs_time = tt * 1000 / CLOCKS_PER_SEC;

    std::cout << "Private key:  " << pk << std::endl;
    std::cout << "Message:      " << decrypt(p, pk, c1, c2) << std::endl;
    printf("Time elapsed: %.3f s\n", bsgs_time / 1000);

    std::cout << "\nBrute force algorithm:\n";
    
    st = clock();
    pk = brute_force(p, g, gxp);
    tt = clock() - st;

    double bf_time = tt * 1000 / CLOCKS_PER_SEC;

    std::cout << "Private key:  " << pk << std::endl;
    std::cout << "Message:      " << decrypt(p, pk, c1, c2) << std::endl;
    printf("Time elapsed: %.3f s\n", bf_time / 1000);

    // There's an excellent chance this fails due to overflowing
    // multiplication (can't just modulo the result unfortunately)
    // When I have time to convert this to using arbitrary precision
    // integers it'll work fine. Uncomment if you want to see it working.

    /*
    p = 83187685431865799;
    g = 231541186;
    gxp = 58121575766172118;
    c1 = 15714167638989179;
    c2 = 1416052582726447;

    pk = baby_steps_giant_steps(p, g, gxp);

    std::cout << pk << std::endl;
    std::cout << decrypt(p, pk, c1, c2);
    */
}

