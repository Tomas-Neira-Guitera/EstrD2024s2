#include <iostream>
using namespace std;

void printN(int n, string s){
    for (n > 0; n--;)
    {
        cout << s; 
    }
}

void cuentaRegresiva(int n){
    for (n > 0; n--;)
    {
        cout << n << endl; 
    }
}

void desdeCeroHastaN(int n){
    for (int i = 0; i < n; i++)
    {
        cout << i << endl; 
    }
}

int mult(int n, int m){
    if (n == 0)
    {
        return 0;
    }
    for ((n-1) > 0; n--;)
    {
        m = m + m;
    }
    return m;
}

void primerosN(int n, string s){
    for (int i = 0; n > i; i++)
    {
        cout << s[i] << endl;
    }
}

bool pertenece(char c, string s){
    for (char ch : s)
    {
        if (ch == c)
        {
            return true;
        }
    }
    return false;
}

int apariciones(char c, string s){
    int a = 0;
    for (char ch : s)
    {
        if (ch == c)
        {
            a++;
        }
    }
    return a;
}





