#include <iostream>
#include <vector>

using namespace std;

typedef int ll;

long solve(ll a, ll b, ll n, ll A, ll B, ll N) {

}

void format(ll a, ll b, ll n, ll& A, ll& B, ll& N) {
    A = a;
    B = b;
    N = n;
}

int main() {
    freopen("input.txt", "r", stdin);
    
    ll a, b, n;

    // x : a ^ x = b 
    //           n
    
    cin >> a >> b >> n;
    ll n_a, n_b, n_n;
    format(a, b, n, n_a, n_b, n_n);

    solve(a, b, n, n_a, n_b, n_n);
}
