#include <iostream>
#include <vector>
#include <cassert>

using namespace std;

typedef long long ll;

struct diof_solution {
    ll gcd;
    ll a;
    ll b;
};

diof_solution solve_diof_no_check(ll a, ll b) {
    // a * x + b * y == 1
    cout << a << " * x + " << b << " * y == 1" << endl;
    if (b == 0) {
        cout << "b = 0, solution == {1, 0}" << endl;
        cout << "начинаем раскручивать цепочку обратно" << endl;
        cout << "к каждому решению применяем следующую формулу, чтобы получить предыдущее" << endl;
        cout << "{a, b} -> {b, a - (a / b нацело) * b}" << endl;
        return diof_solution{a, 1, 0};
    } else {
        cout << "b != 0, решаем следующее уравнение с коэффициентами " << b << " " << a % b << endl;
        diof_solution next = solve_diof_no_check(b, a % b);
        cout << "решение предыдущего уравнения " << next.b << " " << next.a - (a / b) * next.b << endl;
        return diof_solution{next.gcd, next.b, next.a - (a / b) * next.b};
    }
}

ll pow(ll a, ll b, ll n) {
    ll res = 1;
    while (b > 0) {
        if (b % 2 == 1) {
            res = res * a % n;
        }
        a = a * a % n;
        b /= 2;
    }
    return res;
}

int main() {
    // freopen("input.txt", "r", stdin);
    ios::sync_with_stdio(false);
    // assert(pow(2, 4, 100) == 16);
    // assert(pow(2, 5, 15) == 2);
    // assert(pow(1, 4, 100) == 1);
    //
    // 73 * b + 157 * k == 1
    //
    cout << "73 и 157 взаимно просты, можем решить с помощью расширенного алгоритм Евклида" << endl;
    diof_solution s = solve_diof_no_check(73, 157);
    cout << s.a << " " << s.b << endl;

    // int n, e, c;
    // cin >> n >> e >> c;
    // int p = 0, q = 0;
    // for (int i = 2; i * i  <= n; i++) {
    //     if (n % i == 0) {
    //         p = i;
    //         q = n / i;
    //         break;
    //     }
    // }
    //
    // assert(p * q == n);
    //
    // int phi = (p - 1) * (q - 1);
    // diof_solution s = solve_diof_no_check(e, phi);
    // assert(s.gcd == 1);
    // int d = (int)s.a;
    // while (d < 0) {
    //     d += phi;
    // }
    //
    // cout << pow(c, d, n) << endl;
}
