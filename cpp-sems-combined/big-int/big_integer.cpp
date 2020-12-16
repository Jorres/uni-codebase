#include "big_integer.h"
#include "my_vector.cpp"
#include <algorithm>
#include <iostream>
#include <string>
#include <vector>
#include <cassert>

typedef uint32_t udig;
typedef uint64_t lludig;
typedef int32_t dig;

const dig  starting_capacity = 1;
const udig first_bit = (static_cast<udig>(1) << 31);
const udig max_uns = static_cast<udig>(-1);
const udig bitsize = 32;

big_integer::big_integer() {
    data.assign(starting_capacity, 0);
}

big_integer::big_integer(dig a) {
    new (this) big_integer();
    data[0] = static_cast<udig>(a);
    if (a != 0 && a != -1) {
        data.push_back(a < 0 ? max_uns : 0);
    }
}

big_integer::big_integer(udig a) {
    new (this) big_integer();
    data[0] = a;
    if (a != 0) {
        data.push_back(0);
    }
}
big_integer::big_integer(big_integer const& other) {
    data = other.data;
}
big_integer::big_integer(std::string const& s) {
    udig base = 10;
    new (this) big_integer();
    if (s.size() == 0) {
        return;
    }

    bool cast = false;
    size_t st = 0;
    if (s[st] == '-') {
        cast = true;
        st++;
    }

    for (size_t i = st; i < s.size(); i++) {
        *this *= big_integer(base);
        *this += big_integer(static_cast<dig>(s[i] - '0'));
    }

    if (cast) {
        custom_negate();
    }
}

udig big_integer::get_on(size_t pos) const {
    if (pos < data.size()) {
        return data[pos];
    } else {
        return get_neutral();
    }
}

big_integer& big_integer::operator=(big_integer const& other) {
    data = other.data;
    return *this;
}
    
bool big_integer::has_overflow(udig a, udig b) {
    return (max_uns - a) < b;
}

big_integer& big_integer::operator+=(big_integer const& other) {
    if (other.data.size() < data.size() && !(other.data.back() & first_bit)) {
        data.push_back(data.back());
        udig carry = 0;
        for (size_t i = 0; i < other.data.size(); i++) {
            bool fo = has_overflow(data[i], other.data[i]) ||
                      has_overflow(data[i] + other.data[i], carry);

            udig tu = data[i] + other.data[i] + carry;
            carry = (fo ? 1 : 0);
            data[i] = tu;
        }        

        if (carry == 1) {
            add_with_carry(1, other.data.size());
        }
    } else {
        expand(std::max(other.data.size(), data.size()) + 1);
    
        udig carry = 0;
        for (size_t i = 0; i < data.size(); i++) {
            bool fo = has_overflow(data[i], other.get_on(i)) ||
                      has_overflow(data[i] + other.get_on(i), carry);

            udig tu = data[i] + other.get_on(i) + carry;

            carry = (fo ? 1 : 0);
            data[i] = tu;
        }        
    }
    simplify();
    return *this;
}
big_integer operator+(big_integer a, big_integer const& b) {
    a += b;
    return a;
}

big_integer& big_integer::operator-=(big_integer const& other) {
    big_integer tmp = other;
    tmp.custom_negate();
    return *this += tmp;
}
big_integer operator-(big_integer a, big_integer const& b) {
    a -= b;
    return a;
}

void big_integer::add_with_carry(udig carry, dig pos) {
    for (size_t i = pos; i < data.size() && carry > 0; i++) {
        bool o = has_overflow(data[i], carry);

        data[i] += carry;
        carry = (o ? 1 : 0);
    }               
}

big_integer& big_integer::operator*=(big_integer const& other) {
    dig sign = 0;
    big_integer s = other;

    if (*this < 0) {
        sign++;
        custom_negate();
    }
    if (s < 0) {
        sign++;
        s.custom_negate();
    }

    big_integer result;
    result.expand(data.size() + s.data.size() + 1);

    for (size_t i = 0; i < data.size(); i++) {
        if (data[i] == 0) {
            continue;
        }
        for (size_t j = 0; j < s.data.size(); j++) {
            if (s.data[j] == 0) {
                continue;
            }

            lludig t = static_cast<lludig>(data[i]) *
                static_cast<lludig>(s.data[j]);

            result.add_with_carry(static_cast<udig>(t), i + j);
            result.add_with_carry(static_cast<udig>(t >> bitsize), i + j + 1);
        }
    }

    if (sign == 1) {
        result.custom_negate();
    }
    result.simplify();

    return *this = result;
}
big_integer operator*(big_integer a, big_integer const& b) {
    a *= b;
    return a;
}

big_integer& big_integer::operator/=(big_integer const& other) {
    big_integer a = *this, b = other;
    dig sign = 0;
    if (a < 0) {
        a.custom_negate();
        sign++;
    }
    if (b < 0) {
        b.custom_negate();
        sign++;
    }

    udig h = b.data[b.data.size() - 2];
    dig hlp_shift = 0;

    while (h < max_uns / static_cast<udig>(2)) {
        h *= 2;
        hlp_shift++;
    }

    a <<= hlp_shift;
    b <<= hlp_shift;

    dig fa = a.data.size() - 2, fb = b.data.size() - 2;

    if (fb > fa) {
        return *this = 0;
    }
    dig m = fa - fb;

    big_integer result;
    result.expand(static_cast<size_t>(m + 2));  

    big_integer rem = (a >> ((m + 1) * bitsize));
    for (dig j = m; j >= 0; j--) {
        rem <<= bitsize;
        rem += a.data[j];
        big_integer t_rem = rem >> (fb * bitsize);

        lludig tl = t_rem.data[0];
        lludig th = (t_rem.data.size() > 1 ? t_rem.data[1] : 0);
        lludig t = (th << bitsize) + tl;
        udig pdig = static_cast<udig>(t / b.data[fb]);

        big_integer my_lesser = b;
        big_integer minusb = b;
        minusb.custom_negate();

        my_lesser *= pdig;

        while (rem < my_lesser) {
            my_lesser += minusb;
            pdig--;
        }

        rem -= my_lesser;
        result.data[j] = pdig;
    }

    if (sign == 1) {
        result.custom_negate();
    }

    result.simplify();
    return *this = result;
}
big_integer operator/ (big_integer a, big_integer const& b) {
    a /= b;
    return a;
}

big_integer& big_integer::operator%=(big_integer const& other) {
    big_integer t = *this;
    t /= other;
    *this -= (t * other);
    return *this;
}
big_integer operator% (big_integer a, big_integer const& b) {
    a %= b;
    return a;
}

big_integer& big_integer::operator++ () {
    add_with_carry(1, 0);
    return *this;
}
big_integer& big_integer::operator-- () {
    return *this -= 1;
}

bool operator==(big_integer const& a, big_integer const& b) {
    return a.data == b.data;
}
bool operator!=(big_integer const& a, big_integer const& b) {
    return !(a == b);
}
bool operator<(big_integer const& a, big_integer const& b) {
    dig as = (a.get_data().back() & first_bit), bs = (b.get_data().back() & first_bit);
    if (as != bs) {
        return as;
    }

    for (dig i = a.data.size() - 1; i >= 0; i--) {
        if (a.get_on(i) < b.get_on(i)) {
            return true; 
        } else if (a.get_on(i) > b.get_on(i)) {
            return false;
        }
    }

    return false;
}
bool operator>(big_integer const& a, big_integer const& b) {
    return !(a == b || a < b);
}
bool operator>=(big_integer const& a, big_integer const& b) {
    return !(a < b);
}
bool operator<=(big_integer const& a, big_integer const& b) {
    return !(a > b);
}

big_integer big_integer::operator+() const {
    big_integer t = *this;
    return t;
}
big_integer big_integer::operator-() const {
    big_integer t = *this;
    t.custom_negate();
    return t;
}

big_integer& big_integer::operator&=(big_integer const& a) {
    expand(a.data.size());
    for (size_t i = 0; i < data.size(); i++) {
        udig elem = a.get_on(i);
        data[i] &= elem;
    }
    simplify();
    return *this;
}
big_integer operator&(big_integer a, big_integer const& b) {
    a &= b;
    return a;
}
big_integer& big_integer::operator|=(big_integer const& a) {
    expand(a.data.size());
    for (size_t i = 0; i < data.size(); i++) {
        udig elem = a.get_on(i);
        data[i] |= elem;
    }
    simplify();
    return *this;
}
big_integer operator|(big_integer a, big_integer const& b) {
    a |= b;
    return a;
}
big_integer& big_integer::operator^=(big_integer const& a) {
    expand(a.data.size());
    for (size_t i = 0; i < data.size(); i++) {
        udig elem = a.get_on(i);
        data[i] ^= elem;
    }
    simplify();
    return *this;
}
big_integer operator^(big_integer a, big_integer const& b) {
    a ^= b;
    return a;
}
big_integer big_integer::operator~() const {
    big_integer t = *this;
    for (size_t i = 0; i < t.data.size(); i++) {
        t.data[i] = ~t.data[i];
    }
    return t;
}

void big_integer::shift_big_left(dig am) {
    for (dig i = 0; i < am; i++) {
        data.push_back(0);
    } 
    for (dig i = data.size() - 1; i >= am; i--) {
        data[i] = data[i - am];
    }
    for (dig i = am - 1; i >= 0; i--) {
        data[i] = 0;
    }
}
void big_integer::shift_big_right(dig am) {
    for (size_t i = 0; i < data.size(); i++) {
        if (i + am < data.size()) {
            data[i] = data[i + am];
        } else {
            data[i] = get_neutral();
        }
    }
}
big_integer& big_integer::operator<<= (dig sh) {
    if (sh < 0) {
        return (*this) >>= -sh;
    }

    dig am = sh / bitsize;

    shift_big_left(am);

    sh %= bitsize;

    if (sh == 0) {
        return *this;
    }

    data.push_back(get_neutral());

    for (dig i = data.size() - 1; i >= 0; i--) {
        data[i] <<= sh; 
        if (i > 0) {
            udig next = (data[i - 1] >> (bitsize - sh));
            data[i] |= next;
        }
    }
    simplify();
    return *this;
}
big_integer operator<< (big_integer a, dig t) {
    a <<= t;
    return a;
}
big_integer& big_integer::operator>>= (dig sh) {
    if (sh < 0) {
        return (*this) <<= -sh;
    }

    dig am = sh / bitsize;
    shift_big_right(am);
    sh %= bitsize;

    if (sh == 0) {
        return *this;
    }

    for (size_t i = 0; i < data.size(); i++) {
        udig elem = (i + 1 < data.size()) ? data[i + 1] : get_neutral();
        data[i] >>= sh;
        elem <<= (bitsize - sh);
        data[i] |= elem;
    }
    simplify();
    return *this;
}
big_integer operator>> (big_integer a, dig t) {
    a >>= t;
    return a;
}

udig big_integer::get_neutral() const {
    return (data.back() & first_bit) ? max_uns : 0;
}
vector<udig> const& big_integer::get_data() const {
    return data;
}

void big_integer::expand(size_t desired) {
    if (data.size() < desired) {
        udig last = data.back();
        size_t ps = data.size();
        data.resize(desired);
        if (last == 0) {
            return;
        }
        for (size_t i = ps; i < data.size(); i++) {
            data[i] = last;
        }
    }
}

void big_integer::simplify() {
    while (data.size() > 1 && data.back() == data[data.size() - 2]) {
        data.pop_back();
    }
}
void big_integer::custom_negate() {
    data.push_back(data.back());
    for (size_t i = 0; i < data.size(); i++) {
        data[i] = ~data[i];
    }

    add_with_carry(1, 0);
    simplify();
}

std::ostream& operator<<(std::ostream& s, big_integer const& a) {
    s << to_string(a);
    return s;
}
std::string to_string(big_integer const& a) {
    dig radix = 1000000000;
    std::string s = "";
    big_integer t = a;

    bool minus = false;
    if (t < 0) {
        t.custom_negate();
        minus = true;
    }

    do {
        dig smalldig = (t % radix).get_data()[0];
        dig req_len = 9;

        do {
            s.push_back(static_cast<int>(smalldig % 10) + '0');
            smalldig /= 10;
            req_len--;
        } while (smalldig > 0);

        for (dig i = 0; i < req_len; i++) {
            s.push_back('0');
        }

        t /= radix;
        
    } while (t > 0);

    while (s.size() > 1 && s[s.size() - 1] == '0') {
        s.pop_back();
    }

    if (minus) {
        s.push_back('-');
    }

    std::reverse(s.begin(), s.end());
    return s;
}

big_integer::~big_integer() {

}