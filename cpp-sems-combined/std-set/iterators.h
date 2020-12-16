#pragma once

class node_base {
public:
    node_base *left;
    node_base *right;
    node_base *parent;
    node_base() : left(nullptr), right(nullptr), parent(nullptr) { }
    virtual ~node_base() {};
};

struct node : node_base {
    T val;
    node(T const &x) : node_base(), val(x) { }
};

node_base root;
template<typename U>
struct it;

template<typename U>
struct it {
    typedef U value_type;
    typedef U &reference;
    typedef std::ptrdiff_t difference_type;
    typedef U *pointer;
    typedef std::bidirectional_iterator_tag iterator_category;

    friend class set;
    it() {}
    it &operator--() {
        if (!p) { 
            return *this;
        }
        if (p->left) {
            p = p->left;
            while (p->right) {
                p = p->right;
            }
            return *this;
        }

        auto other = p->parent;
        while(other && p == other->left) {
            p = other;
            other = other->parent;
        }
        p = other;
        return *this;
    }

    it operator--(int) {
        it tmp = *this;
        --(*this);
        return tmp;
    }

    it &operator++() {
        if (!p) {
            return *this;
        }
        if (p->right != nullptr) {
            p = p->right;
            while (p->left != nullptr) {
                p = p->left;
            }
            return *this;
        }
        auto other = p->parent;
        while(other && p == other->right) {
            p = other;
            other = other->parent;
        }
        p = other;
        return *this;
    }

    it operator++(int) {
        it tmp = *this;
        ++(*this);
        return tmp;
    }

    friend bool operator==(it const &a, it const &b) {
        return a.p == b.p;
    }

    friend bool operator!=(it const &a, it const &b) {
        return !(a == b);
    }

    U const &operator*() const {
        return dynamic_cast<node *>(p)->val;
    }

    U const *operator->() const {
        return &(dynamic_cast<node * > (p)->val);
    }

private:
    it(node_base *ptr) : p(ptr) {}
    node_base *p;
};
