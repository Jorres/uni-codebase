#pragma once
#include <iterator>
#include <cassert>
#include "iterators.h"

template<typename T>
struct my_set {
    size_t _size;
    node root;

public:
    typedef set_iterator<const T> iterator;
    typedef iterator const_iterator;
    typedef std::reverse_iterator<iterator> reverse_iterator;
    typedef std::reverse_iterator<const_iterator> const_reverse_iterator;

    std::pair<iterator, bool> insert_(T const &x, node *curr) {
        while (curr) {
            const T val = static_cast<node_t<T>*>(curr)->val;
            if (!curr->left && x < val) {
                curr->left = new node_t<T>(x);
                curr->left->parent = curr;
                return {iterator(curr->left), true};
            }
            if (!curr->right && val < x) {
                curr->right = new node_t<T>(x);
                curr->right->parent = curr;
                return {iterator(curr->right), true};
            }
            if(x < val) curr = curr->left;
            if(val < x) curr = curr->right;
            if(x == val) {
                return {iterator(curr), false};
            }
        }
        return {end(), false};
    }

    const_iterator find_(T const &x, node *curr) const {
        while(curr) {
            T const &val = static_cast<node_t<T> *>(curr)->val;
            if (val < x) curr = curr->right;
            if (val > x) curr = curr->left;
            if (val == x) {
                return iterator(curr);
            }
        }
        return end();
    }

    const_iterator lower_bound_(T const &x, node *curr) const {
        while (curr) {
            T const &val = static_cast<node_t<T> *>(curr)->val;
            if(val < x) {
                if (!curr->right) {
                    auto tmp = const_iterator(curr);
                    return ++tmp;
                }
                curr = curr->right;
            } else {
                if (!curr->left) {
                    return curr;
                }
                curr = curr->left;
            }
        }
        return end();
    }

    const_iterator upper_bound_(T const &x, node *curr) const {
        while (curr) {
            T const &val = static_cast<node_t<T> *>(curr)->val;
            if(x < val) {
                if (!curr->left) {
                    return curr;
                }
                curr = curr->left;
            } else {
                if (!curr->right) {
                    auto tmp = const_iterator(curr);
                    return ++tmp;
                }
                curr = curr->right;
            }
        }
        return end();
    }

    const_iterator erase_(const_iterator place) {
        const_iterator res(place);
        res++;
        if (place == end()) return end();
        node* v = place.p;
        if (!v->left && !v->right) {
           if (v->parent->left == v)
               v->parent->left = nullptr;
           else
               v->parent->right = nullptr;
        } else if (!v->left) {
            if (v->parent->left == v) {
                v->parent->left = v->right;
            } else {
                v->parent->right = v->right;
            }
            v->right->parent = v->parent;
        } else if (!v->right) {
            if (v->parent->left == v) {
                v->parent->left = v->left;
            } else {
                v->parent->right = v->left;
            }
            v->left->parent = v->parent;
        } else {
            iterator cur(v);
            cur++;
            node* next = cur.p;
            if (next->parent->left == next) {
                next->parent->left = next->right;
                if (next->right)
                    next->right->parent = next->parent;
            } else {
                next->parent->right = next->right;
                if (next->right)
                    next->right->parent = next->parent;
            }
            if (v->left) {
                next->left = v->left;
                v->left->parent = next;
            }
            if (v->right) {
               next->right = v->right;
               v->right->parent = next;
            }
            next->parent = v->parent;
            if (v->parent->left == v)
                v->parent->left = next;
            else
                v->parent->right = next;
        }
        delete v;
        return res;
    }


public:

    my_set() noexcept : _size(0), root() { }

    my_set(my_set const &other) : _size(0) {
        try {
            for (iterator it = other.begin(); it != other.end(); it++) {
                insert(*it);
            }
        } catch (...) {
            clear();
            throw;
        }
    }

    ~my_set() {
        clear();
    }

    my_set &operator=(my_set other) {
        swap(other, *this);
        return *this;
    }

    iterator begin() {
        node *curr = &root;
        while (curr->left) {
            curr = curr->left;
        }
        return curr;
    }

    const_iterator begin() const {
        const node *curr = &root;
        while (curr->left) {
            curr = curr->left;
        }
        return const_cast<node *const>(curr);
    }

    iterator end() {
        return iterator(&root);
    }

    const_iterator end() const {
        return const_iterator(const_cast<node*>(&root));
    }

    reverse_iterator rbegin() {
        return reverse_iterator(end());
    }

    const_reverse_iterator rbegin() const {
        return const_reverse_iterator(end());
    }

    reverse_iterator rend() {
        return reverse_iterator(begin());
    }

    const_reverse_iterator rend() const {
        return const_reverse_iterator(begin());
    }

    std::pair<iterator, bool> insert(T const &x) {
        if (empty()) {
            root.left = new node_t<T>(x);
            root.left->parent = &root;
            ++_size;
            return {root.left, true};
        }
        auto tmp = insert_(x, root.left);
        _size += tmp.second;
        return tmp;
    }

    const_iterator find(T const &x) const {
        return find_(x, root.left);
    }

    iterator erase(const_iterator it) {
        --_size;
        return erase_(it);
    }

    const_iterator lower_bound(T const &x) const {
        if (empty()) return end();
        return lower_bound_(x, root.left);
    }

    const_iterator upper_bound(T const &x) const {
        if (empty()) return end();
        return upper_bound_(x, root.left);
    }

    bool empty() const {
        return root.left == nullptr;
    }

    size_t size() const {
        return _size;
    }

    void clear() {
        iterator it = begin();
        while (it != end()) {
            it = erase(it);
        }
    }

    friend void swap(my_set<T> &a, my_set<T> &b) {
        std::swap(a.root, b.root);
        if (b.root.left) {
            b.root.left->parent = &b.root;
        }
        if (a.root.left) {
            a.root.left->parent = &a.root;
        }
        std::swap(a._size, b._size);
    }
};
