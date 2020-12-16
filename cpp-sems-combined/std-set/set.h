#pragma once
#include <iterator>
#include <cassert>

template<typename T>
struct set {

private:
#include "iterators.h"

public:
    typedef it<const T> iterator;
    typedef it<const T> const_iterator;
    typedef std::reverse_iterator<iterator> reverse_iterator;
    typedef std::reverse_iterator<iterator> const_reverse_iterator;

    std::pair<iterator, bool> custom_insert(T const &other, node_base *vert) {
        while (vert) {
            T const & val = dynamic_cast<node*>(vert)->val;
            if (vert->left == nullptr && other < val) {
                vert->left = new node(other);
                vert->left->parent = vert;
                return std::make_pair(iterator(vert->left), true);
                // depr assert(other < val);
            } else if (vert->right == nullptr && val < other) {
                vert->right = new node(other);
                vert->right->parent = vert;
                return std::make_pair(iterator(vert->right), true);
            } else if (val < other) { 
                vert = vert->right;
            } else if (other < val) { 
                vert = vert->left;
            } else {
                assert(true);
                return std::make_pair(iterator(vert), false);
            }
        }
        return std::make_pair(end(), false);
    }
    iterator custom_lower_bound(T const &other, node_base *vert) const {
        while (vert) {
            T const &val = dynamic_cast<node *>(vert)->val;
            if(val > other && vert->left == nullptr) {
                return vert;
            } else if (vert->right == nullptr) {
                auto tmp = const_iterator(vert);
                tmp++;
                return tmp;
            }
            vert = (val > other) ? vert->left : vert->right;
        }
        return end();
    }

    iterator custom_upper_bound(T const &other, node_base *vert) const {
    
    }

    iterator erase(iterator place) {
        iterator res(place);
        res++;
        if (place == end()) {
            return end();
        }
        node_base* v = place.p;
        if (!v->left && !v->right) {
           if (v->parent->left == v) {
               v->parent->left = nullptr;
           } else {
               v->parent->right = nullptr;
           }
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
            node_base* next = (++iterator(v)).p;
            if (next->parent->left == next) {
                next->parent->left = next->right;
                if (next->right) {
                    next->right->parent = next->parent;
                }
            } else {
                next->parent->right = next->right;
                if (next->right) {
                    next->right->parent = next->parent;
                }
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
            if (v->parent->left == v) {
                v->parent->left = next;
            } else {
                v->parent->right = next;
            }
        }
        delete v;
        return res;
    }


public:

    set() noexcept: root() {}

    set(set const& other) {
        try {
            for (iterator it = other.begin(); it != other.end(); it++) {
                insert(*it);
            }
        } catch (...) {
            clear();
            throw;
        }
    }

    ~set() {
        clear();
    }

    set &operator=(set other) {
        swap(*this, other);
        return *this;
    }

    iterator begin() const {
        node_base const *vert = &root;
        while (vert->left) {
            vert = vert->left;
        }
        return const_cast<node_base *const>(vert);
    }

    iterator end() const {
        return iterator(const_cast<node_base*>(&root));
    }

    const_reverse_iterator rbegin() const {
        return const_reverse_iterator(end());
    }

    const_reverse_iterator rend() const {
        return const_reverse_iterator(begin());
    }

    std::pair<iterator, bool> insert(T const &other) {
        if (empty()) {
            root.left = new node(other);
            root.left->parent = &root;
            return {root.left, true};
        }
        auto tmp = custom_insert(other, root.left);
        return tmp;
    }

    iterator find(T const &other) const {
        node_base* vert = root.left;
        while(vert != nullptr) {
            T const &val = dynamic_cast<node *>(vert)->val;
            if (val > other) {
                vert = vert->left;
            } else if (val < other) { 
                vert = vert->right;
            } else {
                return iterator(vert);
            }
        }
        return end();
    }

    iterator lower_bound(T const &other) const {
        if (empty()) return end();
        node_base* vert = root.left;
        while (vert) {
            T const &val = dynamic_cast<node *>(vert)->val;
            if(val < other) {
                if (!vert->right) {
                    auto tmp = const_iterator(vert);
                    tmp++;
                    return tmp;
                }
                vert = vert->right;
            } else {
                if (!vert->left) {
                    return vert;
                }
                vert = vert->left;
            }
        }
        return end();
    }

    iterator upper_bound(T const &other) const {
        if (empty()) {
            return end();
        }
        node_base* vert = root.left;
        while (vert) {
            T const &val = dynamic_cast<node *>(vert)->val;
            if(other < val) {
                if (!vert->left) {
                    return vert;
                }
                vert = vert->left;
            } else {
                if (!vert->right) {
                    auto tmp = iterator(vert);
                    return ++tmp;
                }
                vert = vert->right;
            }
        }
        return end();
    }

    bool empty() const noexcept {
        return root.left == nullptr;
    }

    void clear() noexcept {
        for (iterator it = begin(); it != end(); it = erase(it)) {}
    }

    friend void swap(set<T> &a, set<T> &b) {
        std::swap(a.root, b.root);
        if (b.root.left) {
            b.root.left->parent = &b.root;
        }
        if (a.root.left) {
            a.root.left->parent = &a.root;
        }
    }
};
