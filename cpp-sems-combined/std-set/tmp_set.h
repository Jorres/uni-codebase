#ifndef SET_H
#define SET_H

#include <iostream>
#include <cassert>
#include <cstdlib>
#include <cstring>
#include <new>
#include <type_traits>
#include <utility>
#include <algorithm>
#include "iterators.h"

template <class T> 
struct set {
	typedef it<const T> iterator;
	typedef iterator const_iterator;

	typedef std::reverse_iterator<iterator> reverse_iterator;
    typedef std::reverse_iterator<iterator> const_reverse_iterator;

	set() noexcept;
	set(set<T> const &);
	set& operator=(set<T>);

	iterator begin() const;
	iterator end() const;

	reverse_iterator rbegin() const;
	reverse_iterator rend() const;

	bool empty() const noexcept;
	void clear() noexcept;

	~set() noexcept;

	void copy_whole(set<T> const &);

	iterator find(T const &) const;
	iterator lower_bound(T const &) const;
	iterator upper_bound(T const &) const;

	std::pair<iterator, bool> insert(T const &);
	iterator erase(iterator) noexcept;

	friend void swap(set<T> &a, set<T> &b) {
	    std::swap(a.root, b.root);
	    if (b.root.l) {
	        b.root.l->p = &b.root;
	    }
	    if (a.root.l) {
	        a.root.l->p = &a.root;
	    }
	}

private:
	std::pair<iterator, bool> my_insert(T const &, node_base *);
	iterator my_find(T const &, node_base *) const;
	iterator my_lower_bound(T const &, node_base *) const;
	iterator my_upper_bound(T const &, node_base *) const;
	iterator my_erase(iterator) noexcept;

	node_base root; 
};

template <typename T>
void set<T>::copy_whole(set<T> const & v) {
	for (typename set<T>::iterator it = v.begin(); it != v.end(); it++) {
		insert(*it);
	}
}

template <typename T>
set<T>::set() noexcept : root() {}

template <typename T>
set<T>::set(set<T> const & v) : set() {
	try {
        for (typename set<T>::iterator it = v.begin(); it != v.end(); it++) {
            insert(*it);
        }
    } catch (...) {
        clear();
        throw;
    }
}

template <typename T>
set<T> & set<T>::operator=(set<T> v) {
	swap(*this, v);
	return *this;
}

template <typename T>
set<T>::~set() noexcept {
	clear();
}

template <typename T>
bool set<T>::empty() const noexcept {
	return root.l == nullptr;
}

template <typename T>
typename set<T>::const_iterator set<T>::begin() const {
    const node_base* curr = &root;
    while (curr->l) {
        curr = curr->l;
    }
    return const_cast<node_base* const>(curr);
}

template <typename T>
typename set<T>::const_iterator set<T>::end() const {
    return typename set<T>::iterator(const_cast<node_base*>(&root));
}

template <typename T>
typename set<T>::reverse_iterator set<T>::rbegin() const {
    return typename set<T>::reverse_iterator(end());
}


template <typename T>
typename set<T>::reverse_iterator set<T>::rend() const {
    return typename set<T>::reverse_iterator(begin());
}

template <typename T>
typename set<T>::iterator set<T>::my_find(T const & ins, node_base* curv)  const {
	while(curv) {
        T const &val = dynamic_cast<node<T> *>(curv)->val;
        if (val < ins) { 
        	curv = curv->r;
        }
        if (val > ins) { 
        	curv = curv->l;
        }
        if (val == ins) {
            return iterator(curv);
        }
    }
    return end();
}

template <typename T>
std::pair<typename set<T>::iterator, bool> set<T>::my_insert(T const &x, node_base *curv) {
    while (curv != nullptr) {
        const T val = dynamic_cast<node<T> *>(curv)->val;
        if (!curv->l && x < val) {
            curv->l = new node<T>(x);
            curv->l->p = curv;
            return std::make_pair(iterator(curv->l), true);
        }
        if (!curv->r && val < x) {
            curv->r = new node<T>(x);
            curv->r->p = curv;
            return std::make_pair(iterator(curv->r), true);
        }
        if(x < val) { 
        	curv = curv->l;
        } else if(val < x) {
        	curv = curv->r;
        } else if(x == val) {
            return std::make_pair(iterator(curv), false);
        }
    }
    return std::make_pair(end(), false);
}

template <typename T>
typename set<T>::iterator set<T>::my_lower_bound(T const &x, node_base *curv) const {
    while (curv != nullptr) {
        T const &val = dynamic_cast<node<T>*>(curv)->val;
        if(val < x) {
            if (!curv->r) {
                auto tmp = set<T>::const_iterator(curv);
                return ++tmp;
            }
            curv = curv->r;
        } else {
            if (!curv->l) {
                return curv;
            }
            curv = curv->l;
        }
    }
    return end();
}

template <typename T>
typename set<T>::const_iterator set<T>::my_upper_bound(T const &x, node_base *curv) const {
    while (curv) {
        T const &val = dynamic_cast<node<T> *>(curv)->val;
        if(x < val) {
            if (!curv->l) {
                return curv;
            }
            curv = curv->l;
        } else {
            if (!curv->r) {
                auto tmp = set<T>::const_iterator(curv);
                return ++tmp;
            }
            curv = curv->r;
        }
    }
    return end();
}

template <typename T>
typename set<T>::iterator set<T>::my_erase(typename set<T>::iterator place) noexcept {
    typename set<T>::iterator res(place);
    res++;
    if (place == end()) {
    	return end();
    }
    node_base* v = place.p_;
    if (!v->l && !v->r) {
       if (v->p->l == v)
           v->p->l = nullptr;
       else
           v->p->r = nullptr;
    } else if (!v->l) {
        if (v->p->l == v) {
            v->p->l = v->r;
        } else {
            v->p->r = v->r;
        }
        v->r->p = v->p;
    } else if (!v->r) {
        if (v->p->l == v) {
            v->p->l = v->l;
        } else {
            v->p->r = v->l;
        }
        v->l->p = v->p;
    } else {
        typename set<T>::iterator cur(v);
        cur++;
        node_base* next = cur.p_;
        if (next->p->l == next) {
            next->p->l = next->r;
            if (next->r)
                next->r->p = next->p;
        } else {
            next->p->r = next->r;
            if (next->r)
                next->r->p = next->p;
        }
        if (v->l) {
            next->l = v->l;
            v->l->p = next;
        }
        if (v->r) {
           next->r = v->r;
           v->r->p = next;
        }
        next->p = v->p;
        if (v->p->l == v)
            v->p->l = next;
        else
            v->p->r = next;
    }
    delete v;
    return res;
}

template <typename T>
std::pair<typename set<T>::iterator, bool> set<T>::insert(T const &x) {
    if (empty()) {
        root.l = new node<T>(x);
        root.l->p = &root;
        return std::make_pair(root.l, true)	;
    }
    auto tmp = my_insert(x, root.l);
    return tmp;
}

template <typename T>
typename set<T>::iterator set<T>::find(T const &x) const {
    return my_find(x, root.l);
}

template <typename T>
typename set<T>::iterator set<T>::erase(typename set<T>::iterator it) noexcept {
    return my_erase(it);
}

template <typename T>
typename set<T>::iterator set<T>::lower_bound(T const &x) const {
    if (empty()) { 
    	return end(); 
    }
    return my_lower_bound(x, root.l);
}

template <typename T>
typename set<T>::iterator set<T>::upper_bound(T const &x) const {
    if (empty()) { 
    	return end();
    }
    return my_upper_bound(x, root.l);
}

template <typename T>
void set<T>::clear() noexcept {
    typename set<T>::iterator it = begin();
    while (it != end()) {
        it = erase(it);
    }
}


#endif // SET_H