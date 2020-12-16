#ifndef LIST_H
#define LIST_H

#include <iostream>
#include <cassert>
#include <cstdlib>
#include <cstring>
#include <new>
#include <type_traits>
#include <utility>
#include "iterators.h"

template <class T> 
struct list {
	typedef it<T, 1, T> iterator;
    typedef it<const T, 1, T> const_iterator;
    typedef it<T, -1, T> reverse_iterator;
    typedef it<const T, -1, T> const_reverse_iterator;

	list() noexcept;
	list(list<T> const &);
	list& operator=(list<T> const &);

	T& front();
	T& back();
	T const & front() const;
	T const & back() const;

	void push_front(T const &);
	void push_back(T const &);
	void push_before(node_base*, T const &);
	void pop_front() noexcept;
	void pop_back() noexcept;

	iterator begin();
	iterator end();
	const_iterator begin() const;
	const_iterator end() const;

	reverse_iterator rbegin();
	reverse_iterator rend();
	const_reverse_iterator rbegin() const;
	const_reverse_iterator rend() const;

	bool empty() const noexcept;
	void clear() noexcept;

	void copy_whole(list<T> const &);
	void custom_erase(node_base*) noexcept; 
	void swap(list<T> &);
	void print_list() const;

	~list() noexcept;

	iterator insert(const_iterator, T const &);
	iterator erase(const_iterator) noexcept;
	iterator erase(const_iterator, const_iterator) noexcept;
	void splice(const_iterator pos, list& other, const_iterator first, const_iterator last);

	node_base* fake_addr;
	node_base fake;
};

template <typename T>
void list<T>::clear() noexcept {
	node_base* cur = fake.r;
	while (cur != fake_addr) {
		node_base* next = cur->r;
		delete (dynamic_cast<node<T>*>(cur));
		cur = next;
	}
	fake.l = fake.r = &fake;
}

template <typename T>
void list<T>::push_before(node_base * p, T const & val) {
	node<T>* new_node = new node<T>(val);

	new_node->l = p->l;
	new_node->r = p;

	p->l->r = dynamic_cast<node_base*>(new_node);
	p->l    = dynamic_cast<node_base*>(new_node);
}

template <typename T>
void list<T>::print_list() const {
	const_iterator it = begin();
	const_iterator end_ = end();
	while (it != end_) {
		std::cout << *it << " ";
		++it;
	}
	std::cout << std::endl;
}

template <typename T>
void list<T>::copy_whole(list<T> const & v) {
	node_base* now = dynamic_cast<node_base*>(v.fake_addr->r);
	while (now != v.fake_addr) {
		push_before(fake_addr, dynamic_cast<node<T>*>(now)->val);
		now = now->r;
	}
}

template <typename T>
list<T>::list() noexcept {
	fake_addr = &fake;
	fake.l = fake.r = fake_addr;
}

template <typename T>
list<T>::list(list<T> const & v) :list() {
	copy_whole(v);
}

template <typename T>
list<T> & list<T>::operator=(list<T> const & v) {
	if (&v == this) {
		return *this;
	}
	clear();
	copy_whole(v);
	return *this;
}

template <typename T>
list<T>::~list() noexcept {
	clear();
}

template <typename T>
bool list<T>::empty() const noexcept {
	return (fake.l == &fake && fake.r == &fake);
}

template <typename T>
T& list<T>::front() {
	return dynamic_cast<node<T>*>(fake.r)->val;
}

template <typename T>
T& list<T>::back() {
	return dynamic_cast<node<T>*>(fake.l)->val;
}

template <typename T>
T const & list<T>::front() const {
	return dynamic_cast<node<T>*>(fake.r)->val;
}

template <typename T>
T const & list<T>::back() const {
	return dynamic_cast<node<T>*>(fake.l)->val;
}

template <typename T>
void list<T>::push_front(T const & val) {
	push_before(fake.r, val);
}

template <typename T>
void list<T>::push_back(T const & val) {
	push_before(fake_addr, val);
}

template <typename T>
void list<T>::custom_erase(node_base * p) noexcept {	
	p->l->r = p->r;
	p->r->l = p->l;
	delete dynamic_cast<node<T> *>(p);
}

template <typename T>
void list<T>::pop_front() noexcept {
	custom_erase(fake.r);
}

template <typename T>
void list<T>::pop_back() noexcept {
	custom_erase(fake.l);
}

template <typename T>
typename list<T>::iterator list<T>::begin() {
	return iterator(fake_addr->r);
}

template <typename T>
typename list<T>::iterator list<T>::end() {
	return iterator(fake_addr);
}

template <typename T>
typename list<T>::const_iterator list<T>::begin() const {
	return const_iterator(fake_addr->r);
}

template <typename T>
typename list<T>::const_iterator list<T>::end() const {
	return const_iterator(fake_addr);
}

template <typename T>
typename list<T>::reverse_iterator list<T>::rbegin() {
	return reverse_iterator(fake_addr->l);
}

template <typename T>
typename list<T>::reverse_iterator list<T>::rend() {
	return reverse_iterator(fake_addr);
}

template <typename T>
typename list<T>::const_reverse_iterator list<T>::rbegin() const {
	return const_reverse_iterator(fake.l);
}

template <typename T>
typename list<T>::const_reverse_iterator list<T>::rend() const {
	return const_reverse_iterator(fake_addr);
}

template <typename T>
typename list<T>::iterator list<T>::insert(list<T>::const_iterator pos, T const & val) {
	node_base* p = pos.p_;
	push_before(p, val);
	return list<T>::iterator(p->l);
}

template <typename T>
typename list<T>::iterator list<T>::erase(list<T>::const_iterator pos) noexcept {
	node_base* prev = pos.p_->l;
	custom_erase(pos.p_);
	return list<T>::iterator(prev->r);
}

template <typename T>
typename list<T>::iterator list<T>::erase(list<T>::const_iterator left,
									      list<T>::const_iterator right) noexcept {
	node_base* prev = left.p_->l;
	while (left != right) {
		node_base* tmp = left.p_->r;
		custom_erase(left.p_);
		left = list<T>::const_iterator(tmp);
	}
	return list<T>::iterator(prev->r);
}

template <typename T>
void list<T>::splice(list<T>::const_iterator pos, list<T> & other,
	list<T>::const_iterator first, list<T>::const_iterator last) {
	if (first == last) {
		return;
	}

	node_base* pl = first.p_->l;
	node_base* pr = last.p_;

	node_base* ld = pos.p_;
	node_base* fd = ld->l;

	node_base* fs = first.p_;
	node_base* ls = last.p_->l;

	fd->r = fs;
	fs->l = fd;

	ld->l = ls;
	ls->r = ld;

	pl->r = pr;
	pr->l = pl;
}

template <typename T>
void list<T>::swap(list<T> & b) {
	list<T> l(*this);
	*this = b;
	b = l;
}

template <typename T>
void swap(list<T> & a, list<T> & b) {
	a.swap(b);
}

#endif // LIST_H