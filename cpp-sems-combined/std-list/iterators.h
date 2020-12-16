#include <type_traits>

template<typename T1, typename T2>
struct enable_if_cast {
	typedef typename std::enable_if<std::is_same<const T1, const T2>::value, bool>::type type;
};

template <typename T>
class list;

struct node_base {
	node_base *l, *r;
	node_base() {
		l = r = this;
	}
	virtual void foo() {}
};

template <class T>
struct node : node_base {
	T val;
	node(T const & val_) : val(val_) {}
	~node() {}
};

template <typename V, int32_t step = 1, typename CAST_TO = V>
class it {
	friend class list<V>;
	friend class list<typename std::remove_const<V>::type>;
	friend class it<typename std::remove_const<V>::type, step, CAST_TO>;
	friend class it<const V, step, CAST_TO>;
public:

	 typedef V value_type;
	 typedef V &reference;
	 typedef std::ptrdiff_t difference_type;
	 typedef V *pointer;
	 typedef std::bidirectional_iterator_tag iterator_category;

	it() {
		p_ = nullptr;
	}

	it(it const & other) {
		p_ = other.p_;
	}

	it& operator++() {
		p_ = (step == 1) ? p_->r : p_->l;
		return *this;
	}

	it& operator--(	) {
		p_ = (step == 1) ? p_->l : p_->r;
		return *this;
	}

	it operator++(int) {
		it t(*this);
		++(*this);
		return t;
	}

	it operator--(int) {
		it t(*this);
		--(*this);
		return t;
	}

	template<typename X>
   	typename enable_if_cast<V, X>::type operator==(it<X, step, CAST_TO> const & b) const noexcept {
   		return p_ == b.p_;
   	}

   	template<typename X>
   	typename enable_if_cast<V, X>::type operator!=(it<X, step, CAST_TO> const & b) const noexcept {
   		return p_ != b.p_;
   	}

   	V& operator*() const {
   		return dynamic_cast<node<CAST_TO>*>(p_)->val;
   	}

   	operator it<const V, step>() const noexcept {
   		return it<const V, step>(p_);
   	};

   	V* operator->() const {
   		return &(dynamic_cast<node<CAST_TO>*>(p_)->val);
   	};

   	operator it<const V, step, CAST_TO>() {
   		return it<const V, step, CAST_TO>(p_);
   	} 

private:
	it(node_base* p) {
		p_ = p;
	}
	node_base* p_;
};

template <typename V, int32_t step = 1, typename CAST_TO, typename X>
typename enable_if_cast<V, X>::type operator==(it<V, step, CAST_TO> const & a,
	                                           it<X, step, CAST_TO> const & b) noexcept {
	return a == b;
}

template <typename V, int32_t step = 1, typename CAST_TO, typename X>
typename enable_if_cast<V, X>::type operator!=(it<V, step, CAST_TO> const & a,
								               it<X, step, CAST_TO> const & b) noexcept {
	return !(a == b);
}