#include <type_traits>

template <typename T>
class set;

struct node_base {
	node_base *l, *r, *p;
	node_base() {
		l = r = p = nullptr;
	}
	virtual ~node_base() {}
};

template <class T>
struct node : node_base {
	T val;
	explicit node(T const & val_) : node_base(), val(val_) {}
};

template <typename V>
class it {
	friend class set<V>;
	friend class set<const V>;
public:
	 typedef V value_type;
	 typedef V &reference;
	 typedef std::ptrdiff_t difference_type;
	 typedef V *pointer;
	 typedef std::bidirectional_iterator_tag iterator_category;

	it() {}

	it(it const & other) {
		p_ = other.p_;
	}

	it& operator++() {
		if (!p_) { return *this; }
        if (p_->r) {
            p_ = p_->r;
            while (p_->l) {
                p_ = p_->l;
            }
            return *this;
        }
        auto y = p_->p;
        while(y && p_ == y->r) {
            p_ = y;
            y = y->p;
        }
        p_ = y;
        return *this;
	}

	it& operator--(	) {
		if (!p_) { return *this; }
        if (p_->l) {
            p_ = p_->l;
            while (p_->r) {
                p_ = p_->r;
            }
            return *this;
        }

        auto y = p_->p;
        while(y && p_ == y->l) {
            p_ = y;
            y = y->p;
        }
        p_ = y;
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

   	bool operator==(it const & b) const noexcept {
   		return p_ == b.p_;
   	}

   	bool operator!=(it const & b) const noexcept {
   		return p_ != b.p_;
   	}

   	V const & operator*() const {
   		return dynamic_cast<node<V>*>(p_)->val;
   	}

   	V const * operator->() const {
   		return &(dynamic_cast<node<V>*>(p_)->val);
   	};

	it(node_base* p) {
		p_ = p;
	}

	node_base* p_;
};

template <typename T>
bool operator==(typename set<T>::iterator const & a, typename set<T>::iterator const & b) {
	return a.p_ == b.p_;
}

template <typename T>
bool operator!=(typename set<T>::iterator const & a, typename set<T>::iterator const & b) {
	return a.p_ != b.p_;
}