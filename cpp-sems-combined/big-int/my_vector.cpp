#ifndef VECTOR_CPP
#define VECTOR_CPP

#include <vector>
#include <memory>
#include <cstring>
#include <algorithm>

typedef int32_t dig;
typedef uint32_t udig;

const int FB_LEN = 16;

template<typename T>
class vector {
public:
    using sh_ptr = std::shared_ptr<std::vector<T>>;

    vector() {
    	is_short = true;
    	for (int i = 0; i < FB_LEN; i++) {
    		data[i] = 0;
    	}
    	sz = 0;
    }

    vector(vector const &other) {
        is_short = other.is_short;
        sz = other.sz;
        if (is_short) {
        	memcpy(data, other.data, sz * sizeof(udig));
        } else {
            long_data = other.long_data;
        }
    }

    vector<T> & operator=(vector<T> const & other) {
    	sz = other.sz;
    	is_short = other.is_short;
    	if (is_short) {
            memcpy(data, other.data, sz * sizeof(udig));
		} else {
			long_data = other.long_data;
		}

		return *this;
	}

    T & operator[] (dig pos) {
		if (is_short) {
			return data[pos];
		} else {
			actuate();
			return (*long_data.get())[pos];
		}
	}

	T const & operator[] (dig pos) const {
		if (is_short) {
			return data[pos];
		} else {
			return (*(long_data.get()))[pos];
		}
	}

    size_t size() const {
        if (is_short) {
        	return sz;
        } else { 
        	return long_data.get()->size();
        }
    }

    void pop_back() {
    	if (is_short) {
    		sz--;
    	} else {
    		actuate();	
    		if (long_data.get()->size() == FB_LEN + 1) {
            	is_short = true;
            	for (dig i = 0; i < FB_LEN; i++) {
            		data[i] = (*long_data.get())[i];
            	}
            	sz = FB_LEN;
	        } else {
	            long_data.get()->pop_back();
	        }
    	}
    }

    void push_back(T elem) {
    	if (is_short && sz < FB_LEN) {
    		data[sz++] = elem;
    	} else if (is_short && sz == FB_LEN) {
            is_short = false;
            long_data = std::make_shared<std::vector<T>>(FB_LEN);
            //memcpy((*long_data.get()).data, data, FB_LEN * sizeof(udig));
            for (dig i = 0; i < FB_LEN; i++) {
            	(*long_data.get())[i] = data[i];
            }
            long_data.get()->push_back(elem);
        } else {
            actuate();
        	long_data.get()->push_back(elem);
        }
    }

    void assign(size_t req_sz, T val) {
    	if (req_sz > FB_LEN) {
    		long_data = std::make_shared<std::vector<T>>(req_sz, val);
		} else {
			sz = req_sz;
			for (dig i = 0; i < sz; i++) {
				data[i] = val;
			}
		}
		is_short = req_sz <= FB_LEN;
    }

    bool operator== (vector const & other) const {
    	if ((is_short && !other.is_short) || (!is_short && other.is_short)) {
    		return false;
    	}

    	if (is_short) {
    		if (sz != other.sz) {
    			return false;
    		}
    		for (dig i = 0; i < sz; i++) {
    			if (data[i] != other.data[i]) {
    				return false;
    			}
    		}
    		return true;
    	} else {
    		return (*long_data.get()) == (*other.long_data.get());
    	}
    }

    T & back() {
    	if (is_short) {
    		return data[sz - 1];
    	} else {
    		actuate();
    		return long_data.get()->back();
    	}
    }

    T const & back() const {
    	if (is_short) {
    		return data[sz - 1];
    	} else {
    		return long_data.get()->back();
    	}
    }

    void resize(dig len) {
    	if (len <= FB_LEN) {
    		if (is_short && sz < len) {
    			for (dig i = sz; i < len; i++) {
    				data[i] = 0;
    			}
    		}
    		is_short = true;
    		sz = len;
    	} else {
    		if (!is_short) {
    			long_data.get()->resize(len);
    		} else {
    			is_short = false;
    			long_data = std::make_shared<std::vector<T>>(len, 0);
    			for (dig i = 0; i < FB_LEN; i++) {
    				(*long_data.get())[i] = data[i];
    			}
    		}
    	}
    }

private:
    bool is_short;
    sh_ptr long_data;
    T data[FB_LEN];
    dig sz;

    void actuate() {
        if (!long_data.unique()) {
            long_data = std::make_shared<std::vector<T>>(*long_data.get());
        }
    }
};

#endif //VECTOR_CPP
