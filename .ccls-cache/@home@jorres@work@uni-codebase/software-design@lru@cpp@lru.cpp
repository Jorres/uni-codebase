#include <iostream>
#include <unordered_map>
#include <vector>
#include <list>

using namespace std;

template <typename K>
class TLru {
public:
    TLru(size_t size): max_size(size) { }

    void put(K key) {
        auto res = links.find(key);
        if (res == links.end()) {
            if (keys.size() == max_size) {
                auto toDelete = keys.first();
                links.erase(toDelete);
                keys.pop_front();
            } 
        } else {
            keys.erase(links[key]);
        }
        keys.push_back(key);
        links[key] = keys.end();
    }

    void display() {
        cout << "Displaying cache content:" << endl;
        for (const auto& t : keys) {
            cout << t << endl;
        }
    }

    bool present(K key) {
        return links.find(key) != links.end();
    }

private:
    list<K> keys;
    unordered_map<K, typename list<K>::iterator> links;

    size_t max_size;
};

int main() {
    TLru<int> lru = TLru<int>(100);
    lru.display();
}
