#include <numeric>

#include <gtest/gtest.h>

#include "lru.h"


// Demonstrate some basic assertions.
TEST(LRUTest, CheckSingleIntNoPreemption) {
    TLru<int> lru = TLru<int>(1);
    int key = 123;
    lru.put(key);
    EXPECT_TRUE(lru.present(key));
}

// Demonstrate some basic assertions.
TEST(LRUTest, CheckMultipleIntsNoPreemption) {
    TLru<int> lru = TLru<int>(1);
    std::vector<int> keys(10);

    std::iota(keys.begin(), keys.end(), 0);

    for (const auto& key : keys) {
        std::cout << key << std::endl;
        lru.put(key);
    }

    // for (const auto& key : keys) {
        EXPECT_TRUE(lru.present(6)) << "wrong key " << 6 << endl;
    // }
}

/* #include <iostream>

#include "lru.h"

void testInsertOneElement() {

}

int main() {
    TLru<int> lru = TLru<int>(100);
    lru.display();
} */
