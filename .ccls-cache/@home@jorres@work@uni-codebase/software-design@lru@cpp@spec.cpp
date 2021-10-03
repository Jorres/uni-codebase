#include <gtest/gtest.h>
#include "lru.h"

// Demonstrate some basic assertions.
TEST(LRUTest, PutSingleInt) {
    TLru<int> lru = TLru<int>(1);
    int key = 123;
    lru.put(key);
    EXPECT_TRUE(lru.present(key));
}

// Demonstrate some basic assertions.
TEST(LRUTest, BasicAssertions2) {
  // Expect two strings not to be equal.
  EXPECT_STRNE("hello", "world");
  // Expect equality.
  EXPECT_EQ(7 * 6, 42);
}
/* #include <iostream>

#include "lru.h"

void testInsertOneElement() {

}

int main() {
    TLru<int> lru = TLru<int>(100);
    lru.display();
} */
