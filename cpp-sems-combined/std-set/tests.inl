#include <gtest/gtest.h>
#include <sstream>

#include "fault_injection.h"

template <typename T>
T const& as_const(T& obj)
{
    return obj;
}

template <typename C, typename T>
void mass_insert(C& c, std::initializer_list<T> elems)
{
    for (T const& e : elems)
        c.insert(e);
}

template <typename It, typename T>
void expect_eq(It i1, It e1, std::initializer_list<T> elems)
{
    std::vector<typename std::remove_const<typename std::iterator_traits<It>::value_type>::type> vals;

    for (; i1 != e1; ++i1)
        vals.push_back(*i1);

    if (!std::equal(vals.begin(), vals.end(), elems.begin(), elems.end()))
    {
        std::stringstream ss;
        ss << '{';

        bool add_comma = false;
        for (auto const& e : vals)
        {
            if (add_comma)
                ss << ", ";
            ss << e;
            add_comma = true;
        }

        ss << "} != {";

        add_comma = false;
        for (auto const& e : elems)
        {
            if (add_comma)
                ss << ", ";
            ss << e;
            add_comma = true;
        }

        ss << "}";

        ADD_FAILURE() << ss.str();
    }
}

template <typename C, typename T>
void expect_eq(C const& c, std::initializer_list<T> elems)
{
    expect_eq(c.begin(), c.end(), elems);
}

template <typename C, typename T>
void expect_reverse_eq(C const& c, std::initializer_list<T> elems)
{
    expect_eq(c.rbegin(), c.rend(), elems);
}

TEST(correctness, single_element)
{
    counted::no_new_instances_guard g;

    container c;
    c.insert(42);
}

TEST(correctness, insert)
{
    counted::no_new_instances_guard g;

    container c;
    mass_insert(c, {1, 2, 3, 4});   
    expect_eq(c, {1, 2, 3, 4});
}

TEST(correctness, copy_ctor)
{
    counted::no_new_instances_guard g;

    container c;
    mass_insert(c, {1, 2, 3, 4});
    container c2 = c;
    expect_eq(c2, {1, 2, 3, 4});
}

TEST(correctness, copy_ctor_2)
{
    counted::no_new_instances_guard g;
    
    container c;
    mass_insert(c, {3, 4, 2, 5, 1});
    container c2 = c;
    expect_eq(c2, {1, 2, 3, 4, 5});
}

TEST(correctness, copy_ctor_empty)
{
    counted::no_new_instances_guard g;

    container c;
    container c2 = c;
    EXPECT_TRUE(c2.empty());
}

TEST(correctness, assignment_operator)
{
    counted::no_new_instances_guard g;

    container c;
    mass_insert(c, {1, 2, 3, 4});
    container c2;
    mass_insert(c2, {5, 6, 7, 8});
    c2 = c;
    expect_eq(c2, {1, 2, 3, 4});
}

TEST(correctness, self_assignment)
{
    counted::no_new_instances_guard g;

    container c;
    mass_insert(c, {1, 2, 3, 4});
    c = c;
    expect_eq(c, {1, 2, 3, 4});
}

TEST(correctness, empty)
{
    counted::no_new_instances_guard g;

    container c;
    EXPECT_EQ(c.begin(), c.end());
    EXPECT_TRUE(c.empty());
    std::pair<container::iterator, bool> p = c.insert(1);
    EXPECT_NE(c.begin(), c.end());
    EXPECT_FALSE(c.empty());
    c.erase(p.first);
    EXPECT_EQ(c.begin(), c.end());
    EXPECT_TRUE(c.empty());
}

TEST(correctness, reverse_iterators)
{
    counted::no_new_instances_guard g;

    container c;
    mass_insert(c, {3, 1, 2, 4});
    expect_reverse_eq(c, {4, 3, 2, 1});

    EXPECT_EQ(4, *c.rbegin());
    EXPECT_EQ(3, *std::next(c.rbegin()));
    EXPECT_EQ(1, *std::prev(c.rend()));
}

TEST(correctness, iterator_conversions)
{
    counted::no_new_instances_guard g;

    container c;
    container::const_iterator i1 = c.begin();
    container::iterator i2 = c.end();
    EXPECT_TRUE(i1 == i1);
    EXPECT_TRUE(i1 == i2);
    EXPECT_TRUE(i2 == i1);
    EXPECT_TRUE(i2 == i2);
    EXPECT_FALSE(i1 != i1);
    EXPECT_FALSE(i1 != i2);
    EXPECT_FALSE(i2 != i1);
    EXPECT_FALSE(i2 != i2);
}

TEST(correctness, iterators_postfix)
{
    counted::no_new_instances_guard g;

    container s;
    mass_insert(s, {1, 2, 3});
    container::iterator i = s.begin();
    EXPECT_EQ(1, *i);
    container::iterator j = i++;
    EXPECT_EQ(2, *i);
    EXPECT_EQ(1, *j);
    j = i++;
    EXPECT_EQ(3, *i);
    EXPECT_EQ(2, *j);
    j = i++;
    EXPECT_EQ(s.end(), i);
    EXPECT_EQ(3, *j);
    j = i--;
    EXPECT_EQ(3, *i);
    EXPECT_EQ(s.end(), j);
}

TEST(correctness, iterators_decrement)
{
    counted::no_new_instances_guard g;

    container s;
    mass_insert(s, {5, 3, 8, 1, 2, 6, 7, 10});
    container::iterator i = s.end();
    EXPECT_EQ(10, *--i);
    EXPECT_EQ(8, *--i);
    EXPECT_EQ(7, *--i);
    EXPECT_EQ(6, *--i);
    EXPECT_EQ(5, *--i);
    EXPECT_EQ(3, *--i);
    EXPECT_EQ(2, *--i);
    EXPECT_EQ(1, *--i);
    EXPECT_EQ(s.begin(), i);
}

TEST(correctness, iterator_default_ctor)
{
    counted::no_new_instances_guard g;

    container::iterator i;
    container::const_iterator j;
    container s;
    mass_insert(s, {4, 1, 8, 6, 3, 2, 6});
    
    i = s.begin();
    j = s.begin();
    EXPECT_EQ(1, *i);
    EXPECT_EQ(1, *j);
}

TEST(correctness, insert_simple)
{
    counted::no_new_instances_guard g;

    container c;
    mass_insert(c, {8, 4, 2, 10, 5});
    expect_eq(c, {2, 4, 5, 8, 10});
}

TEST(correctness, insert_duplicates)
{
    counted::no_new_instances_guard g;

    container c;
    mass_insert(c, {8, 4, 2, 4, 4, 4});
    expect_eq(c, {2, 4, 8});
}

TEST(correctness, reinsert)
{
    counted::no_new_instances_guard g;

    container c;
    mass_insert(c, {6, 2, 3, 1, 9, 8});
    c.erase(c.find(6));
    c.insert(6);
    expect_eq(c, {1, 2, 3, 6, 8, 9});
}

TEST(correctness, erase_begin)
{
    counted::no_new_instances_guard g;

    container c;
    mass_insert(c, {1, 2, 3, 4});
    c.erase(c.begin());
    expect_eq(c, {2, 3, 4});
}

TEST(correctness, erase_middle)
{
    counted::no_new_instances_guard g;

    container c;
    mass_insert(c, {1, 2, 3, 4});
    c.erase(std::next(c.begin(), 2));
    expect_eq(c, {1, 2, 4});
}

TEST(correctness, erase_close_to_end)
{
    counted::no_new_instances_guard g;

    container c;
    mass_insert(c, {6, 1, 4, 3, 2, 5});
    c.erase(std::next(c.begin(), 4));
    expect_eq(c, {1, 2, 3, 4, 6});
}

TEST(correctness, erase_end)
{
    counted::no_new_instances_guard g;

    container c;
    mass_insert(c, {1, 2, 3, 4});
    c.erase(std::prev(c.end()));
    expect_eq(c, {1, 2, 3});
}

TEST(correctness, erase_root)
{
    counted::no_new_instances_guard g;

    container c;
    mass_insert(c, {5, 3, 8, 1, 2});
    c.erase(c.find(5));
    expect_eq(c, {1, 2, 3, 8});
}

TEST(correctness, erase_1)
{
    counted::no_new_instances_guard g;

    container c;
    mass_insert(c, {5, 3, 8, 1, 2, 7, 9, 10, 11, 12});
    c.erase(c.find(8));
    expect_eq(c, {1, 2, 3, 5, 7, 9, 10, 11, 12});
}

TEST(correctness, erase_2)
{
    counted::no_new_instances_guard g;

    container c;
    mass_insert(c, {5, 3, 17, 15, 20, 19, 18});
    c.erase(c.find(17));
    expect_eq(c, {3, 5, 15, 18, 19, 20});
}

TEST(correctness, erase_3)
{
    counted::no_new_instances_guard g;

    container c;
    mass_insert(c, {10, 5, 15, 14, 13});
    c.erase(c.find(15));
    expect_eq(c, {5, 10, 13, 14});
}

TEST(correctness, erase_4)
{
    counted::no_new_instances_guard g;

    container c;
    mass_insert(c, {10, 5, 15, 3, 4});
    c.erase(c.find(5));
    expect_eq(c, {3, 4, 10, 15});
}

TEST(correctness, erase_5)
{
    counted::no_new_instances_guard g;

    container c;
    mass_insert(c, {5, 2, 10, 6, 14, 7, 8});
    c.erase(c.find(5));
    expect_eq(c, {2, 6, 7, 8, 10, 14});
}

TEST(correctness, erase_6)
{
    counted::no_new_instances_guard g;

    container c;
    mass_insert(c, {7, 3, 2, 6, 10, 9});
    c.erase(c.find(3));
    c.erase(c.find(6));
    c.erase(c.find(7));
    c.erase(c.find(10));
    c.erase(c.find(2));
    c.erase(c.find(9));
    EXPECT_TRUE(c.empty());
}

TEST(correctness, erase_7)
{
    counted::no_new_instances_guard g;

    container c;
    mass_insert(c, {5, 3, 8});
    c.erase(c.find(5));
    EXPECT_FALSE(c.empty());
}

TEST(correctness, erase_iterator_invalidation)
{
    counted::no_new_instances_guard g;

    container c;
    mass_insert(c, {8, 2, 6, 10, 3, 1, 9, 7});
    container::iterator i = c.find(8);
    container::iterator j = std::next(i);
    c.erase(i);
    EXPECT_EQ(9, *j);
}

TEST(correctness, erase_return_value)
{
    counted::no_new_instances_guard g;

    container c;
    mass_insert(c, {7, 4, 10, 1, 8, 12});
    container::iterator i = c.find(7);
    i = c.erase(i);
    EXPECT_EQ(8, *i);
}

/*TEST(correctness, size)
{
    container c;
    for (size_t i = 0; i != 10; ++i)
    {
        EXPECT_EQ(i, c.size());
        c.insert(42 + i);
    }
    EXPECT_EQ(10, c.size());
}*/

TEST(correctness, clear)
{
    counted::no_new_instances_guard g;

    container c;
    mass_insert(c, {1, 2, 3, 4, 5, 6});
    EXPECT_FALSE(c.empty());
    c.clear();
    EXPECT_TRUE(c.empty());
    EXPECT_TRUE(c.empty());
    EXPECT_EQ(c.end(), c.begin());
}

TEST(correctness, iterator_deref_1)
{
    counted::no_new_instances_guard g;

    container c;
    mass_insert(c, {1, 2, 3, 4, 5, 6});
    container::iterator const i = c.find(4);
    EXPECT_EQ(4, *i);
}

void magic(counted& c)
{
    c = 42;
}

void magic(counted const& c)
{}

TEST(correctness, iterator_deref_2)
{
    counted::no_new_instances_guard g;

    container c;
    mass_insert(c, {1, 2, 3, 4, 5, 6});
    container::iterator i = c.find(4);
    EXPECT_EQ(4, *i);
    magic(*i);
    expect_eq(c, {1, 2, 3, 4, 5, 6});
}

TEST(correctness, iterator_deref_3)
{
    counted::no_new_instances_guard g;

    container c;
    mass_insert(c, {1, 2, 3, 4, 5, 6});
    container::iterator const i = c.find(4);
    magic(*i.operator->());
    expect_eq(c, {1, 2, 3, 4, 5, 6});
}

TEST(correctness, swap)
{
    counted::no_new_instances_guard g;

    container c1, c2;
    mass_insert(c1, {1, 2, 3, 4});
    mass_insert(c2, {5, 6, 7, 8});
    swap(c1, c2);
    expect_eq(c1, {5, 6, 7, 8});
    expect_eq(c2, {1, 2, 3, 4});
}

TEST(correctness, swap_self)
{
    counted::no_new_instances_guard g;

    container c1;
    mass_insert(c1, {1, 2, 3, 4});
    swap(c1, c1);
}

TEST(correctness, swap_empty)
{
    counted::no_new_instances_guard g;

    container c1, c2;
    mass_insert(c1, {1, 2, 3, 4});
    swap(c1, c2);
    EXPECT_TRUE(c1.empty());
    expect_eq(c2, {1, 2, 3, 4});
    swap(c1, c2);
    expect_eq(c1, {1, 2, 3, 4});
    EXPECT_TRUE(c2.empty());
}

TEST(correctness, swap_empty_empty)
{
    counted::no_new_instances_guard g;

    container c1, c2;
    swap(c1, c2);
}

TEST(correctness, swap_empty_self)
{
    counted::no_new_instances_guard g;

    container c1;
    swap(c1, c1);
}

TEST(correctness, swap_iterator_validity)
{
    counted::no_new_instances_guard g;

    container c1, c2;
    mass_insert(c1, {1, 2, 3, 4, 5, 6, 7, 8, 9, 10});
    c2.insert(11);

    container::const_iterator c1_begin = c1.begin();
    //container::const_iterator c1_end = c1.end();

    container::const_iterator c2_begin = c2.begin();
    //container::const_iterator c2_end = c2.end();

    swap(c1, c2);

    EXPECT_EQ(1, *c1_begin++);
    EXPECT_EQ(2, *c1_begin++);
    EXPECT_EQ(3, *c1_begin++);
    std::advance(c1_begin, 7);
    // EXPECT_EQ(c1_end, c1_begin);

    EXPECT_EQ(11, *c2_begin++);
    //EXPECT_EQ(c2_end, c2_begin);
}

TEST(correctness, swap_1)
{
    counted::no_new_instances_guard g;

    container c;
    mass_insert(c, {3, 2, 4, 1});
    container tmp(c);
    swap(c, tmp);
    expect_eq(c, {1, 2, 3, 4});
}

TEST(correctness, swap_iterators_1)
{
    counted::no_new_instances_guard g;

    container c;
    mass_insert(c, {1, 2, 3});
    container c2;
    mass_insert(c2, {4, 5, 6});
    container::iterator i = c.find(2);
    container::iterator j = c2.find(5);

    {    
        using std::swap;
        swap(i, j);
    }

    c2.erase(i);
    c.erase(j);
    expect_eq(c, {1, 3});
    expect_eq(c2, {4, 6});
}

TEST(correctness, find_in_empty)
{
    counted::no_new_instances_guard g;

    container c;

    EXPECT_EQ(c.end(), c.find(0));
    EXPECT_EQ(c.end(), c.find(5));
    EXPECT_EQ(c.end(), c.find(42));
}

TEST(correctness, finds)
{
    counted::no_new_instances_guard g;

    container c;
    
    mass_insert(c, {8, 3, 5, 4, 3, 1, 8, 8, 10, 9});
    
    EXPECT_EQ(c.end(), c.find(0));
    EXPECT_EQ(c.begin(), c.find(1));
    EXPECT_EQ(c.end(), c.find(2));
    EXPECT_EQ(std::next(c.begin(), 1), c.find(3));
    EXPECT_EQ(std::next(c.begin(), 2), c.find(4));
    EXPECT_EQ(std::next(c.begin(), 3), c.find(5));
    EXPECT_EQ(c.end(),                 c.find(6));
    EXPECT_EQ(c.end(),                 c.find(7));
    EXPECT_EQ(std::next(c.begin(), 4), c.find(8));
    EXPECT_EQ(std::next(c.begin(), 5), c.find(9));
    EXPECT_EQ(std::next(c.begin(), 6), c.find(10));
    EXPECT_EQ(c.end(),                 c.find(11));
}

TEST(correctness, lower_bound_empty)
{
    counted::no_new_instances_guard g;

    container c;
    EXPECT_EQ(c.end(), c.lower_bound(5));
}

TEST(correctness, lower_bounds)
{
    counted::no_new_instances_guard g;

    container c;
    
    mass_insert(c, {8, 3, 5, 4, 3, 1, 8, 8, 10, 9});
    
    EXPECT_EQ(c.begin(), c.lower_bound(0));
    EXPECT_EQ(c.begin(), c.lower_bound(1));
    EXPECT_EQ(std::next(c.begin()), c.lower_bound(2));
    EXPECT_EQ(std::next(c.begin()), c.lower_bound(3));
    EXPECT_EQ(std::next(c.begin(), 2), c.lower_bound(4));
    EXPECT_EQ(std::next(c.begin(), 3), c.lower_bound(5));
    EXPECT_EQ(std::next(c.begin(), 4), c.lower_bound(6));
    EXPECT_EQ(std::next(c.begin(), 4), c.lower_bound(7));
    EXPECT_EQ(std::next(c.begin(), 4), c.lower_bound(8));
    EXPECT_EQ(std::next(c.begin(), 5), c.lower_bound(9));
    EXPECT_EQ(std::next(c.begin(), 6), c.lower_bound(10));
    EXPECT_EQ(std::next(c.begin(), 7), c.lower_bound(11));
}

TEST(correctness, upper_bounds)
{
    counted::no_new_instances_guard g;

    container c;
    
    mass_insert(c, {8, 3, 5, 4, 3, 1, 8, 8, 10, 9});
    
    EXPECT_EQ(c.begin(), c.upper_bound(0));
    EXPECT_EQ(std::next(c.begin()), c.upper_bound(1));
    EXPECT_EQ(std::next(c.begin()), c.upper_bound(2));
    EXPECT_EQ(std::next(c.begin(), 2), c.upper_bound(3));
    EXPECT_EQ(std::next(c.begin(), 3), c.upper_bound(4));
    EXPECT_EQ(std::next(c.begin(), 4), c.upper_bound(5));
    EXPECT_EQ(std::next(c.begin(), 4), c.upper_bound(6));
    EXPECT_EQ(std::next(c.begin(), 4), c.upper_bound(7));
    EXPECT_EQ(std::next(c.begin(), 5), c.upper_bound(8));
    EXPECT_EQ(std::next(c.begin(), 6), c.upper_bound(9));
    EXPECT_EQ(std::next(c.begin(), 7), c.upper_bound(10));
    EXPECT_EQ(std::next(c.begin(), 7), c.upper_bound(11));
}

TEST(correctness, upper_bound_empty)
{
    counted::no_new_instances_guard g;

    container c;
    EXPECT_EQ(c.end(), c.upper_bound(5));
}

TEST(fault_injection, non_throwing_default_ctor)
{
    faulty_run([]
    {
        try
        {
            container();
        }
        catch (...)
        {
            fault_injection_disable dg;
            ADD_FAILURE();
            throw;
        }
    });
}

TEST(fault_injection, copy_ctor)
{
    faulty_run([]
    {
        container c;
        mass_insert(c, {3, 2, 4, 1});
        container c2 = c;
        fault_injection_disable dg;
        expect_eq(c, {1, 2, 3, 4});
    });
}

TEST(fault_injection, non_throwing_clear)
{
    faulty_run([]
    {
        container c;
        mass_insert(c, {3, 2, 4, 1});
        try
        {
            c.clear();
        }
        catch (...)
        {
            fault_injection_disable dg;
            ADD_FAILURE();
            throw;
        }
    });
}

TEST(fault_injection, assignment_operator)
{
    faulty_run([]
    {
        container c;
        mass_insert(c, {3, 2, 4, 1});
        container c2;
        mass_insert(c2, {8, 7, 2, 14});
        
        try
        {
            c = c2;
        }
        catch (...)
        {
            fault_injection_disable dg;
            expect_eq(c, {1, 2, 3, 4});
            throw;
        }
        
        fault_injection_disable dg;
        expect_eq(c, {2, 7, 8, 14});
    });
}

TEST(fault_injection, insert)
{
    faulty_run([]
    {
        container c;
        mass_insert(c, {3, 2, 4, 1});
        
        try
        {
            c.insert(5);
        }
        catch (...)
        {
            fault_injection_disable dg;
            expect_eq(c, {1, 2, 3, 4});
            throw;
        }
        fault_injection_disable dg;
        expect_eq(c, {1, 2, 3, 4, 5});
    });
}

TEST(fault_injection, erase)
{
    faulty_run([]
    {
        container c;
        mass_insert(c, {6, 3, 8, 2, 5, 7, 10});
        counted val = 6;
        try
        {
            c.erase(c.find(val));
        }
        catch (...)
        {
            fault_injection_disable dg;
            expect_eq(c, {2, 3, 5, 6, 7, 8, 10});
            throw;
        }
        fault_injection_disable dg;
        expect_eq(c, {2, 3, 5, 7, 8, 10});
    });
}

/*
TEST(invalid, empty_deref_end)
{
    EXPECT_EXIT(
    {
        container c;
        *c.end();
    }, ::testing::KilledBySignal(SIGABRT), "");
}

TEST(invalid, empty_inc_end)
{
    EXPECT_EXIT(
    {
        container c;
        container::const_iterator i = c.end();
        ++i;
    }, ::testing::KilledBySignal(SIGABRT), "");
}

TEST(invalid, empty_dec_end)
{
    EXPECT_EXIT(
    {
        container c;
        container::const_iterator i = c.end();
        --i;
    }, ::testing::KilledBySignal(SIGABRT), "");
}

TEST(invalid, deref_after_erase)
{
    EXPECT_EXIT(
    {
        container c;
        mass_insert(c, {1, 2, 3, 4});
        container::const_iterator i = c.find(3);
        container::const_iterator i2 = i;
        c.erase(i);
        *i2;
    }, ::testing::KilledBySignal(SIGABRT), "");
}

TEST(invalid, inc_after_erase)
{
    EXPECT_EXIT(
    {
        container c;
        mass_insert(c, {1, 2, 3, 4});
        container::const_iterator i = c.find(3);
        container::const_iterator i2 = i;
        c.erase(i);
        ++i2;
    }, ::testing::KilledBySignal(SIGABRT), "");
}

TEST(invalid, deref_after_dtor)
{
    EXPECT_EXIT(
    {
        container::const_iterator i;
        {
            container c;
            mass_insert(c, {1, 2, 3, 4});
            i = c.find(3);
        }
        *i;
    }, ::testing::KilledBySignal(SIGABRT), "");
}

TEST(invalid, erase_wrong_container)
{
    EXPECT_EXIT(
    {
        container c;
        container c2;
        mass_insert(c, {1, 2, 3, 4});
        container::const_iterator i = c.find(3);
        c2.erase(i);
    }, ::testing::KilledBySignal(SIGABRT), "");
}

TEST(invalid, erase_default)
{
    EXPECT_EXIT(
    {
        container c;
        container c2;
        mass_insert(c, {1, 2, 3, 4});
        c2.erase(container::const_iterator());
    }, ::testing::KilledBySignal(SIGABRT), "");
}

TEST(invalid, iterator_default_ctor_deref)
{
    EXPECT_EXIT(
    {
        container::const_iterator i;
        *i;
    }, ::testing::KilledBySignal(SIGABRT), "");
}

TEST(invalid, iterator_default_ctor_inc)
{
    EXPECT_EXIT(
    {
        container::const_iterator i;
        ++i;
    }, ::testing::KilledBySignal(SIGABRT), "");
}

TEST(invalid, iterator_default_ctor_postinc)
{
    EXPECT_EXIT(
    {
        container::const_iterator i;
        i++;
    }, ::testing::KilledBySignal(SIGABRT), "");
}

TEST(invalid, iterator_default_ctor_dec)
{
    EXPECT_EXIT(
    {
        container::const_iterator i;
        --i;
    }, ::testing::KilledBySignal(SIGABRT), "");
}

TEST(invalid, iterator_default_ctor_postdec)
{
    EXPECT_EXIT(
    {
        container::const_iterator i;
        i--;
    }, ::testing::KilledBySignal(SIGABRT), "");
}

TEST(invalid, dec_begin)
{
    EXPECT_EXIT(
    {
        container s;
        mass_insert(s, {5, 3, 8, 1, 2, 6});
        container::const_iterator i = s.begin();
        --i;
    }, ::testing::KilledBySignal(SIGABRT), "");
}

TEST(invalid, swap_iterator_self)
{
    EXPECT_EXIT(
    {
        using std::swap;
        container::iterator j;
        {
            container c;
            mass_insert(c, {1, 2, 3});
            j = c.begin();
            container::iterator i = c.begin();
            swap(i, i);
        }
        *j;
    }, ::testing::KilledBySignal(SIGABRT), "");
}
*/
