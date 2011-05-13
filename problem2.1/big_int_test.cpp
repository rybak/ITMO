#define BOOST_TEST_MODULE big_int test
#define BOOST_TEST_DYN_LINK
#define BOOST_TEST_MAIN
#include <boost/test/unit_test.hpp>

#include "big_int.h"

#include <boost/random/mersenne_twister.hpp>
#include <boost/random/uniform_int.hpp>
#include <boost/random/variate_generator.hpp>
#include <boost/random/geometric_distribution.hpp>
#include <boost/random/bernoulli_distribution.hpp>
#include <boost/lexical_cast.hpp>
#include <boost/foreach.hpp>


#include <string>
#include <vector>
#include <algorithm>
#include <limits>

using namespace std;
using namespace boost;

#define foreach BOOST_FOREACH

const int TESTS_SIZE = 10;

namespace 
{
mt19937 gen;

bool implies(bool a, bool b) 
{
    return !a || b;
}

big_int next_random_big_int()
{
    variate_generator<mt19937&, geometric_distribution<> > length_generator(
            gen,
            geometric_distribution<>(0.98));

    variate_generator<mt19937&, uniform_int<> > digit_generator(
            gen,
            uniform_int<>(0, 9));

    int length = length_generator() - 1;
    big_int res;
    for (int i = 0; i < length; ++i) {
        res *= big_int(10);
        res += big_int(digit_generator());
    }

    variate_generator<mt19937&, bernoulli_distribution<> > sign_generator(
            gen,
            bernoulli_distribution<>());

    if (sign_generator())
        return -res;
    return res;
}

vector<big_int> get_numbers()
{
    vector<big_int> numbers;
    numbers.push_back(big_int());
    numbers.push_back(big_int(1));
    numbers.push_back(lexical_cast<big_int>("00000000000000000000312321"));
    numbers.push_back(lexical_cast<big_int>("-0000000000000000000000312"));
    generate_n(back_inserter(numbers), TESTS_SIZE - numbers.size(), next_random_big_int);
    return numbers;
}

}


BOOST_AUTO_TEST_CASE( construction_test ) 
{
    return;
    big_int a;
    big_int b(42);
    big_int c(-1);
    big_int d(numeric_limits<int>::max());
    big_int e(numeric_limits<int>::min());
    big_int f(a);
    big_int h(d);
}

BOOST_AUTO_TEST_CASE( arithmetic_test )
{
    return;
    for (int i = -TESTS_SIZE; i <= TESTS_SIZE; ++i) 
    {
        for (int j = -TESTS_SIZE; j <= TESTS_SIZE; ++j)
        {
            BOOST_CHECK_EQUAL(big_int(i) + big_int(j), big_int(i + j));
            BOOST_CHECK_EQUAL(big_int(i) - big_int(j), big_int(i - j));
            BOOST_CHECK_EQUAL(big_int(i) * big_int(j), big_int(i * j));
            if (j != 0)
            {
                BOOST_CHECK_EQUAL(big_int(i) / big_int(j), big_int(i / j));
                BOOST_CHECK_EQUAL(big_int(i) % big_int(j), big_int(i % j));
            }

        }
        BOOST_CHECK_EQUAL(abs(big_int(i)), big_int(abs(i)));
    }

    const big_int ZERO;
    const big_int ONE(1);

    vector<big_int> numbers = get_numbers();

    size_t size = numbers.size();
    for (size_t i = 0; i < size; ++i) 
    {
        for (size_t j = 0; j < size; ++j) 
        {
            for (size_t k = 0; k < size; ++k) 
            {
                // properties of integers from Wikipedia
                big_int a(numbers[i]);
                big_int b(numbers[j]);
                big_int c(numbers[k]);

                BOOST_CHECK_EQUAL(a + b, b + a);
                BOOST_CHECK_EQUAL(a + (b + c), (a + b) + c);
                BOOST_CHECK_EQUAL(a + ZERO, a);
                BOOST_CHECK_EQUAL(ZERO + a, a);

                BOOST_CHECK_EQUAL(a + (-a), ZERO);

                BOOST_CHECK_EQUAL(a * b, b * a);
                BOOST_CHECK_EQUAL(a * (b * c), (a * b) * c);
                BOOST_CHECK_EQUAL(a * ONE, a);

                BOOST_CHECK_EQUAL(a * (b + c), (a * b) + (a * c));
                BOOST_CHECK_EQUAL((a + b) * c, (a * c) + (b * c));

                if (a * b == ZERO) 
                {
                    BOOST_CHECK((a == ZERO) || (b == ZERO));
                }

                if (b != ZERO) 
                {
                    big_int q = a / b;
                    big_int r = a % b;
                    BOOST_CHECK_EQUAL(a, q * b + r);
                    if (a < ZERO)
                    {
                        BOOST_CHECK(r <= ZERO);
                        BOOST_CHECK(r > -abs(b));
                    }
                    else
                    {
                        BOOST_CHECK(r >= ZERO);
                        BOOST_CHECK(r < abs(b));
                    }
                }

                big_int a1(a);
                (a1 += b) += c;
                BOOST_CHECK_EQUAL(a1, a + b + c);

                big_int a2(a);
                (a2 -= b) -= c;
                BOOST_CHECK_EQUAL(a2, a - b - c);

                big_int a3(a);
                (a3 *= b) *= c;
                BOOST_CHECK_EQUAL(a3, a * b * c);

                if (b != ZERO)
                {
                    big_int a4(a);
                    (a4 /= b) += c;
                    BOOST_CHECK_EQUAL(a4, a / b + c);

                    big_int a5(a);
                    (a5 %= b) += c;
                    BOOST_CHECK_EQUAL(a5, a % b + c);
                }

                BOOST_CHECK(a <= abs(a));
                BOOST_CHECK(-a <= abs(a));
                BOOST_CHECK((abs(a) == a) || (abs(a) == -a));
            }
        }
    }
}

BOOST_AUTO_TEST_CASE( comparison_test )
{
    return;
    BOOST_CHECK(big_int() == big_int()); 
    BOOST_CHECK(big_int() == big_int(0)); 

    for (int i = -TESTS_SIZE; i <= TESTS_SIZE; ++i) 
    {
        for (int j = -TESTS_SIZE; j <= TESTS_SIZE; ++j) 
        {
            BOOST_CHECK_EQUAL(big_int(i) == big_int(j), i == j);
            BOOST_CHECK_EQUAL(big_int(i) != big_int(j), i != j);

            BOOST_CHECK_EQUAL(big_int(i) < big_int(j), i < j);
            BOOST_CHECK_EQUAL(big_int(i) <= big_int(j), i <= j);

            BOOST_CHECK_EQUAL(big_int(i) > big_int(j), i > j);
            BOOST_CHECK_EQUAL(big_int(i) >= big_int(j), i >= j);
        }
    }

    const big_int ZERO;
    const big_int ONE(1);

    vector<big_int> numbers = get_numbers();

    size_t size = numbers.size();
    for (size_t i = 0; i < size; ++i) 
    {
        for (size_t j = 0; j < size; ++j) 
        {
            for (size_t k = 0; k < size; ++k) 
            {
                big_int a(numbers[i]);
                big_int b(numbers[j]);
                big_int c(numbers[k]);

                big_int a1(a);
                big_int a2;
                a2 = a;

                BOOST_CHECK_EQUAL(a, a1);
                BOOST_CHECK_EQUAL(a, a2);

                BOOST_CHECK(implies((a < b) && (b < c), a < c));
                BOOST_CHECK(implies((a <= b) && (b <= c), a <= c));

                BOOST_CHECK(implies((a > b) && (b > c), a > c));
                BOOST_CHECK(implies((a >= b) && (b >= c), a >= c));

                BOOST_CHECK(implies(a != b, (a < b) || (a > b)));
                BOOST_CHECK(implies(a == b, (a <= b) && (a >= b)));

                BOOST_CHECK_EQUAL(a < b, b > a);
                BOOST_CHECK_EQUAL(a <= b, b >= a);

                BOOST_CHECK_EQUAL(a < b, b > a);
                BOOST_CHECK_EQUAL(a <= b, b >= a);

                BOOST_CHECK_EQUAL(a == b, b == a);
                BOOST_CHECK_EQUAL(a != b, b != a);

                BOOST_CHECK((a < b) ^ (a >= b));
                BOOST_CHECK((a > b) ^ (a <= b));
                BOOST_CHECK((a == b) ^ (a != b));

                BOOST_CHECK(implies(a < b, a + ONE <= b));
                BOOST_CHECK(implies(a > b, a - ONE >= b));

                BOOST_CHECK(implies(a == b, a + ONE > b));
                BOOST_CHECK(implies(a == b, a - ONE < b));
            }
        }

    }
}

BOOST_AUTO_TEST_CASE( swap_test )
{
    return;
    big_int a(42);
    big_int b(39);
    big_int c(next_random_big_int());
    big_int d(next_random_big_int());

    big_int a1(a);
    big_int b1(b);
    big_int c1(c);
    big_int d1(d);

    a1.swap(b1);
    BOOST_CHECK_EQUAL(a1, b);
    BOOST_CHECK_EQUAL(b1, a);

    c1.swap(d1);
    BOOST_CHECK_EQUAL(c1, d);
    BOOST_CHECK_EQUAL(d1, c);

    a1.swap(c1);
    BOOST_CHECK_EQUAL(a1, d);
    BOOST_CHECK_EQUAL(c1, b);

    b1.swap(d1);
    BOOST_CHECK_EQUAL(b1, c);
    BOOST_CHECK_EQUAL(d1, a);
}

BOOST_AUTO_TEST_CASE( io_test )
{
    big_int const ZERO;
    BOOST_CHECK_EQUAL(lexical_cast<string>(ZERO), "0");

    variate_generator<mt19937&, geometric_distribution<> > length_generator(
            gen,
            geometric_distribution<>(0.98));

    variate_generator<mt19937&, uniform_int<> > digit_generator(
            gen,
            uniform_int<>(0, 9));
    for (int i = 0; i < TESTS_SIZE; ++i)
    {

        int length = length_generator();
        big_int test_int;
        string string_representation;
        for (int i = 0; i < length; ++i) {
            test_int *= big_int(10);
            int digit;

            do {
                digit = digit_generator();
            }
            while ((i == 0) && (digit == 0));

            test_int += big_int(digit);
            string_representation += static_cast<char>(digit + '0');
            BOOST_CHECK_EQUAL(lexical_cast<string>(test_int), string_representation);
        }


        test_int = -test_int;
        string_representation = "-" + string_representation;

        BOOST_CHECK_EQUAL(lexical_cast<string>(test_int), string_representation);
	}

    vector<big_int> numbers = get_numbers();
    size_t size = numbers.size();
    for (size_t i = 0; i < size; ++i) 
    {
        for (size_t j = 0; j < size; ++j) 
        {
            big_int a = numbers[i];
            big_int b = numbers[j];

            string as = lexical_cast<string>(a);
            string bs = lexical_cast<string>(b);

            BOOST_CHECK_EQUAL(a == b, as == bs);
            BOOST_CHECK_EQUAL(a != b, as != bs);

            big_int a1 = lexical_cast<big_int>(as);
            big_int b1 = lexical_cast<big_int>(bs);

            BOOST_CHECK_EQUAL(a, a1);
            BOOST_CHECK_EQUAL(b, b1);


        }
    }

    vector<string> bad_strings;
    bad_strings.push_back("");
    bad_strings.push_back(" ");
    bad_strings.push_back(" +");
    bad_strings.push_back(" -");
    bad_strings.push_back(" + ");
    bad_strings.push_back(" - ");
    bad_strings.push_back(" -z");

    foreach(string const & bad_string, bad_strings)
    {
        istringstream iss1(bad_string);

        big_int x1;
        iss1 >> x1;

        istringstream iss2(bad_string);

        int x2;
        iss2 >> x2;

        BOOST_CHECK_EQUAL(iss1.good(), iss2.good());
        BOOST_CHECK_EQUAL(iss1.eof(), iss2.eof());
        BOOST_CHECK_EQUAL(iss1.fail(), iss2.fail());
    }

    vector<string> ok_strings;
    ok_strings.push_back("42");
    ok_strings.push_back(" 42");
    ok_strings.push_back(" 42 ");
    ok_strings.push_back(" 42.da");
    ok_strings.push_back("42 asdf");

    foreach(string const & ok_string, ok_strings)
    {
        istringstream iss1(ok_string);

        big_int x1;
        string s1;
        iss1 >> x1 >> s1;

        istringstream iss2(ok_string);

        int x2;
        string s2;
        iss2 >> x2 >> s2;

        BOOST_CHECK_EQUAL(x1, big_int(x2));
        BOOST_CHECK_EQUAL(s1, s2);
        BOOST_CHECK_EQUAL(iss1.good(), iss2.good());
        BOOST_CHECK_EQUAL(iss1.eof(), iss2.eof());
        BOOST_CHECK_EQUAL(iss1.fail(), iss2.fail());
    }
    
    big_int test_number = lexical_cast<big_int>("87612319782231237389123382");
    big_int test_number_copy(test_number);

    // int test_number = 11;
    // int test_number_copy(test_number);

    istringstream iss("42");
    iss.setstate(ios::failbit);

    iss >> test_number;
    BOOST_CHECK_EQUAL(test_number, test_number_copy);
}
