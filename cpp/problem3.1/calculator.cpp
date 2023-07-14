#include <boost/spirit/include/qi.hpp>

#include "calculator.h"
namespace
{
   namespace qi = boost::spirit::qi;
   namespace ascii = qi::ascii;

   template <typename Iterator>
   struct calculator : qi::grammar<Iterator, big_int(), ascii::space_type>
   {  
      calculator() : calculator<Iterator>::base_type(start)
      {
         using qi::_1;
         using qi::_2;
         using qi::_val;
         using qi::lit;
         big_int_str = +(ascii::digit);
         function_name = +(ascii::alpha);
         number = big_int_str[_val = construct_big_int()(_1)];
         expr = number[_val = _1]
            | (lit("-") >> expr)[_val = -_1]
            | (lit("(") >> sum >> lit(")"))[_val = _1]
            | (function_name >> lit("(") >> sum >> lit(")"))[_val = user_function()(_1, _2)];
         power = expr[_val = _1] >> -(lit("^") >> power)[_val = pow()(_val, _1)];
         product = power[_val = _1] >> *
         (
            (lit("*") >> power)[_val *= _1]
            | (lit("/") >> power)[_val /= _1]
         );
         sum = product[_val = _1] >> *
         (
            (lit("+") >> product)[_val += _1]
            | (lit("-") >> product)[_val -= _1]
         );
         start %= sum > qi::eoi;
      }
   private:
      qi::rule<Iterator, big_int(), ascii::space_type>
         expr, product, power, 
         number, sum, start;
      qi::rule<Iterator, std::string(), ascii::space_type>
         function_name, big_int_str;
   };

   calculator<std::string::const_iterator>& get_grammar()
   {
      static calculator<std::string::const_iterator> calc;
      return calc;
   }
}

big_int evaluate(const std::string &expr)
{
   big_int ans;
   std::string::const_iterator begin = expr.begin(), end = expr.end();
   qi::phrase_parse(begin, end, get_grammar(), ascii::space, ans);
   return ans;
}


