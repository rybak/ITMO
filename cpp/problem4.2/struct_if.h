#ifndef STRUCT_IF
#define STRUCT_IF

/******************************************************************************/
/* struct struct_if output result */

template<bool condition, typename Then, typename Else>
struct struct_if
{};

template<typename Then, typename Else>
struct struct_if<true, Then, Else>
{
   typedef Then result;
};

template<typename Then, typename Else>
struct struct_if<false, Then, Else>
{
   typedef Else result;
};

/*************/

template<bool condition, digit_t Then, digit_t Else>
struct struct_if_values
{};

template<digit_t Then, digit_t Else>
struct struct_if_values<true, Then, Else>
{
   static const digit_t result = Then;
};

template<digit_t Then, digit_t Else>
struct struct_if_values<false, Then, Else>
{
   static const digit_t result = Else;
};

#endif
