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

#endif
