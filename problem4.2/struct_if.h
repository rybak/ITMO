#ifndef STRUCT_IF
#define STRUCT_IF

/******************************************************************************/
/* struct struct_if output result */

template<bool condition, typename Then, typename Else>
struct struct_if
{};

template<typename Then, typename Else>
struct struct_if<true, typename Then, typename Else>
{
   typedef Then result;
};

template<typename Then, typename Else>
struct struct_if<false, typename Then, typename Else>
{
   typedef Else result;
};

#endif
