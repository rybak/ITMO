#include "helper.h"

boost::phoenix::function<helper::construct_big_int_impl>& construct_big_int()
{
        static boost::phoenix::function<helper::construct_big_int_impl> f;
        return f;
}

boost::phoenix::function<helper::power_impl>& pow()
{
        static boost::phoenix::function<helper::power_impl> f;
        return f;
}

boost::phoenix::function<helper::user_function_impl>& user_function()
{
        static boost::phoenix::function<helper::user_function_impl> f;
        return f;
}