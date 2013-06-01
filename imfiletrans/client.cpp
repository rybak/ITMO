#include "client.h"

void client::process_client()
{
    switch(state)
    {
    case UNDEF:
        process_undef();
        break;
    case IN:
        process_in();
        break;
    case OUT:
        process_out();
        break;
    default:
        std::cerr << "THIS SHOULD NOT HAVE HAPPENED" << std::endl;
        exit(1);
    }
}

void client::process_undef()
{

}
