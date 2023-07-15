a : int;
// hello comment
c : int; b : int;

f (x : int, y : int) -> (int) -> int
{
    a : int;
    b : (int) -> int;
}

g () -> int
{
    a : int;
    f = a;
    c = a;
    a = c;
}
