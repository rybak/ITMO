import random

def write_test(prefix, num, points):
  filename = prefix + ("%02d.in" % num)
  f = open(filename, "w")
  plist = list(points)
  hstring = "%d\n" % len(plist)
  f.write(hstring)
  for p in plist:
    pstring = "%f %f\n" % p
    f.write(pstring)
  f.close()

def small_test(start_num):
  test_count = 20
  for i in range(test_count):
    r = random.uniform(1.0, 2.0 + i)
    rget = lambda: random.uniform(-r, r)
    write_test("./correctness_tests/", i + start_num, ((rget(), rget()) for j in xrange(10 * (i + 1))))

small_test(1)
