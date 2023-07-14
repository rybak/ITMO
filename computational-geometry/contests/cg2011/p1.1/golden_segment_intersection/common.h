#pragma once


namespace cg
{
#  pragma pack(push, 1)

   struct point_2
   {
      point_2()
         : x(0)
         , y(0)
      {}

      point_2(double x, double y)
         : x(x)
         , y(y)
      {}

      double x;
      double y;
   };

   struct segment_2
   {
      segment_2() {}

      segment_2(point_2 const & a, point_2 const & b)
         : p0(a)
         , p1(b)
      {}

      point_2 p0;
      point_2 p1;
   };

   struct range
   {
      range()
         : lo_(1)
         , hi_(0)
      {}

      range(double x)
         : lo_(x)
         , hi_(x)
      {}

      range(double x, double y)
         : lo_(std::min(x, y))
         , hi_(std::max(x, y))
      {}

      bool has_intersection(range const & r) const
      {
         if (empty() || r.empty())
            return false;

         if (hi_ < r.lo_ || r.hi_ < lo_)
            return false;

         return true;
      }

      bool empty() const
      {
         return lo_ > hi_;
      }

   private:
      // lo_ <= hi_  => valid range
      // else        => empty range
      double lo_;
      double hi_;
   };

   struct rectangle_2
   {
      rectangle_2(point_2 const & p1, point_2 const & p2)
         : x_range_(p1.x, p2.x)
         , y_range_(p1.y, p2.y)
      {}

      bool has_intersection(rectangle_2 const & r) const
      {
         return x_range_.has_intersection(r.x_range_)
             && y_range_.has_intersection(r.y_range_);
      }

   private:
      range x_range_;
      range y_range_;
   };

   point_2 & operator -= (point_2 & a, point_2 const & b)
   {
      a.x -= b.x;
      a.y -= b.y;

      return a;
   }

   point_2 operator - (point_2 a, point_2 const & b)
   {
      return a -= b;
   }

   double cross(point_2 const & a, point_2 const & b)
   {
      return a.x * b.y - a.y * b.x;
   }

#  pragma pack(pop)
}
