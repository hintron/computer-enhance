/* ========================================================================
   LISTING 32
   ======================================================================== */

class shape_base
{
public:
    shape_base() {}
    virtual f32 Area() = 0;
    virtual u32 CornerCount() = 0;
};

class square : public shape_base
{
public:
    square(f32 SideInit) : Side(SideInit) {}
    virtual f32 Area() {return Side*Side;}
    virtual u32 CornerCount() {return 4;}

private:
    f32 Side;
};

class rectangle : public shape_base
{
public:
    rectangle(f32 WidthInit, f32 HeightInit) : Width(WidthInit), Height(HeightInit) {}
    virtual f32 Area() {return Width*Height;}
    virtual u32 CornerCount() {return 4;}

private:
    f32 Width, Height;
};

class triangle : public shape_base
{
public:
    triangle(f32 BaseInit, f32 HeightInit) : Base(BaseInit), Height(HeightInit) {}
    virtual f32 Area() {return 0.5f*Base*Height;}
    virtual u32 CornerCount() {return 3;}

private:
    f32 Base, Height;
};

class circle : public shape_base
{
public:
    circle(f32 RadiusInit) : Radius(RadiusInit) {}
    virtual f32 Area() {return Pi32*Radius*Radius;}
    virtual u32 CornerCount() {return 0;}

private:
    f32 Radius;
};