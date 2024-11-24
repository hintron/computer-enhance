/* ========================================================================
   LISTING 25
   ======================================================================== */

enum shape_type : u32
{
    Shape_Square,
    Shape_Rectangle,
    Shape_Triangle,
    Shape_Circle,

    Shape_Count,
};

struct shape_union
{
    shape_type Type;
    f32 Width;
    f32 Height;
};

f32 GetAreaSwitch(shape_union Shape)
{
    f32 Result = 0.0f;

    switch(Shape.Type)
    {
        case Shape_Square: {Result = Shape.Width * Shape.Width;} break;
        case Shape_Rectangle: {Result = Shape.Width * Shape.Height;} break;
        case Shape_Triangle: {Result = 0.5f * Shape.Width * Shape.Height;} break;
        case Shape_Circle: {Result = Pi32 * Shape.Width * Shape.Width;} break;

        case Shape_Count: {} break;
    }

    return Result;
}