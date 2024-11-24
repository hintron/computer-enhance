/* ========================================================================
   LISTING 34
   ======================================================================== */

u32 GetCornerCountSwitch(shape_type Type)
{
    u32 Result = 0;

    switch(Type)
    {
        case Shape_Square: {Result = 4;} break;
        case Shape_Rectangle: {Result = 4;} break;
        case Shape_Triangle: {Result = 3;} break;
        case Shape_Circle: {Result = 0;} break;

        case Shape_Count: {} break;
    }

    return Result;
}