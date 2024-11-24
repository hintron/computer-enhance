/* ========================================================================
   LISTING 27
   ======================================================================== */

f32 const CTable[Shape_Count] = {1.0f, 1.0f, 0.5f, Pi32};
f32 GetAreaUnion(shape_union Shape)
{
    f32 Result = CTable[Shape.Type]*Shape.Width*Shape.Height;
    return Result;
}