/* ========================================================================
   LISTING 36
   ======================================================================== */

f32 const CTable[Shape_Count] = {1.0f / (1.0f + 4.0f), 1.0f / (1.0f + 4.0f), 0.5f / (1.0f + 3.0f), Pi32};
f32 GetCornerAreaUnion(shape_union Shape)
{
    f32 Result = CTable[Shape.Type]*Shape.Width*Shape.Height;
    return Result;
}