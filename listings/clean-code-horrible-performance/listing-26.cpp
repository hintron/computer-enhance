/* ========================================================================
   LISTING 26
   ======================================================================== */

f32 TotalAreaSwitch(u32 ShapeCount, shape_union *Shapes)
{
    f32 Accum = 0.0f;

    for(u32 ShapeIndex = 0; ShapeIndex < ShapeCount; ++ShapeIndex)
    {
        Accum += GetAreaSwitch(Shapes[ShapeIndex]);
    }

    return Accum;
}

f32 TotalAreaSwitch4(u32 ShapeCount, shape_union *Shapes)
{
    f32 Accum0 = 0.0f;
    f32 Accum1 = 0.0f;
    f32 Accum2 = 0.0f;
    f32 Accum3 = 0.0f;

    ShapeCount /= 4;
    while(ShapeCount--)
    {
        Accum0 += GetAreaSwitch(Shapes[0]);
        Accum1 += GetAreaSwitch(Shapes[1]);
        Accum2 += GetAreaSwitch(Shapes[2]);
        Accum3 += GetAreaSwitch(Shapes[3]);

        Shapes += 4;
    }

    f32 Result = (Accum0 + Accum1 + Accum2 + Accum3);
    return Result;
}