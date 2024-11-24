/* ========================================================================
   LISTING 35
   ======================================================================== */

f32 CornerAreaSwitch(u32 ShapeCount, shape_union *Shapes)
{
    f32 Accum = 0.0f;

    for(u32 ShapeIndex = 0; ShapeIndex < ShapeCount; ++ShapeIndex)
    {
        Accum += (1.0f / (1.0f + (f32)GetCornerCountSwitch(Shapes[ShapeIndex].Type))) * GetAreaSwitch(Shapes[ShapeIndex]);
    }

    return Accum;
}

f32 CornerAreaSwitch4(u32 ShapeCount, shape_union *Shapes)
{
    f32 Accum0 = 0.0f;
    f32 Accum1 = 0.0f;
    f32 Accum2 = 0.0f;
    f32 Accum3 = 0.0f;

    ShapeCount /= 4;
    while(ShapeCount--)
    {
        Accum0 += (1.0f / (1.0f + (f32)GetCornerCountSwitch(Shapes[0].Type))) * GetAreaSwitch(Shapes[0]);
        Accum1 += (1.0f / (1.0f + (f32)GetCornerCountSwitch(Shapes[1].Type))) * GetAreaSwitch(Shapes[1]);
        Accum2 += (1.0f / (1.0f + (f32)GetCornerCountSwitch(Shapes[2].Type))) * GetAreaSwitch(Shapes[2]);
        Accum3 += (1.0f / (1.0f + (f32)GetCornerCountSwitch(Shapes[3].Type))) * GetAreaSwitch(Shapes[3]);

        Shapes += 4;
    }

    f32 Result = (Accum0 + Accum1 + Accum2 + Accum3);
    return Result;
}