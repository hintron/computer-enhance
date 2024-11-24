/* ========================================================================
   LISTING 24
   ======================================================================== */

f32 TotalAreaVTBL4(u32 ShapeCount, shape_base **Shapes)
{
    f32 Accum0 = 0.0f;
    f32 Accum1 = 0.0f;
    f32 Accum2 = 0.0f;
    f32 Accum3 = 0.0f;

    u32 Count = ShapeCount/4;
    while(Count--)
    {
        Accum0 += Shapes[0]->Area();
        Accum1 += Shapes[1]->Area();
        Accum2 += Shapes[2]->Area();
        Accum3 += Shapes[3]->Area();

        Shapes += 4;
    }

    f32 Result = (Accum0 + Accum1 + Accum2 + Accum3);
    return Result;
}