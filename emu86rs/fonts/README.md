# Fonts - Overview

Fonts were selected from Google fonts as well as fontspace.com. I chose to use
the static fonts instead of variable fonts, as I don't know if ab_glyph supports
variable fonts.

Fonts can be either .otf or .ttf; both seem to be supported in ab_glyph.

## Fonts looked at

* KodeMono - It's a nice futuristic-looking monospaced font. My favorite so far.
* NaturalMono
* Hack
* Excluded - A cool-looking font. The italic version is better, because the
characters don't look so smushed together.
* JetBrainsMono - It's probably the easiest to read of the lot.
* Excluded - A cool-looking font, but only upper-case and can't expand beyond
ascii.
* Instruction - It's one of the nicest-looking fonts I've found, but it has no
lowercase.
* Opensticks - It is a fun font, but I could not reduce the size. It was 250 kb.


## Reduced Font Sizes

To reduce font file size, I trimmed down the fonts to be ascii only.

I used the Python `fonttools` package:

```shell
$ pip install fonttools
$ pyftsubset JetBrainsMono-Regular.ttf --output-file=JetBrainsMono-Regular-ascii.ttf --unicodes=U+0020-007E
```

See https://stackoverflow.com/a/66238628/1416379.


## Font terminology

* Glyph - Item in font (character)
* Baseline - The imaginary line the glyphs rest on.
* Ascent - The recommended distance above baseline for the tallest glyph in the font.
* Descent - The recommended distance below the baseline befor the next line (single-spaced).
* Bottom - The maximum distance below baseline for lowest glyph in the font.
* Top - The maximum distance above baseline for the tallest glyph in the font.
* Leading - The recommended additional space between lines of text.
* Line Spacing - The height from baseline to baseline.
* Kerning/Kern - The adjustment of space between two characters to make them look more natural in non-monospaced fonts. For example: dj, le vs. dj, le
* Line gap - 
* Font height - ascent - descent (assuming descent is a negative value relative to baseline)

References:
* https://stackoverflow.com/questions/27631736/meaning-of-top-ascent-baseline-descent-bottom-and-leading-in-androids-font
* https://www.fonts.com/content/learning/fontology/level-2/text-typography/line-spacing-for-text
* https://www.fonts.com/content/learning/fontology/glossary/k


