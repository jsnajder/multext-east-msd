multext-east-msd
================
  
Haskell implementation of the MULTEXT-East morphosyntactic descriptions.

MULTEXT-East encodes values of morphosyntatic attributes in a single string,
sing positional encoding. Each attribute is represented by a single letter at a
predefined position, while non-applicable attributes are represented by
hyphens. For details, refer to the [MULTEXT-East](http://nl.ijs.si/ME) home
page.

The library enables the conversion of strings from/to MSD descriptors, setting
and unsetting of MSD attribute values, and wildcard MSD matching. Currently,
only MULTEXT-East Version 3 is supported, covering morphosyntactic descriptions
for Bulgarian, Croatian, Czech, English, Estonian, Hungarian, Lithuanian,
Macedonian, Persian, Polish, Resian, Romanian, Russian, Serbian, Slovak,
Slovene, and Ukrainian. For details, refer to the [MULTEXT-East Version
3](http://nl.ijs.si/ME/V3) home page.

Library documentation is available on
[Hackage](http://hackage.haskell.org/package/multext-east-msd).

