module AsListOfStrings exposing (petEnum)

import Enum



-- This enum is just a string, so provides no guarantees that an instance of it really is in the enum.
-- The toString function is easy to write.
-- This enum is easy to modify, requiring only that a new item be added to the list.


petEnum =
    Enum.make
        [ "Cat"
        , "Dog"
        , "Snake"
        , "Spider"
        ]
        identity
