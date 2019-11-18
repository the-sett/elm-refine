module AsGuardedType exposing (petEnum)

import Enum



-- The Pet type could be made opaque so that outside of its defining module, new values cannot be added to the enum
-- The toString function is easy to write.
-- This enum is easy to modify, requiring only that a new item be added to the list.


type Pet
    = Pet String


petEnum =
    Enum.make
        [ Pet "Cat"
        , Pet "Dog"
        , Pet "Snake"
        , Pet "Spider"
        ]
        (\Pet kind -> kind)
