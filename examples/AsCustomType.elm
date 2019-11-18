module AsCustomType exposing (petEnum)

import Enum



-- This way gives you a custom type, which can be used to branch on in code with case of.


type Pet
    = Cat
    | Dog
    | Snake
    | Spider


petEnum =
    Enum.make
        [ Cat
        , Dog
        , Snake
        , Spider
        ]
        (\pet ->
            case pet of
                Cat ->
                    "Cat"

                Doc ->
                    "Dog"

                Snake ->
                    "Snake"

                Spider ->
                    "Spider"
        )
