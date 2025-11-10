module ReaderLocationTests exposing (..)

import Data.ApiId exposing (ApiId(..))
import Data.Chapter exposing (Chapter)
import Data.ReaderLocation exposing (..)
import Expect
import Fuzz exposing (Fuzzer, intAtLeast, intRange)
import Request exposing (getChapter)
import Test exposing (..)


getChapter : Int -> Chapter
getChapter id =
    { id = ApiId id
    , seriesId = ApiId 1
    , number = id
    , name = "Chapter " ++ String.fromInt id
    , pageCount = 20
    , tags = []
    , volumeNumber = Nothing
    , summary = Nothing
    }


chapters : List Chapter
chapters =
    List.map
        getChapter
        (List.range 1 100)


randomLocation : Fuzzer ReaderLocation
randomLocation =
    Fuzz.map2
        (\chapter pageNumber ->
            { seriesChapters = chapters
            , chapter = getChapter chapter
            , page = pageNumber
            }
        )
        (intRange 2 99)
        (intRange 2 19)


readerLocationTests : Test
readerLocationTests =
    describe "ReaderLocation"
        [ describe "nextPage"
            [ test "at the end of the chapter" <|
                \_ ->
                    let
                        location =
                            { seriesChapters = chapters, chapter = getChapter 4, page = 20 }
                    in
                    nextPage location
                        |> Expect.equal { seriesChapters = chapters, chapter = getChapter 5, page = 1 }
            , test "at the end of the last chapter" <|
                \_ ->
                    let
                        location =
                            { seriesChapters = chapters, chapter = getChapter 100, page = 20 }
                    in
                    nextPage location
                        |> Expect.equal location
            , fuzz randomLocation "mid chapter locations" <|
                \location ->
                    nextPage location |> Expect.equal { location | page = location.page + 1 }
            , fuzz randomLocation "ends of chapters" <|
                \location ->
                    nextPage { location | page = 20 }
                        |> Expect.equal { location | page = 1, chapter = getChapter (location.chapter.number + 1) }
            ]
        , describe "previousPage"
            [ test "at the start of the chapter" <|
                \_ ->
                    let
                        location =
                            { seriesChapters = chapters, chapter = getChapter 3, page = 1 }
                    in
                    previousPage location |> Expect.equal { location | chapter = getChapter 2, page = 20 }
            , test "at the start of the first chapter" <|
                \_ ->
                    let
                        location =
                            { seriesChapters = chapters, chapter = getChapter 1, page = 1 }
                    in
                    previousPage location
                        |> Expect.equal location
            , fuzz randomLocation "mid chapter locations" <|
                \location ->
                    previousPage location |> Expect.equal { location | page = location.page - 1 }
            , fuzz randomLocation "starts of chapters" <|
                \location ->
                    previousPage { location | page = 1 }
                        |> Expect.equal { location | page = 20, chapter = getChapter (location.chapter.number - 1) }
            ]
        , describe "utils"
            (let
                exampleList =
                    List.range 1 30
             in
             [ describe "list find"
                [ fuzz (intRange 1 30) "finds item" <|
                    \i ->
                        find (\x -> x == i) exampleList
                            |> Expect.equal (Just i)
                , fuzz (intAtLeast 31) "doesn't find missing item" <|
                    \i ->
                        find (\x -> x == i) exampleList
                            |> Expect.equal Nothing
                , test "impossible test fails" <|
                    \_ ->
                        find (\_ -> False) exampleList
                            |> Expect.equal Nothing
                , test "trivial test gives end item" <|
                    \_ ->
                        find (\_ -> True) exampleList
                            |> Expect.equal (Just 30)
                ]
             , describe "itemBefore"
                [ test "before first item" <|
                    \_ -> itemBefore exampleList 1 |> Expect.equal Nothing
                , test "before last item" <|
                    \_ -> itemBefore exampleList 30 |> Expect.equal (Just 29)
                , fuzz (intRange 2 30) "random items in list" <|
                    \i -> itemBefore exampleList i |> Expect.equal (Just (i - 1))
                , fuzz (intAtLeast 31) "items not in list" <|
                    \i -> itemBefore exampleList i |> Expect.equal Nothing
                ]
             , describe "itemAfter"
                [ test "after first item" <|
                    \_ -> itemAfter exampleList 1 |> Expect.equal (Just 2)
                , test "after last item" <|
                    \_ -> itemAfter exampleList 30 |> Expect.equal Nothing
                , fuzz (intRange 1 29) "random items in list" <|
                    \i -> itemAfter exampleList i |> Expect.equal (Just (i + 1))
                , fuzz (intAtLeast 31) "items not in list" <|
                    \i -> itemAfter exampleList i |> Expect.equal Nothing
                ]
             ]
            )
        ]
