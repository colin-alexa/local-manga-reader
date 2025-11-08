module Data.ReaderLocation exposing (..)

import Data.ApiId exposing (ApiId)
import Data.Chapter exposing (Chapter)
import Http
import RemoteData exposing (WebData)


type alias ReaderLocation =
    { seriesChapters : List Chapter, chapter : Chapter, page : Int }


find : (x -> Bool) -> List x -> Maybe x
find test haystack =
    List.foldl
        (\y _ ->
            if test y then
                Just y

            else
                Nothing
        )
        Nothing
        haystack


itemBefore : List x -> x -> Maybe x
itemBefore haystack needle =
    case haystack of
        [] ->
            Nothing

        _ :: [] ->
            Nothing

        x :: y :: rest ->
            if y == needle then
                Just x

            else
                itemBefore rest needle


itemAfter : List x -> x -> Maybe x
itemAfter haystack needle =
    case haystack of
        [] ->
            Nothing

        _ :: [] ->
            Nothing

        x :: y :: rest ->
            if x == needle then
                Just y

            else
                itemAfter rest needle


locationOr404 : ApiId -> Int -> List Chapter -> WebData ReaderLocation
locationOr404 chapterId page chapters =
    let
        foundChapter =
            find (\chapter -> chapter.id == chapterId) chapters
    in
    case foundChapter of
        Just chapter ->
            if chapter.pageCount >= page && page > 0 then
                RemoteData.Success { seriesChapters = chapters, chapter = chapter, page = page }

            else
                RemoteData.Failure <| Http.BadStatus 404

        Nothing ->
            RemoteData.Failure <| Http.BadStatus 404


previousPage : ReaderLocation -> ReaderLocation
previousPage location =
    if location.page == 1 then
        let
            { chapter } =
                previousChapter location
        in
        { location | chapter = chapter, page = chapter.pageCount }

    else
        { location | page = location.page - 1 }


previousChapter : ReaderLocation -> ReaderLocation
previousChapter location =
    let
        chapter =
            itemBefore location.seriesChapters location.chapter
    in
    case chapter of
        Nothing ->
            location

        Just c ->
            { location | chapter = c, page = 1 }


nextPage : ReaderLocation -> ReaderLocation
nextPage location =
    if location.page == location.chapter.pageCount then
        let
            { chapter } =
                nextChapter location
        in
        { location | chapter = chapter, page = 1 }

    else
        { location | page = location.page + 1 }


nextChapter : ReaderLocation -> ReaderLocation
nextChapter location =
    let
        chapter =
            itemAfter location.seriesChapters location.chapter
    in
    case chapter of
        Nothing ->
            location

        Just c ->
            { location | chapter = c, page = 1 }
