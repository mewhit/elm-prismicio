module Examples.Slice exposing (..)

import Prismic exposing (Decoder, Document)
import Prismic.Field as Field
import Prismic.Group as Group
import Prismic.Slice as Slice exposing (Slice)


type alias MyDoc =
    { sections : List Section }


type Section
    = -- The "my-content" slice has a non-repeating zone.
      MyContent Field.StructuredText
    | -- The "my-image-gallery" slice has a repeating zone.
      MyImageGallery (List Field.ImageViews)
    | -- The "my-links-section" slice has both non-repeating and repeating
      -- zones.
      MyLinksSection LinksSection


type alias LinksSection =
    { title : Field.StructuredText
    , links : List Field.Link
    }


myDocDecoder : Decoder Document MyDoc
myDocDecoder =
    Prismic.decode MyDoc
        |> Prismic.custom
            (Prismic.sliceZone "sections" sectionDecoder)


sectionDecoder : Decoder Slice Section
sectionDecoder =
    Slice.oneOf
        [ Slice.slice "my-content"
            -- Decode the non-repeating zone and ignore the repeating zone.
            (\content _ -> MyContent content)
            (Group.field "text" Field.structuredText)
            (Prismic.succeed ())
        , Slice.slice "my-image-gallery"
            -- Ignore the non-repeating zone and decode the repeating zone.
            (\_ images -> MyImageGallery images)
            (Prismic.succeed ())
            (Group.field "image" Field.image)
        , Slice.slice "my-links-section"
            -- Decode both the non-repeating and repeating zones.
            (\title links -> MyLinksSection (LinksSection title links))
            (Group.field "title" Field.structuredText)
            (Group.field "link" Field.link)
        ]
