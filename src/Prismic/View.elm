module Prismic.View exposing (..)

import Json.Encode as Json
import Html exposing (..)
import Html.Attributes exposing (..)
import Prismic.Types exposing (..)


asHtml : DocumentField -> Html msg
asHtml field =
    case field of
        Text t ->
            span [] [ text t ]

        Date t ->
            span [] [ text t ]

        Number n ->
            span [] [ text (toString n) ]

        Select t ->
            span [] [ text ("<Select> " ++ t) ]

        Color t ->
            span [] [ text ("<Color> " ++ t) ]

        Link l ->
            linkAsHtml l

        Image i ->
            imageAsHtml i.main

        StructuredText fields ->
            div [] (List.map structuredTextFieldAsHtml fields)


structuredTextFieldAsHtml : StructuredTextField -> Html msg
structuredTextFieldAsHtml field =
    case field of
        SSimple simpleField ->
            simpleFieldAsHtml simpleField

        SImage image ->
            imageAsHtml image

        SEmbed embed ->
            embedAsHtml embed


simpleFieldAsHtml : SimpleStructuredTextField -> Html msg
simpleFieldAsHtml field =
    let
        el =
            case field.fieldType of
                Heading1 ->
                    h1

                Heading2 ->
                    h2

                Heading3 ->
                    h3

                Paragraph ->
                    p

                ListItem ->
                    -- TODO: unify ULs?
                    (\attrs childs ->
                        ul [] [ li attrs childs ]
                    )
    in
        el [] [ text field.text ]


imageAsHtml : ImageProperties -> Html msg
imageAsHtml image =
    let
        (Url urlStr) =
            image.url
    in
        img [ src urlStr ] []


embedAsHtml : EmbedProperties -> Html msg
embedAsHtml embed =
    case embed.embedType of
        EmbedVideo ->
            div [ property "innerHTML" (Json.string embed.html) ] []


linkAsHtml : LinkField -> Html msg
linkAsHtml link =
    case link of
      DocumentLink linkedDoc isBroken ->
        pre [] [text (toString linkedDoc)]
      WebLink (Url url) ->
        a [href url] [text url]
