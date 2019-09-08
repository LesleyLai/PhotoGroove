module PhotoGroove exposing (main)

import Array exposing (Array)
import Browser
import Html exposing (Html, button, div, h1, h3, img, text, label, input)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Random

type alias Photo = { url: String }

type Msg
    = ClickedPhoto String
    | ClickedSize ThumbnailSize
    | ClickedSurpriseMe
    | GotSelectedIndex Int
        
type ThumbnailSize = Small | Medium | Large
    

type alias Model = { photos: List Photo
                   , selectedUrl: String
                   , chosenSize : ThumbnailSize     
                   }

initialModel : Model
initialModel =
    { photos = [ { url = "1.jpeg" }
               , { url = "2.jpeg" }
               , { url = "3.jpeg" }
               ]
    , selectedUrl = "1.jpeg"
    , chosenSize = Medium
    }

urlPrefix : String
urlPrefix =
    "http://elm-in-action.com/"

viewThumbnail : String -> Photo -> Html Msg
viewThumbnail selectedUrl thumb  =
    img
    [ src (urlPrefix ++ thumb.url)
    , classList [ ( "selected", selectedUrl == thumb.url ) ]
    , onClick (ClickedPhoto thumb.url)
    ] []
    
viewSizeChooser : ThumbnailSize -> Html Msg
viewSizeChooser size =
    label []
        [ input [ type_ "radio", name "size", onClick (ClickedSize size) ] []
        , text (sizeToString size)
        ]

sizeToString : ThumbnailSize -> String
sizeToString size =
    case size of
        Small ->
            "small"
        Medium ->
            "med"
        Large ->
            "large"
                
getPhotoUrl : Int -> Array Photo -> String
getPhotoUrl index photoArray =
    case Array.get index photoArray of
        Just photo ->
            photo.url
        Nothing ->
            ""

view : Model -> Html Msg
view model =
    div [ class "content" ]
        [ h1 [] [ text "Photo Groove" ]
        , button
              [ onClick ClickedSurpriseMe ]
              [ text "Surprise Me!" ]
        , h3 [] [ text "Thumbnail Size:" ]
        , div [ id "choose-size" ]
            (List.map viewSizeChooser [ Small, Medium, Large ])
        , div [ id "thumbnails", class (sizeToString model.chosenSize) ]
            (List.map (viewThumbnail model.selectedUrl) model.photos)
        , img
              [ class "large"
              , src (urlPrefix ++ "large/" ++ model.selectedUrl)
              ]
              []
        ]

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let photoArray = Array.fromList model.photos in
    case msg of
        ClickedPhoto url ->
            ({ model | selectedUrl = url }, Cmd.none )
        ClickedSize size ->
            ({ model | chosenSize = size }, Cmd.none )
        ClickedSurpriseMe ->
            ( model, Random.generate GotSelectedIndex (Random.int 0 (Array.length photoArray - 1)) )
        GotSelectedIndex i ->
            ({ model | selectedUrl = getPhotoUrl i photoArray }, Cmd.none )
        
main : Program () Model Msg
main =
    Browser.element
        { init = \flags -> ( initialModel, Cmd.none )
        , subscriptions = \model -> Sub.none
        , view = view
        , update = update
        }
