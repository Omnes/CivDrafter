module Main exposing (..)

import Html exposing (..) --(Html, button, div, strong, small, text, br, h1, img)
import Html.Events exposing (onClick)
import Html.Attributes exposing (..)
import Random exposing (Seed, generate)
import Random.List exposing (shuffle)
import List.Extra exposing (getAt)
import Json.Decode as Decode
import Json.Decode.Pipeline as Pipeline

import LeaderJson exposing (leaderJson)

main : Program Never Model Msg
main = Html.program { init = init, view = view, update = update, subscriptions = \_ -> Sub.none }


-- MODEL

type alias Leader = {
    id : Int,
    name : String,
    civlization : String,
    banned : Bool,
    protraitUrl : String,
    dlc : Bool
}

type alias Settings = {
    numberOfPlayers : Int,
    civsPerPlayer : Int,
    maxNumberOfPlayers : Int,
    maxNumberOfCivsPerPlayer : Int
}

type alias DraftResult = {
    playerNumber : Int,
    choices : List Int
}

type alias Model = {
    settings : Settings,
    leaders : List Leader,
    drafts : Maybe (List DraftResult),
    players : List String
}

init : (Model, Cmd Msg)
init = 
    (
    Model 
        (Settings 2 3 8 5)
        (getLeadersFromJson leaderJson)
        Nothing
        ["Sam", "Robin", "Viktor", "Banana", "Circle", "Green", "Trumpet", "Ant"]
    , 
    Cmd.none
    )


-- UPDATE

type Msg
  = Draft
  | CreateDraft (List Int)
  | ToggleBanned Int
  | SetPlayers Int
  | SetCivsPerPlayer Int


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Draft ->
        model ! [shuffleLeaders model.leaders CreateDraft]

    CreateDraft shuffledLeaders ->
        { model | drafts = createDrafts model.settings shuffledLeaders } ! []
    
    ToggleBanned id ->
        { model | leaders = (toggleBanned id model.leaders)} ! []

    SetPlayers count ->
        { model | settings = (setPlayers model.settings count), drafts = Nothing} ! []

    SetCivsPerPlayer count -> 
        { model | settings = (setCivsPerPlayer model.settings count), drafts = Nothing} ! []

-- VIEW
renderDraftLeader : Leader -> Html Msg
renderDraftLeader leader = 
    div [class "box"] [
        div [class "media"] [
            div [class "media-left"][
                div [class "image is-48x48"][
                    img [src (leader.protraitUrl)] []
                ]
            ],
            div [class "media-content"][
                div [class "content"][
                    strong [] [text leader.name],
                    br [] [],
                    small [] [text (" " ++ leader.civlization)]
                ]
            ]
        ]
    ]

renderDraft : List Leader -> List String -> DraftResult -> Html Msg
renderDraft leaders players draftResult =
    let
        player = players |> getAt (draftResult.playerNumber-1) |> Maybe.withDefault "Unknown"
    in
        div [class "column"][
            div [class "box"] [
                Html.p [class "tile"] [text (player)],
                div [] (
                    draftResult.choices 
                    |> List.map (getLeaderById leaders)
                    |> List.map renderDraftLeader 
                    )
            ]
        ]

renderDrafts : List Leader -> List String -> Maybe (List DraftResult) -> Html Msg
renderDrafts leaders players draftResults  =
    div [class "columns is-parent has-text-centered"](
    case draftResults of
        Nothing ->
             []
        Just results ->
            results |> List.map (renderDraft leaders players)
    
    )

renderLeader : Leader -> Html Msg
renderLeader leader =
    let 
        classString = "button" ++ (if leader.banned then " is-danger is-outlined" else (if leader.dlc then " is-info is-outlined" else ""))
    in 
    div [class classString, onClick (ToggleBanned leader.id)] [
        div [] [
            {--div[class "image is-16x16"] [
                img [src (portraitBaseUrl++leader.protraitUrl)] []
            ],--}
            strong [] [text (leader.name)],
            small [] [text (" - " ++ leader.civlization)]
        ]
    ] 

renderLeaderBanList : List Leader -> Html Msg
renderLeaderBanList leaders = 
    div [class "box"] (leaders |> List.sortBy (\l -> l.civlization) |> List.map renderLeader)

renderSettingsButton : a -> (a -> msg) -> a -> Html msg
renderSettingsButton selected message i =
    button [class ("button"++(if selected == i then " is-active" else "")), onClick (message i)] [text (toString i)]

renderSettings : Settings -> Html Msg
renderSettings settings = 
    div[class "box"] [
        div [class "level"][
            div[class "level-item"] [
                text "Players",
                div[]( List.range 1 settings.maxNumberOfPlayers |> List.map (renderSettingsButton settings.numberOfPlayers SetPlayers))
            ],
            div[class "level-item"] [
                text "Civs per player",
                div[]( List.range 1 settings.maxNumberOfCivsPerPlayer |> List.map (renderSettingsButton settings.civsPerPlayer SetCivsPerPlayer))
            ]
        ]
    ]


view : Model -> Html Msg
view model =
  div [class "container has-text-centered"]
    [
        h1 [class "title"] [text "Robin's Civ drafter"],
        renderSettings model.settings,
        renderLeaderBanList model.leaders,
        div [class "box"] [button [class "button is-centered", onClick Draft ] [ text "Draft" ]],
        renderDrafts model.leaders model.players model.drafts
    ]

-- UTIL
createDraftForPlayer : List Int -> Int -> Int -> DraftResult
createDraftForPlayer availableLeaders civs player =
    DraftResult player (List.take civs availableLeaders)

getLeaderById : List Leader -> Int -> Leader
getLeaderById leaders id =
    let
        leader = leaders 
        |> List.filter (\le -> id == le.id)
        |> List.head
    in
        case leader of 
            Nothing ->
                Leader 0 "Leader 0" "Civ 0" False "" False
            Just l ->
                l

toggleBanned : Int -> List Leader -> List Leader
toggleBanned id leaders =
    let
        toggle i leader = 
            if i == leader.id then
                {leader | banned = (not leader.banned)}
            else
                leader
    in
        leaders |> List.map (toggle id) 

setPlayers : Settings -> Int -> Settings
setPlayers settings count =
    {settings | numberOfPlayers = count }

setCivsPerPlayer: Settings -> Int -> Settings
setCivsPerPlayer settings count =
    {settings | civsPerPlayer = count }

        
-- COMMANDS

createDrafts : Settings -> List Int -> Maybe (List DraftResult)
createDrafts settings shuffledLeaders =
    List.range 1 settings.numberOfPlayers
    |> List.map (\p -> createDraftForPlayer (shuffledLeaders |> List.drop ((p-1)*settings.civsPerPlayer)) settings.civsPerPlayer p)
    |> Just

shuffleLeaders : List Leader -> (List Int -> Msg) -> Cmd Msg
shuffleLeaders leaders message  = 
    generate message (
        leaders 
        |> List.filter (\l -> l.banned == False) 
        |> List.map (\l -> l.id) 
        |> shuffle
    )

-- JSON
getLeadersFromJson : String -> List Leader
getLeadersFromJson json =
    Decode.decodeString leadersDecoder json |> Result.withDefault []


leadersDecoder : Decode.Decoder (List Leader)
leadersDecoder = Decode.list leaderDecoder

leaderDecoder : Decode.Decoder Leader
leaderDecoder =
  Pipeline.decode Leader
    |> Pipeline.required "id" Decode.int
    |> Pipeline.required "name" Decode.string
    |> Pipeline.required "civilization" Decode.string
    |> Pipeline.hardcoded False
    |> Pipeline.required "protraitUrl" Decode.string
    |> Pipeline.optional "dlc" Decode.bool False

