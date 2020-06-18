port module Main exposing (..)


import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Random
import Http
import Json.Decode as JD
import Json.Encode as JE


-- MAIN


main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }


-- PORTS


port ping : (JE.Value -> msg) -> Sub msg 
port pong : JE.Value -> Cmd msg


-- MODEL

type SocketMsg
  = UnknownMsg String
  | IsMultiJoin
  | Gotplayers (List String)
  | Gotmatches (List Match)
  | Joined String
  | Kick
  | RolledAll (List Int)
  | Move Stone Bool


type Player
  = Human String String
  | AI


type Players
  = PlayerOne
  | PlayerTwo


type alias Stone = 
  { location: Int
  , target: Maybe Int
  , ownedBy: Players
  , img: String
  }


type Stage
  = StartingTurn
  | Rolling
  | Playing (Maybe Stone)
  | GameOver


type alias Board =
  { dice : List Int
  , turn : Int
  , playerOne : Player
  , playerTwo : Player
  , stage : Stage
  , actionOn : Players
  , stones : List Stone
  , yourPosition : Players
  }


type alias Match =
  { label : String
  , playerOne : Player
  , playerTwo : Maybe Player
  }


type JoinStage
  = YoureMaking
  | TheyreJoining Player
  | YoureJoining Match


type Model
  = Loading (Maybe String)
  | LoginForm String
  | Multijoin
  | JoiningLobby (Maybe (List Player)) (Maybe (List Match))
  | Lobby (List Player) (List Match)
  | JoiningGame JoinStage
  | Game Board


newBoard : Player -> Player -> Players -> Board
newBoard p1 p2 position =
  let
    stones player img =
      List.repeat 7 (Stone 0 Nothing player img)
  in
    Board
      (List.repeat 4 0)
      1 
      p1
      p2
      StartingTurn
      PlayerOne
      ((stones PlayerOne "(p1)") ++ (stones PlayerTwo "(p2)"))
      position



init : () -> (Model, Cmd Msg)
init _ =
  (Loading Nothing, isLoggedIn)


-- UPDATE


type Msg
  = LoggedIn (Result Http.Error Bool)
  | Ping JE.Value
  | NameChg String
  | LoginPing
  | LoginPong (Result Http.Error (Maybe String))
  | Logout
  | MakeGame
  | JoinGame Match
  | Roll
  | Rolled Int
  | ClickStone Stone
  | ClickSquare Stone Bool
  | QuitGame


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case model of
    Loading _ ->
      case msg of
        LoggedIn result ->
          case result of
            Ok loggedIn ->
              case loggedIn of
                True ->
                  (JoiningLobby Nothing Nothing, pong (JE.object
                    [ ("msg", JE.string "MakeSocket") ]
                  ))

                False ->
                  (LoginForm "", Cmd.none)

            Err _ ->
              (Loading (Just "network error"), Cmd.none)

        LoginPong result ->
          case result of
            Ok maybeErr ->
              case maybeErr of
                Nothing ->
                  (JoiningLobby Nothing Nothing, pong (JE.object
                    [ ("msg", JE.string "MakeSocket") ]
                  ))

                Just err ->
                  (Loading (Just err), Cmd.none)

            Err _ ->
              (Loading (Just "network error"), Cmd.none)

        _ ->
          (model, Cmd.none)

    JoiningLobby maybePlayers maybeMatches ->
      case msg of
        Ping val ->
          case JD.decodeValue decodePing val of
            Ok socketMsg ->
              case socketMsg of
                IsMultiJoin ->
                  (Multijoin, Cmd.none)

                Gotplayers players ->
                  case maybeMatches of
                    Just matches ->
                      players
                        |> List.map makeHuman
                        |> (\p -> (Lobby p matches, Cmd.none))

                    Nothing ->
                      players
                        |> List.map makeHuman
                        |> (\p -> (JoiningLobby (Just p) Nothing, Cmd.none))

                Gotmatches matches ->
                  case maybePlayers of
                    Just players ->
                      (Lobby players matches, Cmd.none)

                    Nothing ->
                      (JoiningLobby Nothing (Just matches), Cmd.none)
                        

                UnknownMsg m ->
                  (Loading (Just <| "Unknown msg: " ++ m), Cmd.none)

                _ ->
                  (model, Cmd.none)

            Err err ->
              (Loading (Just (JD.errorToString err)), Cmd.none)

        _ ->
          (model, Cmd.none)

    LoginForm name ->
      case msg of
        NameChg newName ->
          (LoginForm newName, Cmd.none)

        LoginPing ->
          (Loading Nothing, loginPlayer name)

        _ -> (model, Cmd.none)

    Lobby players matches ->
      case msg of
        JoinGame m ->
          ( JoiningGame (YoureJoining m)
          , pong 
            ( JE.object
              [ ("msg", JE.string "JoinBoard")
              , ("board", JE.string m.label)
              ]
            )
          )

        Ping val ->
          case JD.decodeValue decodePing val of
            Ok socketMsg ->
              case socketMsg of
                Gotplayers newPlayers ->
                  (Lobby (List.map makeHuman newPlayers) matches, Cmd.none)

                Gotmatches newMatches ->
                  (Lobby players newMatches, Cmd.none)


                _ ->
                  (model, Cmd.none)

            Err err ->
              (Loading (Just (JD.errorToString err)), Cmd.none)

        Logout ->
          (model, pong (JE.object [("msg", JE.string "LogMeOut")]))

        MakeGame ->
          ( JoiningGame YoureMaking
          , pong (JE.object [("msg", JE.string "MakeBoard")])
          )

        _ -> (model, Cmd.none)

    JoiningGame stage ->
      case msg of
        Ping val ->
          case JD.decodeValue decodePing val of
            Ok socketMsg ->
              case socketMsg of
                Joined name ->
                  case stage of
                    YoureMaking ->
                      (JoiningGame (TheyreJoining (makeHuman name)), Cmd.none)

                    TheyreJoining p1 ->
                      (Game (newBoard p1 (makeHuman name) PlayerOne)
                      , Cmd.none
                      )

                    YoureJoining match ->
                      (Game
                        ( newBoard
                          match.playerOne
                          (makeHuman name)
                          PlayerTwo
                        )
                      , Cmd.none
                      )

                _ ->
                  (model, Cmd.none)

            Err err ->
              (Loading (Just (JD.errorToString err)), Cmd.none)

        _ ->
          (model, Cmd.none)



    Game board ->
      case msg of
        QuitGame ->
          (JoiningLobby Nothing Nothing, pong (JE.object
            [ ("msg", JE.string "QuitGame") ]
          ))

        Ping val ->
          case JD.decodeValue decodePing val of
            Ok socketMsg ->
              case socketMsg of
                Kick ->
                  ( JoiningLobby Nothing Nothing
                  , pong (JE.object [("msg", JE.string "Refresh")])
                  )

                RolledAll die ->
                  let
                    totalRoll =
                      List.sum die

                    newStones =
                      board.stones
                        |> List.map (\s ->
                          { s | target = stoneTarget totalRoll board s }
                          )

                    withTargets =
                      newStones
                        |> List.filter (\s ->
                          case s.target of
                            Just _ -> True
                            Nothing -> False
                          )
                  in
                    if totalRoll == 0 || (List.isEmpty withTargets) then
                      (newTurn False { board | dice = die }, Cmd.none)
                    else
                      ( Game
                        { board 
                        | dice = die
                        , stones = newStones
                        , stage = Playing Nothing 
                        }
                      , Cmd.none
                      )

                Move stone doubleMove ->
                  case stone.target of
                    Just loc ->
                      let
                        movedStones =
                          replaceOne stone ({ stone | location = loc }) board.stones
                        bumpedStone =
                          if loc >= 5 && loc <= 12 then
                            movedStones
                              |> List.filter
                                (\s ->
                                  case s.ownedBy of
                                    PlayerOne ->
                                      case board.actionOn of
                                        PlayerOne ->
                                          False

                                        PlayerTwo ->
                                          s.location == loc

                                    PlayerTwo ->
                                      case board.actionOn of
                                        PlayerTwo ->
                                          False

                                        PlayerOne ->
                                          s.location == loc
                                )
                              >> List.head
                          else
                            Nothing
                        swapStones newStones =
                          { board | stones = newStones }

                        nextTurn = newTurn doubleMove
                      in
                        case bumpedStone of
                          Just s ->
                            ( nextTurn
                              ( swapStones
                                (replaceOne s { s | location = 0} movedStones)
                              )
                            , Cmd.none
                            )

                          Nothing ->
                            ( nextTurn (swapStones movedStones)
                            , Cmd.none
                            )

                    Nothing ->
                      (model, Cmd.none)

                _ ->
                  (model, Cmd.none)

            Err err ->
              (Loading (Just (JD.errorToString err)), Cmd.none)

        Roll ->
          ( Game { board | stage = Rolling, dice = [] }
          , Cmd.batch
            [ Random.generate Rolled (Random.int 0 1)
            , Random.generate Rolled (Random.int 0 1)
            , Random.generate Rolled (Random.int 0 1)
            , Random.generate Rolled (Random.int 0 1)
            ]
          )

        Rolled roll ->
          let
            die =
              roll :: board.dice
          in
            if List.length die /= 4 then
              (Game {board | dice = die}, Cmd.none)
            else
              ( model
              , pong
                ( JE.object
                  [ ("msg", JE.string "Rolled")
                  , ("roll", JE.list JE.int die)
                  ]
                )
              )

        ClickStone stone ->
          (Game { board | stage = Playing (Just stone) }, Cmd.none)

        ClickSquare stone doubleMove ->
          ( model
          , pong
            ( JE.object 
              [ ("msg", JE.string "Move")
              , ("stone", stoneEncoder stone)
              , ("doubleMove", JE.bool doubleMove)
              ]
            )
          )

        _ -> (model, Cmd.none)

    _ ->
      (model, Cmd.none)


yourTurn : {r|yourPosition: Players, actionOn: Players} -> Bool
yourTurn board =
  case board.actionOn of
    PlayerOne ->
      case board.yourPosition of
        PlayerOne -> True
        PlayerTwo -> False

    PlayerTwo ->
      case board.yourPosition of
        PlayerOne -> False
        PlayerTwo -> True


makeHuman : String -> Player
makeHuman name =
  Human name ""


stoneTarget : Int -> Board -> Stone -> Maybe Int
stoneTarget roll board stone =
  let
    target = stone.location + roll
    offBoard = target > 15
    onSafe = target == 8
    blocked =
      if target == 15 then
        False
      else
        board.stones
          |> List.filter (\s -> s.location == target)
          |> List.filter (\s ->
            case s.ownedBy of
              PlayerOne ->
                case board.actionOn of
                  PlayerOne -> True
                  PlayerTwo -> onSafe

              PlayerTwo ->
                case board.actionOn of
                  PlayerOne -> onSafe
                  PlayerTwo -> True
            )
          >> List.isEmpty
          >> not
    validTarget =
      if offBoard || blocked then Nothing else Just target
  in
    case board.actionOn of
      PlayerOne ->
        case stone.ownedBy of
          PlayerTwo -> Nothing
          PlayerOne -> validTarget

      PlayerTwo ->
        case stone.ownedBy of
          PlayerOne -> Nothing
          PlayerTwo -> validTarget


newTurn : Bool -> Board -> Model
newTurn doubleMove board =
  let
    nextPlayer =
      case board.actionOn of
        PlayerOne -> PlayerTwo
        PlayerTwo -> PlayerOne
    stonesHome =
      board.stones
        |> List.map (\s ->
          case board.actionOn of
            PlayerOne ->
              case s.ownedBy of
                PlayerOne ->
                  if s.location == 15 then 1 else 0
                PlayerTwo -> 0
            PlayerTwo ->
              case s.ownedBy of
                PlayerOne -> 0
                PlayerTwo ->
                  if s.location == 15 then 1 else 0
          )
  in
    if List.sum stonesHome == 7 then
      Game { board | stage = GameOver }
    else
      Game
        { board
        | turn = board.turn + 1
        , actionOn = if doubleMove then board.actionOn else nextPlayer
        , stage = StartingTurn
        }


replaceOne : a -> a -> List a -> List a
replaceOne oldItem newItem items =
  let
    onlyItem = List.filter ((==) oldItem) items
    noItem = List.filter (\i -> i /= oldItem) items
    onlyItemLen = List.length onlyItem
    oneLessItem =
      if onlyItemLen <= 1 then
        []
      else
        List.take (onlyItemLen - 1) onlyItem
  in
    newItem :: (noItem ++ oneLessItem)


-- VIEW


type Square = Blank | Regular | Double | Safe


view : Model -> Html Msg
view model =
  case model of
    Loading maybeErr ->
      case maybeErr of
        Just err ->
          wrapper [ text <| "Loading Error: " ++ err ]

        Nothing ->
          wrapper [ text "Loading..." ]

    JoiningLobby _ _ ->
      wrapper [ text "Joining Lobby..." ]

    Multijoin ->
      wrapper [ text "Joined elsewhere..." ]

    LoginForm name ->
      wrapper
        [ div [] [ text "Please enter your name" ]
        , div []
          [ input 
            [ value name
            , onInput NameChg 
            , class "uk-input"
            , class "uk-form-width-medium"
            ] [] 
          ]
        , div
          [ style "margin-top" "20px"
          ] [ button ((onClick LoginPing) :: ukButton) [ text "Login" ] ]
        ] 

    Lobby players matches ->
      wrapper
        [ div 
          [ style "display" "grid" 
          , style "grid-template-columns" "2fr 1fr"
          ]
          [ div []
            [ h4 [] 
              [ span [] 
                [ text "Matches" 
                , button
                  ( [ onClick MakeGame
                    , class "uk-button-small" 
                    , style "margin-left" "20px" 
                    ] ++ ukButton
                  ) [ text "Make Game" ]
                , button
                  (
                    [ style "margin-left" "20px"
                    , class "uk-button-small" 
                    , onClick Logout
                    ] ++ ukButton
                  )
                  [ text "Logout" ]
                ]
              ]
            , table
              [ class "uk-table"
              , class "uk-table-small"
              , class "uk-table-divider"
              ]
              [ thead []
                [ tr [] 
                  [ td [] [ text "Match" ]
                  , td [] [ text "Player One" ]
                  , td [] [ text "Player Two" ]
                  ]
                ]
              , tbody []
                (matches |> List.map (\m ->
                  let
                    playerOrLink =
                      case m.playerTwo of
                        Just playerTwo ->
                          text (playerName playerTwo)

                        Nothing ->
                          button
                            [ class "uk-button"
                            , class "uk-button-link"
                            , onClick (JoinGame m)
                            ] [ text "Join Game" ]
                  in
                    tr []
                      [ td [] [ text m.label ]
                      , td [] [ text (playerName m.playerOne) ]
                      , td [] [ playerOrLink ]
                      ]
                  )
                )
              ]
            ]
          , div []
            [ h4 [] [ text "Players in Lobby" ]
            , ul [ class "uk-list" ]
              (List.map (\p -> li [] [ text (playerName p) ]) players)
            ]
          ]
        ]

    JoiningGame stage ->
      case stage of
        YoureMaking -> wrapper [ text "Creating game..." ]
        TheyreJoining _ -> wrapper [ text "Awaiting another player..." ]
        YoureJoining _ -> wrapper [ text "Joining game..." ]

    Game board ->
      let
        die =
          ( text "Dice " :: 
            ( List.intersperse (text " ")
              <| List.map (\d -> text (if d == 0 then "( )" else "(+)"))
              <| board.dice
            )
          )
        currentAction = playerAction board
        big = style "font-size" "1.5rem"

        allStones =
          stonesOnSquare board.stones

        targetTop =
          case board.actionOn of
            PlayerOne -> True
            PlayerTwo -> False

        targetBottom = not targetTop

        makeSqr = drawSquare (yourTurn board) board.stage
        sD = makeSqr Double
        sR = makeSqr Regular
        sB = makeSqr Blank
        sS = makeSqr Safe

        playerSide =
          [ (sD, 4), (sR, 3), (sR, 2), (sR, 1), (sB, 99), (sB, 15), (sD, 14), (sR, 13) ]

        topSide =
          playerSide
            |> List.map (\(sqr, loc) -> sqr (playerStones PlayerOne <| allStones loc) targetTop loc)

        bottomSide =
          playerSide
            |> List.map (\(sqr, loc) -> sqr (playerStones PlayerTwo <| allStones loc) targetBottom loc)

        middle =
          [ (sR, 5), (sR, 6), (sR, 7), (sS, 8), (sR, 9), (sR, 10), (sR, 11), (sR, 12) ]
            |> List.map (\(sqr, loc) -> sqr (allStones loc) True loc)

        activeStone =
          stoneHtml (yourTurn board) board.stage
      in
        wrapper
          [ div []
            ( span [ big ] [ text (playerName board.playerOne) ] :: (
              List.map activeStone (playerStones PlayerOne (allStones 0))
            ))
          , div 
            [ style "display" "grid" 
            , style "grid-template-columns" "repeat(8, 100px)"
            , style "grid-template-rows" "repeat(3, 100px)"
            ] (topSide ++ middle ++ bottomSide)
          , div []
            ( span [ big ] [ text (playerName board.playerTwo) ] :: (
              List.map activeStone (playerStones PlayerTwo (allStones 0))
            ))
          , div [ style "margin-top" "40px" ] 
            [ 
              ( case board.stage of
                  Rolling ->
                    div [ big ] currentAction
                  
                  _ ->
                    div []
                      [ div [ big ] die
                      , div [ big, style "margin-top" "20px" ] [ span [] currentAction ]
                      ]
              )
            , div [ style "margin-top" "20px" ] 
              [ text <| "Turn: " ++ String.fromInt board.turn ]
            , div [ style "margin-top" "20px" ] 
              [ button
                [ class "uk-button"
                , class "uk-button-default"
                , onClick QuitGame
                ] [ text "Quit Game" ]
              ]
            ]
          ]
  

wrapper : List (Html msg) -> Html msg
wrapper html =
      div 
        [ style "margin-top" "40px"
        , class "uk-flex"
        , class "uk-flex-center" 
        ]
        [ div [ class "uk-width-2-3" ]
          ( [ h1 [ class "uk-heading-line", class "uk-text-center" ]
              [ span [] [ text "Royal Game of Ur" ] ]
            ] ++ html
          )
        ]


stonesOnSquare : List Stone -> Int -> List Stone
stonesOnSquare allStones loc =
  List.filter (\s -> s.location == loc) allStones
  

playerStones : Players -> List Stone -> List Stone
playerStones forPlayer allStones =
  let
    forPlayerOne =
      (\s ->
        case s.ownedBy of
          PlayerOne -> True
          PlayerTwo -> False
      )
  in
    case forPlayer of 
      PlayerOne ->
        List.filter forPlayerOne allStones

      PlayerTwo ->
        List.filter (not << forPlayerOne) allStones


drawSquare : Bool -> Stage -> Square -> List Stone -> Bool -> Int -> Html Msg
drawSquare onYou stage kind locStones canTarget loc =
  let
    baseStyle = 
      [ style "display" "flex"
      , style "justify-content" "center"
      , style "align-items" "center"
      , style "flex-wrap" "wrap"
      ]
    squareImg =
      case kind of
        Double -> "R"
        Safe -> "[R]"
        _ -> ""

    canDouble =
      case kind of
        Double -> True
        Safe -> True
        _ -> False

    maybeTarget =
      case stage of
        Playing maybeTargeting ->
          case maybeTargeting of
            Just targetingStone ->
              case targetingStone.target of
                Just t ->
                  if t == loc then Just targetingStone else Nothing
                Nothing -> Nothing
            Nothing -> Nothing

        _ -> Nothing

    regularOutline =
      case kind of
        Blank ->
          baseStyle
        _ ->
          ((style "border" "1px solid black") :: baseStyle)

    styles =
      case maybeTarget of
        Just targetingStone ->
          if canTarget then
            [ style "border" "3px solid blue"
            , onClick (ClickSquare targetingStone canDouble)
            , style "cursor" "pointer"
            ] ++ baseStyle
          else
            regularOutline

        Nothing ->
          regularOutline
  in
    if List.isEmpty locStones then
      div styles [ text squareImg ]
    else
      case maybeTarget of
        Just _ ->
          div styles [ text (squareImg ++ 
            (List.foldr (\c a -> a ++ " " ++ c.img) "" locStones))
          ]

        Nothing ->
          div styles
            (
              (span [] [ text squareImg ])
              :: (List.map (stoneHtml onYou stage) locStones)
            )


stoneHtml : Bool -> Stage -> Stone -> Html Msg
stoneHtml onYou stage stone =
  let
    link =
      case stage of
        Playing _ ->
          case stone.target of
            Just _ ->
              if onYou then
                [ onClick (ClickStone stone), style "cursor" "pointer" ]
              else
                []
            Nothing -> []
        _ -> []
  in
    span (style "margin-left" "10px" :: link) [ text stone.img ]


playerName : Player -> String
playerName p =
  case p of
    Human n _ -> n
    AI -> "Computer"


playerAction : Board -> List (Html Msg)
playerAction board =
  let
    currentPlayer =
      case board.actionOn of
        PlayerOne -> board.playerOne
        PlayerTwo -> board.playerTwo
    name =
      playerName currentPlayer
  in
    case board.stage of
      StartingTurn ->
        if yourTurn board then
          [ text (name ++ "'s turn to ")
          , button
            ( (onClick Roll) :: ukButton )
            [ text "Roll!" ]
          ]
        else
          [ text (name ++ "'s turn to roll!") ]

      Rolling ->
        [ text (name ++ " is rolling...") ]

      Playing maybeStone ->
        case maybeStone of
          Just _ ->
            [ text (name ++ "'s turn to place a piece") ]

          Nothing ->
            [ text (name ++ "'s turn to move a piece") ]

      GameOver ->
        [ text (name ++ " has won!!")
        , button
          ( ukButton ++ [ onClick QuitGame, style "margin-left" "20px"]
          ) [ text "Back to lobby" ]
        ]

ukButton : List (Attribute msg)
ukButton =
  [ class "uk-button"
  , class "uk-button-primary"
  , class "uk-button-small"
  , style "border-radius" "5px"
  ]


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
  ping Ping


-- HTTP


decodePlayer : JD.Decoder Player
decodePlayer =
  JD.map2 Human
    (JD.string)
    (JD.succeed "")


decodeMatch : JD.Decoder Match
decodeMatch =
  JD.field "open" JD.bool
    |> JD.andThen (\open ->
      let
        always =
          JD.map3 Match
            (JD.field "label" JD.string)
            (JD.field "p1" decodePlayer)
      in
        case open of
          True ->
            always (JD.succeed Nothing)

          False ->
            always (JD.field "p2" (JD.map Just decodePlayer))
      )

decodePing : JD.Decoder SocketMsg
decodePing =
  JD.field "msg" JD.string
    |> JD.andThen (\msg ->
      case msg of
        "MULTIJOIN" ->
          JD.succeed IsMultiJoin

        "GOTPLAYERS" ->
          JD.map Gotplayers (JD.field "players" (JD.list JD.string))

        "GOTGAMES" ->
          JD.map Gotmatches (JD.field "games" (JD.list decodeMatch))

        "JOINED" ->
          JD.map Joined (JD.field "playerName" JD.string)

        "KICK" ->
          JD.succeed Kick

        "Rolled" ->
          JD.map RolledAll
            (JD.field "roll" (JD.list JD.int))

        "Move" ->
          JD.map2 Move
            (JD.field "stone" stoneDecoder)
            (JD.field "doubleMove" JD.bool)

        _ ->
          JD.succeed (UnknownMsg msg)
    )


isLoggedIn : Cmd Msg
isLoggedIn =
  Http.get
    { url = "/authenticated"
    , expect = Http.expectJson LoggedIn JD.bool
    }


loginPlayer : String -> Cmd Msg
loginPlayer name =
  Http.post
    { url = "/login"
    , body = Http.jsonBody (JE.string name)
    , expect = Http.expectJson LoginPong loginDecoder
    }


loginDecoder : JD.Decoder (Maybe String)
loginDecoder =
  JD.field "ok" JD.bool
    |> JD.andThen (\ok ->
      case ok of
        True ->
          JD.succeed Nothing

        False ->
          JD.map Just (JD.field "data" JD.string)
    )


targetDecoder : JD.Decoder (Maybe Int)
targetDecoder =
  JD.int
    |> JD.andThen (\t ->
        if t == -1 then JD.succeed Nothing else JD.succeed (Just t)
      )


playersDecoder : JD.Decoder Players
playersDecoder =
  JD.int
    |> JD.andThen (\p ->
      if p == 1 then JD.succeed PlayerOne else JD.succeed PlayerTwo
    )


stoneDecoder : JD.Decoder Stone
stoneDecoder =
  JD.map4 Stone
    (JD.field "location" JD.int)
    (JD.field "target" targetDecoder)
    (JD.field "ownedBy" playersDecoder)
    (JD.field "img" JD.string)


-- ENCODERS


playersEncoder : Players -> JE.Value
playersEncoder players =
  case players of
    PlayerOne -> JE.int 1
    PlayerTwo -> JE.int 2


stoneEncoder : Stone -> JE.Value
stoneEncoder stone =
  let
    targetEncoder =
      case stone.target of
        Just t ->
          JE.int t

        Nothing ->
          JE.int -1
  in
    JE.object
      [ ("location", JE.int stone.location)
      , ("target", targetEncoder)
      , ("ownedBy", playersEncoder stone.ownedBy)
      , ("img", JE.string stone.img)
      ]
