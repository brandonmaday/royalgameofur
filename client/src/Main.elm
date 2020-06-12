import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Random


-- MAIN


main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }


-- MODEL


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
  }


type Model
  = Lobby (List Player)
  | Game Board


startingBoard =
  let
    stones player img =
      List.repeat 7 (Stone 0 Nothing player img)
  in
    Board
      (List.repeat 4 0)
      1 
      (Human "brandon" "brandon@email")
      AI
      StartingTurn
      PlayerOne
      ((stones PlayerOne "(p1)") ++ (stones PlayerTwo "(p2)"))

init : () -> (Model, Cmd Msg)
init _ =
  (Lobby [], Cmd.none)


-- UPDATE


type Msg
  = Start
  | Roll
  | Rolled Int
  | ClickStone Stone
  | ClickSquare Stone Bool
  | ToLobby


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case model of
    Lobby _ ->
      case msg of
        Start ->
          (Game startingBoard, Cmd.none)

        _ -> (model, Cmd.none)

    Game board ->
      case msg of
        ToLobby ->
          (Lobby [], Cmd.none)

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
              let
                totalRoll =
                  List.sum die

                newStones =
                  board.stones
                    |> List.map (\s ->
                      {s | target = stoneTarget totalRoll board.actionOn board.stones s}
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
                  ( Game { board | dice = die, stones = newStones, stage = Playing Nothing }
                  , Cmd.none
                  )

        ClickStone stone ->
          (Game { board | stage = Playing (Just stone) }, Cmd.none)

        ClickSquare stone doubleMove ->
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
                    ( nextTurn (swapStones (replaceOne s { s | location = 0} movedStones))
                    , Cmd.none
                    )

                  Nothing ->
                    (nextTurn (swapStones movedStones), Cmd.none)

            Nothing ->
              (model, Cmd.none)

        _ -> (model, Cmd.none)



stoneTarget : Int -> Players -> List Stone -> Stone -> Maybe Int
stoneTarget roll actionOn stones stone =
  let
    target = stone.location + roll
    offBoard = target > 15
    onSafe = target == 8
    blocked =
      if target == 15 then
        False
      else
        stones
          |> List.filter (\s -> s.location == target)
          |> List.filter (\s ->
            case s.ownedBy of
              PlayerOne ->
                case actionOn of
                  PlayerOne -> True
                  PlayerTwo -> onSafe

              PlayerTwo ->
                case actionOn of
                  PlayerOne -> onSafe
                  PlayerTwo -> True
            )
          >> List.isEmpty
          >> not
    validTarget =
      if offBoard || blocked then Nothing else Just target
  in
    case actionOn of
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
    Lobby players ->
      div 
        [ style "margin-top" "100px"
        , class "uk-flex"
        , class "uk-flex-center" 
        ]
        [ div [ class "uk-width-1-2" ]
          [ h1 [ class "uk-heading-line", class "uk-text-center" ]
            [ span [] [ text "Royal Game of Ur player lobby" ] ]
            , div 
              [ style "display" "grid" 
              , style "grid-template-columns" "2fr 1fr"
              ]
              [ div []
                [ h4 [] [ text "Connected Players" ]
                ]
              , div []
                [ button ((onClick Start) :: ukButton) [ text "Start Game" ] 
                ]
              ]
          ]
        ]

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

        makeSqr = drawSquare board.stage
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
      in
        div 
          [ style "margin-top" "100px"
          , class "uk-flex"
          , class "uk-flex-center" 
          ]
          [ div [ class "uk-width-1-2" ]
            [ div []
              ( span [ big ] [ text "Player One" ] :: (
                List.map (stoneHtml board.stage) (playerStones PlayerOne (allStones 0))
              ))
            , div 
              [ style "display" "grid" 
              , style "grid-template-columns" "repeat(8, 100px)"
              , style "grid-template-rows" "repeat(3, 100px)"
              ] (topSide ++ middle ++ bottomSide)
            , div []
              ( span [ big ] [ text "Player Two" ] :: (
                List.map (stoneHtml board.stage) (playerStones PlayerTwo (allStones 0))
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
              ]
            ]
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


drawSquare : Stage -> Square -> List Stone -> Bool -> Int -> Html Msg
drawSquare stage kind locStones canTarget loc =
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
          div styles ((span [] [ text squareImg ]) :: (List.map (stoneHtml stage) locStones))


stoneHtml : Stage -> Stone -> Html Msg
stoneHtml stage stone =
  let
    link =
      case stage of
        Playing _ ->
          case stone.target of
            Just _ ->
              [ onClick (ClickStone stone), style "cursor" "pointer" ]
            Nothing -> []
        _ -> []
  in
    span (style "margin-left" "10px" :: link) [ text stone.img ]


playerAction : Board -> List (Html Msg)
playerAction board =
  let
    currentPlayer =
      case board.actionOn of
        PlayerOne -> board.playerOne
        PlayerTwo -> board.playerTwo
    name =
      case currentPlayer of
        Human n _ -> n
        AI -> "Computer"
  in
    case board.stage of
      StartingTurn ->
        [ text (name ++ "'s turn to ")
        , button
          ( (onClick Roll) :: ukButton )
          [ text "Roll!" ]
        ]

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
          ( ukButton ++ [ onClick ToLobby, style "margin-left" "20px"]
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
subscriptions model =
  Sub.none
