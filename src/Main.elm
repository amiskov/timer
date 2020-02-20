port module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (attribute, class, selected, src, style, type_)
import Html.Events exposing (onClick, onInput, onSubmit)
import Time


port loadSample : String -> Cmd msg


port play : String -> Cmd msg


type Sound
    = StartWork
    | StartRest


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Timer =
    { rounds : Int
    , work : Int
    , rest : Int
    , circuits : Int
    , circuitRest : Int
    }


type Phase
    = Work Int
    | Rest Int
    | RestBetweenCircuits Int


type Step
    = Setup
    | Countdown
    | Running Phase
    | Paused Step
    | Finished


type alias Model =
    { step : Step
    , timer : Timer
    , countdown : Int
    , currentRound : Int
    , currentCircuit : Int
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { step = Setup
      , timer =
            { rounds = 10
            , work = 40
            , rest = 20
            , circuits = 4
            , circuitRest = 120
            }
      , currentRound = 1
      , currentCircuit = 1
      , countdown = 0
      }
    , Cmd.batch [ loadSample "alert", loadSample "bell" ]
    )


type Msg
    = Tick Time.Posix
    | RunTimer
    | TogglePause
    | ShowCountdown
    | UpdateCircuits String
    | UpdateCircuitRest String
    | UpdateRoundsQuantity String
    | UpdateWork String
    | UpdateRest String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RunTimer ->
            ( { model | step = Running (Work model.timer.work) }
            , Cmd.none
            )

        TogglePause ->
            let
                _ =
                    Debug.log "paused" model.step
            in
            case model.step of
                Paused s ->
                    ( { model | step = s }
                    , Cmd.none
                    )

                _ ->
                    ( { model | step = Paused model.step }
                    , Cmd.none
                    )

        ShowCountdown ->
            ( { model | step = Countdown }
            , Cmd.none
            )

        UpdateRoundsQuantity r ->
            let
                upd t v =
                    { t | rounds = v }
            in
            ( { model | timer = updateTimerSetting model.timer upd r }
            , Cmd.none
            )

        UpdateRest newRest ->
            let
                upd t v =
                    { t | rest = v }
            in
            ( { model | timer = updateTimerSetting model.timer upd newRest }
            , Cmd.none
            )

        UpdateWork newWork ->
            let
                upd t v =
                    { t | work = v }
            in
            ( { model | timer = updateTimerSetting model.timer upd newWork }
            , Cmd.none
            )

        UpdateCircuits newCircuits ->
            let
                upd t v =
                    { t | circuits = v }
            in
            ( { model | timer = updateTimerSetting model.timer upd newCircuits }
            , Cmd.none
            )

        UpdateCircuitRest newValue ->
            let
                upd t v =
                    { t | circuitRest = v }
            in
            ( { model | timer = updateTimerSetting model.timer upd newValue }
            , Cmd.none
            )

        Tick _ ->
            case model.step of
                Countdown ->
                    if model.countdown <= 0 then
                        ( { model | step = Running (Work model.timer.work) }
                        , play "bell"
                        )

                    else
                        ( { model | countdown = model.countdown - 1 }
                        , Cmd.none
                        )

                Running phase ->
                    case phase of
                        Work t ->
                            case t of
                                0 ->
                                    if model.currentRound == model.timer.rounds then
                                        if model.currentCircuit == model.timer.circuits then
                                            ( { model | step = Finished }
                                            , play "finalSong"
                                            )

                                        else
                                            ( { model
                                                | step = Running (RestBetweenCircuits model.timer.circuitRest)
                                              }
                                            , play "alert"
                                            )

                                    else
                                        ( { model | step = Running (Rest model.timer.rest) }
                                        , play "alert"
                                        )

                                _ ->
                                    ( { model | step = Running (Work (t - 1)) }
                                    , Cmd.none
                                    )

                        Rest t ->
                            if t == 0 then
                                ( { model
                                    | step = Running (Work model.timer.work)
                                    , currentRound = model.currentRound + 1
                                  }
                                , play "bell"
                                )

                            else
                                ( { model | step = Running (Rest (t - 1)) }, Cmd.none )

                        RestBetweenCircuits t ->
                            if t == 0 then
                                ( { model
                                    | step = Running (Work model.timer.work)
                                    , currentRound = 1
                                    , currentCircuit = model.currentCircuit + 1
                                  }
                                , play "bell"
                                )

                            else
                                ( { model | step = Running (RestBetweenCircuits (t - 1)) }
                                , Cmd.none
                                )

                _ ->
                    ( model, Cmd.none )


updateTimerSetting timer updater newValue =
    let
        newVal =
            Maybe.withDefault 0 (String.toInt newValue)
    in
    updater timer newVal


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.step of
        Running _ ->
            Time.every 1000 Tick

        Countdown ->
            Time.every 1000 Tick

        _ ->
            Sub.none


view : Model -> Html Msg
view model =
    div
        []
        [ case model.step of
            Setup ->
                viewTimerForm model.timer

            Finished ->
                viewFinished model

            Paused _ ->
                viewPaused model

            Countdown ->
                div [ class "countdown" ]
                    (if model.countdown > 0 then
                        [ text (String.fromInt model.countdown) ]

                     else
                        [ text "Go!" ]
                    )

            Running phase ->
                viewRunningTimer model phase
        ]


viewFinished model =
    div []
        [ h1 [] [ text "Well Done!" ]
        , div [ class "final-image" ]
            [ img [ src "./img/arny_thumbs_up.jpg" ] []
            ]
        ]


viewPaused model =
    div [ class "paused" ]
        [ h2 [] [ text "Paused" ]
        , div [ class "row row_btn row_btn-paused" ]
            [ button [ onClick TogglePause ] [ text "Continue" ]
            ]
        ]


viewRunningTimer model phase =
    let
        ( phaseText, phaseTime ) =
            case phase of
                Work t ->
                    ( "Work", t )

                Rest t ->
                    ( "Rest", t )

                RestBetweenCircuits t ->
                    ( "Big Rest", t )
    in
    div
        [ class "wrapper" ]
        [ div
            [ class "phase"
            , class ("phase_" ++ String.toLower phaseText)
            ]
            [ h2 [ class "phase-title" ]
                [ if phaseTime == 0 then
                    s [] [ text phaseText ]

                  else
                    text (phaseText ++ ": " ++ String.fromInt phaseTime)
                ]
            , p [ class "round" ]
                [ text
                    ("Round "
                        ++ String.fromInt model.currentRound
                        ++ " of "
                        ++ String.fromInt model.timer.rounds
                    )
                ]
            ]
        , button
            [ onClick TogglePause
            , class "btn-pause"
            ]
            [ text "Pause" ]
        ]


viewTimerForm { rounds, work, rest, circuits, circuitRest } =
    form
        [ onSubmit ShowCountdown ]
        [ section [ class "repeats" ]
            [ section [ class "rounds" ]
                [ section [ class "phases" ]
                    [ div [ class "row row_work" ]
                        [ label [] [ text "Work" ]
                        , select [ onInput UpdateWork ] (viewRenderOptions 1 240 work)
                        ]
                    , div [ class "row row_rest" ]
                        [ label [] [ text "Rest" ]
                        , select [ onInput UpdateRest ] (viewRenderOptions 1 120 rest)
                        ]
                    ]
                , div [ class "row row_rounds" ]
                    [ label [] [ text "Rounds" ]
                    , select [ onInput UpdateRoundsQuantity ] (viewRenderOptions 1 20 rounds)
                    ]
                ]
            , div [ class "row row_repeat" ]
                [ label [] [ text "Repeat" ]
                , select [ onInput UpdateCircuits ] (viewRenderOptions 1 10 circuits)
                , label [ class "label_repeat" ] [ text "circuits with" ]
                , select [ onInput UpdateCircuitRest ] (viewRenderOptions 0 200 circuitRest)
                , label [ class "label_repeat" ] [ text "rest in between" ]
                ]
            ]
        , div [ class "row row_btn row_go" ]
            [ button
                [ type_ "submit"
                ]
                [ text "Go!" ]
            ]

        -- , div [ class "dots" ]
        --     (drawCircuit (drawRoundDots rounds work rest) circuits circuitRest)
        ]


drawCircuit roundDots c r =
    List.map
        (\_ ->
            div [ class "circuit-with-rest" ]
                [ div [ class "circuit-rounds" ]
                    roundDots
                , div
                    [ class "circuit-rest"
                    , class "dot"
                    , class "rest"
                    , style "width" (String.fromInt r ++ "px")
                    ]
                    []
                ]
        )
        (List.range 1 c)


drawRoundDots n w r =
    List.map
        (\_ ->
            div [ class "round-scheme" ]
                [ div
                    [ class "dot"
                    , class "work"
                    , style "width" (String.fromInt w ++ "px")
                    ]
                    []
                , div
                    [ class "dot"
                    , class "rest"
                    , style "width" (String.fromInt r ++ "px")
                    ]
                    []
                ]
        )
        (List.range 1 n)


viewRenderOptions start end selectedOptionNum =
    List.map
        (\n -> option [ selected (selectedOptionNum == n) ] [ text (String.fromInt n) ])
        (List.range start end)
