module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (class, selected, src, style, type_)
import Html.Events exposing (onClick, onInput, onSubmit)
import Task
import Time


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Timer =
    { counter : Int
    , rounds : Int
    , work : Int
    , rest : Int
    , currentRound : Int
    }


type Phase
    = Work Int
    | Rest Int


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
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { step = Setup
      , timer =
            { counter = 1
            , rounds = 2
            , work = 4
            , rest = 2
            , currentRound = 1
            }
      , countdown = 3
      }
    , Cmd.none
    )


type Msg
    = Tick Time.Posix
    | RunTimer
    | TogglePause
    | ShowCountdown
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
                    Debug.log "paused 222" model.step
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

        Tick _ ->
            case model.step of
                Countdown ->
                    if model.countdown <= 0 then
                        ( { model | step = Running (Work model.timer.work) }, Cmd.none )

                    else
                        ( { model | countdown = model.countdown - 1 }
                        , Cmd.none
                        )

                Running phase ->
                    let
                        singleRoundTime =
                            model.timer.work + model.timer.rest

                        totalTime =
                            singleRoundTime * model.timer.rounds

                        currentRoundTime =
                            remainderBy singleRoundTime model.timer.counter

                        currentPhaseTime =
                            case phase of
                                Work _ ->
                                    remainderBy model.timer.work currentRoundTime

                                Rest _ ->
                                    remainderBy model.timer.rest currentRoundTime

                        _ =
                            Debug.log "round, phase" ( currentRoundTime, currentPhaseTime )

                        nextRound =
                            (model.timer.counter > 0)
                                && (0 == remainderBy singleRoundTime model.timer.counter)
                    in
                    if model.timer.counter == (totalTime - model.timer.rest) then
                        ( { model
                            | step = Finished
                          }
                        , Cmd.none
                        )

                    else
                        ( { model
                            | timer = updateTimer nextRound model.timer
                            , step =
                                case phase of
                                    Work _ ->
                                        if currentPhaseTime == 0 && model.timer.counter /= 0 then
                                            Running (Rest currentPhaseTime)

                                        else
                                            Running (Work currentPhaseTime)

                                    Rest _ ->
                                        if currentPhaseTime == 0 then
                                            Running (Work currentPhaseTime)

                                        else
                                            Running (Rest currentPhaseTime)
                          }
                        , Cmd.none
                        )

                _ ->
                    ( model, Cmd.none )


updateTimer nextRound t =
    { t
        | counter = t.counter + 1
        , currentRound =
            if nextRound then
                t.currentRound + 1

            else
                t.currentRound
    }


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
                    [ text
                        (if model.countdown > 0 then
                            String.fromInt model.countdown

                         else
                            "Go!"
                        )
                    ]

            Running phase ->
                let
                    phaseData =
                        case phase of
                            Work t ->
                                ( "Work", model.timer.work - t )

                            Rest t ->
                                ( "Rest", model.timer.rest - t )
                in
                viewRunningTimer model phaseData
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


viewRunningTimer model ( phase, phaseTime ) =
    let
        { work, rounds, currentRound } =
            model.timer

        phaseText =
            phase
                ++ ": "
                ++ String.fromInt
                    (if phaseTime == 0 then
                        -- start with work time from settings
                        work

                     else
                        phaseTime
                    )
    in
    div
        [ class "wrapper" ]
        [ div
            [ class "phase"
            , class ("phase_" ++ String.toLower phase)
            ]
            [ h2 [class "phase-title"] [ text phaseText ]
            , p [ class "round" ]
                [ text ("Round " ++ String.fromInt currentRound ++ " of " ++ String.fromInt rounds) ]
            ]
        , button
            [ onClick TogglePause
            , class "btn-pause"
            ]
            [ text "Pause" ]
        ]


viewTimerForm { rounds, work, rest } =
    form
        [ onSubmit ShowCountdown ]
        [ div [ class "row row_rounds" ]
            [ label [] [ text "Rounds" ]
            , select [ onInput UpdateRoundsQuantity ] (viewRenderOptions 20 rounds)
            ]
        , div [ class "row row_work" ]
            [ label [] [ text "Work" ]
            , select [ onInput UpdateWork ] (viewRenderOptions 240 work)
            ]
        , div [ class "row row_rest" ]
            [ label [] [ text "Rest" ]
            , select [ onInput UpdateRest ] (viewRenderOptions 120 rest)
            ]
        , div [ class "row row_btn row_go" ]
            [ button
                [ type_ "submit"
                ]
                [ text "Go!" ]
            ]
        ]


viewRenderOptions count selectedOptionNum =
    List.map
        (\n -> option [ selected (selectedOptionNum == n) ] [ text (String.fromInt n) ])
        (List.range 1 count)
