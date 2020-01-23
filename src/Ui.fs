module Ui
    
    open AsyncEventQueue
    open GameState
    open Fable.Import
    open Fable.Core
    open Browser.Dom
    open Browser.Types

    type IAlert =
        abstract snackbar : message:string -> unit

    [<ImportAll("../public/snackbar.js")>]
    let js: IAlert = jsNative

    type Ui(ev:AsyncEventQueue<Message>) =

        let mutable startDom : HTMLDivElement = unbox document.getElementById "start"
        let mutable gameDom : HTMLDivElement = unbox document.getElementById "game"
        let mutable finishDom : HTMLDivElement = unbox document.getElementById "finished"
        let mutable settingsDom : HTMLDivElement = unbox document.getElementById "settings"
        let mutable loadingDom : HTMLDivElement = unbox document.getElementById "loading"

        let mutable cancelLoadingbtn: HTMLButtonElement = unbox document.getElementById "cancel_loading"

        let mutable startBtn: HTMLButtonElement = unbox document.getElementById "start_start"
        let mutable settingsBtn: HTMLButtonElement = unbox document.getElementById "start_settings"

        let mutable gameBoard: HTMLDivElement = unbox document.getElementById "game_board"
        let mutable gameText: HTMLParagraphElement = unbox document.getElementById "game_text"

        let mutable finishedBtn: HTMLButtonElement = unbox document.getElementById "finished_start"
        let mutable finishedText: HTMLParagraphElement = unbox document.getElementById "finished_text"

        let mutable settingsApplyBtn: HTMLButtonElement = unbox document.getElementById "settings_apply"
        let mutable settingsCancelBtn: HTMLButtonElement = unbox document.getElementById "settings_cancel"

        do
            startBtn.addEventListener("click", (fun _ -> ev.Post Load))
            finishedBtn.addEventListener("click", (fun _ -> ev.Post Clear))
            settingsBtn.addEventListener("click", (fun _ -> ev.Post Opts))
            settingsApplyBtn.addEventListener("click", (fun _ -> ev.Post Apply))
            settingsCancelBtn.addEventListener("click", (fun _ -> ev.Post Cancel))
            cancelLoadingbtn.addEventListener("click", (fun _ -> ev.Post Cancel))

        let showDom (dom: HTMLDivElement) =
            for a in [startDom; gameDom; finishDom; settingsDom; loadingDom] do
                a.classList.add "hidden"
            dom.classList.remove "hidden"
        
        let renderPlayingDom (gs: GameState) =
            let elem i n = sprintf "<button id='%d-%d' class='mdl-button mdl-js-button mdl-button--raised mdl-js-ripple-effect'>%d</button>" n i n
            let bucket i n = 
                let elems = [1..n] |> Seq.map (fun n -> elem i n) |> String.concat ""
                "<div class='bucket'>" + elems + "</div>"
            snd (List.fold (fun (i, acc) e -> (i + 1, acc + bucket i e)) (0, "") gs.Data)

        let makePlayingDomInteractive (gs:GameState) =
            [0..(gs.Data.Length - 1)]
            |> List.map (fun i -> [1..(gs.Data.Item i)] |> List.map (fun n -> (n, i)))
            |> List.reduce List.append
            |> List.fold (fun _ (n, i) -> 
                let mutable btn : HTMLButtonElement = unbox document.getElementById (sprintf "%d-%d" n i)
                btn.addEventListener("click", (fun _ -> ev.Post (Turn(n, i)))); ()) ()
        
        member _.SetBuckets (amount: int) =
            let mutable input: HTMLInputElement = unbox document.getElementById "stngs_buckets"
            input.value <- string amount

        member _.SetMatches (amount: int) =
            let mutable input: HTMLInputElement = unbox document.getElementById "stngs_matches"
            input.value <- string amount
        member _.SetEasyMode (b: bool) =
            let mutable input: HTMLInputElement = unbox document.getElementById "stngs_easymode"
            input.``checked`` <- b

        member _.ShowGame = showDom gameDom
        member _.ShowStart = showDom startDom
        member _.ShowLoading = showDom loadingDom
        member _.ShowFinish (text:string) = 
            finishedText.innerText <- text
            showDom finishDom
        member _.ShowSettings = showDom settingsDom


        member _.SettingsBuckets: int =  
            let mutable input: HTMLInputElement = unbox document.getElementById "stngs_buckets"
            try 
                int input.value
            with
                | _ -> 5
        member _.SettingsMatches: int =  
            let mutable input: HTMLInputElement = unbox document.getElementById "stngs_matches"
            try 
                int input.value
            with
                | _ -> 5
        member _.SettingsEasyMode: bool =
            let mutable input: HTMLInputElement = unbox document.getElementById "stngs_easymode"
            try 
                input.``checked``
            with
                | _ -> false

        member a.RenderGame (gs:GameState) (interactive:bool) =
            a.ShowGame
            gameBoard.innerHTML <- renderPlayingDom gs
            if interactive then
                gameText.innerText <- "Your turn..."
                makePlayingDomInteractive gs
            else
                gameText.innerText <- "Enemy thinking..."
                if not gs.IsEasyMode && gs.CanTaunt then 
                    js.snackbar "You will be utterly crushed (:"
                    gs.Taunted
