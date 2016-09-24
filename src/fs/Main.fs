namespace Daedalus

open System
open System.Collections.Generic

open Fable.Core
open Fable.Core.JsInterop
open Fable.Import
open Fable.Helpers.Virtualdom
open Fable.Helpers.Virtualdom.App
open Fable.Helpers.Virtualdom.Html

open Daedalus.Js

module Main =
    type Event = 
        | Init | Teleport | Tick 
        | MouseMove of double*double 
        | MouseClick
    
    let SCALE = (250.0, 150.0)
    let BOOTXY = (20.0, 125.0)
    let COUNTDOWN score = 
        10 + Random.random() * 20 + (20 - score) |> int |> max 10
    let MAXSTRENGTH = 150.0

    type Model = {
        Mouse: double*double
        Boot: double*double
        Cat: double*double
        BootMoving: bool
        CatDead: bool
        BootAngle: double
        BootRotation: double
        BootVector: double*double
        Countdown: int
        Score: int
    }

    let randomCatLocation () = 
        let sx, sy = SCALE
        let x = (sx*0.5) + Random.random() * sx * 0.5
        let y = (sy*0.1) + Random.random() * sy * 0.8
        (x, y)

    let newModel () = {
        Mouse = (0.0, 0.0)
        Boot = BOOTXY
        BootMoving = false
        Cat = randomCatLocation ()
        CatDead = false
        BootAngle = 0.0
        BootRotation = 20.0 
        BootVector = (10.0, -10.0)
        Countdown = COUNTDOWN 0
        Score = 0
    }

    let refreshModel model =
        
        let score = model.Score 
        { newModel () with
            Mouse = model.Mouse  
            Countdown = COUNTDOWN score
            Score = score 
        }

    let friction = 0.1
    let gravity = 1.0

    let min a b = if a <= b then a else b
    let max a b = if a >= b then a else b
    let rec wrap l v = if v > l then wrap l (v - l) else if v < 0.0 then wrap l (v + l) else v

    let launch model = 
        let bx, by = model.Boot
        let mx, my = model.Mouse
        let dx = mx - bx
        let dy = my - by
        let v = (dx / 7.0, dy / 7.0)
        let f = Math.Sqrt(dx*dx + dy*dy)
        { model with
            BootMoving = true
            BootVector = v
            BootRotation = f / 7.0
        }

    let glide model = 
        let x, y = model.Boot 
        let dx, dy = model.BootVector
        let r = model.BootAngle
        let dr = model.BootRotation
        let cx, cy = model.Cat

        let hit = 
            let dx = x - cx
            let dy = y - cy
            Math.Sqrt(dx*dx + dy*dy) < 10.0
        
        let longDead = model.CatDead || hit
        let justDead = not model.CatDead && hit

        { model with
            CatDead = longDead
            Score = if justDead then model.Score + 1 else model.Score
            Boot = (x + dx, y + dy)
            BootAngle = r + dr |> wrap 360.0
            BootVector = (dx - friction |> max 0.0, dy + gravity) 
        }

    let teleport model =
        let countdown = model.Countdown - 1
        if model.CatDead || model.BootMoving then model
        elif countdown > 0 then { model with Countdown = countdown }
        else { model with Cat = randomCatLocation (); Countdown = COUNTDOWN model.Score }
    
    let normalizeMouse x y =
        let sx, sy = BOOTXY
        let dx, dy = x - sx, y - sy
        let r = Math.Sqrt(dx*dx + dy*dy)
        let max = MAXSTRENGTH
        if r <= max then (x, y)
        else 
            let f = r / max
            let dx, dy = dx / f, dy / f
            (sx + dx, sy + dy)

    let nextModelTick model =
        if model.BootMoving then model |> glide else model
        |> teleport

    let nextModelUI model event = 
        match event with
        | MouseMove (x, y) -> { model with Mouse = normalizeMouse x y }
        | MouseClick -> model |> launch
        | _ -> model

    let nextTick model push = 
        let x, y = model.Boot
        let w, h = SCALE
        if y > 2.0 * w || x > 2.0 * h then 
            push Init
        else
            Browser.window.setTimeout((fun () -> push Tick), 1000.0 / 20.0)
            |> ignore

    let update model event =
        match event with
        | Init -> refreshModel model, [nextTick model]
        | Tick -> nextModelTick model, [nextTick model] 
        | _ -> nextModelUI model event, []

    let view model =
        let a = Html.Attributes.attribute
        let e = Svg.svgElem
        let width w = a "width" (string w)
        let height h = a "height" (string h)
        let viewbox x y w h = a "viewBox" (sprintf "%s %s %s %s" (string x) (string y) (string w) (string h))
        let rect x y w h c = e "rect" [a "x" (string x); a "y" (string y); width w; height h; a "fill" c] []

        let load url id =
            e "g" [a "id" id] [
                e "image" [width 100; height 100; a "href" url] []
            ]

        let boot () =
            let x, y = model.Boot
            let r = model.BootAngle
            let translation = sprintf "translate(%g %g)" x y
            let rotation = sprintf "scale(0.2) rotate(%g 25 25)" r
            e "g" [a "transform" translation] [
                e "g" [a "transform" "translate(-5 -5)"] [
                    e "use" [a "href" "#boots"; a "transform" rotation] []
                ]
            ]

        let cat () = 
            let x, y = model.Cat
            let translation = sprintf "translate(%g %g)" x y
            e "g" [a "transform" translation] [
                e "use" [a "href" "#cat"; a "transform" "scale(0.15) translate(-50 -70)"] []
            ]

        let boom () = 
            let x, y = model.Cat
            let translation = sprintf "translate(%g %g)" x y
            e "g" [a "transform" translation] [
                e "use" [a "href" "#boom"; a "transform" "scale(0.15) translate(-50 -50)"] []
            ]

        let scaleX, scaleY = SCALE
        
        let aim () = 
            if model.BootMoving then e "g" [] []
            else
                let sx, sy = model.Boot
                let ex, ey = model.Mouse
                e "path" [a "d" (sprintf "M %g %g L %g %g" sx sy ex ey); a "stroke" "yellow"] []

        let score () =
            e "text" [a "x" "20"; a "y" "20"] [text (string model.Score)] 

        let title () = 
            e "text" [a "x" "150"; a "y" "20"] [text ("Angry Boot")]

        let mouseMove = 
            onMouseMove (fun e ->
                let x = (e ? clientX |> unbox<double>) * scaleX / 1000.0
                let y = (e ? clientY |> unbox<double>) * scaleY / 600.0
                MouseMove (x, y)
            )
        
        let mouseClick = 
            onMouseClick (fun e -> MouseClick)

        div [a "id" "container"; width 1000; height 600] [
            Svg.svg [a "id" "content"; viewbox 0 0 scaleX scaleY] [
                e "defs" [] [
                    load "Boots.svg" "boots"
                    load "Cat.svg" "cat"
                    load "Explosion.svg" "boom"
                ]
                score ()
                aim ()
                boot ()
                cat ()
                (if model.CatDead then boom () else e "g" [] [])
                (if model.Score = 0 then title () else e "g" [] [])
            ]
            div [a "id" "overlay"; mouseMove; mouseClick] []
        ]

    let main () =
        printfn "version 8"
        
        createApp (newModel ()) view update 
        |> withStartNodeSelector "#main" 
        |> withInitMessage (fun h -> h Init)
        |> start renderer

