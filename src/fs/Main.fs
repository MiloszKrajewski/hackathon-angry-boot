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
    type Event = | Init | Tick | MouseMove of double*double | MouseClick
    let scale = (250.0, 150.0)

    type Model = {
        Mouse: double*double
        Boot: double*double
        Cat: double*double
        BootMoving: bool
        CatDead: bool
        BootAngle: double
        BootRotation: double
        BootVector: double*double
    }

    let newModel () = {
        Mouse = (0.0, 0.0)
        Boot = (20.0, 125.0)
        BootMoving = false
        Cat = (175.0, 125.0)
        CatDead = false
        BootAngle = 0.0
        BootRotation = 20.0 
        BootVector = (10.0, -10.0)
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
            BootRotation = f / 10.0
        }

    let glide model = 
        let x, y = model.Boot 
        let dx, dy = model.BootVector
        let r = model.BootAngle
        let dr = model.BootRotation
        let cx, cy = model.Cat

        let hit () = 
            let dx = x - cx
            let dy = y - cy
            Math.Sqrt(dx*dx + dy*dy) < 10.0

        { model with
            CatDead = model.CatDead || hit ()
            Boot = (x + dx, y + dy)
            BootAngle = r + dr |> wrap 360.0
            BootVector = (dx - friction |> max 0.0, dy + gravity) 
        }      

    let nextModelTick model =
        if model.BootMoving then model |> glide else model

    let nextModelUI model event = 
        match event with
        | MouseMove (x, y) -> { model with Mouse = (x, y) }
        | MouseClick -> model |> launch
        | _ -> model


    let nextTick model push = 
        let _, y = model.Boot
        let _, c = scale
        if y > 2.0 * c then 
            push Init
        else
            Browser.window.setTimeout((fun () -> push Tick), 1000.0 / 10.0)
            |> ignore

    let update model event =
        match event with
        | Init -> newModel(), [nextTick model]
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

        let scaleX, scaleY = scale
        
        let aim () = 
            if model.BootMoving then e "g" [] []
            else
                let sx, sy = model.Boot
                let ex, ey = model.Mouse
                e "path" [a "d" (sprintf "M %g %g L %g %g" sx sy ex ey); a "stroke" "yellow"] []

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
                aim ()
                boot ()
                cat ()
                (if model.CatDead then boom () else e "g" [] [])
            ]
            div [a "id" "overlay"; mouseMove; mouseClick] []
        ]

    let main () =
        printfn "version 8"
        
        createApp (newModel ()) view update 
        |> withStartNodeSelector "#main" 
        |> withInitMessage (fun h -> h Init)
        |> start renderer

