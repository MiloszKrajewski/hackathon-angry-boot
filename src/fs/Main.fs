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

    type Model = {
        Mouse: double*double
        BootMoving: bool
        BootPosition: double*double
        BootAngle: double
        BootRotation: double
        BootVector: double*double
    }

    let newModel () = {
        Mouse = (0.0, 0.0)
        BootMoving = false 
        BootPosition = (50.0, 250.0)
        BootAngle = 0.0
        BootRotation = 20.0 
        BootVector = (10.0, -10.0)
    }

    let friction = 0.01
    let gravity = 1.0

    let min a b = if a <= b then a else b
    let max a b = if a >= b then a else b
    let rec wrap l v = if v > l then wrap l (v - l) else if v < 0.0 then wrap l (v + l) else v 

    let nextModelTick model =
        if model.BootMoving then
            let x, y = model.BootPosition 
            let dx, dy = model.BootVector
            let r = model.BootAngle
            let dr = model.BootRotation

            { model with
                BootPosition = (x + dx, y + dy)
                BootAngle = r + dr |> wrap 360.0
                BootVector = (dx - friction |> max 0.0, dy + gravity) 
            }
        else
            model

    let nextModelUI model event = 
        match event with
        | MouseMove (x, y) -> { model with Mouse = (x, y) }
        | MouseClick -> 
            let bx, by = model.BootPosition
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

    let nextTick push = 
        Browser.window.setTimeout((fun () -> push Tick), 1000.0 / 10.0)
        |> ignore

    let update model event =
        match event with
        | Init -> newModel(), [nextTick]
        | Tick -> nextModelTick model, [nextTick] 
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
            let x, y = model.BootPosition
            let r = model.BootAngle
            let translation = sprintf "translate(%g %g)" x y
            let rotation = sprintf "scale(0.2) rotate(%g 25 25)" r
            e "g" [a "transform" translation] [
                e "g" [a "transform" "translate(-5 -5)"] [
                    e "use" [a "href" "#boots"; a "transform" rotation] []
                ]
            ]
        
        let aim () = 
            if model.BootMoving then e "g" [] []
            else
                let sx, sy = model.BootPosition
                let ex, ey = model.Mouse
                e "path" [a "d" (sprintf "M %g %g L %g %g" sx sy ex ey); a "stroke" "yellow"] []

        let mouseMove = 
            onMouseMove (fun e ->
                let x = (e ? clientX |> unbox<double>) * 500.0 / 1000.0
                let y = (e ? clientY |> unbox<double>) * 300.0 / 600.0
                MouseMove (x, y)
            )
        
        let mouseClick = 
            onMouseClick (fun e -> MouseClick)

        Svg.svg [width 1000; height 600; viewbox 0 0 500 300; mouseMove; mouseClick] [
            e "defs" [] [
                load "Boots.svg" "boots"
                load "Cat.svg" "cat"
            ]
            aim ()
            boot ()

        ]

    let main () =
        printfn "version 8"
        
        createApp (newModel ()) view update 
        |> withStartNodeSelector "#main" 
        |> withInitMessage (fun h -> h Init)
        |> start renderer

