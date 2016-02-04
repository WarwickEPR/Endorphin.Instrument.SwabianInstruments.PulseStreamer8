// Copyright (c) University of Warwick. All Rights Reserved. Licensed under the Apache License, Version 2.0. See LICENSE.txt in the project root for license information.

#r "../Endorphin.Core/bin/Debug/Endorphin.Core.dll"
#r "bin/Debug/Endorphin.Instrument.SwabianInstruments.PulseStreamer8.dll"
#r "../packages/FSharp.Data.2.2.5/lib/net40/Fsharp.Data.dll"
#r "../packages/Newtonsoft.Json.6.0.8/lib/net45/Newtonsoft.Json.dll"
#r "../packages/FifteenBelow.Json.0.2.0.12/lib/net40/FifteenBelow.Json.dll"

open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
open Endorphin.Core
open Endorphin.Instrument.SwabianInstruments.PulseStreamer8
open Endorphin.Instrument.SwabianInstruments.PulseStreamer8.Pulse
open FifteenBelow.Json
open System.Collections.Generic
open Newtonsoft.Json

Async.Start <| async {
    let acq = Channel.Channel0
    let laser = Channel.Channel1
    let uW = Channel.Channel2

    let pulse = seq {
        yield Pulse.create      [uW]            100u
        yield Pulse.create      [acq; laser]    5u
        yield Pulse.create      [laser]         25u
    }

    let pulses = seq {
        for i in 1u..100u do
            yield Pulse.create          [uW]            10u
            yield Pulse.createDelay                     i
            yield Pulse.create          [uW]            20u
            yield Pulse.createDelay                     i
            yield Pulse.create          [uW]            10u
            yield Pulse.create          [acq; laser]    1000u }

    let compiledSequence = pulses
                           |> List.ofSeq
                           |> Pulse.Transform.compensateHardwareDelays [acq; laser] 20u
                           |> Pulse.Transform.collate

    let! streamer = PulseStreamer.openDevice("http://192.168.21.1:4444/")
    do! PulseStreamer.PulseSequence.writeSequence compiledSequence 1u streamer
    do! PulseStreamer.close streamer
    printfn "complete"
}