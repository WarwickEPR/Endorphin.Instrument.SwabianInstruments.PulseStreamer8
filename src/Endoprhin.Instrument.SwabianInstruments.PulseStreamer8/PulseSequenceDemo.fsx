// Copyright (c) University of Warwick. All Rights Reserved. Licensed under the Apache License, Version 2.0. See LICENSE.txt in the project root for license information.

#I "../../packages"

#r "bin/Release/Endorphin.Instrument.SwabianInstruments.PulseStreamer8.dll"
#r "FSharp.Data/lib/net40/Fsharp.Data.dll"
#r "Newtonsoft.Json/lib/net45/Newtonsoft.Json.dll"
#r "FifteenBelow.Json/lib/net40/FifteenBelow.Json.dll"

open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
open Endorphin.Instrument.SwabianInstruments.PulseStreamer8
open Endorphin.Instrument.SwabianInstruments.PulseStreamer8.Pulse
open FifteenBelow.Json
open System.Collections.Generic
open Newtonsoft.Json

Async.Start <| async {
    let acq = Channel.Channel0
    let laser = Channel.Channel1
    let uwX = Channel.Channel2
    let uwY = Channel.Channel3

    let stop = seq {
                    yield Pulse.empty                           1u
                    }
                    |> List.ofSeq

    let turnOnMicrowaves = Seq.singleton (Pulse.create [uwY] 100u)
    let turnOnLaser = (Seq.singleton (Pulse.create [laser] 100u)) |> List.ofSeq
    let turnOnLaserAndMicrowaves = Seq.singleton (Pulse.create [laser; uwX] 100u)
    let finalState = Pulse.create [laser] 0u
    let errorState = Pulse.empty 0u

    let microwaveTest =
        seq {
            yield Pulse.create [Channel1; Channel2] 200u
            yield Pulse.empty 500u
        }

    let laserTest =
        seq {
            yield Pulse.create [Channel1] 10000000u
            yield Pulse.empty 200000000u
        }

    let picoHarpPulse =
        seq {
            yield Pulse.create [Channel0; Channel1] 20u
            yield Pulse.create [Channel1] 100000000u }

    let bigRabiSequence =
        seq { for i in 0..999 do
                yield Pulse.empty 500u
                yield Pulse.create
                    <| [Channel2]
                    <| (uint32 (i * 2))
                yield Pulse.create
                    <| [Channel1; Channel0]
                    <| 30u
                yield Pulse.create
                    <| [Channel1]
                    <| (uint32 (4000)) }
        |> List.ofSeq
        |> Pulse.Transform.compensateHardwareDelays [Channel1; Channel0] (uint32 1280)
        |> Pulse.Transform.compensateHardwareDelays [Channel2] (uint32 410)

    let lifetimeSequence =
        seq {
            yield Pulse.create [Channel1] 50u
            yield Pulse.empty 10u
            yield Pulse.create [Channel7] 10u
            yield Pulse.empty 1000u
        }
        |> List.ofSeq
        |> Pulse.Transform.compensateHardwareDelays [Channel1] (uint32 800)

    let! streamer = PulseStreamer.openDevice("http://192.168.1.100:8050/json-rpc")
    let sequence = turnOnLaser// stop //bigRabiSequence //AndMicrowaves
    printfn "Sequence length: %d" (Pulse.sequenceLength sequence)

    do! PulseStreamer.PulseSequence.writeSequence
        <| sequence
        <| 0u
        <| finalState
        <| errorState
        <| Immediate
        <| streamer

    //do! PulseStreamer.PulseSequence.setState [laser] streamer

    printfn "Done."
}