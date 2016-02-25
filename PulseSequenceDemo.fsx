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
    let uwX = Channel.Channel2
    let uwY = Channel.Channel3

    let pulse = seq { 
        yield Pulse.create      [acq]               10u
        yield Pulse.empty                           30u
        yield Pulse.create      [laser]             20u
        yield Pulse.create      [acq; laser]        20u
        yield Pulse.empty                           300u }

    let stop = seq {
        yield Pulse.empty                           1u
        }

    let rabiSequence numberOfPoints ShotsPerPoint  PulseStreamerMicrowaveChannel InitialPulseDuration PulseDurationChange PulseStreamerAcquisitionChannel PulseStreamerLaserChannel DetectionLaserDuration = 
         seq { for i in 0..(numberOfPoints - 1) do     
                 for j in 0..(ShotsPerPoint - 1) do      
                     yield Pulse.create 
                           <| [PulseStreamerMicrowaveChannel] 
                           <| (uint32 (InitialPulseDuration + i * PulseDurationChange))
                     yield Pulse.create
                           <| [PulseStreamerLaserChannel; PulseStreamerAcquisitionChannel]
                           <| 5u
                     yield Pulse.create 
                           <| [PulseStreamerLaserChannel]
                           <| (uint32 (DetectionLaserDuration)) }
         |> Pulse.Transform.compensateHardwareDelays [PulseStreamerLaserChannel] (uint32 10)

    let turnOnMicrowaves = Seq.singleton (Pulse.create [uwY] 100u)
    let turnOnLaser = Seq.singleton (Pulse.create [laser] 100u)
    let finalState = Pulse.create [acq] 1000u
    let errorState = Pulse.empty 0u

    let rS = rabiSequence 100 1 Channel.Channel2 0 1 Channel.Channel0 Channel.Channel1 4000

    let! streamer = PulseStreamer.openDevice("http://192.168.1.100:8050/json-rpc")
    //do! PulseStreamer.PulseSequence.writeSequence pulse 10000000u finalState errorState streamer 
    //do! PulseStreamer.PulseSequence.writeSequence rS 0u finalState errorState streamer
    //do! PulseStreamer.PulseSequence.writeSequence turnOnMicrowaves 0u finalState errorState streamer 
    do! PulseStreamer.PulseSequence.writeSequence turnOnLaser 0u finalState errorState streamer
    //do! PulseStreamer.PulseSequence.writeSequence stop 10u finalState errorState streamer
    printfn "Done..."
}