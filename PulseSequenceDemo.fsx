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
    let pulses = seq {
        yield {Channels = [Channel.Channel6]; Length = uint32 10; Analogue0 = single 0.0; Analogue1 = single 0.0}
        yield {Channels = [Channel.None]; Length = uint32 6; Analogue0 = single 0.0; Analogue1 = single 0.0 }
        yield {Channels = [Channel.Channel0]; Length = uint32 2; Analogue0 = single 0.0; Analogue1 = single 0.0} }

    let compiledSequence = compile pulses

    printfn "%A" compiledSequence

    let! streamer = PulseStreamer.openDevice("http://192.168.21.1:4444/")//("http://www.raboof.com/projects/jayrock/demo.ashx")//
    do! PulseStreamer.PulseSequence.writeSequence compiledSequence 1u streamer
    do! PulseStreamer.close streamer
}