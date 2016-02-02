namespace Endorphin.Instrument.SwabianInstruments.PulseStreamer8

open Endorphin.Core.CommandRequestAgent

[<AutoOpen>]
module Model = 
    type internal PulseStreamer8Identity = { Address : string }

    type PulseStreamer8 = internal PulseStreamer8 of agent : CommandRequestAgent<PulseStreamer8Identity>

    type Channel = 
        | None = 0
        | Channel0   = 1
        | Channel1   = 2
        | Channel2   = 4
        | Channel3   = 8
        | Channel4   = 16
        | Channel5   = 32
        | Channel6   = 64
        | Channel7   = 128