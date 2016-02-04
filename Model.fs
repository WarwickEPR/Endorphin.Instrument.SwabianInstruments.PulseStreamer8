namespace Endorphin.Instrument.SwabianInstruments.PulseStreamer8

open Endorphin.Core.CommandRequestAgent

[<AutoOpen>]
module Model = 
    type internal PulseStreamer8Identity = { Address : string }

    type PulseStreamer8 = internal PulseStreamer8 of agent : CommandRequestAgent<PulseStreamer8Identity>

    type internal ChannelEnum = 
        | None     = 0
        | Channel0 = 1
        | Channel1 = 2
        | Channel2 = 4
        | Channel3 = 8
        | Channel4 = 16
        | Channel5 = 32
        | Channel6 = 64
        | Channel7 = 128

    type Channel =
        | Channel0
        | Channel1
        | Channel2
        | Channel3
        | Channel4
        | Channel5
        | Channel6
        | Channel7

    type Sample = 
        internal { Channels  : Set<Channel>
                   Analogue0 : single
                   Analogue1 : single}

    type PulseElement = internal { Sample : Sample ; Length : uint32 }

    type PulseSequence = PulseElement seq
