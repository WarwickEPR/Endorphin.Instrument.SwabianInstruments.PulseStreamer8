// Copyright (c) University of Warwick. All Rights Reserved. Licensed under the Apache License, Version 2.0. See LICENSE.txt in the project root for license information.

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

    type TriggerMode =
        | Immediate
        | SoftwareTrigger
        | HardwareTrigger

    type Sample =
        internal { Channels  : Set<Channel>
                   Analogue0 : int16
                   Analogue1 : int16 }

    type PulseElement = internal { Sample : Sample ; Length : uint32 }

    type PulseSequence = PulseElement list