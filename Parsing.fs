namespace Endorphin.Instrument.SwabianInstruments.PulseStreamer8

module internal Parse =
    
    let triggerEnum = function
        | Immediate -> 0
        | SoftwareTrigger -> 1
        | HardwareTrigger -> 2

    let channelEnum = function
        | Channel0 -> ChannelEnum.Channel0
        | Channel1 -> ChannelEnum.Channel1
        | Channel2 -> ChannelEnum.Channel2
        | Channel3 -> ChannelEnum.Channel3
        | Channel4 -> ChannelEnum.Channel4
        | Channel5 -> ChannelEnum.Channel5
        | Channel6 -> ChannelEnum.Channel6
        | Channel7 -> ChannelEnum.Channel7

    let channelMask = Set.map channelEnum >> Set.fold (|||) ChannelEnum.None