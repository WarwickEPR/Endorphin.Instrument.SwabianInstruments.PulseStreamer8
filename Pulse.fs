namespace Endorphin.Instrument.SwabianInstruments.PulseStreamer8

open MiscUtil

module Pulse =

    type PulseElement = 
        internal { Channels  : Set<Channel>
                   Length    : uint32
                   Analogue0 : single
                   Analogue1 : single }

    type PulseSequence = PulseElement list

    /// creation functions
    let create channels length = 
        { Channels = Set.ofList channels; Length = length; Analogue0 = 0.0f; Analogue1 = 0.0f }

    let createDelay length = 
        { Channels = Set.empty ; Length = length; Analogue0 = 0.0f; Analogue1 = 0.0f }

    let createWithAnalogue channels length analogue0 analogue1 = 
        { Channels = Set.ofList channels; Length = length; Analogue0 = analogue0; Analogue1 = analogue1 }

    /// modification functions
    let withChannels channels pulse = { pulse with Channels = Set.ofList channels }

    let unionChannels channels pulse = { pulse with Channels = channels |> Set.ofList |> Set.union pulse.Channels }

    let withLength length pulse = { pulse with Length = length }  

    let withAnalogue0 analogue0 pulse = { pulse with Analogue0 = analogue0 } 

    let withAnalogue1 analogue1 pulse = { pulse with Analogue1 = analogue1 } 

    /// query functions
    let channels pulse = pulse.Channels |> Set.toList

    let length pulse = pulse.Length

    let analogue0 pulse = pulse.Analogue0

    let analogue1 pulse = pulse.Analogue1

    let containsChannel channel pulse = pulse.Channels |> Set.contains channel

    [<AutoOpen>]
    module Encode = 
        /// create a bytestream from the given pulse sequence
        let internal encode (ps : PulseSequence) =
            let uint32ToBigEndian (x : uint32) = Conversion.EndianBitConverter.Big.GetBytes x
            let singleToBigEndian (x : single) = Conversion.EndianBitConverter.Big.GetBytes x
 
            ps
            |> Seq.map (fun p -> 
                let bytes = Array.zeroCreate 13
                bytes.[0]     <- p.Channels  |> Parse.channelMask |> byte
                bytes.[1..4]  <- p.Length    |> uint32ToBigEndian
                bytes.[5..8]  <- p.Analogue0 |> singleToBigEndian
                bytes.[9..12] <- p.Analogue1 |> singleToBigEndian
                bytes)
            |> Array.concat
            |> System.Convert.ToBase64String

    [<AutoOpen>]
    module Transform =
       /// combine two pulse sequences assuming the same start time using the given mapping function to combine elements
        let mapParallel map (first : PulseSequence) (second : PulseSequence) =
            let subtractLength l p = p |> withLength (length p - l)

            let rec loop list1 list2 acc = 
                match list1, list2 with
                | p1 :: tail1, p2 :: tail2 when length p1 < length p2 ->
                    let p' = map p1 p2 |> withLength (length p1)
                    let list2' = (p2 |> subtractLength (length p1)) :: tail2
                    loop tail1 list2' (p' :: acc)
                
                | p1 :: tail1, p2 :: tail2 when length p1 > length p2 ->
                    let p' = map p1 p2 |> withLength (length p2)
                    let list1' = (p1 |> subtractLength (length p2)) :: tail1
                    loop list1' tail2 (p' :: acc)

                | p1 :: tail1, p2 :: tail2 ->
                    let p' = map p1 p2
                    loop tail1 tail2 (p' :: acc)

                | p :: tail, []        -> loop tail [] (p :: acc)
                | [],        p :: tail -> loop [] tail (p :: acc)
                | [],        []        -> acc

            loop first second [] |> List.rev

        /// combine two pulse sequences assuming the same start time and taking the union of the set channels at each element,
        /// whilst keeping the analogue values from the first sequence
        let unionParallel = mapParallel (fun p1 p2 -> p1 |> unionChannels (channels p2))

        /// merge neighbouring pulses which have the same active channels and remove zero-length elements
        let collate (ps : PulseSequence) = 
            let areSimilar p1 p2 = ((p1 |> withLength 0u) = (p2 |> withLength 0u))

            let rec loop xs (accHead, accTail) =
                match xs, accHead with
                | p :: tail, _       when length p = 0u   -> loop tail (accHead, accTail)
                | p :: tail, Some p' when areSimilar p p' -> loop tail (Some (p |> withLength (length p + length p')), accTail)
                | p :: tail, Some p'                      -> loop tail (Some p, p' :: accTail)
                | p :: tail, None                         -> loop tail (Some p, accTail)
                | [], Some p'                             -> p' :: accTail
                | [], None                                -> accTail

            loop ps (None, []) |> List.rev

        /// apply the given delay to several channels. currently doesn't pay attention to analogue outputs
        let compensateHardwareDelays channelsWithDelay (delay : uint32) (ps : PulseSequence) =
            // a hardware delay (i.e. the hardware takes a long time to switch) is compensated for
            // by delaying the *other* channels in the sequence, having the effect of bringing the 
            // hardware delay channels forward in time.
            let isChannelWithHardwareDelay c = Set.ofList channelsWithDelay |> Set.contains c
            let channelsWithHardwareDelay    = channels >> List.filter isChannelWithHardwareDelay 
            let channelsWithoutHardwareDelay = channels >> List.filter (not << isChannelWithHardwareDelay)

            let delayedInSequence = (createDelay delay) :: (ps |> List.map (fun p -> p |> withChannels (channelsWithoutHardwareDelay p)))
            let other   = ps |> List.map (fun p -> p |> withChannels (channelsWithHardwareDelay p))
            unionParallel other delayedInSequence