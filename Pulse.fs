namespace Endorphin.Instrument.SwabianInstruments.PulseStreamer8

open MiscUtil

module Pulse =

    type PulseElement = 
        { Channels  : Set<Channel>
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
    module Compile = 

        /// create a bytestream from the given pulse sequence
        let internal createStream (sequence : PulseSequence) =
            let uint32ToBigEndian (x : uint32) = Conversion.EndianBitConverter.Big.GetBytes x
            let singleToBigEndian (x : single) = Conversion.EndianBitConverter.Big.GetBytes x

            sequence
            |> Seq.fold (fun (byteSequence : seq<byte[]>) pulseElement -> 
                let bArray : byte[] = Array.zeroCreate 13
                bArray.[0]     <- pulseElement.Channels |> Parse.channelMask |> byte
                bArray.[1..4]  <- uint32ToBigEndian pulseElement.Length
                bArray.[5..8]  <- singleToBigEndian pulseElement.Analogue0
                bArray.[9..12] <- singleToBigEndian pulseElement.Analogue1
                Seq.append byteSequence (Seq.singleton bArray)) Seq.empty<byte[]>
            |> Array.concat

        /// encode a bytestream to base64
        let internal encode stream = System.Convert.ToBase64String stream

        /// compile a pulse sequence to the base64 string representation required by the PulseStreamer
        let compile = createStream >> encode

    [<AutoOpen>]
    module Transform =

        /// combine two pulse sequences assuming the same start time using the given mapping function to combine elements
        let mapParallel map (first : PulseSequence) (second : PulseSequence) =
            let subtractLength l p = p |> withLength (length p - l)

            let rec loop list1 list2 acc = 
                match list1, list2 with
                | p1 :: tail1, p2 :: tail2 when length p1 < length p2 ->
                    let p'     = map p1 p2
                    let list2' = (p2 |> subtractLength (length p1)) :: tail2
                    loop tail1 list2' (p' :: acc)
                
                | p1 :: tail1, p2 :: tail2 when length p1 > length p2 ->
                    let p'     = map p1 p2
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
            
            let rec loop xs acc =
                match xs with
                | p1 :: p2 :: tail when areSimilar p1 p2 -> loop tail ((p1 |> withLength (length p1 + length p2)) :: acc)
                | p :: tail        when length p = 0u    -> loop tail acc
                | p :: tail                              -> loop tail (p :: acc)
                | []                                     -> acc

            loop ps [] |> List.rev

        /// apply the given delay to several channels. currently doesn't pay attention to analogue outputs
        let delayChannels channelsToDelay (delay : uint32) (ps : PulseSequence) =
            let isDelayChannel c = Set.ofList channelsToDelay |> Set.contains c
            let delayedChannels  = channels >> List.filter isDelayChannel 
            let otherChannels    = channels >> List.filter (not << isDelayChannel)

            let delayed = ps |> List.map (fun p -> p |> withChannels (delayedChannels p))
            let other   = (createDelay delay) :: (ps |> List.map (fun p -> p |> withChannels (otherChannels p)))
            unionParallel other delayed