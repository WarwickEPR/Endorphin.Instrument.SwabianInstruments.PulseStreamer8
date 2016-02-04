namespace Endorphin.Instrument.SwabianInstruments.PulseStreamer8

open MiscUtil

module Pulse =

    type PulseElement = 
        { Channels  : Set<Channel>
          Length    : uint32
          Analogue0 : single
          Analogue1 : single }

    type PulseSequence = PulseElement seq

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
        let internal createStream (ps : PulseSequence) =
            let uint32ToBigEndian (x : uint32) = Conversion.EndianBitConverter.Big.GetBytes x
            let singleToBigEndian (x : single) = Conversion.EndianBitConverter.Big.GetBytes x

            ps
            |> Seq.map (fun p -> 
                let bytes = Array.zeroCreate 13
                bytes.[0]     <- p.Channels |> Parse.channelMask |> byte
                bytes.[1..4]  <- uint32ToBigEndian p.Length
                bytes.[5..8]  <- singleToBigEndian p.Analogue0
                bytes.[9..12] <- singleToBigEndian p.Analogue1
                bytes)
            |> Array.concat

        /// encode a bytestream to base64
        let internal encode stream = System.Convert.ToBase64String stream

        /// compile a pulse sequence to the base64 string representation required by the PulseStreamer
        let compile ps = createStream ps |> encode

    [<AutoOpen>]
    module Transform =

        let rec mapParallel mapping (first : PulseSequence) (second : PulseSequence) = seq {
            let subtractLength l p = p |> withLength (length p - l)
            let cons x xs = Seq.append (Seq.singleton x) xs

            match Seq.tryHead first, Seq.tryHead second with
            | Some p1, Some p2 when length p1 < length p2 ->
                let residual = p2 |> subtractLength (length p1)
                yield mapping p1 p2 |> withLength (length p1)
                yield! mapParallel mapping (Seq.tail first) (cons residual (Seq.tail second))
                
            | Some p1, Some p2 when length p2 < length p1 ->
                let residual = p1 |> subtractLength (length p2)
                yield mapping p1 p2 |> withLength (length p2)
                yield! mapParallel mapping (cons residual (Seq.tail first)) (Seq.tail second)
                
            | Some p1, Some p2 ->
                yield mapping p1 p2 
                yield! mapParallel mapping (Seq.tail first) (Seq.tail second)
                
            | Some _, None    -> yield! first  |> Seq.map (fun p -> mapping p (createDelay (length p)))
            | None,    Some _ -> yield! second |> Seq.map (fun p -> mapping (createDelay (length p)) p)
            | None,    None   -> () }

        /// combine two pulse sequences assuming the same start time and taking the union of the set channels at each element,
        /// whilst keeping the analogue values from the first sequence
        let unionParallel = mapParallel (fun p1 p2 -> p1 |> unionChannels (channels p2))

        /// merge neighbouring pulses which have the same active channels and remove zero-length elements
        let rec collate (ps : PulseSequence) = seq {
            let areSimilar p1 p2 = ((p1 |> withLength 0u) = (p2 |> withLength 0u))
            match Seq.tryHead ps with
            | Some p when length p = 0u -> yield! Seq.tail ps
            | Some p1 ->
                let ps' = Seq.tail ps
                match Seq.tryHead ps' with
                | Some p2 when areSimilar p1 p2 ->
                    yield p1 |> withLength (length p1 + length p2)
                    yield! Seq.tail ps'
                | _ ->
                    yield p1
                    yield! ps'
            | None -> () }
                
        /// apply the given delay to several channels. currently doesn't pay attention to analogue outputs
        let delayChannels channelsToDelay (delay : uint32) (ps : PulseSequence) =
            let isDelayChannel c = Set.ofList channelsToDelay |> Set.contains c
            let delayedChannels  = channels >> List.filter isDelayChannel 
            let otherChannels    = channels >> List.filter (not << isDelayChannel)

            let delayed = 
                Seq.append 
                <| (Seq.singleton <| createDelay delay)
                <| (ps |> Seq.map (fun p -> p |> withChannels (delayedChannels p)))
            
            let other   = ps |> Seq.map (fun p -> p |> withChannels (otherChannels p))
            unionParallel other delayed