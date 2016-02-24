namespace Endorphin.Instrument.SwabianInstruments.PulseStreamer8

open MiscUtil

module internal Sample =

    let empty = { Channels = Set.empty ; Analogue0 = 0s ; Analogue1 = 0s }
    
    let create channels = { Channels = Set.ofList channels ; Analogue0 = 0s ; Analogue1 = 0s }

    let createWithAnalogue channels analogue0 analogue1 = 
        { Channels = Set.ofList channels ; Analogue0 = analogue0 ; Analogue1 = analogue1 }

    let withChannels channels sample = { sample with Channels = Set.ofList channels }

    let unionChannels channels sample = { sample with Channels = channels |> Set.ofList |> Set.union sample.Channels }

    let filterChannels predicate sample = { sample with Channels = sample.Channels |> Set.filter predicate }
    
    let withAnalogue0 analogue0 sample = { sample with Analogue0 = analogue0 } 

    let withAnalogue1 analogue1 sample = { sample with Analogue1 = analogue1 }

    let channels sample = sample.Channels

    let channelList sample = sample.Channels |> Set.toList

    let analogue0 sample = sample.Analogue0

    let analogue1 sample = sample.Analogue1

    let containsChannel channel sample = sample.Channels |> Set.contains channel

module Pulse =

    /// creation functions
    let create channels length = { Sample = (Sample.create channels); Length = length}

    let internal createWithSample length sample = { Sample = sample; Length = length}

    let empty length = { Sample = Sample.empty; Length = length }

    /// modification functions
    let withSample sample pulse = { pulse with Sample = sample }

    let withLength length pulse = { pulse with Length = length }  

    /// query functions
    let internal sample pulse = pulse.Sample

    let length pulse = pulse.Length

    let sequenceLength (pulseSequence : PulseSequence) = Seq.fold (fun length pulse -> length + (uint64 pulse.Length)) (uint64 0) pulseSequence

    [<AutoOpen>]
    module Encode = 
        /// create a bytestream from the given pulse sequence
        let internal encode (ps : PulseSequence) =
            let uint32ToBigEndian (x : uint32) = Conversion.EndianBitConverter.Big.GetBytes x
            let shortToBigEndian  (x : int16)  = Conversion.EndianBitConverter.Big.GetBytes x
 
            ps
            |> Seq.map (fun p -> 
                let bytes = Array.zeroCreate 9
                bytes.[0..3] <- length p |> uint32ToBigEndian
                bytes.[4]    <- sample p |> Sample.channels |> Parse.channelMask |> byte
                bytes.[5..6] <- sample p |> Sample.analogue0 |> shortToBigEndian
                bytes.[7..8] <- sample p |> Sample.analogue1 |> shortToBigEndian
                bytes)
            |> Array.concat
            |> System.Convert.ToBase64String

    module private Iterator =
        open System.Collections.Generic
        
        type Iterator<'T> = IEnumerator<'T>

        let ofSeq (seq : 'T seq) : Iterator<'T> = seq.GetEnumerator()

        let next (iterator : Iterator<_>) = if iterator.MoveNext() then Some iterator.Current else None

        let current (iterator : Iterator<_>) = iterator.Current

    [<AutoOpen>]
    module Transform =
        /// merge neighbouring pulses which have the same active channels and remove zero-length elements
        let collate (ps : PulseSequence) : PulseSequence = 
            let rec loop (head, iterator) = seq {
                match head, Iterator.next iterator with
                | Some p, Some p' when length p' = 0u       -> yield! loop (Some p, iterator)
                | Some p, Some p' when sample p = sample p' -> yield! loop (p |> withLength (length p + length p') |> Some, iterator)
                | Some p, Some p'                           -> yield p ; yield! loop (Some p', iterator)
                | Some p, None                              -> yield p
                | None,   Some p                            -> yield! loop (Some p, iterator)
                | None,   None                              -> () }
            
            loop (None, Iterator.ofSeq ps)

       /// combine two pulse sequences assuming the same start time using the given mapping function to combine elements
        let mapParallel mapping (first : PulseSequence) (second : PulseSequence) : PulseSequence =
            let subtractLength l p = p |> withLength (length p - l)

            let rec loop (residual1, iterator1) (residual2, iterator2) = seq {
                let p1 =
                    match residual1 with
                    | Some p -> Some p
                    | None   -> iterator1 |> Iterator.next

                let p2 = 
                    match residual2 with
                    | Some p -> Some p
                    | None   -> iterator2 |> Iterator.next

                match p1, p2 with
                | Some p1', Some p2' when length p1' < length p2' ->
                    let residual2' = p2' |> subtractLength (length p1')
                    yield mapping (sample p1') (sample p2') |> createWithSample (length p1')
                    yield! loop (None, iterator1) (Some residual2', iterator2)
                
                | Some p1', Some p2' when length p2' < length p1' ->
                    let residual1' = p1' |> subtractLength (length p2')
                    yield mapping (sample p1') (sample p2') |> createWithSample (length p2')
                    yield! loop (Some residual1', iterator1) (None, iterator2)

                | Some p1', Some p2' ->
                    yield mapping (sample p1') (sample p2') |> createWithSample (length p1')
                    yield! loop (None, iterator1) (None, iterator2)

                | Some p1', None -> 
                    yield mapping (sample p1') Sample.empty |> createWithSample (length p1')
                    yield! loop (None, iterator1) (None, iterator2)
                
                | None, Some p2' ->
                    yield mapping Sample.empty (sample p2') |> createWithSample (length p2')
                    yield! loop (None, iterator1) (None, iterator2)

                | None, None -> () }

            loop (None, Iterator.ofSeq first) (None, Iterator.ofSeq second) |> collate

        /// combine two pulse sequences assuming the same start time and taking the union of the set channels at each element,
        /// whilst keeping the analogue values from the first sequence
        let unionParallel = mapParallel (fun s1 s2 -> s1 |> Sample.unionChannels (Sample.channelList s2))

        /// apply the given delay to several channels. currently doesn't pay attention to analogue outputs
        let compensateHardwareDelays channelsWithDelay (delay : uint32) (ps : PulseSequence) =
            // a hardware delay (i.e. the hardware takes a long time to switch) is compensated for
            // by delaying the *other* channels in the sequence, having the effect of bringing the 
            // hardware delay channels forward in time.
            let isChannelWithHardwareDelay c = Set.ofList channelsWithDelay |> Set.contains c
            let hardwareDelayedPart p = p |> withSample (sample p |> Sample.filterChannels isChannelWithHardwareDelay)
            let softwareDelayedPart p = p |> withSample (sample p |> Sample.filterChannels (not << isChannelWithHardwareDelay))

            let delayedInSoftware = seq {
                yield Sample.empty |> createWithSample delay
                yield! ps |> Seq.map softwareDelayedPart }

            let delayedInHardware = ps |> Seq.map hardwareDelayedPart
            unionParallel delayedInHardware delayedInSoftware