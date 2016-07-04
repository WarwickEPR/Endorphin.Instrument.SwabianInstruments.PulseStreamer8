// Copyright (c) University of Warwick. All Rights Reserved. Licensed under the Apache License, Version 2.0. See LICENSE.txt in the project root for license information.

namespace Endorphin.Instrument.SwabianInstruments.PulseStreamer8

open System

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
    let internal withSample sample pulse = { pulse with Sample = sample }

    let withLength length pulse = { pulse with Length = length }

    /// query functions
    let internal sample pulse = pulse.Sample

    let length pulse = pulse.Length

    let sequenceLength (pulseSequence : PulseSequence) = Seq.fold (fun length pulse -> length + (uint64 pulse.Length)) (uint64 0) pulseSequence

    [<AutoOpen>]
    module Encode =
        let internal encodeChannels (sm : Sample) =
            sm |> Sample.channels |> Parse.channelMask |> byte

        /// Convert an arary into big-endian ordering, regardless of host ordering.
        let private toBigEndian (data : byte array) =
            if BitConverter.IsLittleEndian then Array.rev data else data

        /// create a bytestream from the given pulse sequence
        let internal encode (ps : PulseSequence) =
            let uint32ToBigEndian (x : uint32) = BitConverter.GetBytes x |> toBigEndian
            let shortToBigEndian  (x : int16)  = BitConverter.GetBytes x |> toBigEndian

            ps
            |> Seq.map (fun p ->
                let bytes = Array.zeroCreate 9
                bytes.[0..3] <- length p |> uint32ToBigEndian
                bytes.[4]    <- encodeChannels (sample p)
                bytes.[5..6] <- sample p |> Sample.analogue0 |> shortToBigEndian
                bytes.[7..8] <- sample p |> Sample.analogue1 |> shortToBigEndian
                bytes)
            |> Array.concat
            |> System.Convert.ToBase64String

    [<AutoOpen>]
    module Transform =
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

            loop first second [] |> List.rev |> collate

        /// combine two pulse sequences assuming the same start time and taking the union of the set channels at each element,
        /// whilst keeping the analogue values from the first sequence
        let unionParallel = mapParallel (fun p1 p2 ->
            let unionSample = (Sample.unionChannels (Sample.channelList (sample p2)) (sample p1))
            createWithSample (length p1) unionSample)

        /// apply the given delay to several channels. currently doesn't pay attention to analogue outputs
        let compensateHardwareDelays channelsWithDelay (delay : uint32) (ps : PulseSequence) =
            // a hardware delay (i.e. the hardware takes a long time to switch) is compensated for
            // by delaying the *other* channels in the sequence, having the effect of bringing the
            // hardware delay channels forward in time.
            let isChannelWithHardwareDelay c = Set.ofList channelsWithDelay |> Set.contains c
            let hardwareDelayedPart  p  = p |> withSample (sample p |> Sample.filterChannels isChannelWithHardwareDelay)
            let softwareDelayedPart p = p |> withSample (sample p |> Sample.filterChannels (not << isChannelWithHardwareDelay))

            let delayedInSoftware = (empty delay) :: (ps |> List.map softwareDelayedPart)
            let delayedInHardware = ps |> List.map hardwareDelayedPart

            unionParallel delayedInHardware delayedInSoftware