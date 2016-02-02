namespace Endorphin.Instrument.SwabianInstruments.PulseStreamer8

open MiscUtil

module Pulse =
    type PulseElement = 
        { Channels  :   List<Channel>
          Length    :   uint32
          Analogue0 :   single
          Analogue1 :   single }

    type PulseSequence = seq<PulseElement> 

    /// creation functions
    let create channels length = 
        { Channels = channels; Length = length; Analogue0 = 0.0f; Analogue1 = 0.0f }

    let createDelay length = 
        { Channels = [Channel.None] ; Length = length; Analogue0 = 0.0f; Analogue1 = 0.0f }

    let createWithAnalogueValues channels length analogue0 analogue1 = 
        { Channels = channels; Length = length; Analogue0 = analogue0; Analogue1 = analogue1 }

    /// modification functions
    let withChannels element channels = 
        { Channels = channels; Length = element.Length; Analogue0 = element.Analogue0; Analogue1 = element.Analogue1 }

    let withLength element length = 
        { Channels = element.Channels; Length = length; Analogue0 = element.Analogue0; Analogue1 = element.Analogue1 }

    let withAnalogue0 element analogue0 = 
        { Channels = element.Channels; Length = element.Length; Analogue0 = analogue0; Analogue1 = element.Analogue1 }

    let withAnalogue1 element analogue1 = 
        { Channels = element.Channels; Length = element.Length; Analogue0 = element.Analogue0; Analogue1 = analogue1 }

    /// query functions
    let containsChannel (element : PulseElement) channel = 
        List.contains channel element.Channels

    /// transform a list of channels into a byte representation
    let private createChannelMask (channels : List<Channel>) =
        channels
        |> List.fold (|||) Channel.None
        |> byte

    [<AutoOpen>]
    module Compilation = 
        /// create a bytestream from the given pulse sequence
        let internal createStream (sequence : PulseSequence) =
            sequence
            |> Seq.fold (fun (byteSequence : seq<byte[]>) pulseElement -> 
                let bArray : byte[] = Array.zeroCreate 13
                bArray.[0] <- createChannelMask pulseElement.Channels
                bArray.[1..4] <- Conversion.EndianBitConverter.Big.GetBytes(pulseElement.Length)
                bArray.[5..8] <- Conversion.EndianBitConverter.Big.GetBytes(pulseElement.Analogue0)
                bArray.[9..12] <- Conversion.EndianBitConverter.Big.GetBytes(pulseElement.Analogue1)
                Seq.append byteSequence (Seq.singleton bArray)) Seq.empty<byte[]>
            |> Array.concat

        /// encode a bytestream to base64
        let internal encode sequenceStream = 
            System.Convert.ToBase64String sequenceStream

        /// compile a pulse sequence to the base64 string representation required by the PulseStreamer
        let compile sequence = 
            createStream sequence
            |> encode

    module Tools = 
        /// fetch unique channels and remove None
        let internal getUniqueChannels channels = 
            channels
            |> List.distinct
            |> List.except [Channel.None]

        /// combine two pulse sequences assuming the same start time
        let rec combine (seq1 : seq<PulseElement>) (seq2 : seq<PulseElement>) (union: seq<PulseElement>) = 
            let result = 
                match (Seq.tryHead seq1, Seq.tryHead seq2) with
                | Some e1, Some e2 when e1.Length < e2.Length -> 
                    let newUnion = 
                        Seq.append 
                        <| union 
                        <| (Seq.singleton <| withChannels e1 (List.append e1.Channels e2.Channels))
                    combine 
                    <| (Seq.skip 1 seq1) 
                    <| (Seq.append (Seq.singleton <| withLength e2 (e2.Length - e1.Length)) (Seq.skip 1 seq2)) 
                    <| newUnion
                | Some e1, Some e2 when e1.Length > e2.Length ->
                    let newUnion = 
                        Seq.append 
                        <| union 
                        <| (Seq.singleton <| withChannels e2 (List.append e1.Channels e2.Channels))
                    combine 
                    <| (Seq.append (Seq.singleton <| withLength e1 (e1.Length - e2.Length)) (Seq.skip 1 seq1)) 
                    <| (Seq.skip 1 seq2)  
                    <| newUnion  
                | Some e1, Some e2 ->
                    let newUnion = 
                        Seq.append 
                        <| union 
                        <| (Seq.singleton <| withChannels e1 (List.append e1.Channels e2.Channels))
                    combine (Seq.skip 1 seq1) (Seq.skip 1 seq2) newUnion
                | Some e1, None ->
                    Seq.append
                    <| union
                    <| seq1
                | None, Some e2 ->
                    Seq.append
                    <| union
                    <| seq2
                | None, None ->
                    union
            result

        /// merge neighbouring pulses which have the same active channels and remove zero-length elements
        let collate (pulseSequence : PulseSequence) = 
            Seq.foldBack (fun (element : PulseElement) (state : seq<PulseElement>) -> 
                /// possible options are: element length > 0 and channels are identical         -> combine
                ///                       element length > 0 and channels are not identical     -> do not combine
                ///                       element length = 0                                    -> drop pulse
                ///                       no elements                                           -> add first pulse to list
                match Seq.tryHead state with
                | Some s when element.Length > 0u ->     
                    if createChannelMask element.Channels = createChannelMask s.Channels then
                        Seq.append 
                        <| Seq.singleton (withChannels (withLength element (element.Length + s.Length)) <| getUniqueChannels element.Channels)
                        <| Seq.skip 1 state
                    else
                        Seq.append
                        <| Seq.singleton (withChannels element <| getUniqueChannels element.Channels)
                        <| state
                | None -> Seq.singleton (withChannels element <| getUniqueChannels element.Channels)
                | _    -> state ) pulseSequence Seq.empty<PulseElement>

        /// apply the given delay to several channels. currently doesn't pay attention to analogue outputs
        let applyDelay (channels : List<Channel>) (delay : uint32) (pulseSequence : PulseSequence) =
            /// split the sequence into two sequences - a sequence containing the channel to be delayed, and another containing
            /// the rest of the sequence. apply a delay pulse to the start of the main sequence, then merge the delayed and 
            /// main sequences
            let delayPulseSequence sequence channel = 
                Seq.foldBack (fun (element : PulseElement) (state : seq<PulseElement> * seq<PulseElement>) ->
                    if containsChannel element channel then
                        let delayedSequence = 
                            Seq.append
                            <| Seq.singleton (create [channel] element.Length)
                            <| snd state 
                        let mainSequence = 
                                Seq.append                   
                                <| Seq.singleton (withChannels element (List.except [channel] element.Channels))
                                <| fst state
                        (mainSequence, delayedSequence)
                    else
                        let delayedSequence = 
                            Seq.append
                            <| Seq.singleton (createDelay element.Length)
                            <| snd state
                        let mainSequence = 
                            Seq.append
                            <| Seq.singleton element
                            <| fst state
                        (mainSequence, delayedSequence) ) sequence (Seq.empty<PulseElement>, Seq.empty<PulseElement>)

            // generate a main sequence (containing none of the delay channels) and a list of delay channel sequences
            let (mainSequence, delayedSequences) = 
                channels 
                |> List.fold (fun state element ->
                    let mainSeq, delaySeq = delayPulseSequence (fst state) element
                    (mainSeq, List.append [delaySeq] (snd state))) (pulseSequence, [])

            // merge delayed sequences
            let delayedSequence =  
                delayedSequences 
                |> List.fold (fun combinedDelayedSequence delayedSequence ->
                    combine combinedDelayedSequence delayedSequence Seq.empty<PulseElement>) Seq.empty<PulseElement>

            // delay primary sequence
            let mainSequence = Seq.append 
                                <| Seq.singleton (createDelay delay)
                                <| mainSequence

            // combine main and delayed sequences
            combine mainSequence delayedSequence Seq.empty<PulseElement>