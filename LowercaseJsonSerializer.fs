namespace Newtonsoft.Json

open FifteenBelow.Json
open System.Collections.Generic

module Serialization = 
    type internal LowercaseContractResolver() =
        inherit Serialization.DefaultContractResolver()
        override x.ResolvePropertyName(propertyName) = propertyName.ToLower()

[<AutoOpen>]
module Serializer = 
    type LowercaseJsonSerializer() =
        let converters =
            [ OptionConverter () :> JsonConverter; 
              TupleConverter () :> JsonConverter ] |> List.toArray :> IList<JsonConverter>

        let settings =
            JsonSerializerSettings (
                Converters = converters,
                NullValueHandling = NullValueHandling.Include,
                ContractResolver = new Serialization.LowercaseContractResolver() )

        member x.SerializeObject (o: obj) = JsonConvert.SerializeObject(o, settings)
        member x.DeserializeObject (s: string) : 'T = JsonConvert.DeserializeObject<'T>(s, settings)