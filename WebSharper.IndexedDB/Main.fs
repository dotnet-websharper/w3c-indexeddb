namespace WebSharper.Bindings.IndexedDB

open WebSharper
open WebSharper.JavaScript
open WebSharper.InterfaceGenerator

module Definition =
    
    [<AutoOpen>]
    module Types =
        let DOMException = T<Dom.Exception>
        let IDBKey = T<string>
    let IDBKeyRange = 
        Class "IDBKeyRange"
        |+> Instance [
            for n in ["lower";"upper"] do
                $"{n}" =? IDBKey
                $"{n}Open" =? T<bool>
            "includes" => IDBKey ^-> T<bool>
        ]
        |+> Static [
            "bound" => IDBKey?lower * T<string>?upper * !?T<bool>?lowerOpen * !?T<bool>?upperOpen ^-> TSelf
            "only" => IDBKey?value ^-> TSelf
            "lower" => IDBKey?lower * !?T<bool>?``open`` ^-> TSelf
            "upper" => IDBKey?upper * !?T<bool>?``open`` ^-> TSelf
        ]
    let IDBCursorDirection =
        Pattern.EnumStrings "IDBCursor.CursorDirection" [
            for n in ["next";"prev"] do
                n
                $"{n}unique"
        ]
    let IDBObjectStoreCreateOptions =
        Pattern.Config "IDBObjectStore.CreateOptions" {
            Required = []
            Optional = [
                "keyPath", T<string>
                "autoIncrement", T<bool>
            ]
        }
    let IDBTransactionMode =
        Pattern.EnumStrings "IDBTransaction.TransactionMode" [
            "readonly"
            "readwrite"
            "versionchange"
        ]
        |+> Static [
            "readwriteflush" =? TSelf
                |> WithComment "Non-standard, Firefox-only"
        ]

        
    let IDBTransactionDurability =
        Pattern.EnumStrings "IDBTransaction.TransactionDurability" [
            "default"
            "strict"
            "relaxed"
        ]
    
    let IDBTransactionOptions =
        Class "IDBTransaction.Options"
        |+> Static [
            ObjectConstructor IDBTransactionDurability?durability
        ]
    let IDBTransaction =
        Class "IDBTransaction"
        |=> Inherits T<Dom.EventTarget>
        |=> Nested [IDBTransactionOptions;IDBTransactionDurability;IDBTransactionMode]
        
    let IDBObjectStore = 
        Class "IDBObjectStore"
        |=> Nested [IDBObjectStoreCreateOptions] 

    let IDBIndex =
        Class "IDBIndex"
    let IDBRequest =
        Generic - fun t ->
            Class "IDBRequest"
            |=> Inherits T<Dom.EventTarget>
            |+> Instance [
                "result" =? t
            ]
    
    let IDBCursor = Generic - fun t ->
        Class "IDBCursor"
        |=> Nested [IDBCursorDirection]
        |+> Instance [
            "source" =? IDBObjectStore + IDBIndex
            "direction" =? IDBCursorDirection
            "key" =? T<string>
            "primaryKey" =? T<string>
            "request" =? IDBRequest[t]

            "advance" => T<uint>?count ^-> T<unit>
            "continue" => !?T<string>?key ^-> T<unit>
            "continuePrimaryKey" => T<string>?key * T<string>?primaryKey ^-> T<unit>
            "delete" => T<unit> ^-> IDBRequest[T<unit>]
            |> WithComment "Deletes the record at the cursor's position in a separate thread, then sets the cursor to null. Can't be called from cursors obtained from openKeyCursor."
            "update" => T<obj>?value ^-> IDBRequest[T<unit>]
            |> WithComment "Can't be called from cursors obtained from openKeyCursor."
        ]
    let IDBCursorWithValue = Generic - fun t ->
        Class "IDBCursorWithValue"
        |=> Inherits IDBCursor[t]
        |+> Instance [
            "value" =? t
        ]
    let IDBObjectStoreIndexOptions =
        Pattern.Config "IDBObjectStore.IndexOptions" {
            Required = []
            Optional = [
                "unique", T<bool>
                "multiEntry", T<bool> 
                "locale", T<string>
            ]
        }
    let IDBDatabase = 
        Class "IDBDatabase"
        |=> Inherits T<Dom.EventTarget>
        |+> Instance [
            "name" =? T<string>
            "version" =? T<int64> + T<string>
            "objectStoreNames" =? T<Dom.StringList>

            "onclose" =@ T<Dom.Event> ^-> T<unit>
            "onversionchange" =@ T<Dom.Event> ^-> T<unit>

            "onabort" =@ T<Dom.Event> ^-> T<unit>
            |> WithComment "Bubbled up from IDBTransaction."
            "onerror" =@ T<Dom.Event> ^-> T<unit>
            |> WithComment "Bubbled up from IDBTransaction."

            "createObjectStore" => T<string>?name * IDBObjectStoreCreateOptions?options ^-> IDBObjectStore
                |> WithComment """Can be called only within a "versionchange" transaction."""
            "deleteObjectStore" => T<string>?name ^-> T<unit>
                |> WithComment """Can be called only within a "versionchange" transaction."""
            "transaction" => (!|T<string>)?storeNames * !?IDBTransactionMode?mode * !?IDBTransactionOptions?options ^-> IDBTransaction // TODO options, mode enum
        ]
    
    let IDBOpenDBRequest =
        Class "IDBOpenDBRequest"
        |=> Inherits IDBRequest[IDBDatabase]
        |+> Instance [
            "onblocked" =@ T<Dom.Event> ^-> T<unit>
            "onupgradeneeded" =@ T<Dom.Event> ^-> T<unit>
        ]
    let IDBVersionChangeEvent =
        let VersionChangeType =
            Pattern.EnumStrings "IDBVersionChangeEvent.EventType" [
                "versionchange";"success";"blocked"
            ]
        let VersionChangeOptions =
            Pattern.Config "IDBVersionChangeEvent.Options" {
                Required = []
                Optional = [
                    "oldVersion", T<int>
                    "newVersion", T<int>
                ]
            }
        Class "IDBVersionChangeEvent"
        |=> Inherits T<Dom.Event>
        |=> Nested [VersionChangeType;VersionChangeOptions]
        |+> Instance [
            "oldVersion" =? T<int>
            "newVersion" =? T<int>
        ]
        |+> Static [
            Constructor (VersionChangeType?``type`` * !?VersionChangeOptions?options)
        ]
    let IDBDictElement =
        Class "IDBDictElement"
        |=> Inherits T<System.Collections.Generic.Dictionary<string,uint>>
        |+> Instance [
            "name" =? T<string>
            "version" =? T<uint>
        ]
    let IDBFactory = 
        Class "IDBFactory"
        |=> Nested [IDBDictElement]
        |+> Instance [
            "open" => T<string>?name * !?T<uint>?version ^-> IDBOpenDBRequest
            "deleteDatabase" => T<string>?name ^-> IDBOpenDBRequest
            Generic - fun t -> 
                t.Constraints <- [T<System.IComparable>]
                "cmp" => t?first * t?second ^-> T<int>
            "databases" => T<unit> ^-> T<Promise<_>>[!|IDBDictElement]
        ]

    IDBTransaction
    |+> Instance [
        "db" =? IDBDatabase
        "durability" =? IDBTransactionDurability
        "error" =? !?DOMException
        "mode" =? IDBTransactionMode

        "objectStore" => T<string> ^-> IDBObjectStore

        "onabort" =@ T<Dom.Event> ^-> T<unit>
        "oncomplete" =@ T<Dom.Event> ^-> T<unit>
        "onerror" =@ T<Dom.Event> ^-> T<unit>
    ]
    |> ignore


    IDBObjectStore
    |=> Nested [IDBObjectStoreIndexOptions]
    |+> Instance [
        "indexNames" =? T<string>
        "keyPath" =? T<string>
        "name" =? T<string>
        "transaction" =? IDBTransaction
        "autoIncrement" =? T<bool>

        "add" => T<obj> * !?T<string> ^-> IDBRequest[T<obj>]
        "clear" => T<unit> ^-> IDBRequest[T<unit>]
        "count" => T<string> + IDBKeyRange ^-> IDBRequest[T<int>]
        "createIndex" => T<string>?indexName * T<string>?keyPath * !?IDBObjectStoreIndexOptions?options ^-> IDBIndex // TODO options
        "delete" => T<string>?key ^-> IDBRequest[T<unit>]
        "deleteIndex" => T<string>?indexName ^-> IDBRequest[T<unit>]
        Generic - fun t -> "get" => T<string>?key ^-> IDBRequest[t]
        Generic - fun t ->  "getKey" => T<string>?key ^-> IDBRequest[t]
        Generic - fun t ->  "getAll" => !?T<string>?query * !?T<uint>?count ^-> IDBRequest[!|t]
        "getAllKeys" => !?T<string>?query * !?T<uint>?count ^-> IDBRequest[!|T<string>]
        "index" => T<string>?name ^-> IDBIndex
        "put" => T<obj>?item * !?T<string>?key ^-> IDBRequest[T<string>]
        Generic - fun t -> "openCursor" => !?T<string>?query * !?IDBCursorDirection?direction ^-> IDBRequest[IDBCursorWithValue[t]] // "in separate thread, IDBCursorWithValue"
        Generic - fun t -> "openKeyCursor" => !?T<string>?query * !?IDBCursorDirection?direction ^-> IDBRequest[IDBCursorWithValue[t]] // "in separate thread, IDBCursorWithValue"
    ]
    |> ignore

        
    // Generic - fun t ->
    IDBIndex
    |+> Instance [
        "isAutoLocale" =? T<bool>
        "locale" =? T<string>
        "name" =? T<string>
        "objectStore" =? IDBObjectStore
        "keyPath" =? T<string>
        "multiEntry" =? T<bool>
        "unique" =? T<bool>
        
        "count" => !?(T<string> + IDBKeyRange) ^-> IDBRequest[T<int>]
        Generic - fun t -> "get" => !?(T<string> + IDBKeyRange) ^-> IDBRequest[t] // TODO IDBKeyRange and IDBKey
        Generic - fun t ->  "getKey" => !?(T<string> + IDBKeyRange) ^-> IDBRequest[t]
        Generic - fun t ->  "getAll" => !?T<string>?query * !? T<int>?count ^-> IDBRequest[t]
        "getAllKeys" => !?T<string>?query * !? T<int>?count ^-> IDBRequest[!|T<string>]
        Generic - fun t -> "openCursor" =? !?IDBKeyRange?range * IDBCursorDirection?direction ^-> IDBRequest[IDBCursorWithValue[t]]
        Generic - fun t -> "openKeyCursor" =? !?IDBKeyRange?range * IDBCursorDirection?direction ^-> IDBRequest[IDBCursorWithValue[t]]
    ]
    |> ignore

    
    IDBRequest
    |+> Instance [
        "error" =? DOMException

        "source" =? !?(IDBIndex + IDBObjectStore)
        "readyState" =? T<string>
        "transaction" =? IDBTransaction

        "onsuccess" =@ T<Dom.Event> ^-> T<unit>
        "onerror" =@ T<Dom.Event> ^-> T<unit>
    ]
    |> ignore

    let Assembly =
        Assembly [
            Namespace "WebSharper.IndexedDB" [
                IDBKeyRange
                IDBTransaction
                IDBObjectStore
                IDBIndex
                IDBRequest
                IDBCursor
                IDBCursorWithValue
                IDBDatabase
                IDBOpenDBRequest
                IDBVersionChangeEvent
                IDBFactory
            ]
        ]

[<Sealed>]
type Extension() =
    interface IExtension with
        member ext.Assembly =
            Definition.Assembly

[<assembly: Extension(typeof<Extension>)>]
do ()
