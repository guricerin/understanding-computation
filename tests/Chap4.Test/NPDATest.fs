module UnderstandingComputation.Chap4.NPDATest

open Expecto
open UnderstandingComputation.Chap4
open PDA
open NPDA

module NPDAPalindrome =

    // マーカー文字のいらない回文
    let rulebook =
        [ PDARule.create 1 (Some 'a') 1 '$' [ 'a'; '$' ]
          PDARule.create 1 (Some 'a') 1 'a' [ 'a'; 'a' ]
          PDARule.create 1 (Some 'a') 1 'b' [ 'a'; 'b' ]
          PDARule.create 1 (Some 'b') 1 '$' [ 'b'; '$' ]
          PDARule.create 1 (Some 'b') 1 'a' [ 'b'; 'a' ]
          PDARule.create 1 (Some 'b') 1 'b' [ 'b'; 'b' ]
          PDARule.create 1 None 2 '$' [ '$' ]
          PDARule.create 1 None 2 'a' [ 'a' ]
          PDARule.create 1 None 2 'b' [ 'b' ]
          PDARule.create 2 (Some 'a') 2 'a' []
          PDARule.create 2 (Some 'b') 2 'b' []
          PDARule.create 2 None 3 '$' [ '$' ] ]

    [<Tests>]
    let ``npda read string`` =
        test "npda read string" {
            let config = PDAConfig.create 1 [ '$' ]
            let npda = NPDA.create (Set.ofList [ config ]) (Set.ofList [ 3 ]) rulebook
            Expect.isTrue (NPDA.isAccepting npda) ""

            let npda = NPDA.readString "abb" npda
            Expect.isFalse (NPDA.isAccepting npda) ""

            let npda = NPDA.readString "a" npda
            Expect.isTrue (NPDA.isAccepting npda) ""
        }

    [<Tests>]
    let ``npda design`` =
        test "npda design" {
            let design = NPDADesign.create 1 '$' (Set.ofList [ 3 ]) rulebook
            Expect.isTrue (NPDADesign.accepts "abba" design) ""
            Expect.isTrue (NPDADesign.accepts "babbaabbab" design) ""
            Expect.isFalse (NPDADesign.accepts "abb" design) ""
            Expect.isFalse (NPDADesign.accepts "baabaa" design) ""
        }
