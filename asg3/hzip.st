#!/afs/cats.ucsc.edu/courses/cse112-wm/usr/smalltalk/bin/gst -f

nl := Character nl.
Character extend [
   isPrint [
      ^ (Character space <= self) & (self <= $~)
   ]
   visible [
      self isPrint ifTrue: [^ '$', self asString]
                   ifFalse: [^ self asInteger printStringRadix: 8]
   ]
].

Object subclass: ZeroArray [
   |theArray|

   ZeroArray class >> new [
      self shouldNotImplement.
   ]

   ZeroArray class >> new: size [
      |result|
      result := super new.
      result init: size.
      ^ result
   ]

   init: size [
      theArray := Array new: size.
   ]

   size [ ^ theArray size.  ]

   at: index [
      ^ theArray at: index + 1.
   ]

   at: index put: value [
      ^ theArray at: index + 1 put: value.
   ]

   incr: index [
      (self at: index) isNil
         ifTrue: [ self at: index put: 0].
      self at: index put: (self at: index) + 1.
   ]

   keysAndValuesDo: aBlock [
      (0 to: (self size) - 1) do: [:index |
         aBlock value: index value: (self at: index).
      ]
   ]

].

nl := Character nl.

Object subclass: Leaf [
   |char count|
   char [ ^ char ]
   count [ ^ count ]

   Leaf class >> new [
      self shouldNotImplement
   ]

   Leaf class >> new: aChar count: aCount [
      |result|
      result := super new.
      result setChar: aChar andCount: aCount.
      ^result
   ]

   setChar: aChar andCount: aCount [
      char := aChar.
      count := aCount.
   ]

   <= other [
      ^ (count < other count)
      | ((count = other count) & (char <= other char))
   ]

   printBase: aStream [
      ^ aStream << self class << '(' << char << ',' << count
   ]

   printOn: aStream [
      (self printBase: aStream) << ')'.
   ]

   depthFirst: visitor prefix: string [
      visitor value: char value: string.
   ]

   postOrder: visitor prefix: string [
      visitor value: char value: string.
   ]

].


Leaf subclass: Tree [
    nl := Character nl.
    |left right|

    Tree class >> new: aChar count: aCount [
        self shouldNotImplement
    ]

    Tree class >> new: aChar count: aCount left: aLeft right: aRight [
        |result|
        result := super new: aChar count: aCount.
        result setLeft: aLeft andRight: aRight.
        ^ result
    ]

    setLeft: aLeft andRight: aRight [
        left := aLeft.
        right := aRight.
    ]

    printOn: aStream [
        (self printBase: aStream) << ',' << left << ',' << right << ')'.
    ]

    depthFirst: visitor prefix: string [
        left depthFirst: visitor prefix: string, '0'.
        right depthFirst: visitor prefix: string, '1'.
    ]

    postOrder: visitor prefix: string [
        |selfNode|
        selfNode := 'Node'.
        left postOrder: visitor prefix: string, '0'.
        right postOrder: visitor prefix: string, '1'.
        visitor value: 1 value: 'Node'.
    ]

].

Character extend [
   isGraph [
      ^ (Character space < self) & (self <= $~)
   ]
   visible [
      self isGraph ifTrue: [^ '$', self asString]
                   ifFalse: [^ self asInteger printStringRadix: 16]
   ]
]

Object subclass: BitStream [
   |bitIndex byte myStream|
   BitStream class >> new: fileStream [
      |result|
      result := super new.
      result init: fileStream.
      ^result
   ]
   init: fileStream [
      myStream := fileStream.
      bitIndex := 1.
   ]
   nextBit [
      bitIndex = 1 ifTrue: [
         byte := myStream next.
         bitIndex := 9.
      ].
      bitIndex := bitIndex - 1.
      ^byte value bitAt: bitIndex
   ]
   atEnd [
      ^bitIndex = 1 and: [myStream atEnd]
   ]
   currByte [
      ^byte
   ]
].

Object subclass: OutBits [
   |bitIndex currentByte myStream|
   OutBits class >> new [
      self shouldNotImplement.
   ]
   OutBits class >> new: fileStream [
      |result|
      result := super new.
      result init: fileStream.
      ^result
   ]
   clearByte [
      bitIndex := 8.
      currentByte := 0.
   ]
   init: fileStream [
      myStream := fileStream.
      self clearByte.
   ]
   flushByte [
      bitIndex = 8 ifFalse: [
         myStream nextPutByte: currentByte.
         self clearByte.
      ]
   ]
   writeBit: bit [
      currentByte := currentByte bitAt: bitIndex put: bit.
      bitIndex := bitIndex - 1.
      bitIndex = 0 ifTrue: [self flushByte].
   ]
].

"==============================================================="

parameters := Set new.
parameters add: '-c'; add: '-u'; add: '-t'; add: '-d'.
|option|

nl := Character nl.
Smalltalk arguments size <= 1
    ifTrue: [ObjectMemory quit: 1]
    ifFalse: [
        Smalltalk arguments size > 3
            ifTrue: [ObjectMemory quit: 1]
            ifFalse: [
                (parameters includes: (Smalltalk arguments first))
                    ifTrue: [option := (Smalltalk arguments first)]
                    ifFalse: [ObjectMemory quit: 1]
            ]
    ].


|filename|
filename := (Smalltalk arguments at: 2).
|infile|
infile := FileStream open: filename
                    mode: FileStream read.


freqtable := ZeroArray new: 257.
"
nl := Character nl.
stdinBits := BitStream new: infile.
count := 0.
[stdinBits atEnd] whileFalse: [
    |rando|
    stdout << stdinBits nextBit.
    count := count + 1.
    count = 4 ifTrue: [stdout << ' '].
    count = 8 ifTrue: [
        stdout << ' ' << stdinBits currByte visible
            << ' ' << stdinBits currByte asCharacter visible << nl.
        inByte := stdinBits currByte.
        rando := 
        count := 0
    ].
].
"    
[infile atEnd not] whileTrue: [
    |ordChar|
    ordChar := infile next asInteger.
    freqtable incr: ordChar.
].
freqtable at: 256 put: 1.

infile close.
"
freqtable keysAndValuesDo: [:key :value |
   (value notNil and: [value > 0]) ifTrue: [
      stdout << 'freq[' << key asCharacter visible << ']=' << value
             << nl.
   ]
].
"
sortcol := SortedCollection new.

freqtable keysAndValuesDo: [:key :value |
    (value notNil and: [value > 0]) ifTrue: [
        temp := Leaf new: key count: value.
        sortcol add: temp.
    ]
].
"
stdout << nl << 'Before sortcol do: loop' << nl.
sortcol do: [:item |
   stdout << item << nl.
].
"
|decoding_tree|

"stdout << nl << 'Before remove loop.' << nl."
[sortcol notEmpty] whileTrue: [
    |first second t|
    first := sortcol removeFirst.
    sortcol notEmpty
        ifTrue: [
            second := sortcol removeFirst.
            t := Tree new: (first char) 
                count: ((first count) + (second count))
                left: first right: second.
            sortcol add: t.
        ]
        ifFalse: [
            decoding_tree := first.
        ]
].

"stdout << decoding_tree << nl."
encoding_set := Set new.
encoding_dictionary := Dictionary new.

|tree_dfs_size|
tree_dfs_size := 0.
"stdout << nl << 'Before visiting decoding_tree Tree' << nl."
decoding_tree depthFirst: [:char :string |
    "stdout << '[' << char << ']=' << string << nl."
    encoding_set add: string.
    encoding_dictionary at: char put: string asNumber.
    tree_dfs_size := tree_dfs_size + 1.
] prefix: ''.

"stdout << encoding_set << nl.
stdout << encoding_dictionary << nl.

stdout << nl << option << nl << nl."

"=================================================="

|encoding_string node_count fileBits|
encoding_string := ''.
node_count := 0.
bitArray := Array new: 200. "99999999"
option = '-c'
    ifTrue: [
        |infile filename stdinBits countZero count eCount inByte|
        decoding_tree postOrder: [:char :string |
            "stdout << char << ' ' << string << ' ' 
                << string size << nl."
            string = 'Node'
                ifTrue: [
                    t_bin := 1 bitAt: 1.
                    temp_bin := 1 asString.
                    encoding_string := encoding_string, temp_bin.

                    node_count := node_count + 1.
                    bitArray at: node_count put: t_bin.
                ]
                ifFalse: [
                    |eight seven six five four three two one zero|
                    eight := char bitAt: 9.
                    seven := char bitAt: 8.
                    six := char bitAt: 7.
                    five := char bitAt: 6.
                    four := char bitAt: 5.
                    three := char bitAt: 4.
                    two := char bitAt: 3.
                    one := char bitAt: 2.
                    zero := char bitAt: 1.

                    temp_bin := eight asString, seven asString, 
                        six asString,
                        five asString, four asString, three asString,
                        two asString, one asString, zero asString.

                    "stdout << eight << seven << six << five << 
                        four <<
                        three << two << one << zero << nl."
                    encoding_string := encoding_string, temp_bin.

                    node_count := node_count + 1.
                    bitArray at: node_count put: eight.
                    node_count := node_count + 1.
                    bitArray at: node_count put: seven.
                    node_count := node_count + 1.
                    bitArray at: node_count put: six.
                    node_count := node_count + 1.
                    bitArray at: node_count put: five.
                    node_count := node_count + 1.
                    bitArray at: node_count put: four.
                    node_count := node_count + 1.
                    bitArray at: node_count put: three.
                    node_count := node_count + 1.
                    bitArray at: node_count put: two.
                    node_count := node_count + 1.
                    bitArray at: node_count put: one.
                    node_count := node_count + 1.
                    bitArray at: node_count put: zero.
                ].

            "stdout << encoding_string << nl."
        ] prefix: ''.
        x := 1.
        temp_bin := x bitAt: 1.
        encoding_string := encoding_string, temp_bin asString.
        stdout << encoding_string << nl << nl.

        node_count := node_count + 1.
        bitArray at: node_count put: temp_bin.



        outfilename := (Smalltalk arguments at: 3).
        outfile := FileStream open: outfilename
                            mode: FileStream write.

        outEncoding := OutBits new: outfile.

        "stdout << bitArray << nl << nl."

        countZero := 0.
        bitArray do: [:bit | 
            bit ~= nil
            ifTrue: [
                outEncoding writeBit: bit.
                countZero := countZero + 1.
                
                countZero = 8 ifTrue: [
                    outEncoding flushByte.
                    countZero := 0.
                ].
            ].
        ].
        outEncoding flushByte.
        
    



        "++++++++++++++++++++++++++++++++++++++++++"

        filename := (Smalltalk arguments at: 2).
        infile := FileStream open: filename
                            mode: FileStream read.
        
        nl := Character nl.
        stdinBits := BitStream new: infile.
        count := 0.
        eCount := 0.
        encodedArray := Array new: 200.
        byteArray := Array new: 8.
        outBytes := OutBits new: outfile.
        [stdinBits atEnd] whileFalse: [
            |oneBit|
            oneBit := stdinBits nextBit.
            "stdout << oneBit."
            count := count + 1.

            count = 4 ifTrue: ["stdout << ' '"].
            count = 8 ifTrue: [
                |numberBit tempString tsize|
                "stdout << ' ' << stdinBits currByte visible
                    << ' ' << stdinBits currByte asInteger << nl."


                inByte := stdinBits currByte asInteger.
                encodedByte := encoding_dictionary at: inByte.
                "stdout << encodedByte << nl."
                
                numberBit := encodedByte bitAt: 1.
                "stdout << numberBit << nl."
                "byteArray do: [:bit | outBytes writeBit: bit]."
                outBytes flushByte.
                "
                outBytes := OutBits new: outfile.
                encodedByte do: [:bit| outBytes writeBit: bit].
                outBytes flushByte.
                "
            
                count := 0.

                "stdout << byteArray << nl."
            ].
        ].

        infile close.
        outfile close.
    ].
