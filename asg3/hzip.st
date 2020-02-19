"(1 to: Smalltalk getArgc) do: [ :i |
  (Smalltalk getArgv: i) displayNl
]"

FileStream extend [
   nl := Character nl.
   catFile [
      |count|
      count := 0.
      [self atEnd not] whileTrue: [
         |padded|
         count := count + 1.
         padded := count printPaddedWith: Character space to: 6.
         stdout << padded << ': ' << self nextLine << nl.
      ]
   ]
].

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

|filename|
filename := (Smalltalk arguments at: 2).
|infile|
infile := FileStream open: filename
                    mode: FileStream read.


freqtable := ZeroArray new: 257.

[infile atEnd not] whileTrue: [
   |ordChar|
   ordChar := infile next asInteger.
   freqtable incr: ordChar.
].
freqtable at: 256 put: 1.

infile close.

freqtable keysAndValuesDo: [:key :value |
   (value notNil and: [value > 0]) ifTrue: [
      stdout << 'freq[' << key asCharacter visible << ']=' << value
             << nl.
   ]
].

"==============================================================================="

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

].


Leaf subclass: Tree [
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

].

sortcol := SortedCollection new.

freqtable keysAndValuesDo: [:key :value |
    (value notNil and: [value > 0]) ifTrue: [
        temp := Leaf new: key count: value.
        sortcol add: temp.
    ]
].

stdout << nl << 'Before sortcol do: loop' << nl.
sortcol do: [:item |
   stdout << item << nl.
].

|full_tree|

stdout << nl << 'Before remove loop.' << nl.
[sortcol notEmpty] whileTrue: [
    |first second t|
    first := sortcol removeFirst.
    sortcol notEmpty
        ifTrue: [
            second := sortcol removeFirst.
            t := Tree new: (first char) count: ((first count) + (second count)) left: first right: second.
            sortcol add: t.
        ]
        ifFalse: [
            full_tree := first.
        ]
].

stdout << full_tree << nl.
encoding_string := Set new.

|tree_dfs_size|
tree_dfs_size := 0.
stdout << nl << 'Before visiting full_tree Tree' << nl.
full_tree depthFirst: [:char :string |
    stdout << '[' << char << ']=' << string << nl.
    encoding_string add: string.
    tree_dfs_size := tree_dfs_size + 1.
] prefix: ''.

stdout << encoding_string << nl.

stdout << option << nl.

option = '-t'
    ifTrue: [
        stdout << 'yay' << nl.
    ]