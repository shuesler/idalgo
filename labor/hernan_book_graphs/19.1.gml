Creator	"yFiles"
Version	"2.15"
graph
[
	hierarchic	1
	label	""
	directed	1
	node
	[
		id	0
		label	"L0"
		graphics
		[
			x	180.0
			y	390.0
			w	30.0
			h	30.0
			type	"rectangle"
			raisedBorder	0
			fill	"#FFCC00"
			outline	"#000000"
		]
		LabelGraphics
		[
			text	"L0"
			fontSize	12
			fontName	"Dialog"
			model	"null"
		]
	]
	node
	[
		id	1
		label	"A0"
		graphics
		[
			x	300.0
			y	390.0
			w	30.0
			h	30.0
			type	"rectangle"
			raisedBorder	0
			fill	"#FFCC00"
			outline	"#000000"
		]
		LabelGraphics
		[
			text	"A0"
			fontSize	12
			fontName	"Dialog"
			model	"null"
		]
	]
	node
	[
		id	2
		label	"L1"
		graphics
		[
			x	420.0
			y	390.0
			w	30.0
			h	30.0
			type	"rectangle"
			raisedBorder	0
			fill	"#FFCC00"
			outline	"#000000"
		]
		LabelGraphics
		[
			text	"L1"
			fontSize	12
			fontName	"Dialog"
			model	"null"
		]
	]
	node
	[
		id	3
		label	"A1"
		graphics
		[
			x	540.0
			y	390.0
			w	30.0
			h	30.0
			type	"rectangle"
			raisedBorder	0
			fill	"#FFCC00"
			outline	"#000000"
		]
		LabelGraphics
		[
			text	"A1"
			fontSize	12
			fontName	"Dialog"
			model	"null"
		]
	]
	node
	[
		id	4
		label	"Y"
		graphics
		[
			x	660.0
			y	390.0
			w	30.0
			h	30.0
			type	"rectangle"
			raisedBorder	0
			fill	"#FFCC00"
			outline	"#000000"
		]
		LabelGraphics
		[
			text	"Y"
			fontSize	12
			fontName	"Dialog"
			model	"null"
		]
	]
	node
	[
		id	5
		label	"U0"
		graphics
		[
			x	180.0
			y	510.0
			w	30.0
			h	30.0
			type	"ellipse"
			raisedBorder	0
			fill	"#FFCC00"
			outline	"#000000"
		]
		LabelGraphics
		[
			text	"U0"
			fontSize	12
			fontName	"Dialog"
			model	"null"
		]
	]
	node
	[
		id	6
		label	"U1"
		graphics
		[
			x	420.0
			y	510.0
			w	30.0
			h	30.0
			type	"ellipse"
			raisedBorder	0
			fill	"#FFCC00"
			outline	"#000000"
		]
		LabelGraphics
		[
			text	"U1"
			fontSize	12
			fontName	"Dialog"
			model	"null"
		]
	]
	edge
	[
		source	1
		target	2
		graphics
		[
			fill	"#000000"
			targetArrow	"standard"
		]
	]
	edge
	[
		source	1
		target	3
		graphics
		[
			type	"arc"
			fill	"#000000"
			targetArrow	"standard"
			arcType	"fixedRatio"
			arcHeight	60.0
			arcRatio	1.0
			Line
			[
				point
				[
					x	300.0
					y	390.0
				]
				point
				[
					x	420.0
					y	330.0
				]
				point
				[
					x	540.0
					y	390.0
				]
			]
		]
	]
	edge
	[
		source	1
		target	4
		graphics
		[
			type	"arc"
			fill	"#000000"
			targetArrow	"standard"
			arcType	"fixedRatio"
			arcHeight	90.0
			arcRatio	1.0
			Line
			[
				point
				[
					x	300.0
					y	390.0
				]
				point
				[
					x	480.0
					y	300.0
				]
				point
				[
					x	660.0
					y	390.0
				]
			]
		]
	]
	edge
	[
		source	0
		target	4
		graphics
		[
			type	"arc"
			fill	"#000000"
			targetArrow	"standard"
			arcType	"fixedRatio"
			arcHeight	120.0
			arcRatio	1.0
			Line
			[
				point
				[
					x	180.0
					y	390.0
				]
				point
				[
					x	420.0
					y	270.0
				]
				point
				[
					x	660.0
					y	390.0
				]
			]
		]
	]
	edge
	[
		source	0
		target	2
		graphics
		[
			type	"arc"
			fill	"#000000"
			targetArrow	"standard"
			arcType	"fixedRatio"
			arcHeight	60.0
			arcRatio	1.0
			Line
			[
				point
				[
					x	180.0
					y	390.0
				]
				point
				[
					x	300.0
					y	330.0
				]
				point
				[
					x	420.0
					y	390.0
				]
			]
		]
	]
	edge
	[
		source	1
		target	6
		graphics
		[
			type	"bezier"
			fill	"#000000"
			targetArrow	"standard"
			Line
			[
				point
				[
					x	300.0
					y	390.0
				]
				point
				[
					x	360.0
					y	505.0
				]
				point
				[
					x	360.0
					y	510.0
				]
				point
				[
					x	420.0
					y	510.0
				]
			]
		]
	]
	edge
	[
		source	6
		target	4
		graphics
		[
			type	"bezier"
			fill	"#000000"
			targetArrow	"standard"
			Line
			[
				point
				[
					x	420.0
					y	510.0
				]
				point
				[
					x	629.0
					y	510.0
				]
				point
				[
					x	660.0
					y	450.0
				]
				point
				[
					x	660.0
					y	390.0
				]
			]
		]
	]
	edge
	[
		source	6
		target	2
		graphics
		[
			fill	"#000000"
			targetArrow	"standard"
		]
	]
	edge
	[
		source	5
		target	6
		graphics
		[
			fill	"#000000"
			targetArrow	"standard"
		]
	]
	edge
	[
		source	5
		target	4
		graphics
		[
			type	"bezier"
			fill	"#000000"
			targetArrow	"standard"
			Line
			[
				point
				[
					x	180.0
					y	510.0
				]
				point
				[
					x	180.0
					y	450.0
				]
				point
				[
					x	660.0
					y	450.0
				]
				point
				[
					x	660.0
					y	390.0
				]
			]
		]
	]
	edge
	[
		source	3
		target	4
		graphics
		[
			fill	"#000000"
			targetArrow	"standard"
		]
	]
	edge
	[
		source	5
		target	0
		graphics
		[
			fill	"#000000"
			targetArrow	"standard"
		]
	]
	edge
	[
		source	5
		target	2
		graphics
		[
			type	"bezier"
			fill	"#000000"
			targetArrow	"standard"
			Line
			[
				point
				[
					x	180.0
					y	510.0
				]
				point
				[
					x	360.0
					y	510.0
				]
				point
				[
					x	360.0
					y	390.0
				]
				point
				[
					x	420.0
					y	390.0
				]
			]
		]
	]
	edge
	[
		source	5
		target	4
		graphics
		[
			type	"arc"
			fill	"#000000"
			targetArrow	"standard"
			arcType	"fixedRatio"
			arcHeight	-109.3835678100586
			arcRatio	-0.884313702583313
			Line
			[
				point
				[
					x	180.0
					y	510.0
				]
				point
				[
					x	446.5294189453125
					y	556.11767578125
				]
				point
				[
					x	660.0
					y	390.0
				]
			]
		]
	]
]
