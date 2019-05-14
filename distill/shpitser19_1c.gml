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
		label	"W"
		graphics
		[
			x	120.0
			y	180.0
			w	30.0
			h	30.0
			type	"ellipse"
			raisedBorder	0
			fill	"#FFCC00"
			outline	"#000000"
		]
		LabelGraphics
		[
			text	"W"
			fontSize	12
			fontName	"Dialog"
			model	"null"
		]
	]
	node
	[
		id	1
		label	"M"
		graphics
		[
			x	240.0
			y	180.0
			w	30.0
			h	30.0
			type	"ellipse"
			raisedBorder	0
			fill	"#FFCC00"
			outline	"#000000"
		]
		LabelGraphics
		[
			text	"M"
			fontSize	12
			fontName	"Dialog"
			model	"null"
		]
	]
	node
	[
		id	2
		label	"Y"
		graphics
		[
			x	360.0
			y	180.0
			w	30.0
			h	30.0
			type	"ellipse"
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
		id	3
		label	"A"
		graphics
		[
			x	180.0
			y	240.0
			w	30.0
			h	30.0
			type	"ellipse"
			raisedBorder	0
			fill	"#FFCC00"
			outline	"#000000"
		]
		LabelGraphics
		[
			text	"A"
			fontSize	12
			fontName	"Dialog"
			model	"null"
		]
	]
	node
	[
		id	4
		label	"Z"
		graphics
		[
			x	300.0
			y	240.0
			w	30.0
			h	30.0
			type	"ellipse"
			raisedBorder	0
			fill	"#FFCC00"
			outline	"#000000"
		]
		LabelGraphics
		[
			text	"Z"
			fontSize	12
			fontName	"Dialog"
			model	"null"
		]
	]
	node
	[
		id	5
		label	"U"
		graphics
		[
			x	300.0
			y	120.0
			w	30.0
			h	30.0
			type	"ellipse"
			raisedBorder	0
			fill	"#FFCC00"
			outline	"#000000"
		]
		LabelGraphics
		[
			text	"U"
			fontSize	12
			fontName	"Dialog"
			model	"null"
		]
	]
	edge
	[
		source	0
		target	3
		graphics
		[
			fill	"#000000"
			targetArrow	"standard"
		]
	]
	edge
	[
		source	0
		target	1
		graphics
		[
			fill	"#000000"
			targetArrow	"standard"
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
			arcHeight	30.0
			arcRatio	0.5
			Line
			[
				point
				[
					x	120.0
					y	180.0
				]
				point
				[
					x	240.0
					y	150.0
				]
				point
				[
					x	360.0
					y	180.0
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
			arcHeight	0.0
			arcRatio	0.0
			Line
			[
				point
				[
					x	120.0
					y	180.0
				]
				point
				[
					x	210.0
					y	210.0
				]
				point
				[
					x	300.0
					y	240.0
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
		source	3
		target	1
		graphics
		[
			fill	"#000000"
			targetArrow	"standard"
		]
	]
	edge
	[
		source	3
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
		target	4
		graphics
		[
			fill	"#000000"
			targetArrow	"standard"
		]
		edgeAnchor
		[
			xTarget	0.7333333333333333
		]
	]
	edge
	[
		source	4
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
		target	1
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
			fill	"#000000"
			targetArrow	"standard"
		]
	]
]
