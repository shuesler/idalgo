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
		id	1
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
		id	2
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
		id	3
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
		id	4
		label	"U1"
		graphics
		[
			x	420.0
			y	480.0
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
	node
	[
		id	5
		label	"W0"
		graphics
		[
			x	300.0
			y	480.0
			w	30.0
			h	30.0
			type	"rectangle"
			raisedBorder	0
			fill	"#FFCC00"
			outline	"#000000"
		]
		LabelGraphics
		[
			text	"W0"
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
		source	4
		target	1
		graphics
		[
			fill	"#000000"
			targetArrow	"standard"
		]
	]
	edge
	[
		source	4
		target	3
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
					y	480.0
				]
				point
				[
					x	633.0
					y	480.0
				]
				point
				[
					x	633.0
					y	435.0
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
		source	5
		target	0
		graphics
		[
			type	"bezier"
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
]
