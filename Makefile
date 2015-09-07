dummy:
	scribble --htmls ++style extra.css cmsc631.scrbl

push:	
	cp -r papers/ cmsc631/papers/
	cp -r code/ cmsc631/code/
	cp -r exam/ cmsc631/exam/
	cp -r slides/ cmsc631/slides/
	rsync -avz cmsc631 umd:/fs/www/class/fall2015
