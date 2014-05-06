dummy:
	scribble --htmls ++style extra.css cmsc631.scrbl

push:	
	cp -r papers/ cmsc631/papers/
	cp -r exam/ cmsc631/exam/
	rsync -avz cmsc631 umd:/fs/www/class/spring2014
